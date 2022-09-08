#' Process season
#'
#' @return
#' @export
#'
#' @import tidyr dplyr stringr readr glue
#' @importFrom jsonlite read_json
process_json <- function(season, save_to_fact = FALSE, block_size = 5) {
  load('data/screen_time.rda')

  current <- screen_time |>
    filter(version_season != str_to_upper(season))
  
  eps <- read_json(glue('db/raw/{season}.json'), simplifyVector = TRUE) |> 
    as_tibble() |> 
    transmute(
      version_season = str_to_upper(str_extract(file, '^[a-z]+[0-9]+')),
      episode = as.numeric(str_extract(file, '(?<=[0-9]E)[0-9]+')),
      episode_ts = as.integer(str_extract(file, '(?<=_)[0-9]+')),
      castaway_id = map(faces, str_extract, pattern = '(unknown)|(host)|.*(?=\\-)')
    ) |> 
    group_by(version_season, episode) |> 
    mutate(
      screen_time = min(episode_ts - lag(episode_ts), na.rm = T)
    ) |> 
    ungroup() |> 
    arrange(version_season, episode, episode_ts) 
  
  all_time <- eps |> 
    group_by(version_season, episode, screen_time) |> 
    summarise(max_episode = max(episode_ts)) |> 
    expand(episode_ts = seq(screen_time, max_episode, by = screen_time),
           screen_time = screen_time) |> 
    ungroup()
  
  screen_time <- all_time |> 
    left_join(eps, by = c('version_season', 'episode', 'episode_ts', 'screen_time')) |> 
    unnest(castaway_id, keep_empty = TRUE) |> 
    mutate(blocks = block_size * floor(episode_ts / block_size)) |> 
    count(version_season, castaway_id, episode, blocks, 
          wt = screen_time, name = 'screen_blocks') |> 
    filter(!is.na(castaway_id), !is.na(screen_blocks)) |> 
    mutate(likely_screen_time = case_when(
      # If more than 50% of a block includes screen time for the castaway then
      # we assume they're part of the whole clip
      screen_blocks / block_size >= 0.5 ~ pmax(screen_blocks, block_size),
      TRUE ~ pmin(screen_blocks, block_size)
    )) |> 
    group_by(version_season, castaway_id, episode) |>
    summarise(screen_time = sum(likely_screen_time), .groups = 'drop')
    bind_rows(current) |>
    arrange(version_season, episode, screen_time) |> 
    select(
      version_season,
      episode,
      castaway_id,
      screen_time
    ) |> 
    filter(!is.na(castaway_id))
    
  save(screen_time, file = 'data/screen_time.rda')
  
  if(save_to_fact) {
    season_screen_time <- screen_time |> 
      filter(version_season == str_to_upper(season))
    interval <- eps$screen_time[1]
    
    season_screen_time |> 
      write_rds(glue('db/fact/{season}_{interval}_screen_time.rds'))
    
  }
  
  return(screen_time)
}


#' Plot screen times
#'
#' @return
#' @export
#'
#' @import ggplot2 hrbrthemes showtext gridExtra
plot_screen_time <- function(season, interval) {
  read_rds(glue('db/fact/{season}_{interval}_screen_time.rds')) |> 
    filter(!is.na(castaway_id)) |> 
    left_join(survivoR::confessionals, by = c("version_season", "castaway_id", "episode")) |> 
    group_by(castaway) |>
    summarise(screen_time = sum(screen_time),
           confessional_count = sum(confessional_count)) |>
    filter(!is.na(castaway)) |> 
    ggplot(aes(confessional_count, 
               screen_time / 60)) +
    geom_point(aes(colour = castaway)) +
    geom_label_repel(aes(colour = castaway, label = castaway)) +
    # geom_smooth() +
    scale_y_comma() +
    scale_x_comma() +
    labs(
      x = 'Confessionals count',
      y = 'Total screen time (minutes)',
      colour = "Contestant",
      title = glue('Survivor Screen Time (Season {str_to_upper(season)})'),
      subtitle = glue('Sampling every {interval} seconds')
    ) +
    theme_ipsum_rc()
}

#' Plot screen times (agg)
#'
#' @return
#' @export
#'
#' @import ggplot2 hrbrthemes showtext gridExtra
plot_screen_time_agg <- function(season, interval) {
  read_rds(glue('db/fact/{season}_{interval}_screen_time.rds')) |> 
    filter(!is.na(castaway_id)) |> 
    rename(vid_episode = episode) |> 
    left_join(survivoR::castaways, by = c("version_season", "castaway_id")) |> 
    mutate(full_name = paste0('Ep. ', episode, ' - ', coalesce(full_name, castaway_id)) |> 
             str_wrap(width = 10)) |> 
    group_by(castaway_id, full_name, episode) |> 
    summarise(total_screen_time = sum(screen_time)) |> 
    ungroup() |> 
    mutate(full_name = forcats::fct_reorder(full_name, episode)) |> 
    ggplot(aes(full_name, 
               total_screen_time,
               fill = castaway_id)) +
    geom_bar(stat = 'identity', show.legend = FALSE) +
    scale_y_comma() +
    labs(
      x = 'Contestant',
      y = 'Total screen time (seconds)',
      title = glue('Survivor Screen Time (Season {str_to_upper(season)})'),
      subtitle = glue('Sampling every {interval} seconds')
    ) +
    # coord_flip() +
    theme_ipsum_rc()
}

#' Plot screen times (agg)
#'
#' @return
#' @export
#'
#' @import ggplot2 hrbrthemes showtext gridExtra ggrepel
plot_comparison <- function(season, intervals) {
  stopifnot(length(intervals) == 2)
  summarise_data <- function(season, interval) {
    read_rds(glue('db/fact/{season}_{interval}_screen_time.rds')) |> 
      filter(!is.na(castaway_id)) |> 
      rename(vid_episode = episode) |> 
      left_join(survivoR::castaways, by = c("version_season", "castaway_id")) |>
      mutate(full_name = coalesce(full_name, str_to_title(castaway_id))) |> 
      group_by(castaway_id, full_name) |> 
      summarise(total_screen_time = sum(screen_time),
                .groups = 'drop') |> 
      mutate(full_name = forcats::fct_reorder(full_name, total_screen_time),
             interval = interval)
  }
  
  .data <- map_dfr(intervals, ~{summarise_data(season, .x)}) |> 
    mutate(total_screen_time = total_screen_time/60) |> 
    pivot_wider(c(castaway_id, full_name), 
                names_from = interval, 
                values_from = total_screen_time) |> 
    mutate(x = !!sym(as.character(intervals[1])),
           y = !!sym(as.character(intervals[2])))
  
  .data |> 
    ggplot(aes(x, y, 
               label = full_name,
               colour = full_name)) +
    geom_abline(slope = 1, intercept = c(0, 0), linetype = 2, size = 1) +
    geom_point(show.legend = FALSE) +
    geom_label_repel(show.legend = FALSE) +
    scale_x_comma(limits = c(0, 100)) +
    scale_y_comma(limits = c(0, 100)) +
    labs(
      x = glue('Total screen time (m) sampling every {intervals[1]}s'),
      y = glue('Total screen time (m) sampling every {intervals[2]}s'),
      title = glue('Survivor Screen Time (Season {str_to_upper(season)})'),
      colour = 'Name'
    ) +
    theme_ipsum_rc()
}

#' Compare different interval
#'
#' @return
#' @import purrr
compare_intervals <- function(season, intervals, fn) {
  grobs <- map(intervals, ~{fn(season, .x)})
  
  grid.arrange(grobs = grobs, nrow = 1)
}


# compare_intervals('us42', c(1, 5), plot_screen_time)
# compare_intervals('us42', c(1, 5), plot_screen_time_agg)
