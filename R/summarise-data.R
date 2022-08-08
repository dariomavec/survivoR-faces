#' Process season
#'
#' @return
#' @export
#'
#' @import tidyr dplyr stringr readr glue
#' @importFrom jsonlite read_json
process_json <- function(season) {
  current <- read_rds('db/fact/screen_time.rds') |> 
    filter(version_season != str_to_upper(season))
  
  eps <- read_json(glue('db/raw/{season}.json'), simplifyVector = TRUE) |> 
    as_tibble() |> 
    transmute(
      version_season = str_to_upper(str_extract(file, '^[a-z]+[0-9]+')),
      episode = as.numeric(str_extract(file, '(?<=[0-9]E)[0-9]+')),
      episode_ts = as.integer(str_extract(file, '(?<=_)[0-9]+')),
      screen_time = episode_ts[1],
      castaway_id = map(faces, str_extract, pattern = '(host)|.*(?=\\-)')
    ) |> 
    arrange(season, episode, episode_ts)
  
  all_time <- eps |> 
    group_by(version_season, episode, screen_time) |> 
    summarise(max_episode = max(episode_ts)) |> 
    expand(episode_ts = seq(screen_time, max_episode, by = screen_time),
           screen_time = screen_time) |> 
    ungroup()
  
  output <- all_time |> 
    left_join(eps, by = c('version_season', 'episode', 'episode_ts', 'screen_time')) |> 
    group_by(version_season) |> 
    mutate(season_ts = cumsum(screen_time)) |> 
    unnest(castaway_id, keep_empty = TRUE) |> 
    ungroup() |> 
    bind_rows(current) |>
    arrange(version_season, episode, episode_ts) |> 
    select(
      version_season,
      season_ts,
      episode,
      episode_ts,
      castaway_id,
      screen_time
    )
    
  save(output, file = 'db/fact/screen_time.rda')
  
  return(output)
}


#' Plot episodes
#'
#' @return
#' @export
#'
#' @import ggplot2
plot_eps <- function(season) {
  read_rds(glue('db/fact/{season}_screen_time.rds')) |> 
    filter(!is.na(castaway_id)) |> 
    rename(vid_episode = episode) |> 
    left_join(survivoR::castaways, by = c("version_season", "castaway_id")) |> 
    group_by(castaway_id) |> 
    mutate(total_screen_time = cumsum(screen_time)) |> 
    ggplot(aes(season_ts, 
               total_screen_time, 
               colour = factor(episode),
               group = castaway_id)) +
    geom_line()
}

