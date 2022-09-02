#' Title
#'
#' @return
#' @export
#' @import curl rvest purrr survivoR tidyverse
extract_faces <- function(version, season){
  url <- 'http://gradientdescending.com/survivor/castaways/colour/'
  
  imgs <- survivoR::castaways |> 
    filter(version == !!version,
           season == !!season) |> 
    transmute(
      season, castaway_id, castaway,
      url_suffix = paste0(
        version_season, castaway_id, '.png'
      ),
      folder = paste0('img/us', season, '/cast/'),
      file = paste0(castaway_id, '-', str_to_lower(castaway), '.png'),
      dl = pmap(list(folder, file, url_suffix), ~{
        dir.create(..1, showWarnings = FALSE)
        curl::curl_download(paste0(url, ..3),
                            paste0(..1, '/', ..2))
        
        return(paste0(url, ..3))
      })
    )
}
# 
# version = 'US'
# seasons = 1:20
# 
# walk(seasons, ~{extract_faces(version, .x)})
# walk(seasons, ~{
#   dir.create(paste0('img/us', .x, '/cast/'))
#   files <- list.files(paste0('img/us', .x), pattern = '*.png')
#   file.rename(paste0('img/us', .x, '/', files), paste0('img/us', .x, '/cast/', files))
#   
# })
