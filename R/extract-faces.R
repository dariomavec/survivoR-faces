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
      folder = paste0('us', season),
      file = paste0(castaway_id, '-', str_to_lower(castaway), '.png'),
      dl = pmap(list(folder, file, url_suffix), ~{
        dir.create(..1, showWarnings = FALSE)
        curl::curl_download(paste0(url, ..3),
                            paste0(..1, '/', ..2))
        
        return(paste0(url, ..3))
      })
    )
}

