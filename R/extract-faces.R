library(curl)
library(rvest)
library(purrr)
library(survivoR)
library(tidyverse)

# # Start by reading a HTML page with read_html():
# page <- read_html("https://www.truedorktimes.com/survivor/cast/season.htm") |> 
#   html_elements(".card")
# 
# page |> 
#   # html_table() |> 
#   walk(~{
#     season <- html_element(.x, 'strong') |> 
#       html_text()
#     
#     print(season)
#     
#     contestants <- html_elements(.x, 'img')
#     
#     print(contestants)
#   })

url <- 'http://gradientdescending.com/survivor/castaways0/'

imgs <- survivoR::castaways |> 
  filter(version == 'US',
         season >= 40) |> 
  transmute(
    season, castaway_id, castaway,
    url_suffix = paste0(
      's', season, str_sub(castaway_id, 3, 6), '.png'
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