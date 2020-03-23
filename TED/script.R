library(rstudioapi)
library(tidyverse)
library(dplyr)
library(rvest)
library(rlist)
source('transcriptTED.R')


setwd(dirname(getSourceEditorContext()$path))

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

# URL of the playlist
# "https://www.ted.com/playlists/78/climate_change_oh_it_s_real"
# "https://www.ted.com/playlists/126/the_big_picture"

playlisturl <- "https://www.ted.com/playlists/78/climate_change_oh_it_s_real"

htmlpage <- playlisturl %>% read_html()
htmlpage %>% write_html("playlist.html")

# scrape list of videos from playlist
videolist <- htmlpage %>% html_nodes('body') %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  as.vector() %>%
  gsub(pattern = '\\?(.*)', replacement = "")
  
# clean list of video
videolist <- videolist[grep(x = videolist, pattern = '/talks/')]
videolist <- videolist[-1]
videolist <- paste('https://www.ted.com/', videolist, '/transcript', sep = "")

trnslist <- vector()

for (val in videolist) {
  temp <- val %>% transcriptTED()
  
  trnslist <- trnslist %>% list.append(temp)
}

strn <- "TED.com translations are made possible by volunteer translators. Learn more about the Open Translation Project."
trnslist <- trnslist[-grep(pattern = strn, x = trnslist)]

strn <- "© TED Conferences, LLC. All rights reserved."
trnslist <- trnslist %>% gsub(pattern = strn,replacement = "©")

trnslist <- split(trnslist, cumsum(trnslist == "©"))
trnslist <- trnslist[-length(trnslist)]

trnslist %>% saveRDS(file = "texttranscription.rds")



