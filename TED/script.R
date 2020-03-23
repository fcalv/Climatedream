library(rstudioapi)
library(tidyverse)
library(dplyr)
library(rvest)
source('transcriptTED.R')


setwd(dirname(getSourceEditorContext()$path))

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

# URL of the playlist
playlisturl <- "https://www.ted.com/playlists/78/climate_change_oh_it_s_real"

htmlpage <- playlisturl %>% read_html()
htmlpage %>% write_html("playlist.html")

videolist <- htmlpage %>% html_nodes('body') %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  as.vector() %>%
  gsub(pattern = '\\?(.*)', replacement = "")
  

videolist <- videolist[grep(x = videolist, pattern = '/talks/')]
videolist <- videolist[-1]
videolist <- paste('https://www.ted.com/', videolist, '/transcript', sep = "")


for (val in videolist) {
  val %>% transcriptTED() %>% write("transcripts.txt", append = TRUE)
}




