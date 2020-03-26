library(rstudioapi)
library(tidyverse)
library(dplyr)
library(rvest)
library(rlist)
library(tidytext)
source('transcriptTED.R')


setwd(dirname(getSourceEditorContext()$path))

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

# URL of the playlists
#"https://www.ted.com/playlists/126/the_big_picture",
#"https://www.ted.com/playlists/78/climate_change_oh_it_s_real",
#"https://www.ted.com/playlists/154/how_do_you_solve_a_problem_lik",
#"https://www.ted.com/playlists/493/why_climate_change_is_a_human",
#"https://www.ted.com/playlists/634/a_day_trip_to_antarctica",
#"https://www.ted.com/playlists/439/what_is_the_anthropocene",
#"https://www.ted.com/playlists/151/earth_appreciated",
#"https://www.ted.com/playlists/142/the_forecast_calls_for"

playlisturl <- c("https://www.ted.com/playlists/126/the_big_picture",
                 "https://www.ted.com/playlists/78/climate_change_oh_it_s_real",
                 "https://www.ted.com/playlists/154/how_do_you_solve_a_problem_lik",
                 "https://www.ted.com/playlists/493/why_climate_change_is_a_human",
                 "https://www.ted.com/playlists/634/a_day_trip_to_antarctica",
                 "https://www.ted.com/playlists/439/what_is_the_anthropocene",
                 "https://www.ted.com/playlists/151/earth_appreciated",
                 "https://www.ted.com/playlists/142/the_forecast_calls_for")

fulllist <- vector()

for (val in 1:length(playlisturl)) {
  
htmlpage <- playlisturl[val] %>% read_html()
htmlpage %>% write_html(paste("playlist", as.character(val),".html",sep = ""))

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

fulllist <- append(fulllist,videolist)
}

fulllist <- unique(fulllist)

trnslist <- vector()
for (val in fulllist) {
  temp <- val %>% transcriptTED()
  
  trnslist <- trnslist %>% list.append(temp)
}

strn <- "TED.com translations are made possible by volunteer translators. Learn more about the Open Translation Project."
trnslist <- trnslist[-grep(pattern = strn, x = trnslist)]

strn <- "© TED Conferences, LLC. All rights reserved."
trnslist <- trnslist %>% gsub(pattern = strn,replacement = "©")

trnslist <- split(trnslist, cumsum(trnslist == "©"))
trnslist <- trnslist[-length(trnslist)]

# trnslist %>% saveRDS(file = "texttranscription.rds")

trnslist %>% glimpse

dfspeech <- data.frame()

for (val in 1:length(trnslist)) {
  dfspeech <- rbind(dfspeech, trnslist[val] %>% as.data.frame(col.names = "text") %>% cbind(speech = as.character(val)))
  print(val)
}


dfwords <- dfspeech %>%
  unnest_tokens(word, text) %>%
  count(speech, word, sort = T)

total_words <- dfwords %>% 
  group_by(speech) %>% 
  summarize(total = sum(n))

dfwords <- left_join(dfwords, total_words)

dfwords <- dfwords %>%
  bind_tf_idf(word, speech, n)

N <- 5000

dfwords %>%
  arrange(desc(tf_idf)) %>%
  head(N) %>%
  write_csv("unigramtfidfTED.csv")

# same with bigrams

dfbigrams <- dfspeech %>%
  unnest_tokens(bigram, text, token = 'ngrams', n =2)

# count
# dfbigrams %>%
# count(bigram, sort = T)

# filter stopwords

dfbisep <- dfbigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dfbifiltered <- dfbisep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

dfbifiltered %>%
  count(word1, word2, sort = T) %>%
  write_csv("bigramsfilteredTED.csv")

# tf_idf
N <- 5000

bigr_tf_idf <- dfbigrams %>%
  count(speech, bigram) %>%
  bind_tf_idf(bigram, speech, n) %>%
  arrange(desc(tf_idf))

bigr_tf_idf %>% 
  head(N) %>%
  write_csv("bigramstfidfTED.csv")
