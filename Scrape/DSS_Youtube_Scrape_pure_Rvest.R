#####################################
# ENVIRONMENT
#####################################
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))

library(tidyverse)
library(rvest)
library(XML)
library(jsonlite)

options(stringsAsFactors = FALSE)

#####################################
# SCRAPE FUNCTIONS
#####################################
check_length <- function(data, len){
  if(length(data) > len) data[1:len] %>% return
  else if(length(data) == len) data %>% return
  else if(length(data) < len) c(data, rep('', len-length(data))) %>% return
  else rep('', len) %>% return
}
clean <- function(vector){
  vector <- vector[!is.null(vector)]
  vector <- vector[!is.na(vector)]
  vector <- vector[vector!='NA']
  vector <- vector[vector!='']
  return(vector)
}

get_json <- function(html){
  # html <- paste0('https://www.youtube.com/watch?v=',video_id) %>% read_html
  html %>% as.character %>% write('temp.html')
  txt <- readLines('temp.html')
  
  # Find JSON
  for(i in 1:length(txt)){
    if(grepl('RELATED_PLAYER_ARGS', txt[i])) {
      json <- gsub("^ *'RELATED_PLAYER_ARGS': *|,$", '', txt[i]) %>% fromJSON
      watch_next <- json$watch_next_response %>% fromJSON
      watch_next <- watch_next$contents$twoColumnWatchNextResults$secondaryResults$secondaryResults$results
      break
    }
  }
  watch_next %>% return 
}

# WARNING - Recommendations differ each time it's run!
get_reco <- function(html){
  # html <- paste0('https://www.youtube.com/watch?v=',video_id) %>% read_html
  html %>% as.character %>% write('temp.html')
  txt <- readLines('temp.html')
  
  # Find JSON
  for(i in 1:length(txt)){
    if(grepl('RELATED_PLAYER_ARGS', txt[i])) {
      json <- gsub("^ *'RELATED_PLAYER_ARGS': *|,$", '', txt[i]) %>% fromJSON
      rvs <- json$rvs %>% fromJSON
      watch_next <- json$watch_next_response %>% fromJSON
      watch_next <- watch_next$contents$twoColumnWatchNextResults$secondaryResults$secondaryResults$results
      break
    }
  }
  watch_next$compactVideoRenderer$videoId[3:20] %>% unique %>% clean %>% return 
}

scrape_page <- function(video_id, html, csv_file, keyword_ref, iteration){
  ##### CURRENT VIDEO DATA #####
  title <- html %>% html_nodes('meta[name=title]') %>% html_attr('content')
  title <- gsub('\t','',title)
  
  description <- html %>% html_nodes('meta[name=description]') %>% html_attr('content')
  description <- gsub('\t','',description)
  
  keywords <- html %>% html_nodes('meta[name=keywords]') %>% html_attr('content')
  keywords <- gsub('\t','',keywords)
  keywords <- gsub(',', ';', keywords)
  
  type <- html %>% html_nodes('meta[property="og:type"]') %>% html_attr('content')
   
  paid <- html %>% html_nodes('meta[itemprop="paid"]') %>% html_attr('content')
  channel_id <- html %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
  duration <- html %>% html_nodes('meta[itemprop="duration"]') %>% html_attr('content')
  unlisted <- html %>% html_nodes('meta[itemprop="unlisted"]') %>% html_attr('content')
  family <- html %>% html_nodes('meta[itemprop="isFamilyFriendly"]') %>% html_attr('content')
  
  regions <- html %>% html_nodes('meta[itemprop="regionsAllowed"]') %>% html_attr('content')
  regions <- gsub(',', ';', regions)
  
  views <- html %>% html_nodes('meta[itemprop="interactionCount"]') %>% html_attr('content')
  date_publication <- html %>% html_nodes('meta[itemprop="datePublished"]') %>% html_attr('content')
  date_upload <- html %>% html_nodes('meta[itemprop="uploadDate"]') %>% html_attr('content')
  
  genre <- html %>% html_nodes('meta[itemprop="genre"]') %>% html_attr('content')
  genre <- gsub(',', ';', genre)
   
  #### JSON DATA #### 
  html %>% as.character %>% write('temp.html')
  txt <- readLines('temp.html')
  watch_next <- get_json(html)
  
  ##### RECOMMENDATION DATA #####
  # Autoplay
  next_video_id <- watch_next$compactAutoplayRenderer$contents[[1]]$compactVideoRenderer$videoId
  
  # Other recommendations
  # Alternative: watch_next$compactVideoRenderer$navigationEndpoint$watchEndpoint$videoId[3:20] %>% glimpse
  reco_video_id <- watch_next$compactVideoRenderer$videoId[3:20]
  reco_list <- c(next_video_id, reco_video_id) %>% clean()
  
  ##### WRITE DATA #####
  line_data <- c(keyword_ref, iteration,
            video_id, title, description, keywords, type, paid, channel_id,
            duration, unlisted, family, regions, views, date_publication, date_upload, genre,
            next_video_id, paste(reco_list, collapse=' ; ')
           ) 
  line_data <- line_data %>% as.character() %>% paste0(collapse='\t')
  write(line_data, csv_file, append=TRUE)
  
  ##### RETURN NEXT VIDEO #####
  return(reco_list[1])
}

#####################################
# TEXT QUERIES
#####################################

# queries <- read.csv("Top100GlobalWarming.csv", header=TRUE, sep=";") %>%
  # unite(bigram, word1, word2, sep = " ") %>% select(bigram) %>% pull

queries <- c("al gore", "anthropogenic climate", "atmospheric co2", "climate science", "co2 emissions", "co2 levels", "global climate", "global cooling", "global temperature", "global warming", "greenhouse gases", "ice age", "industrial revolution", "oil companies", "patrick moore", "scientific method", "sea level")


#####################################
# SCRAPE IN ACTION
#####################################

# TRY KEYWORDS
for (user_query in queries){
  paste('QUERY:', user_query) %>% print
  
  ##### INIT CSV RESULTS ####
  csv_header <- c("keyword_ref", "iteration", 
                  "video_id", "title", "description", "keywords", "type", "paid", "channel_id", 
                  "duration", "unlisted", "family", "regions", "views", "date_publication", "date_upload", "genre", 
                   "next_video_id", "reco_videos_id") %>% paste0(collapse='\t')
  csv_file <- paste0('results/scrape_results_',
                     gsub(' ','',user_query),'_',gsub('[^0-9]','_',Sys.time()),'.tsv')
  write(csv_header, csv_file)
  
  
  ##### SEARCH WITH KEYWORDS #####
  search_url <- paste0('https://www.youtube.com/results?search_query=', gsub(' ','+', user_query))
  search_results <- search_url %>% read_html
  # search_results %>% as.character %>% write(paste0('results/scrape_html_',gsub(' ','-',user_query),'_SEARCH_',gsub('[^0-9]','_',Sys.time()),'.html') )
  
  ##### SELECT 1st RECOMMENDATION #####
  search_results <- search_results %>% html_nodes('a[href*="watch"]') %>% html_attr(('href')) %>% unique
  search_results <- gsub('^.*watch.*=','', search_results)
  reco <- search_results[1]
  
  ##### FOLLOW RECOMMENDATIONS 20 TIMES #####
  n_scrape = 20
  for(count in 1:n_scrape){ 
    paste('Iteration', count) %>% print
    html <- paste0('https://www.youtube.com/watch?v=', reco) %>% read_html
    # search_results %>% as.character %>% write(paste0('results/scrape_html_',gsub(' ','-',user_query),'_',count,'_',gsub('[^0-9]','_',Sys.time()),'.html'))
    reco <- scrape_page(reco, html, csv_file, user_query, count)
  }
  
}
