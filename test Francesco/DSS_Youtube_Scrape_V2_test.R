#####################################
# ENVIRONMENT
#####################################
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))

library(tidyverse)
library(rvest)
library(pracma)
library(webdriver)
library(RSelenium)
library(XML)
library(jsonlite)
library(netstat)
library(htmltidy)
library(netstat)
library(lubridate)
# webdriver::install_phantomjs

options(stringsAsFactors = FALSE)

#####################################
# UTILS
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
get_channel <- function(video_list){
  res <- c()
  for(video_id in video_list){
    channel_id <- paste0('https://www.youtube.com/watch?v=',video_id) %>% 
      read_html %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
    res <- c(res, channel_id)
  }
  res %>% return
}
clean_text <- function(text_list){
  text_list <-  gsub('[^[:graph:]]', ' ', text_list)
  text_list <-  gsub('  *', ' ', text_list)
  text_list <-  gsub('\t|\n| *$|^ *|^\n *|;', '', text_list)
  text_list %>% return
}
#####################################
# SCRAPE FUNCTION
#####################################

scrape_page <- function(html_selenium, csv_file, keyword_ref, iteration, html_rvest, url){
  video_id <- gsub('^http.*watch.v=|&.*$','',url)
  
  ##### CURRENT VIDEO DATA (from Rvest) #####
  title <- html_rvest %>% html_nodes('meta[name=title]') %>% html_attr('content')
  title <- gsub('\t','',title) %>% clean_text
  
  description <- html_rvest %>% html_nodes('meta[name=description]') %>% html_attr('content')
  description <- gsub('\t','',description) %>% clean_text
  
  keywords <- html_rvest %>% html_nodes('meta[name=keywords]') %>% html_attr('content')
  keywords <- gsub('\t','',keywords)
  keywords <- gsub(',', ';', keywords)
  
  image <- html_rvest %>% html_nodes('meta[property="og:image"]') %>% html_attr('content')
  type <- html_rvest %>% html_nodes('meta[property="og:type"]') %>% html_attr('content')
   
  paid <- html_rvest %>% html_nodes('meta[itemprop="paid"]') %>% html_attr('content')
  channel_id <- html_rvest %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
  
  duration <- html_rvest %>% html_nodes('meta[itemprop="duration"]') %>% html_attr('content')
  #duration <- str_remove(duration, 'PT')
  #duration <- str_replace_all(duration, c("H" = ":", "M" = ":", S = ":"))
  #duration <- sapply(duration, function(x) if(nchar(x) > 5){ period_to_seconds(hms(x))}else{period_to_seconds(ms(x))})
  #duration <- period_to_seconds(hms(duration))
  
  unlisted <- html_rvest %>% html_nodes('meta[itemprop="unlisted"]') %>% html_attr('content')
  family <- html_rvest %>% html_nodes('meta[itemprop="isFamilyFriendly"]') %>% html_attr('content')
  
  regions <- html_rvest %>% html_nodes('meta[itemprop="regionsAllowed"]') %>% html_attr('content')
  regions <- gsub(',', ';', regions)
  
  views <- html_rvest %>% html_nodes('meta[itemprop="interactionCount"]') %>% html_attr('content')
  date_publication <- html_rvest %>% html_nodes('meta[itemprop="datePublished"]') %>% html_attr('content')
  date_upload <- html_rvest %>% html_nodes('meta[itemprop="uploadDate"]') %>% html_attr('content')
  
  genre <- html_rvest %>% html_nodes('meta[itemprop="genre"]') %>% html_attr('content')
  genre <- gsub(',', ';', genre)
  
  like <- html_rvest %>% html_nodes('.like-button-renderer-like-button span') %>% html_text()
  like <- gsub('[^0-9]','', like[1]) %>% as.numeric
  
  dislike <- html_rvest %>% html_nodes('.like-button-renderer-dislike-button span') %>% html_text()
  dislike <- gsub('[^0-9]','', dislike[1]) %>% as.numeric
   
  ##### RECOMMENDATION (from Selenium) ##### 
  next_video_id <- html_selenium %>% html_nodes('div#items ytd-compact-autoplay-renderer a#thumbnail') %>% html_attr('href') 
  reco_videos_id <- html_selenium %>% html_nodes('div#items ytd-compact-video-renderer a#thumbnail') %>% html_attr('href') 
  reco_videos_id <- gsub('^.*v=', '', reco_videos_id) 
  
  next_video_url <- paste0('https://www.youtube.com/watch?v=', next_video_id)
  reco_urls <- paste0('https://www.youtube.com/watch?v=', reco_videos_id)
  
  reco_titles <- html_selenium %>% html_nodes('span#video-title') %>% html_text()
  
  reco_channels_name <- html_selenium %>% 
    html_nodes('div#items ytd-compact-video-renderer div.ytd-channel-name[id="container"] yt-formatted-string') %>% html_text() 
  
  # Channel urls are not always in the html, eg this does not give all channel names (and gives channels of ads...:
  # html_selenium %>% html_nodes('ytd-video-meta-block ytd-channel-name a')  %>% html_attr('href') 
  reco_channels_id <- get_channel(reco_videos_id)
  
  reco_snippets <- html_selenium %>% html_nodes('ytd-compact-video-renderer h3 span') %>% html_attr('aria-label')
  
  reco_views <- reco_snippets %>% str_extract('[0-9,]* view')
  reco_views <- gsub(',|view','',reco_views) %>% as.numeric
  
  reco_hours <- reco_snippets %>% str_extract('[0-9]* hour')
  reco_hours <- gsub(' hour','',reco_hours) %>% as.numeric %>% replace_na(0)
  reco_mins <- reco_snippets %>% str_extract('[0-9]* minute')
  reco_mins <- gsub(' minute','',reco_mins) %>% as.numeric %>% replace_na(0)
  reco_secs <- reco_snippets %>% str_extract('[0-9]* second')
  reco_secs <- gsub(' second','',reco_secs) %>% as.numeric %>% replace_na(0)
  
  reco_durations <- reco_hours*60*60 + reco_mins*60 + reco_secs
  
  reco_years <- reco_snippets %>% str_extract('[0-9]* year')
  reco_years <- gsub(' year','',reco_years) %>% as.numeric %>% replace_na(0)
  reco_months <- reco_snippets %>% str_extract('[0-9]* month')
  reco_months <- gsub(' month','',reco_months) %>% as.numeric %>% replace_na(0)
  
  reco_ages_month <- reco_years*12 + reco_months
  
  # Flatten the vectors
  reco_videos_id <- reco_videos_id %>% paste(collapse=';')
  reco_urls <- reco_urls %>% paste(collapse=';')
  reco_titles <- reco_titles %>% clean_text %>% paste(collapse=';')
  reco_channels_id <- reco_channels_id %>% paste(collapse=';')
  reco_channels_name <- reco_channels_name %>% clean_text %>% paste(collapse=';')
  reco_snippets <- reco_snippets %>% clean_text %>% paste(collapse=';')
  reco_views <- reco_views %>% paste(collapse=';')
  reco_durations <- reco_durations %>% paste(collapse=';')
  reco_ages_month <- reco_ages_month %>% paste(collapse=';')
  
  ##### WRITE DATA #####
  line_data <- c(keyword_ref, iteration,
            video_id, url, title, description, keywords, image, type, paid, channel_id,
            duration, unlisted, family, regions, views, date_publication, date_upload, genre,
            like, dislike, next_video_id, next_video_url, 
            reco_videos_id, reco_urls, reco_titles, reco_channels_id, reco_channels_name,
            reco_durations, reco_views, reco_ages_month, reco_snippets
           ) 
  print(line_data)
  
  line_data <- line_data %>% as.character() %>% paste0(collapse='\t')
  write(line_data, csv_file, append=TRUE)

  return(reco_videos_id)
}

#####################################
# SEARCH QUERIES
#####################################

search_qeries <- read.csv("queries.txt", header=TRUE, sep="\t")
search_qeries <- search_qeries$Bigram

#####################################
# SCRAPE IN ACTION
#####################################

timer <- Sys.time()

##### START SELENIUM DRIVER #####
# Find port by running run_phantomjs()

# Alternative:
port <- netstat::free_port()

driver <- rsDriver(browser="chrome", port=port, chromever="80.0.3987.106")


remote_driver <- driver[["client"]]
remote_driver$deleteAllCookies()
remote_driver$setWindowSize(1920, 1080)
video_count <- 0

# TRY KEYWORDS
for (row in 1:length(search_qeries)){
  
  ##### GET KEYWORDS #####
  statement <- search_qeries[row]
  paste('KEYWORD:', statement) %>% print
  
  ##### INIT CSV RESULT S####
  csv_header <- c("keyword_ref", "iteration", 
                  "video_id", "url", "title", "description", "keywords", "image", "type", "paid", "channel_id", 
                  "duration", "unlisted", "family", "regions", "views", "date_publication", "date_upload", "genre", 
                  "like", "dislike", "next_video_id", "next_video_url", 
                  "reco_videos_id", "reco_urls", "reco_titles", "reco_channels_id", "reco_channels_name", 
                  "reco_durations", "reco_views", "reco_ages_month", "reco_snippets") %>% paste0(collapse='\t')
  csv_file <- paste0('results/scrape_results_',
                     gsub(' ','',statement),'_',gsub('[^0-9]','_',Sys.time()),'.tsv')
  write(csv_header, csv_file)
  
  
  ##### INIT SESSION #####
  # Refresh youtube and clear all cookies
  remote_driver$navigate("https://www.youtube.com/")
  Sys.sleep(1)
  
  ##### RECORD HTML #####
  # remote_driver$switchToFrame(NULL)
  src <- XML::htmlParse(remote_driver$getPageSource()[[1]])
  saveXML(src, paste0('results/scrape_html_',gsub(' ','-',statement),'_HOME_',gsub('[^0-9]','_',Sys.time()),'.html') )
  
  
  ##### SEARCH WITH KEYWORDS #####
  # Get YouTube search bar, send keywords, and simulate pressing enter
  address_element <- remote_driver$findElement(using = "name", value = "search_query")
  address_element$clearElement()
  address_element$sendKeysToElement(list(statement, key = "enter")) 
  Sys.sleep(2)

  ##### RECORD HTML #####
  src <- XML::htmlParse(remote_driver$getPageSource()[[1]])
  saveXML(src, paste0('results/scrape_html_',gsub(' ','-',statement),'_SEARCH_',gsub('[^0-9]','_',Sys.time()),'.html') )
  
  ##### CLICK ON 1st RECOMMENDATION #####
  remote_driver$findElement(using = "css", ".ytd-video-renderer")$clickElement()
  Sys.sleep(2)
  
  while(grepl(remote_driver$getCurrentUrl() %>% unlist(),pattern = "search_query")){
    print("Still in search query page")
    
    Sys.sleep(5)
    remote_driver$findElement(using = "css",value = ".ytd-video-renderer")$clickElement()
    Sys.sleep(2)
  }
  
  # remote_driver$findElement(using = "css", ".ytd-compact-autoplay-renderer")$clickElement()
  
  ##### FOLLOW RECOMMENDATIONS 20 TIMES #####
  n_scrape = 4
  for(count in 1:n_scrape){ 
    video_count <- video_count + 1
    ##### Init #####
    paste('Iteration', video_count) %>% print
    
    start_url <- remote_driver$getCurrentUrl()[[1]] 
    start_url %>% print
    
    ##### LOAD VIDEO #####
    # Make sure the page has time to load, and is not showing an ad
    Sys.sleep(10)
    # Skip add
    ad <- NULL
    tryCatch({
      try( ad <- remote_driver$findElement(using = "css", ".ytp-ad-skip-button-text"), silent=TRUE )
      if(! is.null(ad)) ad$clickElement()
    }, error=function(cond) {
      print(paste("Error with skipping add:", cond))
    }, warning=function(cond) {
      print(paste("Warning with skipping add:", cond))
    }) 
    
    ##### RECORD HTML #####
    src <- XML::htmlParse(remote_driver$getPageSource()[[1]])
    html_src <- paste0('results/scrape_html_',gsub(' ','-',statement),'_',count,'_',gsub('[^0-9]','_',Sys.time()),'.html')
    saveXML(src, html_src)
    html_selenium <- read_html(html_src)
    
    ##### RECORD RVEST SOURCE #####
    html_rvest <- start_url %>% read_html
    html_src_rvest <- gsub('.html$','_rvest.html', html_src)
    html_rvest %>% as.character %>% write(html_src_rvest)
    
    ##### SCRAPE DATA #####
    next_videos <- scrape_page(html_selenium, csv_file, statement, video_count, html_rvest, start_url)
    
    ##### WATCH #####
    # Click play
    # remote_driver$findElement(using = "css", 'button[aria-label="Play (k)"]')$clickElement()
    
    # Timeout for (very) long videos and live streams (stop watching after N seconds), for now 2 min
    tic(gcFirst = T)
    N <- 1800
    
    # watch the video until the end / timer expiry
    state <- 1
    while((state != 0) & (as.numeric(toc(echo = F)) < N)){
      Sys.sleep(1)
      
      # Skip add
      ad <- NULL
      tryCatch({
        try( ad <- remote_driver$findElement(using = "css", ".ytp-ad-skip-button-text"), silent=TRUE )
        if(! is.null(ad)) ad$clickElement()
      }, error=function(cond) {
        print(paste("Error with skipping add:", cond))
      }, warning=function(cond) {
        print(paste("Warning with skipping add:", cond))
      })

      # # Click popup like "Are you still watching?"
      # popup <- NULL
      # try( popup <- remote_driver$findElement(using = "css", "a paper-button paper-ripple"), silent=TRUE )
      # if(! is.null(popup)) popup$clickElement()

      state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")[[1]]
      paste('Player state:', state) %>% print
    }

    
    ##### NEXT VIDEO #####
    # If it's not the last iteration, and the next video is not displayed yet, click on next recommended video
    if(count != n_scrape ){
      now_url <- remote_driver$getCurrentUrl() %>% unlist
      if(start_url != now_url ) {
        print('Page as changed already')
      } else {
        print('Clicking next')
        remote_driver$findElement(using = "css", 
                                  'ytd-compact-autoplay-renderer div#contents ytd-compact-video-renderer div#dismissable ytd-thumbnail a')$clickElement()
        Sys.sleep(5)
      }
    } 
  }

  ##### CLOSE SELENIUM DRIVER #####
  #remote_driver$close()
  #driver$server$stop()
  
  ##### (abort lagging sessions)  #####
  if (as.numeric(difftime(Sys.time(), timer, units = "hours")) >= 6) {
    print("Timer has expired")
    remote_driver$close()
    driver$server$stop()
    break
  }
  
}
