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
# webdriver::install_phantomjs()

#####################################
# SCRAPE FUNCTION
#####################################

scrape_page <- function(html, csv_file, keyword_ref, iteration, html_src){
  
  ##### CURRENT VIDEO DATA #####
  url <- html %>% html_nodes('link[rel="canonical"]') %>% html_attr('href')
  video_id <- gsub('^http.*watch.v=','',url)
  
  title <- html %>% html_nodes('meta[name=title]') %>% html_attr('content')
  title <- gsub('\t','',title)
  
  description <- html %>% html_nodes('meta[name=description]') %>% html_attr('content')
  description <- gsub('\t','',description)
  
  keywords <- html %>% html_nodes('meta[name=keywords]') %>% html_attr('content')
  keywords <- gsub('\t','',keywords)
  keywords <- gsub(',', ';', keywords)
  
  image <- html %>% html_nodes('meta[property="og:image"]') %>% html_attr('content')
  type <- html %>% html_nodes('meta[property="og:type"]') %>% html_attr('content')
   
  paid <- html %>% html_nodes('meta[itemprop="paid"]') %>% html_attr('content')
  channel_id <- html %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
  # video_id <- html %>% html_nodes('meta[itemprop="videoId"]') %>% html_attr('content')
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
   
  #### JSON + LIKE / DISLIKE #### 
  txt <- readLines(html_src)
  
  likes <- c()
  dislikes <- c()
  
  for(i in 1:length(txt)){
    line <- txt[i]
    temp <- str_extract(line, '[^s]likeCount.?.?.?[0-9 ,]*')
    if(!is.na(temp)) {
      likes <- c(likes, temp)
      temp <- str_extract(line, 'dislikeCount.?.?.?[0-9 ,]*')
      if(!is.na(temp)) dislikes <- c(dislikes, temp)
      
      json <- gsub("^ *'RELATED_PLAYER_ARGS': *|,$", '', line) 
      json <- json %>% fromJSON
      
      watch_next <- json$watch_next_response
      watch_next <- watch_next %>% fromJSON
      watch_next %>% write_json( gsub('[^a-z][a-z]*$','.json',html_src) )
      
      break
    }
  }
  likes <- gsub('[^0-9]', '', sort(likes, decreasing=TRUE)[1])
  dislikes <- gsub('[^0-9]', '', sort(dislikes, decreasing=TRUE)[1])
  
  # more_details <- watch_next$contents$twoColumnWatchNextResults$results$results$contents$itemSectionRenderer$contents
  
  ##### AUTOPLAY DATA #####
  next_video_id <- watch_next$contents$twoColumnWatchNextResults$autoplay$autoplay$sets$autoplayVideo$watchEndpoint$videoId
  next_video_url <- paste0('https://www.youtube.com/watch?v=', next_video_id)
  
  ##### RECOMMENDATION DATA #####
  # Alternative: watch_next$compactVideoRenderer$navigationEndpoint$watchEndpoint$videoId[3:20] %>% glimpse
  
  watch_next <- watch_next$contents$twoColumnWatchNextResults$secondaryResults$secondaryResults$results
  
  reco_videos_id_vector <- watch_next$compactVideoRenderer$videoId[3:20]
  reco_urls <- paste0('https://www.youtube.com/watch?v=', reco_videos_id_vector)
  
  reco_videos_id_vector <- c(next_video_id, reco_videos_id_vector)

  reco_videos_id <- paste(reco_videos_id_vector, collapse=' ; ')
  reco_urls <- paste(reco_urls, collapse=' ; ')
  
  reco_titles <- watch_next$compactVideoRenderer$title$simpleText[3:20]
  reco_titles <- gsub('\t','',reco_titles)
  reco_titles <- paste(reco_titles, collapse=' ; ')
  
  # About channel ID & user ID, these urls are equivalent:
  # User page: https://www.youtube.com/user/sasori6282 
  # Channel page: https://www.youtube.com/channel/UCWvTi6jwfaDd9fGBFy9E_5g
  reco_channels <- watch_next$compactVideoRenderer$longBylineText$runs
  reco_channels_name <- reco_channels_id <- c()
  for(i in 3:20){
    reco_channels_name <- c(reco_channels[[i]]$text, reco_channels_name)
    reco_channels_id <- c(reco_channels[[i]]$navigationEndpoint.browseEndpoint.browseId, reco_channels_id)
  }
  reco_channels_name <- gsub('\t','',reco_channels_name)
  reco_channels_name <- paste(reco_channels_name, collapse=' ; ')
  reco_channels_id <- paste(reco_channels_id, collapse=' ; ')
  
  reco_whys <- watch_next$compactVideoRenderer$viewCountText$simpleText[3:20]
  reco_whys <- paste(reco_whys, collapse=' ; ')
  
  reco_durations <- watch_next$compactVideoRenderer$lengthText$simpleText[3:20]
  for(i in 1:18){
    temp <- reco_durations[i] %>% str_split(':') %>% unlist %>% as.numeric %>% rev 
    duration <- 0
    for(k in 1:length(temp)){
      duration <- duration + temp[k]*60^(k-1)
    }
    reco_durations[i] <- duration
  }
  reco_durations <- paste(reco_durations, collapse=' ; ')
  
  reco_thumbnails <- watch_next$compactVideoRenderer$channelThumbnail$thumbnails %>% 
    unlist()  %>% as.character() 
  reco_thumbnails <- reco_thumbnails[ (1:length(reco_thumbnails)) %% 3 == 1 ]
  reco_thumbnails <- paste(reco_thumbnails, collapse=' ; ')
  
  
  reco_snippets <- watch_next$compactVideoRenderer$accessibility$accessibilityData$label[3:20]
  reco_snippets <- gsub('\t','',reco_snippets)
  reco_snippets <- paste(reco_snippets, collapse=' ; ')
  
  reco_ages_year <- watch_next$compactVideoRenderer$publishedTimeText$simpleText[3:20]
  reco_ages_year[is.na(reco_ages_year)] <- 0
  reco_ages_year <- gsub('[^0-9]','',reco_ages_year) %>% as.numeric
  reco_ages_year <- paste(reco_ages_year, collapse=' ; ')
  
  badges <- watch_next$compactVideoRenderer$ownerBadges
  reco_badges <- c()
  for(i in 3:20) {
    temp <- badges[[i]]  %>% unlist %>% as.character
    if(length(temp) == 0) temp <- ''
    reco_badges <- c(reco_badges,  temp)
  }
  reco_badges <- paste(reco_badges, collapse=' ; ')
  
  
  ##### WRITE DATA #####
  line_data <- c(keyword_ref, iteration,
            video_id, url, title, description, keywords, image, type, paid, channel_id,
            duration, unlisted, family, regions, views, date_publication, date_upload, genre,
            likes, dislikes, next_video_id, next_video_url, 
            reco_videos_id, reco_urls, reco_titles, reco_channels_id, reco_channels_name,
            reco_whys, reco_durations, reco_thumbnails, reco_snippets,
            reco_ages_year, reco_badges
           ) 
  line_data <- line_data %>% as.character() %>% paste0(collapse='\t')
  
  write(line_data, csv_file, append=TRUE)

  
  return(reco_videos_id_vector)
}

#####################################
# SELENIUM
#####################################
# Find port by running run_phantomjs()
port <- run_phantomjs()

# driver <- rsDriver(browser = c("firefox"), port=port$port) #version 74.0.1
# remote_driver$close()
# driver$server$stop()

# to check current version chrome://version/
driver <- rsDriver(browser = c("chrome"), port=port$port, chromever="80.0.3987.106")

remote_driver <- driver[["client"]] 

#####################################
# KEYWORDS
#####################################

Top100GlobalWarming <- read.csv("Top100GlobalWarming.csv", header=TRUE, sep=";") %>%
  unite(bigram, word1, word2, sep = " ")

timer <- Sys.time()

#####################################
# SCRAPE IN ACTION
#####################################

##### TRY KEYWORDS ##### 
for (row in 2:5){
  
  ##### GET KEYWORDS #####
  statement <- Top100GlobalWarming$bigram[row]
  paste('KEYWORD:', statement) %>% print
  
  ##### INIT CSV RESULT S####
  csv_header <- c("keyword_ref", "iteration", 
                  "video_id", "url", "title", "description", "keywords", "image", "type", "paid", "channel_id", 
                  "duration", "unlisted", "family", "regions", "views", "date_publication", "date_upload", "genre", 
                  "likes", "dislikes", "next_video_id", "next_video_url", 
                  "reco_videos_id", "reco_urls", "reco_titles", "reco_channels_id", "reco_channels_name", 
                  "reco_whys", "reco_durations", "reco_thumbnails", "reco_snippets", 
                  "reco_ages_year", " reco_badges") %>% paste0(collapse='\t')
  csv_file <- paste0('results/scrape_results_',statement,'_',gsub('[^0-9]','_',Sys.time()),'.tsv')
  write(csv_header, csv_file)
  
  ##### INIT SESSION #####
  # Refresh youtube and clear all cookies
  remote_driver$navigate("https://www.youtube.com/")
  remote_driver$deleteAllCookies()
  remote_driver$navigate("https://www.youtube.com/")
  Sys.sleep(1)
  
  
  ##### RECORD HTML #####
  # remote_driver$switchToFrame(NULL)
  src <- XML::htmlParse(remote_driver$getPageSource()[[1]])
  saveXML(src, paste0('results/scrape_html_',gsub(' ','-',statement),'_HOME_',gsub('[^0-9]','_',Sys.time()),'.html') )
  
  ##### SEARCH WITH KEYWORDS #####
  
  # get YouTube search bar reference, send text(keyword) to it and simulate pressing enter
  address_element <- remote_driver$findElement(using = "name", value = "search_query")
  address_element$sendKeysToElement(list(statement, key = "enter")) 
  Sys.sleep(2)

  ##### RECORD HTML #####
  # remote_driver$switchToFrame(NULL)
  src <- XML::htmlParse(remote_driver$getPageSource()[[1]])
  saveXML(src, paste0('results/scrape_html_',gsub(' ','-',statement),'_SEARCH_',gsub('[^0-9]','_',Sys.time()),'.html') )
  
  ##### CLICK ON 1st RECOMMENDATION #####
  remote_driver$findElement(using = "css", ".yt-lockup-thumbnail a")$clickElement()
  Sys.sleep(1)
  
  n_scrape = 20
  for(count in 1:n_scrape){ 
    paste('Iteration', count) %>% print
    
    start_url <- remote_driver$getCurrentUrl()[[1]] 
    start_url %>% print
    
    ##### RECORD HTML #####
    # remote_driver$switchToFrame(NULL)
    src <- XML::htmlParse(remote_driver$getPageSource()[[1]])
    src_file <- paste0('results/scrape_html_',gsub(' ','-',statement),'_',count,'_',gsub('[^0-9]','_',Sys.time()),'.html')
    saveXML(src, src_file)
    
    ##### BROWSE (deprecated) #####
    # # Scroll down to read more
    # remote_driver$executeScript("return document.querySelector('#container').scrollIntoView(true)")
    # # Element for scrolling later
    # webElem <- remote_driver$findElement("css", "body")
    # # Click show more
    # show_more <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/paper-button[2]")
    # show_more$clickElement()
    # # Go back up
    # webElem$sendKeysToElement(list(key = "home"))
    
    ##### SCRAPE #####
    # fetch html from current remote_driver url
    html <- remote_driver$getCurrentUrl()[[1]] %>% read_html
    html_src <- gsub('.html$','_rvest.html', src_file)
    html %>% as.character %>% write(html_src)
    html <- read_html(html_src)
    
    # DO SCRAPE
    next_videos <- scrape_page(html, csv_file, statement, count, html_src)
    
    ##### WATCH #####
    # Click play
    # remote_driver$findElement(using = "css", 'button[aria-label="Play (k)"]')$clickElement()
    
    # Timeout for (very) long videos and live streams (stop watching after N seconds), for now 30s
    tic(gcFirst = T)
    N <- 30
    
    # watch the video until the end / timer expiry
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")[[1]]
    
    while( (state != 0) & (toc(echo = F) <= N)){
      state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    
      # sign-in pop-up close
      test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
      if (length(test_element) == 1) {
        test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
        test_element$clickElement() 
      }
      
      # close youtube premium ad (if it's in there)
      test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
      if (length(test_element) == 1) {
        test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
        test_element$clickElement() 
      } 
      
      # close are you still watching
      test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/yt-confirm-dialog-renderer/div[2]/div/yt-button-renderer[2]/a/paper-button/paper-ripple")
      if (length(test_element) == 1) {
        test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/yt-confirm-dialog-renderer/div[2]/div/yt-button-renderer[2]/a/paper-button/paper-ripple")
        test_element$clickElement() 
      }
    }
    
    
    ##### NEXT VIDEO #####
    # If it's not the last iteration, click on next recommended video
    if(count != n_scrape ){
      now_url <- remote_driver$getCurrentUrl() %>% unlist
      now_video <- gsub('^[^=]*|=','', now_url)
      if(start_url != now_url ) {
        print('Page as changed already')
        next
      }
      
      # Try all links
      a <- remote_driver$findElements(using = "css", 'a')
      # for(btn in a) btn$getElementAttribute('href') %>% unlist %>% print
      for(btn in a){
        clicked <- FALSE
        
        # Skip hidden links
        if(btn$isElementDisplayed() %>% unlist) {
          
          h <- btn$getElementAttribute('href') %>% unlist
          
          # Skip empty links & self-link
          if(is.null(h)) next
          if(is.na(h)) next
          if(h == '') next
          if(grepl(now_video, h)) next
          
          for(n in next_videos){
            # Skip empty video ids
            if(is.null(n)) next
            if(is.na(n)) next
            if(n == '') next

            if(grepl(n, h)) {
              remote_driver$getCurrentUrl() %>% unlist %>% paste(' (now)') %>% print
              btn$getElementAttribute('href') %>% unlist %>% paste(' (target)') %>% print
              print('CLICK')
              btn$clickElement()
              remote_driver$getCurrentUrl() %>% unlist %>% paste(' (now)') %>% print
              clicked <- TRUE
              break
            }
          }
          if(clicked) break
        } 
      }
    }
    Sys.sleep(2)
  }

  # break condition after 6 hours
  if (as.numeric(difftime(Sys.time(), timer, units = "hours")) >= 6 ) {
    print("Timer has expired")
    break
  }

}




#####################################
# CRUMBS
#####################################

# n <- html %>% html_nodes('meta')
# n <- html %>% html_nodes('link')
# for(node in n){
#   print(node)
# }
# 
# rvs <- json$rvs
# rvs <- gsub('&','\n', rvs) 
# rvs <- gsub('=','; ', rvs) 
# write(rvs, 'temp.csv')
# rvs <- read.csv2('temp.csv', header=FALSE)
# rvs$V2 %>% sort
# rvs %>% spread(V1, V2) %>% glimpse



# btn <- remote_driver$findElement(using = "css", 'button[aria-label=Play]')
unlist(lapply(a, function(x) {x$getElementAttribute('href')}))
# 
# btn$getElementAttribute('aria-label')
# 
# html %>% html_nodes('button') %>% html_attr('class')
#####################################