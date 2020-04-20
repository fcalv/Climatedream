library(RSelenium)
library(tidyverse)
library(rvest)
library(pracma)
library(netstat)

test_scrape <- function(){
  vid_ref <- remote_driver$findElement(using = "xpath", value="/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/h1/yt-formatted-string")
  current_title <<- vid_ref$getElementText()
  
  #get current video thumbnail
  current_thumbnail <<- html %>%
    html_node('link[itemprop="thumbnailUrl"]') %>%
    html_attr("href") 
  
  channelname_ref <- remote_driver$findElement(using = "xpath", value="/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/div[2]/ytd-video-owner-renderer/div[1]/ytd-channel-name/div/div/yt-formatted-string/a")
  current_channel <<- channelname_ref$getElementText()
  
  subs_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/div[2]/ytd-video-owner-renderer/div[1]/yt-formatted-string")
  span_subs <- subs_ref$getElementText()
  span_subs <- str_remove(span_subs, " subscribers")
  span_subs <- as.numeric(sub("K", "e3", span_subs, fixed = TRUE))
  span_subs <- as.numeric(sub("M", "e6", span_subs, fixed = TRUE))
  span_subs <<- span_subs
  
  desc_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/div/div/yt-formatted-string")
  description <<- desc_ref$getElementText()
  
  category_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/ytd-metadata-row-container-renderer/div[2]/ytd-metadata-row-renderer/div/yt-formatted-string/a")
  current_category <<- category_ref$getElementText()
  
  recc_vids_titles_ref <- remote_driver$findElements(value = "//span[@id = 'video-title']")
  recc_vids_titles <- sapply(recc_vids_titles_ref, function(x) x$getElementText())
  titles <<- paste(recc_vids_titles, collapse = ';')
  
  recc_vids <- remote_driver$findElements(value = "//yt-formatted-string[@class = 'style-scope ytd-channel-name']")
  recc_vid <- sapply(recc_vids, function(x) x$getElementText())
  recc_channs <- recc_vid[nchar(recc_chans) > 0]
  recc_channs <<- paste(recc_channs, collapse = ';')
  
  likes_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/div/div[3]/div/ytd-menu-renderer/div/ytd-toggle-button-renderer[1]/a/yt-formatted-string")
  likes_num <- likes_ref$getElementAttribute("aria-label")
  likes_num <- str_remove(likes_num, " likes")
  likes_num <- str_remove(likes_num, ",")
  likes <- as.integer(likes_num)
  
  dislikes_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/div/div[3]/div/ytd-menu-renderer/div/ytd-toggle-button-renderer[2]/a/yt-formatted-string")
  dislikes_num <- dislikes_ref$getElementText()
  dislikes_num <- str_remove(dislikes_num, " dislikes")
  dislikes_num <- str_remove(dislikes_num, ",")
  dislikes <- as.integer(dislikes_num)
  
  views_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/div/div[1]/div[1]/yt-view-count-renderer/span[1]")
  views <- views_ref$getElementText()
  views <- str_remove(views, " views")
  views <- str_remove(views, ",")
  views <- as.integer(views)
  
  recc_thumbs_ref <- remote_driver$findElements(value = "//img[@class = 'style-scope yt-img-shadow']")
  recc_thumbs <- sapply(recc_thumbs_ref, function(x) x$getElementAttribute("src"))
  recc_thumbs <- sapply(recc_thumbs, function(x) unlist(x))
  recc_thumbs <- recc_thumbs[recc_thumbs != 'NULL']
  thumbs <<- paste(recc_thumbs, collapse = ';')
  
  #get current video monetization status
  paid <<- html %>%
    html_node('meta[itemprop="paid"]') %>%
    html_attr("content")
  
  #get current video family friendly status
  famfriend <<- html %>%
    html_node('meta[itemprop="isFamilyFriendly"]') %>%
    html_attr("content")
  
  links_ref <- remote_driver$findElements(value = "//a[@class = 'yt-simple-endpoint inline-block style-scope ytd-thumbnail']")
  links_ref <- sapply(links_ref, function(x) x$getElementAttribute("href"))
  links_ref <- sapply(links_ref, function(x) unlist(x))
  links_ref <- links_ref[links_ref != 'NULL']
  links <<- paste(links_ref, collapse = ';')
  
  if (length(curtitle) == 0) {
    curtitle <<- ""
  }
  
  if (length(current_channel) == 0) {
    current_channel <<- ""
  }
  
  if (length(current_category) == 0) {
    current_category <<- ""
  }
  
  if (length(span_subs) == 0) {
    span_subs <<- ""
  }
  
  if (length(current_thumbnail) == 0) {
    current_thumbnail <<- ""
  }
  
  if (length(description) == 0) {
    description <<- ""
  }
  
  if (length(famfriend) == 0) {
    famfriend <<- ""
  }
  
  if (length(paid) == 0) {
    paid <<- ""
  }
  
  if (length(views) == 0) {
    views <<- ""
  }
  
  if (length(likes) == 0) {
    likes <<- ""
  }
  
  if (length(dislikes) == 0) {
    dislikes <<- ""
  }
  
  if (length(links) == 0) {
    links <<- ""
  }
  
  if (length(titles) == 0) {
    titles <<- ""
  }
  
  if (length(recc_channs) == 0) {
    recc_channs <<- ""
  }
  
  if (length(thumbs) == 0) {
    thumbs <<- ""
  }
  
}

fport <- netstat::free_port()
driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.16")

remote_driver <- driver[["client"]] 
remote_driver$setTimeout(type = "page load", milliseconds = 1000)
remote_driver$navigate("https://www.youtube.com/")
#########################
# SCRAPING
########################

Top100GlobalWarming <- read.csv("Top100GlobalWarming.csv", header=TRUE, sep=";") %>%
  unite(bigram, word1, word2, sep = " ")

metadata <- data.frame(Keyword = character(), Title = character(), Channel = character(), Category = character(), SUB_CNT = character(), Thumbnail = character(), Descript = character(), Fam_friendly = character(), Monetized = character(), VIEW_CNT = character(), LIKE_CNT = character(), DISLIKE_CNT = character(), recc_videos = character(), recc_titles = character(), recc_channs = character(),Thumbnails = character())

remote_driver$deleteAllCookies()

timer <- Sys.time()

for (row in 1:nrow(Top100GlobalWarming)){
  
  #refresh youtube and clear all cookies
  remote_driver$navigate("https://www.youtube.com/")
  
  Sys.sleep(1)
  
  statement <- Top100GlobalWarming$bigram[row]
  # get YouTube search bar reference, send text(keyword) to it and simulate pressing enter
  address_element <- remote_driver$findElement(using = 'xpath', value = '/html/body/ytd-app/div/div/ytd-masthead/div[3]/ytd-searchbox/form/div/div[1]/input')
  address_element$sendKeysToElement(list(statement, key = "enter"))#first keyword
  
  #wait for 1 second for everything to load properly
  Sys.sleep(2)
  
  # click on first thumbnail
  remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()
  
  Sys.sleep(1)
  
  # Scroll down to read more
  remote_driver$executeScript("return document.querySelector('#container').scrollIntoView(true)")
  # Element for scrolling later
  webElem <- remote_driver$findElement("css", "body")
  # Click show more
  show_more <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/paper-button[2]")
  show_more$clickElement()
  # Go back up
  webElem$sendKeysToElement(list(key = "home"))
  
  #fetch html from current remote_driver url
  currenturl <- toString(remote_driver$getCurrentUrl())
  html <- currenturl %>% read_html()
  
  #scrape results of first keyword data
  scrape()
  
  # add data to the dataframe
  metadata <- rbind(metadata,data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, stringsAsFactors = FALSE, check.rows = FALSE))
  
  count = 1
  while(count <= 3){ 
    
    # Timeout for (very) long videos and live streams (stop watching after N seconds), for now 1h
    tic(gcFirst = T)
    N <- 3600
    
    #watch the video until the end / timer expiry
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    
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
    }
    
    
    #click on next recommended video
    remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a")$clickElement()
    
    Sys.sleep(2)
    
    # Scroll down to read more
    remote_driver$executeScript("return document.querySelector('#container').scrollIntoView(true)")
    # Element for scrolling later
    webElem <- remote_driver$findElement("css", "body")
    # Click show more
    show_more <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/paper-button[2]")
    show_more$clickElement()
    # Go back up
    webElem$sendKeysToElement(list(key = "home"))
    
    
    #fetch html from current remote_driver url
    currenturl <- toString(remote_driver$getCurrentUrl())
    html <- currenturl %>% read_html()
    
    #scrape the data of current video - update global environment variables
    test_scrape()
    
    #append new row of data to data frame
    metadata <- rbind(metadata,data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, stringsAsFactors = FALSE, check.rows = FALSE))
    count <- count + 1
  }
  
  # We need to watch the last video for the keyword to the very end / same checks as before
  
  state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
  while( (state != 0) & (toc(echo = F) <= N))
  {
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    
    # sign-in pop-up close
    test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
    if (length(test_element) == 1) {
      test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
      test_element$clickElement() 
    }
    
    
    # close youtube premium (if it's in there)
    test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
    if (length(test_element) == 1) {
      test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
      test_element$clickElement() 
    } 
  }
  
  # break condition after 6 hours
  if (as.numeric(difftime(Sys.time(), timer, units = "hours")) >= 6 ) {
    break
  }
  
}

##########################
print("Timer has expired")