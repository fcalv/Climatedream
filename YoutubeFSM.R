################
### Original ###
################
library(RSelenium)
library(tidyverse)
library(rvest)
library(pracma)


test_scrape <- function(){
  
  #get title of current video
  curtitle <<- html %>%
    html_nodes('h1') %>% 
    html_nodes('span') %>% 
    xml_attr('title')
  
  #get all reccomended video thumbnails
  thumbs_loc <- html %>%
    html_nodes('img') %>%
    html_attr('data-thumb')
  
  thumbs_loc[!is.na(thumbs_loc)]
  
  #collapse list of thumbnails
  thumbs <<- paste(thumbs_loc, collapse = ';')
  
  #get all links
  links <<- html %>%
    html_nodes('a') %>%
    html_attr('href')
  
  #filter links to only include recommended videos
  links <<- links[links %>% str_detect('/watch')]
  
  #remove empty fields
  links[!is.na(links)]
  links <<- paste(links, collapse = ';')
  
  #get all reccomended video titles
  titles <<- html %>%
    html_nodes("a") %>%
    html_attr("title")
  
  #remove empty fields
  titles <<- titles[!is.na(titles)]
  
  titles <<- paste(titles, collapse = ';')
  
  #get current video category
  current_category <<- html %>%
    html_node('meta[itemprop="genre"]') %>%
    html_attr("content")
  
  #another method
  #category <- html %>%
  #  html_node('#content') %>%
  #  html_nodes('a') %>%
  #  html_attr('href') 
  
  #category <- category[category %>% str_detect('/[a-z]*$')]
  
  #get current video channel name
  current_channel <<- html %>%
    html_node('div[class="yt-user-info"]') %>%
    html_nodes('a') %>%
    html_text()
  
  #get current video thumbnail
  current_thumbnail <<- html %>%
    html_node('link[itemprop="thumbnailUrl"]') %>%
    html_attr("href") 
  
  #helper variable - to get reccomended video channels
  span_texts <- html %>%
    html_nodes('span') %>%
    html_text()
  
  #helper filter variable
  attributes <- html %>%
    html_nodes('span') %>%
    xml_attr('class')
  
  #channel name tag has class 'stat attribution'
  span_texts <-  span_texts[attributes %>% str_detect('stat attribution')]
  
  #remove empty fields
  span_texts <- span_texts[!is.na(span_texts)]
  
  recc_channs <<- paste(span_texts, collapse = ';')
  
  #helper variable - to get current video subscriber count 
  subs_loc <- html %>%
    html_nodes('span') %>%
    html_text()
  
  #again use attributes helper variable to filter by class
  subs_loc <- subs_loc[attributes %>% str_detect('yt-subscription-button-subscriber-count-branded-horizontal yt-subscriber-count')]
  
  #remove empty fields
  span_subs <<- subs_loc[!is.na(subs_loc)]
  
  #get current video view count
  views <<- html %>%
    html_node('meta[itemprop="interactionCount"]') %>%
    html_attr("content")
  
  #get current video monetization status
  paid <<- html %>%
    html_node('meta[itemprop="paid"]') %>%
    html_attr("content")
  
  #get current video family friendly status
  famfriend <<- html %>%
    html_node('meta[itemprop="isFamilyFriendly"]') %>%
    html_attr("content")
  
  #get current video like count
  likes_loc <- html %>%
    html_nodes('span.yt-uix-button-content') %>%
    html_text()
  
  #convert to numeric
  likes <<- as.character(likes_loc[17])
  
  #get current video dislike count
  dislikes_loc <- html %>%
    html_nodes('span.yt-uix-button-content') %>%
    html_text()
  
  #convert to numeric
  dislikes <<- as.character(dislikes_loc[20])
  
  #get current video description
  description <<- html %>% 
    html_nodes(xpath = "//*[@id = 'eow-description']") %>% 
    html_text()
  
  
###############  
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
#################
}

#driver <- rsDriver(browser = c("firefox"), port=5555L)
# to check current version chrome://version/
driver <- rsDriver(browser = c("chrome"), port=5546L, chromever="80.0.3987.106")

remote_driver <- driver[["client"]] 
remote_driver$navigate("https://www.youtube.com/")
#address_element <- remote_driver$findElement(using = 'id', value = 'search')
#address_element$sendKeysToElement(list("first trial", key = "enter"))#first keyword

#alternatives to above 
#button_element <- remote_driver$findElement(using = 'id', value = "button")
#button_element <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/ytd-searchbox/yt-icon-button/button")
#remote_driver$findElement(using = 'id', value = "button")$clickElement()
#button_element$clickElement()


#first keyword video - xpath based on location of tag in webpage - stays the same so can be hardcoded
#remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()

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
  test_scrape()
  
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

print("Timer has expired")
# save metadata
metadata %>% write.table(paste("Metadata/",metadata$Keyword[1],"_",metadata$Keyword[nrow(metadata)],".txt"))
