################
### Original ###
################
library(RSelenium)
library(tidyverse)
library(rvest)

#driver <- rsDriver(browser = c("firefox"), port=5555L)
driver <- rsDriver(browser = c("chrome"), port=5556L, chromever="80.0.3987.16")

remote_driver <- driver[["client"]] 
remote_driver$navigate("https://www.youtube.com/")
address_element <- remote_driver$findElement(using = 'id', value = 'search')
address_element$sendKeysToElement(list("first trial", key = "enter"))#first keyword

#alternatives to above 
#button_element <- remote_driver$findElement(using = 'id', value = "button")
#button_element <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/ytd-searchbox/yt-icon-button/button")
#remote_driver$findElement(using = 'id', value = "button")$clickElement()
#button_element$clickElement()


#first keyword video - xpath based on location of tag in webpage - stays the same so can be hardcoded
remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()

Top100GlobalWarming <- read.csv("C:/visualizationworkshop/Top100GlobalWarming.csv", header=TRUE, sep=";") %>%
  unite(bigram, word1, word2, sep = " ")

for (row in 1:nrow(Top100GlobalWarming)){
  
  #refresh youtube and clear all cookies
  remote_driver$navigate("https://www.youtube.com/")
  remote_driver$deleteAllCookies()
  
  statement <- Top100GlobalWarming$bigram[row]
  #get YouTube search bar reference, send text(keyword) to it and simulate pressing enter
  address_element <- remote_driver$findElement(using = 'id', value = 'search')
  address_element$sendKeysToElement(list(statement, key = "enter"))#first keyword
  
  #wait for 1 second for everything to load properly
  Sys.sleep(1)
  remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()
  
  #fetch html from current remote_driver url
  currenturl <- toString(remote_driver$getCurrentUrl())
  html <- currenturl %>% read_html()

  #scrape results of first keyword data
  test_scrape()
  
  #create dataframe for storing scraping data
  metadata <- data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, stringsAsFactors = FALSE)
  
  count = 1
  while(count <= 3){ 
    
    #watch the video until the end
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    while( state != 0){
      state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    }
    
    #click on next recommended video
    remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a")$clickElement()
                             
    #simulate expanding the video description section - to be able to scrape the description and category, which otherwise stays hidden
    show_more <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/paper-button[2]")
    show_more$clickElement()
    
    #fetch html from current remote_driver url
    currenturl <- toString(remote_driver$getCurrentUrl())
    html <- currenturl %>% read_html()
    
    #scrape the data of current video - update global environment variables
    test_scrape()
    
    #append new row of data to data frame
    rbind(metadata,list(statement, curtitle, current_channel, current_category, span_subs, current_thumbnail, description, famfriend, paid, views, likes, dislikes, links, titles, recc_channs, thumbs))
    count <- count + 1
  }
}

test_scrape <- function(){
  
  #get title of current video
  curtitle <<- html %>%
    html_nodes('h1') %>% 
    html_nodes('span') %>% 
    xml_attr('title')
  
  #get all reccomended video thumbnails
  thumbs <<- html %>%
    html_nodes('img') %>%
    html_attr('data-thumb')
  
  #collapse list of thumbnails
  thumbs <<- paste(thumbs, collapse = ';')
  
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
  likes <<- as.numeric(likes_loc[19])
  
  #get current video dislike count
  dislikes_loc <- html %>%
    html_nodes('span.yt-uix-button-content') %>%
    html_text()
  
  #convert to numeric
  dislikes <<- as.numeric(dislikes_loc[20])
  
  #get current video description
  description <<- html %>% 
    html_nodes(xpath = "//*[@id = 'eow-description']") %>% 
    html_text()

  }
