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
#address_element$sendKeysToElement(sendKeys = list(key = 'control', key = "a"))
#address_element$sendKeysToElement(sendKeys = list(key = "\ue009", key = "a"))
#address_element$sendKeysToElement(list(key = "backspace"))

#saddress_element$sendKeys(Keys.ENTER);
#button_element <- remote_driver$findElement(using = 'id', value = "button")
#button_element <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/ytd-searchbox/yt-icon-button/button")
#remote_driver$findElement(using = 'id', value = "button")$clickElement()
#button_element$clickElement()


#first keyword video
remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()

count = 1
#metadata <- data.frame("Title" = c("first"),Keyword = c("first"), "ReccTitles" = c("first"), "ReccChannels" = c("first"), "ReccImg" = c("first"), stringsAsFactors = FALSE)
#not needed since using executeScript
#previous_video_xpath =  "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a"
Top100GlobalWarming <- read.csv("C:/visualizationworkshop/Top100GlobalWarming.csv", header=TRUE, sep=";")

#apply(Top100GlobalWarming, 1, function(row) {
for (row in 1:nrow(Top100GlobalWarming)){
  statement <- paste(Top100GlobalWarming["word1"], Top100GlobalWarming["word2"], sep=" ")
  #Top100GlobalWarming['word1']
  
  address_element <- remote_driver$findElement(using = 'id', value = 'search')
  address_element$sendKeysToElement(list(statement, key = "enter"))#first keyword
  
  Sys.sleep(1)
  remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()
  
  #Zdaj pa get all video elements on this url via rvest and then process them and order the list etc 
  #subsequently choose a video based on some of this metadata
  currenturl <- toString(remote_driver$getCurrentUrl())
  html <- currenturl %>% read_html()
  #write(html %>% as.character(), 'source.html')
  
  test_scrape()
  
  metadata <- data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = span_texts,"Thumbnails" = thumbs, stringsAsFactors = FALSE)

  while(count <= 3){ 
    #first recc video
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    while( state != 0){
      state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    }
    
    remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[12]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a")$clickElement()
    
    show_more <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/paper-button[2]")
    show_more$clickElement()
    
    currenturl <- toString(remote_driver$getCurrentUrl())
    html <- currenturl %>% read_html()
    
    test_scrape()
    
    rbind(metadata,list(statement, curtitle, current_channel, current_category, span_subs, current_thumbnail, description, famfriend, paid, views, likes, dislikes, links, titles, span_texts, thumbs))
    count <- count + 1
  }
  remote_driver$navigate("https://www.youtube.com/")
  remote_driver$deleteAllCookies()
}

test_scrape <- function(){
  
  curtitle <<- html %>%
    html_nodes('h1') %>% 
    html_nodes('span') %>% 
    xml_attr('title')
  
  thumbs <<- html %>%
    html_nodes('img') %>%
    html_attr('data-thumb')
  
  thumbs <<- paste(thumbs, collapse = ';')
  
  links <<- html %>%
    html_nodes('a') %>%
    html_attr('href')
  
  links <<- links[links %>% str_detect('/watch')]
  
  links[!is.na(links)]
  links <<- paste(links, collapse = ';')
  
  titles <<- html %>%
    html_nodes("a") %>%
    html_attr("title")
  
  titles <<- titles[!is.na(titles)]
  
  titles <<- paste(titles, collapse = ';')
  
  current_category <<- html %>%
    html_node('meta[itemprop="genre"]') %>%
    html_attr("content")
  
  #another method
  #category <- html %>%
  #  html_node('#content') %>%
  #  html_nodes('a') %>%
  #  html_attr('href') 
  
  #category <- category[category %>% str_detect('/[a-z]*$')]
  
  current_channel <<- html %>%
    html_node('div[class="yt-user-info"]') %>%
    html_nodes('a') %>%
    html_text()
  
  
  current_thumbnail <<- html %>%
    html_node('link[itemprop="thumbnailUrl"]') %>%
    html_attr("href") 
  
  span_texts <- html %>%
    html_nodes('span') %>%
    html_text()
  
  attributes <- html %>%
    html_nodes('span') %>%
    xml_attr('class')
  
  span_texts <-  span_texts[attributes %>% str_detect('stat attribution')]
  
  span_texts <- span_texts[!is.na(span_texts)]
  
  recc_channs <<- paste(span_texts, collapse = ';')
  
  subs_loc <- html %>%
    html_nodes('span') %>%
    html_text()
  
  subs_loc <- subs_loc[attributes %>% str_detect('yt-subscription-button-subscriber-count-branded-horizontal yt-subscriber-count')]
  
  span_subs <<- subs_loc[!is.na(subs_loc)]
  #xml_ns_strip(html)
  
  views <<- html %>%
    html_node('meta[itemprop="interactionCount"]') %>%
    html_attr("content")
  
  paid <<- html %>%
    html_node('meta[itemprop="paid"]') %>%
    html_attr("content")
  
  famfriend <<- html %>%
    html_node('meta[itemprop="isFamilyFriendly"]') %>%
    html_attr("content")
  
  likes_loc <- html %>%
    html_nodes('span.yt-uix-button-content') %>%
    html_text()
  
  likes <<- as.numeric(likes_loc[19])
  
  dislikes_loc <- html %>%
    html_nodes('span.yt-uix-button-content') %>%
    html_text()
  dislikes <<- as.numeric(dislikes_loc[20])
  
  description <<- html %>% 
    html_nodes(xpath = "//*[@id = 'eow-description']") %>% 
    html_text()

  }
