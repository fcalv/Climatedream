################
### Original ###
################
library(RSelenium)
library(tidyverse)
library(rvest)

driver <- rsDriver(browser = c("firefox"), port=5555L)
driver <- rsDriver(browser = c("chrome"), port=5405L, chromever="80.0.3987.16")

remote_driver <- driver[["client"]] 
remote_driver$navigate("https://www.youtube.com/")
address_element <- remote_driver$findElement(using = 'id', value = 'search')
address_element$sendKeysToElement(list("first trial", key = "enter"))
#address_element$sendKeys(Keys.ENTER);
#button_element <- remote_driver$findElement(using = 'id', value = "button")
#button_element <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/ytd-searchbox/yt-icon-button/button")
#remote_driver$findElement(using = 'id', value = "button")$clickElement()
#button_element$clickElement()

#first keyword video
remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()

count = 1
metadata <- data.frame("Title" = c("first"),Keyword = c("first"), "ReccTitles" = c("first"), "ReccChannels" = c("first"), "ReccImg" = c("first"), stringsAsFactors = FALSE)
#not needed since using executeScript
#previous_video_xpath =  "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a"

while(count <= 3){
  #first recc video
  state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
  while( state != 0){
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
  }
  
  remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[12]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/div/div[1]/a")$clickElement()
  
  #links PROOF OF DATAFRAME CONCEPT NOT TRUE DATA "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[12]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/div/div[1]/a"
  links <- html %>%
    html_nodes('h3') %>%
    html_nodes('a') %>%
    html_attr('href')
  
  links <- paste(links, collapse = ',')
  
  titles <- html %>%
    html_nodes('h3') %>%
    html_nodes('a') %>%
    html_text()
  
  titles <- paste(titles, collapse = ',')
  
  channels <- html %>%
    html_nodes('.yt-uix-shelfslider-list') %>%
    html_nodes('.yt-lockup-byline') %>%
    html_nodes('a') %>%
    html_attr('href')
  
  channels <- paste(channels, collapse = ',')
  
  images <- html %>%
    html_nodes('img') %>%
    html_attr('src')
  
  images <- paste(images, collapse = ',')
  
  curtitle <- html %>%
    html_nodes('h1') %>% 
    html_nodes('span') %>% 
    xml_attr('title')
  
  rbind(metadata,list(curtitle,c("first trial"),titles,channels,images))
  count <- count + 1
}

#Zdaj pa get all video elements on this url via rvest and then process them and order the list etc 
#subsequently choose a video based on some of this metadata
currenturl <- toString(remote_driver$getCurrentUrl())
html <- currenturl %>% read_html()

#pri tem ne ves ce potem positive feedback-as recomender system al ne ker ne kliknes direktno na recomendation ampak samo vzames link in mu potem sledis
yta <- html %>%
  html_nodes('h3') %>%
  html_nodes('a') %>%
  html_attr('href') 

ytat <- html %>%
  html_nodes('ytd-thumbnail')
  html_nodes('a')

remote_driver$navigate(paste("https://www.youtube.com",yta[as.integer(runif(1,min=0,max=20))],sep = ""))


