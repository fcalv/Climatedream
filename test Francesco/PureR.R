library(rstudioapi)
library(RSelenium)
library(tidyverse)
library(rvest)
library(pracma)
library(netstat)
library(htmltidy)

setwd(dirname(getSourceEditorContext()$path))
options(stringsAsFactors = FALSE)

####################
# SCRAPE FUNCTION #
####################

scrape <- function(){

  tryCatch(
    expr = {
  
      current_url <- remote_driver$getCurrentUrl() %>% unlist()
  current_videoid <- current_url %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")
  videoid <<- current_videoid
  
  html <- remote_driver$getPageSource() %>% unlist()
  write(x = paste("<!doctype html>",html), file = "html.html")
  html <- read_html("html.html")
  
  #vid_ref <- remote_driver$findElement(using = "xpath", value="/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/h1/yt-formatted-string")
  curtitle <- html %>% html_node(xpath = '//*[@id="container"]/h1') %>% html_text()
  curtitle <<- gsub('\t','',curtitle)
  
# https://img.youtube.com/vi/<insert-youtube-video-id-here>/hqdefault.jpg 
  
  #get current video thumbnail
  current_thumbnail <<- paste0("https://img.youtube.com/vi/", current_videoid, "/hqdefault.jpg")
  
  #channelname_ref <- remote_driver$findElement(using = "xpath", value="/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/div[2]/ytd-video-owner-renderer/div[1]/ytd-channel-name/div/div/yt-formatted-string/a")
  current_channel <<- html %>% html_nodes(xpath = '//*[@id="top-row"]/ytd-video-owner-renderer/a') %>% html_attr('href')
  
  #subs_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/div[2]/ytd-video-owner-renderer/div[1]/yt-formatted-string")
  span_subs <- html %>% html_node(xpath = '//*[@id="owner-sub-count"]') %>% html_text()
  span_subs <- str_remove(span_subs, " subscribers")
  span_subs <- gsub(',','',span_subs)
  span_subs <- sub("M", "e6", span_subs, fixed = TRUE)
  span_subs <- sub("K", "e3", span_subs, fixed = TRUE)
  span_subs <- gsub('\t','',span_subs)
  span_subs <<- as.integer(span_subs)
  
  #desc_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/div/div/yt-formatted-string")
  description <- html %>% html_node(xpath = '//*[@id="description"]/yt-formatted-string') %>% html_text()
  description <- gsub('\n','',description)
  description <- gsub('\r','',description)
  description <<- gsub('\t','',description)
  
  #category_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[9]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/ytd-metadata-row-container-renderer/div[2]/ytd-metadata-row-renderer/div/yt-formatted-string/a")
  current_category <- html %>% html_node(xpath = '//*[@id="content"]/yt-formatted-string/a') %>% html_text()
  current_category <<- gsub('\t','',current_category)
  
  recc_vids_titles_ref <- remote_driver$findElements(value = "//span[@id = 'video-title']")
  recc_vids_titles <- sapply(recc_vids_titles_ref, function(x) x$getElementText())
  titles <- paste(recc_vids_titles, collapse = ';')
  titles <<- gsub('\t','',titles)
  
  
  recc_channs <- remote_driver$findElements(value = "//yt-formatted-string[@class = 'style-scope ytd-channel-name']")
  recc_channs <- sapply(recc_channs, function(x) x$getElementText())
  recc_channs <- recc_channs[nchar(recc_channs) > 0]
  recc_channs <- recc_channs[-1]
  recc_channs <- paste(recc_channs, collapse = ';')
  recc_channs <<- gsub('\t','',recc_channs)
  
  #likes_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/div/div[3]/div/ytd-menu-renderer/div/ytd-toggle-button-renderer[1]/a/yt-formatted-string")
  likesdislikes_num <- html %>% html_nodes(xpath = '//*[@id="text"]') %>% html_attr('aria-label')
  
  likes_num <- likesdislikes_num[grep(likesdislikes_num,pattern = " likes")] %>% str_remove(" likes")
  likes_num <- gsub(",", '', likes_num)
  likes_num <- gsub('\t','',likes_num)
  likes_num <- sub("K", "e3", likes_num, fixed = TRUE)
  likes_num <- sub("M", "e6", likes_num, fixed = TRUE)
  likes <<- as.integer(likes_num)
  
  #dislikes_ref <- remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[2]/ytd-video-primary-info-renderer/div/div/div[3]/div/ytd-menu-renderer/div/ytd-toggle-button-renderer[2]/a/yt-formatted-string")
  #dislikes_num <- dislikes_ref$getElementText()
  dislikes_num <- likesdislikes_num[grep(likesdislikes_num,pattern = " dislikes")] %>% str_remove(" dislikes")
  dislikes_num <- gsub(",", '', dislikes_num)
  dislikes_num <- gsub('\t','',dislikes_num)
  dislikes_num <- gsub(',','',dislikes_num)
  dislikes_num <- sub("K", "e3", dislikes_num, fixed = TRUE)
  dislikes_num <- sub("M", "e6", dislikes_num, fixed = TRUE)
  dislikes <<- as.integer(dislikes_num)
  
  views_ref <- html %>% html_nodes(xpath = '//*[@id="count"]/yt-view-count-renderer/span[1]')
  views <- views_ref %>% html_text()
  views <- str_remove(views, " views.{0,}")
  views <- gsub(',','',views)
  views <- gsub('\t','',views)
  views <<- as.integer(views)
  
  # recc_thumbs_ref <- remote_driver$findElements(value = "//img[@class = 'style-scope yt-img-shadow']")
  # recc_thumbs <- sapply(recc_thumbs_ref, function(x) x$getElementAttribute("src"))
  # recc_thumbs <- sapply(recc_thumbs, function(x) unlist(x))
  # recc_thumbs <- recc_thumbs[recc_thumbs != 'NULL']
  # recc_thumbs <- gsub('\t','',recc_thumbs)
  # thumbs <<- paste(recc_thumbs, collapse = ';')

  
  #get current video monetization status
  paid <<- html_rvest %>%
    html_node('meta[itemprop="paid"]') %>%
    html_attr("content")
  
  #get current video family friendly status
  famfriend <<- html_rvest %>%
    html_node('meta[itemprop="isFamilyFriendly"]') %>%
    html_attr("content")
  
  links_ref <- remote_driver$findElements(using = "xpath",value = '//*[@id="dismissable"]/div/div[1]/a')
  links_ref <- sapply(links_ref, function(x) x$getElementAttribute("href"))
  links_ref <- sapply(links_ref, function(x) unlist(x))
  links_ref <- links_ref[links_ref != 'NULL']
  links <- paste(links_ref, collapse = ';')
  links <<- gsub('\t','',links)
  
  recc_thumbs <- links_ref %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")
  recc_thumbs <- sapply(recc_thumbs, function(x) paste0("https://img.youtube.com/vi/", x, "/hqdefault.jpg"))
  recc_thumbs <- recc_thumbs[recc_thumbs != 'NULL']
  recc_thumbs <- gsub('\t','',recc_thumbs)
  thumbs <<- paste(recc_thumbs, collapse = ';')
    
    },
error = function(e){
  statement <<- ""
  curtitle <<- ""
  current_channel <<- ""
  current_category <<- ""
  span_subs <<- ""
  current_thumbnail <<- ""
  description <<- ""
  famfriend <<- ""
  paid <<- ""
  views <<- ""
  likes <<- ""
  dislikes <<- ""
  links <<- ""
  titles <<- ""
  recc_channs <<- ""
  thumbs <<- ""
  }
  )
  
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


####################
# START CLIENT #
####################


fport <- netstat::free_port()
driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.106")
#driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.16")


remote_driver <- driver[["client"]] 
# Causes error
#remote_driver$setTimeout(type = "page load", milliseconds = 1000)

remote_driver$deleteAllCookies()

remote_driver$navigate("https://www.youtube.com/")


# Use your preferred login credentials
mail <- "annypowell1@gmail.com"
password <- "r#'8e$hGN'{!D=m"


t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[3]/div[2]/ytd-button-renderer/a/paper-button") %>% unlist() %>% is.null()
while (t) {
  print("Zzzzzz...")
  Sys.sleep(0.5)
  
  t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[3]/div[2]/ytd-button-renderer/a/paper-button") %>% unlist() %>% is.null()
}

btn <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[3]/div[2]/ytd-button-renderer/a/paper-button")
btn$clickElement()

t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
while (t) {
  print("Zzzzzz...")
  Sys.sleep(0.5)
  
  t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
}

btn <- remote_driver$findElement(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div/div[1]/div/div[1]/input")
btn$sendKeysToElement(list(mail, key = "enter"))

t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div[1]/div/div/div/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
while (t) {
  print("Zzzzzz...")
  Sys.sleep(0.5)
  
  t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div[1]/div/div/div/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
}

btn <- remote_driver$findElement(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div[1]/div/div/div/div/div[1]/div/div[1]/input")
btn$sendKeysToElement(list(password, key = "enter"))





#########################
# SCRAPING
########################

searchQueries <- read.csv("climatechangeskeptics.txt", header=TRUE, sep="\t")


metadata <- data.frame(Keyword = character(), Title = character(), Channel = character(), VideoID = character(), Category = character(), SUB_CNT = character(), Thumbnail = character(), Descript = character(), Fam_friendly = character(), Monetized = character(), VIEW_CNT = character(), LIKE_CNT = character(), DISLIKE_CNT = character(), recc_videos = character(), recc_titles = character(), recc_channs = character(),Thumbnails = character(), InSessionIndex = character())

csvfile <- paste0('results/scrape_results_',gsub(' ','',"searchquery"),'_',gsub('[^0-9]','_',Sys.time()),'.tsv')
metadata %>% write.table(file = csvfile, sep = '\t')



timer <- Sys.time()
video_count <- 0


### YOUTUBE SEARCH AND WATCH FIRST RESULT ###


for (row in 1:nrow(searchQueries)){
  
  video_count <- video_count + 1

  Sys.sleep(2)
  
  statement <- searchQueries$Names[row]
  # get YouTube search bar reference, send text(keyword) to it and simulate pressing enter
  print(paste0("Keyword: ",statement))
  
  t <- remote_driver$findElements(using = "name",value = "search_query") %>% unlist() %>% is.null()
  while (t) {
    print("Zzzzzz...")
    Sys.sleep(0.5)
    
    t <- remote_driver$findElements(using = "name",value = "search_query") %>% unlist() %>% is.null()
  }
  
  address_element <- remote_driver$findElement(using = "name", value = "search_query")
  address_element$clearElement()
  address_element$sendKeysToElement(list(statement, key = "enter")) 

  #wait for 1 second for everything to load properly
  Sys.sleep(2)
  
    
  t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a") %>% unlist() %>% is.null()
  while (t) {
    print("Zzzzzz...")
    Sys.sleep(0.5)
    
    t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a") %>% unlist() %>% is.null()
  }
  
  remote_driver$findElement(using = "xpath", '/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a')$clickElement()
  Sys.sleep(1)
  
  t <- remote_driver$findElements(using = "xpath",value = '//*[@id="container"]/h1/yt-formatted-string') %>% unlist() %>% is.null()
  while (t) {
    print("Zzzzzz...")
    Sys.sleep(0.5)
    
    t <- remote_driver$findElements(using = "xpath",value = '//*[@id="container"]/h1/yt-formatted-string') %>% unlist() %>% is.null()
  }
  
  # Scroll down to read more
  remote_driver$executeScript("return document.querySelector('#container').scrollIntoView(true)")
  # Element for scrolling later
  webElem <- remote_driver$findElement("css", "body")
  # Click show more
  show_more <- remote_driver$findElement(using = 'xpath', value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[1]/div/div[7]/div[3]/ytd-video-secondary-info-renderer/div/ytd-expander/paper-button[2]")
  show_more$clickElement()
  # Go back up
  webElem$sendKeysToElement(list(key = "home"))
  
  # disable autoplay
  if (row == 1) {
    t <- remote_driver$findElements(using = "xpath",value = '//*[@id="toggle"]') %>% unlist() %>% is.null()
    
    while (t) {
      print("Zzzzzz...")
      Sys.sleep(0.5)
      
      t <- remote_driver$findElements(using = "xpath",value = '//*[@id="toggle"]') %>% unlist() %>% is.null()
    }
    
    a <- remote_driver$findElement(using = "xpath", value = '//*[@id="toggle"]')
    a$clickElement()
  }
  
  #fetch html from current remote_driver url
  currenturl <- toString(remote_driver$getCurrentUrl())
  #html <- currenturl %>% read_html()
   Sys.sleep(1)
  #scrape results of first keyword data
  scrape()
  
  # add data to the dataframe

  line <- data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "VideoID" = videoid, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, InSessionIndex = row, stringsAsFactors = FALSE, check.rows = FALSE)

  
  metadata <- rbind(metadata,line)
  write.table(x = line,file = csvfile,append = T,sep = '\t',row.names = F,col.names = F)
  
  ### WATCH 3 RECC VIDEOS ###
  
  count = 1
  while(count <= 3){ 
    
    video_count <- video_count + 1
    # Timeout for (very) long videos and live streams (stop watching after N seconds), for now 1h
    tic(gcFirst = T)
    N <- 30
    
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
    
    statement <- searchQueries$Names[row]
    
    print(paste0("Keyword: ",statement, ", click on recc ", count))
    
    t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a") %>% unlist() %>% is.null()
    while (t) {
      print("Zzzzzz...")
      Sys.sleep(0.5)
      
      t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a") %>% unlist() %>% is.null()
    }
    
    #click on next recommended video
    remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a")$clickElement()
    
    t <- remote_driver$findElements(using = "xpath",value = '//*[@id="container"]/h1/yt-formatted-string') %>% unlist() %>% is.null()
    while (t) {
      print("Zzzzzz...")
      Sys.sleep(0.5)
      
      t <- remote_driver$findElements(using = "xpath",value = '//*[@id="container"]/h1/yt-formatted-string') %>% unlist() %>% is.null()
    }
    
    
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
    html_rvest <- currenturl %>% read_html()
    
    #scrape the data of current video - update global environment variables
    scrape()
    
    #append new row of data to data frame

    line <- data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "VideoID" = videoid, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, InSessionIndex = row, stringsAsFactors = FALSE, check.rows = FALSE)

    
    metadata <- rbind(metadata,line)
    write.table(x = line,file = csvfile,append = T,sep = '\t',row.names = F,col.names = F)
    
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
  if (as.numeric(difftime(Sys.time(), timer, units = "mins")) >= 5 ) {
    break
  }
  
}

##########################
### CLOSE SERVER    ######
##########################
print("Timer has expired")

remote_driver$closeall()
driver$server$stop()

read.table(csvfile,header = T) %>% glimpse()
