library(rstudioapi)
library(RSelenium)
library(tidyverse)
library(rvest)
library(pracma)
library(netstat)
library(htmltidy)

setwd(dirname(getSourceEditorContext()$path))
options(stringsAsFactors = FALSE)

cleanText <- function(stringin){
  stringin <- gsub('\n','',stringin)
  stringin <- gsub('\r','',stringin)
  stringin <- gsub('\t','',stringin)
  
  return(stringin)
}

cleanNum <- function(numin){
  numin <- gsub('\t','',numin)
  numin <- gsub(',','',numin)
  numin <- sub("M", "e6", numin, fixed = TRUE)
  numin <- sub("K", "e3", numin, fixed = TRUE)
  numin <- as.integer(numin)
}


####################
# SCRAPE FUNCTION #
####################

scrape <- function(statement, remote_driver){

  tryCatch(
    expr = {
  
  # get current page URL and extract VideoID
  current_url <- remote_driver$getCurrentUrl() %>% unlist()
  current_videoid <- current_url %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")
  videoid <- current_videoid
  
  # Get page source from Selenium and save it to be navigated with Rvest
  html <- remote_driver$getPageSource() %>% unlist()
  write(x = paste("<!doctype html>",html), file = "html.html")
  html <- read_html("html.html")
  
  # read page source with rvest for non personalized data scraping
  html_rvest <- read_html(current_url)
  
  # extract current video title from html
  curtitle <- html %>% html_node(xpath = '//*[@id="container"]/h1') %>% html_text()
  curtitle <- cleanText(curtitle)
  
  # defaul thumbnail format from VideoID
  # https://img.youtube.com/vi/<insert-youtube-video-id-here>/hqdefault.jpg 
  
  # get current video thumbnail
  current_thumbnail <- paste0("https://img.youtube.com/vi/", current_videoid, "/hqdefault.jpg")
  
  # extract current channel from html
  current_channel <- html %>% html_nodes(xpath = '//*[@id="top-row"]/ytd-video-owner-renderer/a') %>% html_attr('href') %>% cleanText()
  
  # extract channel's subs from html
  span_subs <- html %>% html_node(xpath = '//*[@id="owner-sub-count"]') %>% html_text()
  span_subs <- str_remove(span_subs, " subscribers.{0,}")
  span_subs <- cleanNum(span_subs)
  
  # extract video description from html
  description <- html %>% html_node(xpath = '//*[@id="description"]/yt-formatted-string') %>% html_text()
  description <- cleanText(description)
  
  # extract video category from html
  current_category <- html %>% html_node(xpath = '//*[@id="content"]/yt-formatted-string/a') %>% html_text()
  current_category <- cleanText(current_category)
  
  # extract recc video titles from Selenium
  recc_vids_titles_ref <- remote_driver$findElements(value = "//span[@id = 'video-title']")
  recc_vids_titles <- sapply(recc_vids_titles_ref, function(x) x$getElementText())
  recc_vids_titles <- sapply(recc_vids_titles, function(x) cleanText(x))
  titles <- paste(recc_vids_titles, collapse = ';')
  titles <- gsub('\t','',titles)
  
  
  recc_channs <- remote_driver$findElements(value = "//yt-formatted-string[@class = 'style-scope ytd-channel-name']")
  recc_channs <- sapply(recc_channs, function(x) x$getElementText())
  recc_channs <- sapply(recc_channs, function(x) cleanText(x))
  recc_channs <- recc_channs[nchar(recc_channs) > 0]
  recc_channs <- recc_channs[-1]
  recc_channs <- paste(recc_channs, collapse = ';')
  
  # extract and format likes and dislikes from html
  likesdislikes_num <- html %>% html_nodes(xpath = '//*[@id="text"]') %>% html_attr('aria-label')
  
  likes_num <- likesdislikes_num[grep(likesdislikes_num,pattern = " likes")] %>% str_remove(" likes.{0,}")
  likes <- cleanNum(likes_num)
  

  dislikes_num <- likesdislikes_num[grep(likesdislikes_num,pattern = " dislikes")] %>% str_remove(" dislikes.{0,}")
  dislikes <- cleanNum(dislikes_num)
  
  # extract and format views from html
  views_ref <- html %>% html_nodes(xpath = '//*[@id="count"]/yt-view-count-renderer/span[1]')
  views <- views_ref %>% html_text()
  views <- str_remove(views, " views.{0,}")
  views <- cleanNum(views)
  
  #get current video monetization status rvest
  paid <- html_rvest %>%
    html_node('meta[itemprop="paid"]') %>%
    html_attr("content")
  
  #get current video family friendly status rvest
  famfriend <- html_rvest %>%
    html_node('meta[itemprop="isFamilyFriendly"]') %>%
    html_attr("content")
  
  links_ref <- remote_driver$findElements(using = "xpath",value = '//*[@id="dismissable"]/div/div[1]/a')
  links_ref <- sapply(links_ref, function(x) x$getElementAttribute("href"))
  links_ref <- sapply(links_ref, function(x) unlist(x))
  links_ref <- links_ref[links_ref != 'NULL']
  links <- cleanText(links_ref)
  links <- paste(links_ref, collapse = ';')
  
  recc_thumbs <- links_ref %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")
  recc_thumbs <- sapply(recc_thumbs, function(x) paste0("https://img.youtube.com/vi/", x, "/hqdefault.jpg"))
  recc_thumbs <- recc_thumbs[recc_thumbs != 'NULL']
  recc_thumbs <- gsub('\t','',recc_thumbs)
  thumbs <- paste(recc_thumbs, collapse = ';')
  
  if (length(curtitle) == 0) {
    curtitle <- ""
  }
  
  if (length(current_channel) == 0) {
    current_channel <- ""
  }
  
  if (length(current_category) == 0) {
    current_category <- ""
  }
  
  if (length(span_subs) == 0) {
    span_subs <- ""
  }
  
  if (length(current_thumbnail) == 0) {
    current_thumbnail <- ""
  }
  
  if (length(description) == 0) {
    description <- ""
  }
  
  if (length(famfriend) == 0) {
    famfriend <- ""
  }
  
  if (length(paid) == 0) {
    paid <- ""
  }
  
  if (length(views) == 0) {
    views <- ""
  }
  
  if (length(likes) == 0) {
    likes <- ""
  }
  
  if (length(dislikes) == 0) {
    dislikes <- ""
  }
  
  if (length(links) == 0) {
    links <- ""
  }
  
  if (length(titles) == 0) {
    titles <- ""
  }
  
  if (length(recc_channs) == 0) {
    recc_channs <- ""
  }
  
  if (length(thumbs) == 0) {
    thumbs <- ""
  }
  
  line <- data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "VideoID" = videoid, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, InSessionIndex = row, stringsAsFactors = FALSE, check.rows = FALSE)
  return(line)
    
    },
error = function(e){
  statement <- ""
  curtitle <- ""
  current_channel <- ""
  current_category <- ""
  span_subs <- ""
  current_thumbnail <- ""
  description <- ""
  famfriend <- ""
  paid <- ""
  views <- ""
  likes <- ""
  dislikes <- ""
  links <- ""
  titles <- ""
  recc_channs <- ""
  thumbs <- ""
  
  print("Error in scaping")
  
  line <- data.frame("Keyword" = statement, "Title" = curtitle, "Channel" = current_channel, "VideoID" = videoid, "Category" = current_category, "SUB_CNT" = span_subs, "Thumbnail" = current_thumbnail,"Descript" = description, "Fam_friendly" = famfriend, "Monetized" = paid, "VIEW_CNT" = views, "LIKE_CNT" = likes, "DISLIKE_CNT" = dislikes, "recc_videos" = links, "recc_titles" = titles, "recc_channs" = recc_channs,"Thumbnails" = thumbs, InSessionIndex = row, stringsAsFactors = FALSE, check.rows = FALSE)
  return(line)
  }
  )
}


####################
# START CLIENT #
####################


fport <- netstat::free_port()
driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.106")
#driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.16")


remote_driver <- driver[["client"]] 

remote_driver$deleteAllCookies()

remote_driver$navigate("https://www.youtube.com/")
remote_driver$setWindowSize(1920, 1080)


# Use your preferred login credentials
mail <- "."
password <- "."

t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[3]/div[2]/ytd-button-renderer/a/paper-button") %>% unlist() %>% is.null()
while (t) {
  print("Zzzzzz...")
  Sys.sleep(1)
  
  t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[3]/div[2]/ytd-button-renderer/a/paper-button") %>% unlist() %>% is.null()
}

btn <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[3]/div[2]/ytd-button-renderer/a/paper-button")
btn$clickElement()

t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
while (t) {
  print("Zzzzzz...")
  Sys.sleep(1)
  
  t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
}

btn <- remote_driver$findElement(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div/div[1]/div/div[1]/input")
btn$sendKeysToElement(list(mail, key = "enter"))

t <- remote_driver$findElements(using = "xpath",value = "/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div[1]/div/div/div/div/div[1]/div/div[1]/input") %>% unlist() %>% is.null()
while (t) {
  print("Zzzzzz...")
  Sys.sleep(1)
  
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
  
  statement <- searchQueries$Names[row]
  # get YouTube search bar reference, send text(keyword) to it and simulate pressing enter
  print(paste0("Keyword: ",statement))
  
  t <- remote_driver$findElements(using = "name",value = "search_query") %>% unlist() %>% is.null()
  while (t) {
    print("Zzzzzz...")
    Sys.sleep(1)
    
    t <- remote_driver$findElements(using = "name",value = "search_query") %>% unlist() %>% is.null()
  }
  
  address_element <- remote_driver$findElement(using = "name", value = "search_query")
  address_element$clearElement()
  address_element$sendKeysToElement(list(statement, key = "enter")) 
  
  t <- remote_driver$findElements(using = "xpath", '/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]') %>% unlist() %>% is.null()
  while (t) {
    print("Zzzzzz...")
    Sys.sleep(1)
    
    t <- remote_driver$findElement(using = "xpath", '/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]') %>% unlist() %>% is.null()
  }
  
  # check if the element is displayed in one on of the two xpath
  
  oldurl <- remote_driver$getCurrentUrl() %>% unlist()
  
  remote_driver$findElement(using = "xpath", '/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]')$clickElement()
  
  
  Sys.sleep(3)
  
  newurl <- remote_driver$getCurrentUrl() %>% unlist()
  
  if(oldurl == newurl){
    remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/ytd-thumbnail/a")$clickElement()
  }
  
  
  t <- remote_driver$findElements("css", "body") %>% unlist() %>% is.null()
  while (t) {
    print("Zzzzzz...")
    Sys.sleep(1)
    
    t <- remote_driver$findElements("css", "body") %>% unlist() %>% is.null()
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
      Sys.sleep(1)
      
      t <- remote_driver$findElements(using = "xpath",value = '//*[@id="toggle"]') %>% unlist() %>% is.null()
    }
    
    remote_driver$findElement(using = "xpath", value = '//*[@id="toggle"]')$clickElement()
  }
  
  remote_driver$executeScript("return document.querySelector('#head').scrollIntoView(true)")
  
  Sys.sleep(3)
  
  #scrape results of first keyword data
  line <- scrape(statement,remote_driver)
  
  # add data to the dataframe
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
      
      # # sign-in pop-up close
      # test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
      # if (length(test_element) == 1) {
      #   test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
      #   test_element$clickElement() 
      # }
      # 
      # 
      # # close youtube premium ad (if it's in there)
      # test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
      # if (length(test_element) == 1) {
      #   test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
      #   test_element$clickElement() 
      # }  
    }
    
    statement <- searchQueries$Names[row]
    
    print(paste0("Keyword: ",statement, ", click on recc ", count))
    
    t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a") %>% unlist() %>% is.null()
    while (t) {
      print("Zzzzzz...")
      Sys.sleep(1)
      
      t <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a") %>% unlist() %>% is.null()
    }
    
    #click on next recommended video
    remote_driver$findElement(using = "xpath", value = "/html/body/ytd-app/div/ytd-page-manager/ytd-watch-flexy/div[4]/div[2]/div/div[3]/ytd-watch-next-secondary-results-renderer/div[2]/ytd-compact-autoplay-renderer/div[2]/ytd-compact-video-renderer/div[1]/ytd-thumbnail/a")$clickElement()
    
    t <- remote_driver$findElements(using = "xpath",value = '//*[@id="container"]/h1/yt-formatted-string') %>% unlist() %>% is.null()
    while (t) {
      print("Zzzzzz...")
      Sys.sleep(1)
      
      t <- remote_driver$findElements(using = "xpath",value = '//*[@id="container"]/h1/yt-formatted-string') %>% unlist() %>% is.null()
    }
    
    t <- remote_driver$findElements("css", "body") %>% unlist() %>% is.null()
    while (t) {
      print("Zzzzzz...")
      Sys.sleep(1)
      
      t <- remote_driver$findElements("css", "body") %>% unlist() %>% is.null()
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
    
    # Sys.sleep(2)
    
    remote_driver$executeScript("return document.querySelector('#head').scrollIntoView(true)")
    
    Sys.sleep(3)
    
    #scrape the data of current video - update global environment variables
    line <- scrape(statement,remote_driver)
    
    # check if a different video from the last has been scraped and append new row of data to data frame
    # scrapeTwice(metadata$VideoID[nrow(metadata)],videoid)
    
    metadata <- rbind(metadata,line)
    write.table(x = line,file = csvfile,append = T,sep = '\t',row.names = F,col.names = F)
    
    count <- count + 1
  }
  
  # We need to watch the last video for the keyword to the very end / same checks as before
  
  state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
  while( (state != 0) & (toc(echo = F) <= N))
  {
    state <- remote_driver$executeScript("return document.getElementById('movie_player').getPlayerState()")
    
    # # sign-in pop-up close
    # test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
    # if (length(test_element) == 1) {
    #   test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/iron-dropdown/div/yt-tooltip-renderer/div[2]/div[1]/yt-button-renderer/a/paper-button/yt-formatted-string")
    #   test_element$clickElement() 
    # }
    # 
    # 
    # # close youtube premium (if it's in there)
    # test_element <- remote_driver$findElements(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
    # if (length(test_element) == 1) {
    #   test_element <- remote_driver$findElement(using = "xpath",value = "/html/body/ytd-app/ytd-popup-container/paper-dialog/ytd-mealbar-promo-renderer/div/div[2]/ytd-button-renderer[1]/a/paper-button")
    #   test_element$clickElement() 
    # }
  }
  
  
  # break condition after 6 hours
  if (as.numeric(difftime(Sys.time(), timer, units = "mins")) >= 30 ) {
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
