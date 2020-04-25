library(rstudioapi)
library(RSelenium)
library(tidyverse)
library(rvest)
library(netstat)

setwd(dirname(getSourceEditorContext()$path))
options(stringsAsFactors = FALSE)

# name for the output file
scrapeID <- "skept_2_test_rank"

######################
#### FUNCTIONS #######
######################

cleanText <- function(stringin){
  stringin <- gsub('\n',' ',stringin)
  stringin <- gsub('\r',' ',stringin)
  stringin <- gsub('\t',' ',stringin)
  if(is.na(stringin)) stringin <- " "
  if(is.null(stringin)) stringin <- " "
  if(stringin == "") stringin <- " "
  
  return(stringin)
}

# must be adjusted accordingly to the browser locale
cleanNum <- function(numin){
  numin <- gsub('\t','',numin)
  numin <- sub('K', 'e3', numin)
  numin <- sub('\\mln.*','e6', numin,perl = T)
  numin <- gsub('\\.','',numin)
  numin <- gsub(',','.',numin)
  numin <- gsub('[^a-zA-Z0-9\\.]','',numin)
  if(!grepl('e3|e6',numin)) numin <- gsub('[a-zA-Z]','',numin)
  numin <- as.integer(numin)
  
  return(numin)
}

# scrapes static metadata from a url passed as argument, returns data frame with 10 obs
scrapeStaticMetadata <- function(link){
  
  link <- sub('https','http',link)
  
  html <- read_html(link)
  html %>% write_html("htmlrvest.html")
  
  title <- html %>% html_node(xpath = "/html/head/title") %>% html_text()
  title <- cleanText(title)
  
  videoID <- link %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")
  
  channel <- html %>% html_node(xpath = '//*[@id="watch7-user-header"]/div/a') %>% html_text()
  channel <- cleanText(channel)
  
  channelID <- html %>% html_node(xpath = '//*[@id="watch7-user-header"]/div/a') %>% html_attr('href')
  
  subs <- html %>% html_node(xpath = '//*[@id="watch7-subscription-container"]/span/span[2]') %>% html_attr('title')
  subs <- cleanNum(subs)
  
  desc <- html %>% html_node(xpath = '//*[@id="eow-description"]') %>% html_text()
  desc <- cleanText(desc)
  
  likes <- html %>% html_node(xpath = '//*[@id="watch8-sentiment-actions"]/span/span[2]/button/span') %>% html_text()
  likes <- cleanNum(likes)
  
  dislikes <- html %>% html_node(xpath = '//*[@id="watch8-sentiment-actions"]/span/span[4]/button/span') %>% html_text()
  dislikes <- cleanNum(dislikes)
  
  views <- html %>% html_node(xpath = '//*[@id="watch7-views-info"]/div[1]') %>% html_text()
  views <- cleanNum(views)
  
  thumb <- paste0("https://img.youtube.com/vi/", videoID, "/hqdefault.jpg")
  
  keyw <- html %>% html_node(xpath = '/html/head/meta[4]') %>% html_attr('content')
  keyw <- cleanText(keyw)
  
  cat <- html %>% html_node(xpath = '//*[@id="watch7-content"]/meta[16]') %>% html_attr('content')
  cat <- cleanText(cat)
  
  famfr <- html %>% html_node(xpath = '//*[@id="watch7-content"]/meta[11]') %>% html_attr('content')
  famfr <- cleanText(famfr)
  
  pub <- html %>% html_node(xpath = '//*[@id="watch7-content"]/meta[14]') %>% html_attr('content')
  pub <- cleanText(pub)
  
  paid <- html %>% html_node('meta[itemprop="paid"]') %>%  html_attr("content")
  paid <- cleanText(paid)
  
  line <- data.frame(Title = title,
                     VideoID = videoID, 
                     Channel = channel,
                     ChannelID = channelID,
                     SUB_CNT = subs,
                     Descript = desc,
                     LIKE_CNT = likes,
                     DISLIKES_CNT = dislikes,
                     VIEW_CNT = views, 
                     Thumbnail = thumb,
                     Keyword = keyw,
                     Category = cat,
                     Fam_friendly = famfr,
                     Monetized = paid,
                     Published = pub)
  return(line)
}

########################
##### START CLIENT #####
########################

fport <- netstat::free_port()
driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.106")
#driver <- rsDriver(browser = c("chrome"), port=fport, chromever="80.0.3987.16")


remote_driver <- driver[["client"]] 

remote_driver$deleteAllCookies()


#######################################
### NAVIGATE TO YOUTUBE AND LOGIN  ####
#######################################

remote_driver$navigate("https://www.youtube.com/")
remote_driver$setWindowSize(1920, 1080)


# Use your preferred login credentials
mail <- "annypowell1@gmail.com"
password <- "r#'8e$hGN'{!D=m"

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

Sys.sleep(5)


######################
##### SCRAPING ######
#####################

#initialize empty dataframe
metadata <- data.frame(Title = character(),
                       VideoID = character(), 
                       Channel = character(),
                       ChannelID = character(),
                       SUB_CNT = numeric(),
                       Descript = character(),
                       LIKE_CNT = numeric(),
                       DISLIKES_CNT = numeric(),
                       VIEW_CNT = numeric(), 
                       Thumbnail = character(),
                       Keyword = character(),
                       Category = character(),
                       Fam_friendly = character(),
                       Monetized = character(),
                       Published = character())
#create tsv file
csvfile <- paste0('results/homepage_recc_',gsub(' ','',scrapeID),'_',gsub('[^0-9]','_',Sys.time()),'.tsv')

#number of videos to scrape (use multiples of 10 for now)
N <- 100
Sys.sleep(5)


while (nrow(metadata) < N) {
  elem <- remote_driver$findElements(using = "xpath", value = '//*[@id="video-title-link"]')
  links <- sapply(elem, function(x) x$getElementAttribute('href')) %>% unlist()
  # clean timestamps
  links <- gsub("\\&.*","",links)
  links <- links[grep(pattern = 'watch\\?v=.{11}',x = links)]
  links <- links %>% head(10)
  
  print(paste0("Scrape info from videos ",nrow(metadata), " to ", nrow(metadata) + 10))

  for(row in 1:length(links)){
    line <- scrapeStaticMetadata(links[row])
    
    if(line$Title=="YouTube") line <- scrapeStaticMetadata(links[row])
    
    line$Rank=row
    
    metadata <- rbind(metadata,line)
    
    print(paste("Video ", row))
  }
  
  remote_driver$refresh()
  Sys.sleep(5)
}

############################
#### CLOSE CONNECTION ######
############################

remote_driver$closeall()
driver$server$stop()

write.table(csvfile,x = metadata,sep = "\t",row.names = F)

read.delim(csvfile,sep = '\t',fill = F,quote = "") %>% glimpse()


###################
