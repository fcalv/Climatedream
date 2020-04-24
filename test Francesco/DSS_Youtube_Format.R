#####################################
# ENVIRONMENT
#####################################
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))

library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)
library(jsonlite)

options(stringsAsFactors = FALSE)

#####################################
# GET DATA
#####################################
files <- list.files('results/')
files <- files[grepl('tsv$',files)]
files <- paste0('results/', files)

#currenlty hardcoded because results/ contains both files with Emma & Francesco data 
file <- files[97]


data <- data.frame()
for(f in files){
  d <- read.csv(file, sep='\t', quote = "", row.names = NULL, stringsAsFactors = FALSE)
  
  data <- rbind(data, d)
  
  d %>% glimpse 
}

data %>% glimpse


#####################################
# MAKE JSON 
#####################################
##### Labels & Utils #####
data %>% glimpse
keywords <- data %>% select(X.Keyword.) %>% pull %>% unique
video_id <- data %>% select(X.VideoID.) %>% pull %>% unique

#X.recc_titles. should probably be only the video id not the whole link!
reco_videos_id <- data %>% select(X.recc_videos.) %>% pull #%>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "") %>% unlist %>% unique

#TEMPORARY FIX!!! X.recc_videos. should contain reccomeneded video ids not reccomended video links!
for(i in 1:length(reco_videos_id)){
  reco_videos_id[i] <- strsplit(reco_videos_id[i] %>% unlist,";")
  reco_videos_id[i] <- reco_videos_id[i] %>% unlist %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")  %>% paste(collapse = ';')
}

#video_all <- c(video_id, reco_videos_id) 
#video_all <- gsub('^.*=','', video_all) %>% unique
#video_all <- video_all[!is.null(video_all)]
#video_all <- video_all[!is.na(video_all)]
#video_all <- video_all[video_all != 'NA']
#video_all <- video_all[video_all != '']

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

# WARNING - Not currenlty used
# Result differ each time it's run!
get_reco <- function(video_id){
  html <- paste0('https://www.youtube.com/watch?v=',video_id) %>% read_html
  html %>% as.character %>% write('temp.html')
  txt <- readLines('temp.html')
  
  # Find JSON
  for(i in 1:length(txt)){
    if(grepl('RELATED_PLAYER_ARGS', txt[i])) {
      json <- gsub("^ *'RELATED_PLAYER_ARGS': *|,$", '', txt[i]) %>% fromJSON
      watch_next <- json$watch_next_response %>% fromJSON
      watch_next <- watch_next$contents$twoColumnWatchNextResults$secondaryResults$secondaryResults$results
      break
    }
  }
  watch_next$compactVideoRenderer$videoId[3:20] %>% unique %>% clean %>% return 
}

#NOT USED
descr_out <- c("(This channel used to be called MajorPrep, changed as of January 8th, 2020). I make nerdy and occasionally funny videos.")

##### Make Nodes #####
nodes <- data.frame() 
# If the script is interrupted, restart the loop where it stopped with this line of code:
# for(v in video_all[nrow(nodes)+1:length(video_all)]){
for(v in video_id){
  print(v)
  occurence <- data %>% filter(X.VideoID. == v) %>% slice(1)
  
  if(nrow(occurence)==1){
    session <- occurence$X.Keyword.
    title <- occurence$X.Title.
    description <- occurence$X.Descript.
    #keywords <- occurence$keywords
    genre <- occurence$X.Category.
    image <- occurence$X.Thumbnail.
    #date <- occurence$date_publication
    channel <- occurence$X.Channel.
    views <- occurence$X.VIEW_CNT.
    #duration <- occurence$duration
    
  }
  else if(nrow(occurence)==0){
    print("OCCURENCE ROWS IS 0")
    occurence <- data %>% filter(grepl(v, reco_videos_id)) %>% slice(1)
    
    if(nrow(occurence)==0) {
      print('No Occurence')
      break
    }
    
    session <- occurence$keyword_ref
    
    url <- paste0('https://www.youtube.com/watch?v=', v)
    html <- read_html(url)
    
    title <- html %>% html_nodes('meta[name=title]') %>% html_attr('content')
    title <- gsub('\t','',title)
    
    description <- html %>% html_nodes('meta[name=description]') %>% html_attr('content')
    description <- gsub('\t','',description)
    
    if(length(description) == 0) description <- ''
    if(is.null(description)) description <- ''
    if(is.na(description)) description <- ''
    
    if(description %in% c(nodes$description, descr_out)){
      rec_v <- occurence$reco_videos_id %>% str_split(' *; *') %>% unlist
      index <- match(v, rec_v)
      rec_s <- occurence$reco_snippet %>% str_split(' *; *') %>% unlist
      description <- rec_s[index]
    }
    
    #keywords <- html %>% html_nodes('meta[name=keywords]') %>% html_attr('content')
    #keywords <- gsub('\t','',keywords)
    #keywords <- gsub(',', ';', keywords)
    
    genre <- html %>% html_nodes('meta[itemprop="genre"]') %>% html_attr('content')
    genre <- gsub(',', ';', genre)
    
    image <- html %>% html_nodes('meta[property="og:image"]') %>% html_attr('content')
    #date <- html %>% html_nodes('meta[itemprop="datePublished"]') %>% html_attr('content')
    #channel_id <- html %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
    views <- html %>% html_nodes('meta[itemprop="interactionCount"]') %>% html_attr('content')
    #duration <- html %>% html_nodes('meta[itemprop="duration"]') %>% html_attr('content')
  }
  
  # Format views 
  views <- gsub('[^0-9]','', views)
  if(length(views) == 0) views <- '0'
  if(is.null(views)) views <- '0'
  if(is.na(views)) views <- '0'
  if(views=='') views <- '0'
  
  # Format channel_id 
  #if(length(channel_id) == 0) channel_id <- ''
  #if(is.null(channel_id)) channel_id <- ''
  #if(is.na(channel_id)) channel_id <- ''
  
  # Get channel name
  #if(nchar(channel_id) > 17){
  #  channel_url <- paste0('https://www.youtube.com/channel/', channel_id)
  #  channel <- read_html(channel_url) %>% html_nodes('meta[name=title]') %>% html_attr('content')
  #  channel <- gsub('\t','',channel)
  #} else {
  #  channel <- ''
  #}
  
  # Verif all fields
  title <- title %>% check_length(1)
  description <- description %>% check_length(1)
  #keywords <- keywords %>% check_length(1)
  genre <- genre %>% check_length(1)
  image <- image %>% check_length(1)
  #date <- date %>% check_length(1)
  channel <- channel %>% check_length(1)
  #channel_id <- channel_id %>% check_length(1)
  #duration <- duration %>% check_length(1)
  
  line <- data.frame(id=v, group=session, radius=views, title=title,
                     description=description, genre=genre,
                     image=image, channel=channel, views=views, 
                     stringsAsFactors = FALSE)
  
  # Bind new data
  nodes <- rbind(nodes, line)
}

nodes %>% glimpse
video_id %>% length
nodes$id %>% unique %>% length


##### Make Links #####
links <- data.frame()
for(k in keywords){
  d <- data %>% filter(X.Keyword. == k)
  
  # Direct Links
  for(i in 2:nrow(d)){
    src <- d %>% slice(i-1)
    tar <- d %>% slice(i)
    line <- data.frame(source=src$X.VideoID., target=tar$X.VideoID, rank=(i-1), 
                       session=k, type='direct', value=2)
    links <- rbind(links, line)
    print(line)
    
    #TEMPORARY WORKAROUND - Data needs to be fixed to include reccomended video ids and not reccomeneded video links
    reco_videos_src <- src %>% select(X.recc_videos.) %>% pull
    for(i in 1:length(reco_videos_src)){
      reco_videos_src[i] <- strsplit(reco_videos_src[i] %>% unlist,";")
      reco_videos_src[i] <- reco_videos_src[i] %>% unlist %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")  %>% paste(collapse = ';')
    }
    #TEMPORARY WORKAROUND
    
    # Recommendation links
    #reco <- src$reco_videos_id %>% str_split(' *; *') %>% unlist %>% unique %>% clean - BEFORE
    reco <- reco_videos_src %>% str_split(' *; *') %>% unlist %>% unique %>% clean
    reco <- gsub('^.*=','', reco)
    
    for(r in reco){
      line <- data.frame(source=src$X.VideoID, target=r, rank=(i-1), 
                         session=k, type='recommendation', value=1)
      links <- rbind(links, line)
      print(line)
      
      ### 2nd order links - NOT NOW
      #reco_l2 <- get_reco(r)
      #for(r2 in reco_l2){
      #  occurence <- data %>% filter(video_id == r2)
      #  if(nrow(occurence) != 0) {
      #    line <- data.frame(source=src$video_id, target=r2, rank=(i-1), 
      #                       session=k, type='recommendation', value=0.5)
      #    links <- rbind(links, line)
      #    print(line)
      #  }
      #}
    }
    
    # Reco of last video
    #TEMPORARY WORKAROUND - Data needs to be fixed to include reccomended video ids and not reccomeneded video links
    reco_videos_tar <- tar %>% select(X.recc_videos.) %>% pull
    for(i in 1:length(reco_videos_src)){
      reco_videos_tar[i] <- strsplit(reco_videos_tar[i] %>% unlist,";")
      reco_videos_tar[i] <- reco_videos_tar[i] %>% unlist %>% str_extract(pattern = "=.{11}") %>% sub(pattern = "=",replacement = "")  %>% paste(collapse = ';')
    }
    #TEMPORARY WORKAROUND
    
    if(i==nrow(d)){
      #reco <- tar$reco_videos_id %>% str_split(' *; *') %>% unlist %>% unique %>% clean
      reco <- reco_videos_tar %>% str_split(' *; *') %>% unlist %>% unique %>% clean
      reco <- gsub('^.*=','', reco)
      
      for(r in reco){
        line <- data.frame(source=tar$X.VideoID, target=r, rank=(i), 
                           session=k, type='recommendation', value=1)
        links <- rbind(links, line)
        print(line)
        
        ### 2nd order links - NOT NOW
        #reco_l2 <- get_reco(r)
        #for(r2 in reco_l2){
        #  occurence <- data %>% filter(video_id == r2)
        #  if(nrow(occurence) != 0) {
        #    line <- data.frame(source=tar$video_id, target=r2, rank=(i-1), 
        #                       session=k, type='recommendation', value=0.5)
        #    links <- rbind(links, line)
        #    print(line)
        #  }
        #}
      }
    }
  }
}
links %>% glimpse

#####  Check & Complete #####
# Check number of nodes
length(video_id)
nrow(nodes)
c(links$target, links$source) %>% unique %>% length

# Missing nodes
links_n <- c(links$target, links$source) %>% unique
links_n[! links_n %in% video_id]

# Format nodes
main_video <- data %>% select(X.VideoID.) %>% pull %>% unique

nodes_reformat <- nodes %>% 
  mutate(title = gsub('<.*>','',title)) %>% 
  mutate(description = gsub('<.*>','',description))  %>% 
  mutate(radius = as.numeric(views)) %>% 
  mutate(views = as.numeric(views)) %>%
  rename(session = group) %>%
  mutate(group = ifelse(id %in% main_video, nodes$group, 'recommendation'))

nodes_reformat %>% glimpse

links_equi <- links %>% mutate(value = 1)
links_equi %>% glimpse
#####################################
# NODES WITH ALL SESSIONS
#####################################
nodes_all <- nodes_reformat %>% mutate(session_all = session )
nodes_all %>% glimpse

links_id <- c(links_equi$source, links_equi$target) %>% unique

node_new <- data.frame()
for(i in 1:length(links_id)){
  print(i)
  session_list <- c()
  n <- links_id[i]
  #print(n)
  
  link <- links_equi %>% filter(source == n | target == n)
  link_nodes <- c(link$source, link$target) %>% unique
  
  link_nodes_data <- nodes_all %>% filter(id %in% link_nodes)
  session_list <- c(session_list, link_nodes_data$session)
  
  node <- nodes_all %>% filter(id == n)
  session_list <- c(session_list, node$session)
  #print(nrow(node))
  if(nrow(node) != 0){
    node$session_all <- session_list %>% unique %>% paste(collapse = ' ; ')
    #print(nrow(node))
    node_new <- rbind(node_new, node)
  }
}
node_new$session_all
node_new %>% glimpse

#####################################
# WRITE JSON
#####################################

json <- list(nodes = node_new, links=links_equi)
json %>% toJSON() %>% write('results_l2_equi_allsession.json')

json <- list(nodes = node_new, links=links_equi %>% filter(type=='direct'))
json %>% toJSON() %>% write('results_l2_equi_allsession_direct.json')



