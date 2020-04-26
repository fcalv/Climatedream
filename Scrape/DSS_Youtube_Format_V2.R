#####################################
# ENVIRONMENT
#####################################
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))

library(tidyverse)
library(rvest)
library(XML)
library(jsonlite)

options(stringsAsFactors = FALSE)

#####################################
# GET DATA
#####################################
files <- list.files('results/')
files <- files[grepl('tsv$',files)]
files <- paste0('results/', files)

# sample <-  c("al gore", "anthropogenic climate", "atmospheric co2", "climate science", "co2 emissions", "co2 levels", "global climate", "global cooling", "global temperature", "global warming", "greenhouse gases", "ice age", "industrial revolution", "oil companies", "patrick moore", "scientific method", "sea level")
# sample <- gsub(' ', '', sample)

data <- data.frame()
for(f in files){
  # Select a subset of the session (if relevant)
  # session <- gsub('^results.*results_|_2020.*$', '', f)
  # if(! session %in% sample) next
  # print(session)
    
  d <- read.csv(f, sep='\t', quote = "", row.names = NULL, stringsAsFactors = FALSE)
  
  # Verify that video IDs are valid
  test <- gsub('^.*=', '', d$video_id) %>% clean %>% nchar
  if(FALSE %in% c(test == 11)) next

  data <- rbind(data, d)
  d %>% glimpse 
}

data %>% glimpse


#####################################
# UTILS
#####################################
##### Functions ##### 
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

# WARNING
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
  gsub('^.*watch.*=','', watch_next$compactVideoRenderer$videoId[3:20]) %>% 
    unique %>% clean %>% return 
}

##### Data ##### 
##### List of queries
queries <- data$keyword_ref %>% unique
##### Complete list of video IDs
video_id <- data$video_id %>% unique
reco_video_id <- data$reco_videos_id %>% str_split(' *; *') %>% unlist %>% unique
video_all <- c(video_id, reco_video_id) 
video_all <- gsub('^.*=','', video_all) %>% clean %>% unique 


#####################################
# MAKE NODES 
#####################################
#####  Loop through videos #####
# nodes_backup <- nodes
nodes <- data.frame() 

# If the script is interrupted, restart the loop where it stopped with this line of code:
for(v in video_all[nrow(nodes)+1:length(video_all)]){
# for(v in video_all){
  print(v)
  
  occurence <- data %>% filter(video_id == v) # %>% slice(1)
  occurence_reco <- data %>% filter(grepl(v, reco_videos_id)) # %>% slice(1)
  n_occ <- nrow(occurence) + nrow(occurence_reco)
  
  # Watched
  if(nrow(occurence)!=0){
    session <- occurence$keyword_ref[1]
    session_direct <- occurence$keyword_ref %>% paste(collapse = ';')
    session_all <- c(occurence$keyword_ref) %>% paste(collapse = ';') 
    session_from_reco <- c(occurence_reco$keyword_ref, occurence$keyword_ref) %>% paste(collapse = ';') 
    group <- occurence$keyword_ref[1]
    title <- occurence$title[1]
    description <- occurence$description[1]
    keywords <- occurence$keywords[1]
    genre <- occurence$genre[1]
    date <- occurence$date_publication[1]
    channel_id <- occurence$channel_id[1]
    views <- occurence$views[1]
    duration <- occurence$duration[1]
    
  # Not watched  
  } else { 
    session <- occurence_reco$keyword_ref[1]
    session_direct <- ''
    session_all <- occurence_reco$keyword_ref %>% paste(collapse = ';')
    session_from_reco <- ''
    group <- 'recommendation'
    
    # Scrape video info 
    html <- paste0('https://www.youtube.com/watch?v=', v) %>% read_html
    
    title <- html %>% html_nodes('meta[name=title]') %>% html_attr('content')
    title <- gsub('\t','',title)
    
    description <- html %>% html_nodes('meta[name=description]') %>% html_attr('content')
    description <- gsub('\t','',description)
    
    keywords <- html %>% html_nodes('meta[name=keywords]') %>% html_attr('content')
    keywords <- gsub('\t','',keywords)
    keywords <- gsub(',', ';', keywords)
    
    genre <- html %>% html_nodes('meta[itemprop="genre"]') %>% html_attr('content')
    genre <- gsub(',', ';', genre)
    
    date <- html %>% html_nodes('meta[itemprop="datePublished"]') %>% html_attr('content')
    channel_id <- html %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
    views <- html %>% html_nodes('meta[itemprop="interactionCount"]') %>% html_attr('content')
    duration <- html %>% html_nodes('meta[itemprop="duration"]') %>% html_attr('content')
  }

  # Format sessions list
  session_all <- session_all  %>% str_split(' *; *') %>% unlist %>% clean %>% unique %>% paste(collapse = ';') 
  if(session_all == '') break
  
  # Format views 
  views <- gsub('[^0-9]','', views)
  if(length(views) == 0) views <- '0'
  if(is.null(views)) views <- '0'
  if(is.na(views)) views <- '0'
  if(views=='') views <- '0'
  
  # Format channel_id 
  if(length(channel_id) == 0) channel_id <- ''
  if(is.null(channel_id)) channel_id <- ''
  if(is.na(channel_id)) channel_id <- ''
  
  # Get channel name
  if(nchar(channel_id) > 17){
    channel_url <- paste0('https://www.youtube.com/channel/', channel_id)
    channel <- read_html(channel_url) %>% html_nodes('meta[name=title]') %>% html_attr('content')
    channel <- gsub('\t','',channel)
  } else {
    channel <- ''
  }
  
  # Verif fields
  title <- title %>% check_length(1)
  description <- description %>% check_length(1)
  keywords <- keywords %>% check_length(1)
  genre <- genre %>% check_length(1)
  date <- date %>% check_length(1)
  channel <- channel %>% check_length(1)
  channel_id <- channel_id %>% check_length(1)
  duration <- duration %>% check_length(1)
  
  line <- data.frame(id=v, session=session, 
                     session_direct=session_direct, session_all=session_all, session_from_reco=session_from_reco, 
                     group=group, title=title,
                     description=description, keywords=keywords, genre=genre,
                     date=date, channel=channel, channel_id=channel_id, views=views, 
                     duration=duration, n_occ=n_occ)
  
  # Bind new data
  nodes <- rbind(nodes, line)
  print(line)
}


#####  Format fields #####
nodes <- nodes %>% 
  mutate(title = gsub('<.*>','',title)) %>% 
  mutate(description = gsub('<.*>','',description))  %>% 
  mutate(views = as.numeric(views))

nodes %>% glimpse

#####  Session lists with unique labels ##### 
for(i in 1:nrow(nodes)){
  sessions <- nodes$session_all[i] %>% str_split(' *; *') %>% unlist %>% unique %>% clean 
  nodes$session_n[i] <- sessions %>% length
  nodes$session_all[i] <- sessions %>% paste(collapse=';') 
  sessions <- nodes$session_direct[i] %>% str_split(' *; *') %>% unlist %>% unique %>% clean 
  nodes$session_direct[i] <- sessions %>% paste(collapse=';') 
  sessions <- nodes$session_from_reco[i] %>% str_split(' *; *') %>% unlist %>% unique %>% clean 
  nodes$session_from_reco[i] <- sessions %>% paste(collapse=';') 
}
nodes %>% glimpse
nodes$session_all %>% unique
nodes$session_direct %>% unique
nodes %>% filter(session_direct=='') %>% glimpse



#####  Check number of nodes #####
# Verify number of nodes/videos is consistent
video_all %>% length
nodes$id %>% unique %>% length

#####################################
# MAKE LINKS - LEVEL 1-2
#####################################
#####  Loop through queries #####
links <- data.frame()
for(k in queries){
  d <- data %>% filter(keyword_ref == k) %>% arrange(iteration)
  
  for(i in 1:nrow(d)){
    tar <- d %>% slice(i)
    if(is.na(tar$video_id)) next

    # Direct Links
    if(i != 1){
      src <- d %>% slice(i-1)
      if(!is.na(src$video_id)) {
        new_link_id <- paste(src$video_id, tar$video_id)
        
        if(links %>% filter(link_id==new_link_id) %>% nrow == 0){
          line <- data.frame(source=src$video_id, target=tar$video_id, 
                             session=k, rank=(i-1),
                             session_direct=k, session_all=k, session_n=1,
                             type='direct', level=1, value=1, n_occ=1,
                             link_id=link_id)
          links <- rbind(links, line)
          print(line)
        } else {
          links <- links %>% mutate(session_direct = if_else(
            link_id==link_id, paste0(session_direct,';',k), session_direct
          )) %>% mutate(session_all = if_else(
            link_id==link_id, paste0(session_all,';',k), session_all
          )) %>% 
            mutate(n_occ = if_else(link_id==link_id, n_occ + 1, n_occ)) %>% 
            mutate(type = if_else(link_id==link_id, 'direct', type))
        }
      }
    } 
    
    # Recommendation links
    reco <- tar$reco_videos_id %>% str_split(' *; *') %>% unlist
    reco <- gsub('^.*=','', reco) %>% clean %>% unique 
    
    for(r in reco){
      new_link_id <- paste(tar$video_id, r)
      
      if(links %>% filter(link_id==new_link_id) %>% nrow == 0){
        line <- data.frame(source=tar$video_id, target=r, 
                           session=k, rank=i, 
                           session_direct=k, session_all=k, session_n=1,
                           type='recommendation', level=2, value=1, n_occ=1,
                           link_id=link_id)
        links <- rbind(links, line)
        print(line)
      } else {
        links <- links %>% 
          mutate(session_all = if_else(
            link_id==link_id, paste0(session_all,';',k), session_all )) %>% 
          mutate(n_occ = if_else(link_id==link_id, n_occ + 1, n_occ))
      }
    }
  }
}
links %>% glimpse

#####  Session lists with unique labels ##### 
for(i in 1:nrow(links_all)){
  sessions <- links_all$session_all[i] %>% str_split(' *; *') %>% unlist %>% unique %>% clean 
  links_all$session_n[i] <- sessions %>% length
  links_all$session_all[i] <- sessions %>% paste(collapse=';') 
  sessions <- links_all$session_direct[i] %>% str_split(' *; *') %>% unlist %>% unique %>% clean 
  links_all$session_direct[i] <- sessions %>% paste(collapse=';') 
}
links_all %>% glimpse
links_all$session_all %>% unique
links_all$session_direct %>% unique
links_all %>% filter(session_all=='') %>% glimpse


#####  Check number of nodes ##### 
length(video_all) 
nrow(nodes)
c(links$target, links$source) %>% unique %>% length
nodes$id %>% unique %>% length

links_l2 <- links

#####################################
# !! NEEDS REWORK !! MAKE LINKS - LEVEL 3-4
# #####################################
# #####  Loop through sessions #####
# links_l4 <- links
# for(k in queries){
#   print(k)
#   
#   d <- data %>% filter(keyword_ref == k)
# 
#   for(i in 1:nrow(d)){
#     src <- d %>% slice(i)
# 
#     # Recommendation links
#     reco <- src$reco_videos_id %>% str_split(' *; *') %>% unlist
#     reco <- gsub('^.*=','', reco) %>% unique %>% clean
# 
#     ### Next level of recommendation links
#     for(r in reco){
#       reco_l2 <- get_reco(r)
#       
#       for(r2 in reco_l2){
#         print('L2')
#         print(r2)
#         
#         # Level 3
#         match <- nodes$id[nodes$id==r2] 
#         print('L3')
#         print(match)
#         for(m in match){
#           line <- data.frame(source=r, target=r2, 
#                              session=k, rank=i, 
#                              type='recommendation', level=3, value=1)
#           links_l4 <- rbind(links_l4, line)
#           print(line)
#         }
#           
#         # Level 4
#         match <- reco_videos_all[reco_videos_all==r2]
#         print('L4')
#         print(match)
#         for(m in match){
#           line <- data.frame(source=r, target=r2,
#                              session=k, rank=i,
#                              type='recommendation', level=4, value=1)
#           links_l4 <- rbind(links_l4, line)
#           print(line)
#         }
#       }
#     }
#   }
# }
# #####  Check number of nodes #####  
# length(video_all) 
# nrow(nodes)
# c(links_l4$target, links_l4$source) %>% unique %>% length
# nodes$id %>% unique %>% length
# 
#####################################
# WRITE JSON
#####################################

json <- list(nodes = nodes, links=links_l2)
json %>% toJSON() %>% write('results_v2_L2.json')

json <- list(nodes = nodes, links=links_l4 %>% filter(level!=4))
json %>% toJSON() %>% write('results_v2_L3.json')

json <- list(nodes = nodes, links=links_l4)
json %>% toJSON() %>% write('results_v2_L4.json')

