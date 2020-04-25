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
  
  # Watched
  if(nrow(occurence)!=0){
    session <- occurence$keyword_ref[1]
    session_direct <- occurence$keyword_ref %>% paste(collapse = ';')
    session_all <- c(occurence$keyword_ref,occurence_reco$keyword_ref) %>% paste(collapse = ';') 
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
  
  line <- data.frame(id=v, session=session, session_direct=session_direct, session_all=session_all, group=group, title=title,
                     description=description, keywords=keywords, genre=genre,
                     date=date, channel=channel, channel_id=channel_id, views=views, 
                     duration=duration)
  
  # Bind new data
  nodes <- rbind(nodes, line)
}


#####  Format fields #####
nodes <- nodes %>% 
  mutate(title = gsub('<.*>','',title)) %>% 
  mutate(description = gsub('<.*>','',description))  %>% 
  mutate(views = as.numeric(views))

nodes %>% glimpse

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
        line <- data.frame(source=src$video_id, target=tar$video_id, 
                           session=k, rank=(i-1), 
                           type='direct', level=1, value=1)
        links <- rbind(links, line)
        print(line)
      }
    } 
    
    # Recommendation links
    reco <- tar$reco_videos_id %>% str_split(' *; *') %>% unlist
    reco <- gsub('^.*=','', reco) %>% clean %>% unique 
    
    for(r in reco){
      if(is.na(r)) next
      line <- data.frame(source=tar$video_id, target=r, 
                         session=k, rank=i, 
                         type='recommendation', level=2, value=1)
      links <- rbind(links, line)
      print(line)
    }
  }
}
links %>% glimpse

#####  Check number of nodes #####  
length(video_all) 
nrow(nodes)
c(links$target, links$source) %>% unique %>% length
nodes$id %>% unique %>% length

links_l2 <- links

#####################################
# MAKE LINKS - LEVEL 3-4
#####################################
#####  Loop through sessions #####
links_l4 <- links
for(k in queries){
  print(k)
  
  d <- data %>% filter(keyword_ref == k)

  for(i in 1:nrow(d)){
    src <- d %>% slice(i)

    # Recommendation links
    reco <- src$reco_videos_id %>% str_split(' *; *') %>% unlist
    reco <- gsub('^.*=','', reco) %>% unique %>% clean

    ### Next level of recommendation links
    for(r in reco){
      reco_l2 <- get_reco(r)
      
      for(r2 in reco_l2){
        print('L2')
        print(r2)
        
        # Level 3
        match <- nodes$id[nodes$id==r2] 
        print('L3')
        print(match)
        for(m in match){
          line <- data.frame(source=r, target=r2, 
                             session=k, rank=i, 
                             type='recommendation', level=3, value=1)
          links_l4 <- rbind(links_l4, line)
          print(line)
        }
          
        # Level 4
        match <- reco_videos_all[reco_videos_all==r2]
        print('L4')
        print(match)
        for(m in match){
          line <- data.frame(source=r, target=r2,
                             session=k, rank=i,
                             type='recommendation', level=4, value=1)
          links_l4 <- rbind(links_l4, line)
          print(line)
        }
      }
    }
  }
}
#####  Check number of nodes #####  
length(video_all) 
nrow(nodes)
c(links_l4$target, links_l4$source) %>% unique %>% length
nodes$id %>% unique %>% length

#####################################
# NODES WITH ALL SESSIONS & No. OCCURENCE
#####################################
#####  Loop through nodes #####
video_id_list <- c(links$source, links$target) %>% unique

nodes <-nodes %>% mutate(session_all='') %>% mutate(session_n=0) 

nodes_all <- data.frame()
for(n in video_id_list){
  
  # Get the info of video n
  node <- nodes %>% filter(id == n)
  
  # Get all links pointing to video n
  link <- links %>% filter(source == n | target == n)
  # Get all nodes linked to video n (incl. video n)
  link_nodes <- c(link$source, link$target) %>% unique
  
  # Get the info of nodes linked to video n
  link_nodes_info <- nodes %>% filter(id %in% link_nodes)
  # Get the sessions of these nodes (incl. the session of video n)
  session_list <- c(link_nodes_info$session, node$session) %>% unique
  
  # Select & Count unique sessions
  node$session_all <- session_list  %>% paste(collapse = ' ; ')
  node$session_n <- session_list %>% length
  
  # Store nodes in new dataframe
  nodes_all <- rbind(nodes_all, node)
}

#####  Format fields #####
nodes_all <- nodes_all %>% 
  mutate(title = gsub('<.*>','',title)) %>% 
  mutate(description = gsub('<.*>','',description))  %>% 
  mutate(views = as.numeric(views))
nodes_all %>% glimpse

#####  Check number of nodes #####  
nodes_all$session_all %>% unique
nodes_all %>% glimpse

#####################################
# WRITE JSON
#####################################

json <- list(nodes = nodes_all, links=links_l4 %>% filter(level!=4))
json %>% toJSON() %>% write('results_v2_L3.json')

json <- list(nodes = nodes_all, links=links_l4)
json %>% toJSON() %>% write('results_v2_L4.json')


#####################################
# UNIQUE LINKS WITH ALL SESSIONS & No. OCCURENCE
#####################################
#####  Add columns #####
links_grp <- links_l4 %>% 
  # filter(level < 4) %>%
  mutate(session_all = '') %>% 
  mutate(session_n = 0) %>% 
  mutate(type_all = type) %>% 
  mutate(type_n = 1) %>% 
  mutate(link_id = paste(source, target)) %>% 
  mutate(link_id_rev = paste(target, source)) %>% 
  mutate(link_n = 1)
links_grp %>% glimpse

#####  Loop through links: Reduce & Count #####
# link_id_all <- c(links_grp$link_id, links_grp$link_id_rev) %>% unique
link_list <- c()
for(i in 1:length(links_grp$link_id)){
  if(!(links_grp$link_id[i] %in% link_list | links_grp$link_id_rev[i] %in% link_list)){
    link_list <- c(link_list, links_grp$link_id[i])
  }
}

links_all <- data.frame()
for(l in link_list){
  
  matching_links <- links_grp %>% filter(link_id == l | link_id_rev == l)
  
  line <- matching_links %>% filter(type == 'direct') %>% slice(1)
  if(nrow(line) != 1) line <- matching_links %>% filter(type != 'direct') %>% filter(level == 2) %>% slice(1) 
  if(nrow(line) != 1) line <- matching_links %>% filter(type != 'direct') %>% filter(level != 2) %>% slice(1)

  line$link_n <- nrow(matching_links) 
  
  line$type_all <- matching_links$type %>% unique %>% paste(collapse=';')
  line$type_n <- matching_links$type %>% unique %>% length
  
  print(line)
  links_all <- rbind(links_all, line)
}

links_all %>% glimpse
links_all %>% filter(level < 3) %>% glimpse

# links_all_backup <- links_all
# links_all_L4_backup <- links_all
# links_all <- links_all_L4_backup
#####  Loop through sessions: Get all sessions #####
links_all <- links_all %>% 
  mutate(session_direct = session)%>% 
  mutate(session_all = session)
links_all %>% glimpse

for(k in queries){
  print(k)
  matching_nodes <- nodes_all %>% filter(grepl(k, session_direct))
  
  matching_nodes_L2 <- nodes_all %>% filter(grepl(k, session_all))
  
  for(i in 1:nrow(links_all)) {
    if( ( links_all$source[i] %in% matching_nodes$id & links_all$target[i] %in% matching_nodes_L2$id ) | 
        ( links_all$source[i] %in% matching_nodes_L2$id & links_all$target[i] %in% matching_nodes$id ) #| 
        # ( links_all$source[i] %in% matching_nodes_L2$id & links_all$target[i] %in% matching_nodes_L2$id )
        
    ){
      links_all$session_all[i] <- paste(links_all$session_all[i], ';', k)
      links_all$session_all[i] %>% print
    }
    if( links_all$source[i] %in% matching_nodes$id & links_all$target[i] %in% matching_nodes$id ){
      links_all$session_all[i] <- paste(links_all$session_all[i], ';', k)
      links_all$session_direct[i] <- paste(links_all$session_direct[i], ';', k)
      links_all$session_direct[i] %>% print
    }
  }
}

links_all_L4_session_backup <- links_all

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

links_all %>% filter(grepl('ice age', session_direct)) %>% glimpse

#####################################
# WRITE JSON
#####################################

json <- list(nodes = nodes_all, links=links_all %>% filter(level<3))
json %>% toJSON() %>% write('results_v2_L2_test.json')

json <- list(nodes = nodes_all, links=links_all %>% filter(level!=4))
json %>% toJSON() %>% write('results_v2_L3_test.json')

json <- list(nodes = nodes_all, links=links_all)
json %>% toJSON() %>% write('results_v2_L4_test.json')

