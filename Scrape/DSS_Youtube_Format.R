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

data <- data.frame()
for(f in files){
  d <- read.csv(f, sep='\t', quote = "", row.names = NULL, stringsAsFactors = FALSE)
  
  test <- d %>% select(video_id) %>% pull %>% nchar
  if(FALSE %in% c(test == 11)) next
  
  data <- rbind(data, d)
  
  d %>% glimpse 
}

data %>% glimpse


#####################################
# MAKE JSON 
#####################################
##### Labels & Utils #####
data %>% glimpse
keywords <- data %>% select(keyword_ref) %>% pull %>% unique
video_id <- data %>% select(video_id) %>% pull %>% unique
reco_videos_id <- data %>% select(reco_videos_id) %>% pull %>% 
  str_split(' *; *') %>% unlist %>% unique
video_all <- c(video_id, reco_videos_id) 

video_all <- gsub('^.*=','', video_all) %>% unique
video_all <- video_all[!is.null(video_all)]
video_all <- video_all[!is.na(video_all)]
video_all <- video_all[video_all != 'NA']
video_all <- video_all[video_all != '']

check_length <- function(data, len){
  if(length(data) > len) data[1:len] %>% return
  else if(length(data) == len) data %>% return
  else if(length(data) < len) c(data, rep('', len-length(data))) %>% return
  else rep('', len) %>% return
}

descr_out <- c("(This channel used to be called MajorPrep, changed as of January 8th, 2020). I make nerdy and occasionally funny videos.")

##### Make Nodes #####
nodes <- data.frame() 

# If the script is interrupted, restart the loop where it stopped with this line of code:
# for(v in video_all[nrow(nodes)+1:length(video_all)]){
for(v in video_all){
  print(v)
  occurence <- data %>% filter(video_id == v) %>% slice(1)
  
  if(nrow(occurence)==1){
    session <- occurence$keyword_ref
    title <- occurence$title
    description <- occurence$description
    keywords <- occurence$keywords
    genre <- occurence$genre
    image <- occurence$image
    date <- occurence$date_publication
    channel_id <- occurence$channel_id
    views <- occurence$views
    duration <- occurence$duration
    
  } else if(nrow(occurence)==0){
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
    
    keywords <- html %>% html_nodes('meta[name=keywords]') %>% html_attr('content')
    keywords <- gsub('\t','',keywords)
    keywords <- gsub(',', ';', keywords)
    
    genre <- html %>% html_nodes('meta[itemprop="genre"]') %>% html_attr('content')
    genre <- gsub(',', ';', genre)
    
    image <- html %>% html_nodes('meta[property="og:image"]') %>% html_attr('content')
    date <- html %>% html_nodes('meta[itemprop="datePublished"]') %>% html_attr('content')
    channel_id <- html %>% html_nodes('meta[itemprop="channelId"]') %>% html_attr('content')
    views <- html %>% html_nodes('meta[itemprop="interactionCount"]') %>% html_attr('content')
    duration <- html %>% html_nodes('meta[itemprop="duration"]') %>% html_attr('content')
  }
  
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
  
  # Verif all fields
  title <- title %>% check_length(1)
  description <- description %>% check_length(1)
  keywords <- keywords %>% check_length(1)
  genre <- genre %>% check_length(1)
  image <- image %>% check_length(1)
  date <- date %>% check_length(1)
  channel <- channel %>% check_length(1)
  channel_id <- channel_id %>% check_length(1)
  duration <- duration %>% check_length(1)
  
  line <- data.frame(id=v, group=session, radius=views, title=title,
                     description=description, keywords=keywords, genre=genre,
                     image=image, date=date, channel=channel, channel_id=channel_id, views=views, 
                     duration=duration,
                     stringsAsFactors = FALSE)
  
  # Bind new data
  nodes <- rbind(nodes, line)
}

nodes %>% glimpse
video_all %>% length
nodes$id %>% unique %>% length


##### Make Direct Links #####
links_raw <- data.frame()
for(k in keywords){
  d <- data %>% filter(keyword_ref == k)
  for(i in 2:nrow(d)){
    src <- d %>% slice(i-1)
    tar <- d %>% slice(i)
    links_raw <- rbind(links_raw, data.frame(source=src$video_id, target=tar$video_id))
  }
}
links_raw <- links_raw %>% mutate(link_id = paste(source, '----->', target)) 
links_raw <- links_raw %>% distinct

links_count <- links_raw %>% 
  group_by(link_id) %>% 
  count

links_direct <- links_count %>% 
  ungroup %>%
  mutate(source = gsub(' ----->.*$','',link_id)) %>%
  mutate(target = gsub('^.* -----> ','',link_id)) %>%
  rename(value = n) %>%
  select(source, target, value) 

x <- links_direct %>% nrow
links_direct <- links_direct %>% 
  mutate(type = rep('direct', x))

links_direct %>% glimpse



##### Make Reco Links #####
links_reco_raw <- data.frame()
for(v in video_all){
  occurence <- data %>% filter(video_id == v)
  if(nrow(occurence)==0){
    occurence <- data %>% filter(grepl(v, reco_videos_id)) %>% slice(1)
    
    rec_v <- occurence$reco_videos_id %>% str_split(' *; *') %>% unlist
    rec_v <- gsub('^.*=','', rec_v) %>% unique
    
    for(rec in rec_v){
      if(is.null(rec)) next
      if(is.na(rec)) next
      if(rec == 'NA') next
      if(rec == '') next
      if(! rec %in% video_all) {
        print(rec)
        next
      }
      line <- data.frame(source=v, target=rec, stringsAsFactors = F)
      links_reco_raw <- rbind(links_reco_raw, line)
    }
  }
}
links_reco_raw %>% glimpse

links_reco_raw <- links_reco_raw %>% mutate(link_id = paste(source, '----->', target)) 
links_reco_raw <- links_reco_raw %>% distinct

links_reco_count <- links_reco_raw %>% 
  group_by(link_id) %>% 
  count

n_links <- nrow(links_reco_count)
links_reco <- links_reco_count %>% 
  ungroup %>%
  mutate(source = gsub(' ----->.*$','',link_id)) %>%
  mutate(target = gsub('^.* -----> ','',link_id)) %>%
  rename(value = n) %>%
  mutate(type = rep('recommendation',n_links)) %>%
  select(source, target, value, type) 

links_reco %>% glimpse

##### Check & bind links ##### 
links_direct %>% glimpse
links_reco %>% glimpse
links <- rbind(links_direct, links_reco)

links <- links %>% filter(source!=target)
links %>% glimpse


links <- links %>% select(source, target, value, type)

##### Make - Check - Write #####
# Check number of nodes
length(video_all)
nrow(nodes)
c(links$target, links$source) %>% unique %>% length


# Format nodes
nodes$radius <- as.numeric(nodes$radius)

main_video <- data %>% select(video_id) %>% pull %>% unique

nodes_reformat <- nodes %>% 
  mutate(title = gsub('<.*>','',title)) %>% 
  mutate(description = gsub('<.*>','',description))  %>% 
  mutate(radius = as.numeric(views)) %>% 
  mutate(views = as.numeric(views)) %>%
  rename(session = group) %>%
  mutate(group = ifelse(id %in% main_video, group, 'recommendation'))

nodes_reformat %>% glimpse

#####################################
# WRITE JSON
#####################################

json <- list(nodes = nodes_reformat, links=links)
json %>% toJSON() %>% write('results.json')

json <- list(nodes = nodes_reformat, links=links_direct)
json %>% toJSON() %>% write('results_direct.json')

json <- list(nodes = nodes_reformat, links=links_reco)
json %>% toJSON() %>% write('results_reco.json')


