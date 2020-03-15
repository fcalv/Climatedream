library(rstudioapi)
library(tidyverse)
library(dplyr)
library(tidytext)
library(qdapDictionaries)

setwd(dirname(getSourceEditorContext()$path))

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

# READ FILES

###########
# COMMENTS

# get list of comment files
listfiles <- list.files("Scraped ClimateChange", pattern = "comments") %>% as.vector()

dfcomm <- paste("Scraped ClimateChange/",listfiles[1],sep = "") %>% read.table(sep = '\t', header = T, fill = T, quote = "")
listfiles <- listfiles[-1]

# iterate, read and append all files in "dfcm"
for (val in listfiles) {
  commtemp <- paste("Scraped ClimateChange/",val,sep = "") %>% read.table(sep = '\t', header = T, fill = T, quote = "")
  dfcomm <- rbind(dfcm,commtemp)
  print(val)
}

# DON'T RUN
# print entire list of all comments in "commfull.csv"
# commf %>% write.csv("commfull.csv")


##############
# COMMENT AUTHORS

# get list of authors files
listfiles <- list.files("Scraped ClimateChange", pattern = "authors") %>% as.vector()

dfauth <- paste("Scraped ClimateChange/",listfiles[1],sep = "") %>% read.table(sep = '\t', header = F, fill = T, quote = "")
listfiles <- listfiles[-1]

# iterate, read and append all files in "dfauth"
for (val in listfiles) {
  authtemp <- paste("Scraped ClimateChange/",val,sep = "") %>% read.table(sep = '\t', header = F, fill = T, quote = "")
  dfauth <- rbind(dfauth,authtemp)
  print(val)
}

dfauth <- dfauth %>% rename("Author"="V1", "Commnum"="V2")

# Summarize by comment author across all videos and order
dfauth <- dfauth %>% group_by(Author) %>% summarise(Commnum = sum(Commnum))
dfauth <- dfauth[order(-dfauth$Commnum),]

# Top N comment authors file "TopN users commenting.csv"
N <- 100
dfauth %>% head(N) %>% write.csv("TopN users commenting.csv")

############

############
# Word frequency in comments

# Cast data frame

wddf <- dfcomm %>%
  select(text) %>%
  as.data.frame() %>%
  mutate(text = as.character(text))

# Tidy text

wddfti <- wddf %>% unnest_tokens(word, text)

# Word frequency

wddfti <- wddfti %>%
  count(word, sort = T) 

# Frequent words, unfiltered

wddfti %>%
  write_csv("wordfreq.csv")

# 1000 most frequent words and single characters filtered out
top1000 <- read_file("text.txt")

wddftif <- wddfti %>%
  filter(!grepl(top1000, word))

# print filtered result
wddftif %>%
  write_csv("wordfreqfilter.csv")
