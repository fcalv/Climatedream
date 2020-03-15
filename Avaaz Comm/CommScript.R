library(rstudioapi)
library(tidyverse)
library(dplyr)
library(tidytext)
library(qdapDictionaries)

setwd(dirname(getSourceEditorContext()$path))

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

# folder to use
fold <- "Scraped ClimateChange"

# READ FILES

###########
# COMMENTS

# get list of comment files
listfiles <- list.files(fold, pattern = "comments") %>% as.vector()

dfcomm <- paste(fold,listfiles[1],sep = "/") %>% read.table(sep = '\t', header = T, fill = T, quote = "")
listfiles <- listfiles[-1]

# iterate, read and append all files in "dfcm"
for (val in listfiles) {
  commtemp <- paste(fold,val,sep = "/") %>% read.table(sep = '\t', header = T, fill = T, quote = "")
  dfcomm <- rbind(dfcomm,commtemp)
  print(val)
}

# DON'T RUN
# print entire list of all comments in "commfull.csv"
# commf %>% write.csv("commfull.csv")


##############
# COMMENT AUTHORS

# get list of authors files
listfiles <- list.files(fold, pattern = "authors") %>% as.vector()

dfauth <- paste(fold,listfiles[1],sep = "/") %>% read.table(sep = '\t', header = F, fill = T, quote = "")
listfiles <- listfiles[-1]

# iterate, read and append all files in "dfauth"
for (val in listfiles) {
  authtemp <- paste(fold,val,sep = "/") %>% read.table(sep = '\t', header = F, fill = T, quote = "")
  dfauth <- rbind(dfauth,authtemp)
  print(val)
}

dfauth <- dfauth %>% rename("Author"="V1", "Commnum"="V2")

# Summarize by comment author across all videos and order
dfauth <- dfauth %>% group_by(Author) %>% summarise(Commnum = sum(Commnum))
dfauth <- dfauth[order(-dfauth$Commnum),]

# Top N comment authors file "TopN users commenting.csv"
N <- 100
dfauth %>% head(N) %>% write.csv(paste(fold,"TopN users commenting.csv",sep = "/"))

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


# Anti join stop words

data(stop_words)

wddftif <- wddfti %>%
  anti_join(stop_words)

# Word frequency unfiltered
wddfti <- wddfti %>%
  count(word, sort = T) 

# print filtered result
wddfti %>%
  write_csv(paste(fold,"wordfreq.csv",sep = "/"))

# Word frequency filtered
wddftif <- wddftif %>%
  count(word, sort = T) 

# print filtered result
wddftif %>%
  write_csv(paste(fold,"wordfreqfilter.csv",sep = "/"))

############
# Bigrams

wddf_bigrams <- wddf %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2)

wddf_bigrams %>%
  count(bigram, sort = TRUE) %>%
  write_csv(paste(fold,"bigrams.csv",sep = "/"))

# Bigrams no stopwords

wddf_bigrams_sep <- wddf_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

wddf_bigrams_fil <- wddf_bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# save CSV
wddf_bigrams_fil %>% 
  count(word1, word2, sort = TRUE) %>%
  write_csv(paste(fold,"bigramsfilter.csv",sep = "/"))
