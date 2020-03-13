library(rstudioapi)
library(tidyverse)
library(dplyr)
library(tidytext)
library(qdapDictionaries)

setwd(dirname(getSourceEditorContext()$path))

###########
# READ FILES

fau <- "videoinfo_EhW-B2udhQw_2020_03_12-12_31_32_authors.tab"
fbi <- "videoinfo_EhW-B2udhQw_2020_03_12-12_31_32_basicinfo.tab"
fcm <- "videoinfo_EhW-B2udhQw_2020_03_12-12_31_32_comments.tab"

dfcomm <- fcm %>% read.table(sep = '\t', header = T, fill = T)
dfbas <- fbi %>% read.table(sep = '\t', header = T)
dfauth <- fau %>% read.table(sep = '\t', header = T)

############

# Word frequency

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


wddfti %>%
  write_csv("wordfreq.csv")

# 100 most frequent words
top1000 <- read_file("text.txt")

wddfti <- wddfti %>% filter(!grepl(top100, word))
