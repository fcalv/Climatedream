library(rstudioapi)
library(tidyverse)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

setwd(dirname(getSourceEditorContext()$path))

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

# folder to use
folder <- c("Scraped ClimateChange","Scraped ClimateManipulation","Scraped GlobalWarming")

# 1 = Climate Change
# 2 = Climate Manipulation
# 3 = Global Warming
fold <- folder[3]

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


########
# PLOTS

# word frequency filtered TOP 15
N <- 15

wddftif %>%
  top_n(N) %>%
ggplot(aes(reorder(word, n), n)) +
  coord_flip() +
  geom_col(fill = "dark red") +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  ggtitle("TOP 15 words by frequency")

ggsave(paste(fold, "TOP15freq.jpg",sep = "/"), dpi = 300)

# Bigram map
N <- 300

bigram_graph <- wddf_bigrams_fil %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > N) %>%
  graph_from_data_frame()


a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(color = "lightpink", size = 4) +
  geom_node_text(aes(label = name), vjust = 0.4, hjust = 0.4) +
  theme_void()
  
ggsave(paste(fold, "TOPkeywords.jpg",sep = "/"), dpi = 300, scale = 2)

#################
# Sentiment analysis

# Saif Mohammad and Peter Turney, word count per sentiment
nrc_sent <- get_sentiments("nrc") %>%
  group_by(sentiment)


wddf_sent_nrc <- wddf %>% 
  unnest_tokens(word, text) %>%
  inner_join(nrc_sent) %>%
  count(sentiment, sort=TRUE)

N <- 15
wddf_sent_nrc %>%
  top_n(N) %>%
  ggplot(aes(reorder(sentiment,n),n)) +
  geom_col(fill = "darkred") +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  ggtitle("NRC word count grouped by sentiment") + 
    coord_flip()

ggsave(paste(fold, "TOPSentiment NRC.jpg",sep = "/"), dpi = 300, scale = 2)

# Bing top N count by sentiment

bing_sent <- get_sentiments("bing")

# count of positive and negative sentiment contribution

wddf_sent_bing <- wddf %>% 
  unnest_tokens(word, text) %>%
  inner_join(bing_sent) %>%
  group_by(sentiment) %>%
  count(word, sort=TRUE)


wddf_bing <- wddf %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"), by = c(word = "word"))


wddf_bing %>%
  count(sentiment, word) %>%
  ungroup() %>%
  filter(n >= 1000) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  theme_minimal() +
  coord_flip()

ggsave(paste(fold, "Bing Sentiment.jpg",sep = "/"), dpi = 300, scale = 2)



#################


