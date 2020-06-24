install.packages("stopwords")
install.libraries("udpipe")
install.packages("BTM")
library(udpipe)
library(data.table)
library(stopwords)
library(tidyverse)
library(BTM)
W
data_test <- data
data_test <- data_test[,c("video_id","title","description","keywords")]
data_test$description   <- gsub("'", "", data_test$description)
data_test$description   <- gsub("<.+>", "", data_test$description)

data_test <- data_test %>% rename(doc_id = video_id, text = description)
anno    <- udpipe(data_test, "english", trace = 10)
biterms <- as.data.table(anno)
biterms <- biterms[, cooccurrence(x = lemma, relevant = upos %in% c("NOUN", "ADJ", "VERB") & nchar(lemma) > 2 & !lemma %in% stopwords("en"),skipgram = 3),by = list(doc_id)]

set.seed(123456)
traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords("en") & nchar(lemma) > 2)
traindata <- traindata[, c("doc_id", "lemma")]
model     <- BTM(traindata, biterms = biterms, k = 9, iter = 2000, background = TRUE, trace = 100)
terms(model)
bitermset <- terms(model, "biterms")










