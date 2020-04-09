library(dplyr)
library(rvest)

transcriptTED <- function(pageurl) {
  pagehtml <- pageurl %>% read_html()
  
  transcript <- pagehtml %>% html_nodes('body') %>%
    html_nodes('p') %>%
    html_text(trim = TRUE) %>%
    gsub(pattern = '\t',replacement = '') %>%
    gsub(pattern = '\n', replacement = ' ')
  
  return(transcript)
}