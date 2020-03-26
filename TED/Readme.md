# TED talks keywords

### Scraping

##### TED talks selection

To scrape the text content of the TED talks, the transcript found on the official site was used. Videos of the different talks were also available on YouTube, but only on TED official website the text was showed under the video and not embedded in it. 

To find the talks relevant to climate change, playlist from the official TED website tagged with the topic "Climate change" were selected [1]. Of the playlists in the topic, 8 were entirely dedicated to climate change:

```R
> playlisturl
[1] "https://www.ted.com/playlists/126/the_big_picture"               
[2] "https://www.ted.com/playlists/78/climate_change_oh_it_s_real"    
[3] "https://www.ted.com/playlists/154/how_do_you_solve_a_problem_lik"
[4] "https://www.ted.com/playlists/493/why_climate_change_is_a_human" 
[5] "https://www.ted.com/playlists/634/a_day_trip_to_antarctica"      
[6] "https://www.ted.com/playlists/439/what_is_the_anthropocene"      
[7] "https://www.ted.com/playlists/151/earth_appreciated"             
[8] "https://www.ted.com/playlists/142/the_forecast_calls_for"
```

##### Scraping of talks' links for each playlist

For each playlist html, the links of the videos were scraped with rvest functions [2].

```R
# scrape list of videos from playlist
videolist <- htmlpage %>% html_nodes('body') %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  as.vector() %>%
  gsub(pattern = '\\?(.*)', replacement = "")
  
# clean list of video
videolist <- videolist[grep(x = videolist, pattern = '/talks/')]
videolist <- videolist[-1]
videolist <- paste('https://www.ted.com/', videolist, '/transcript', sep = "")
```

Each URI referral to the relative playlist was then removed by deleting characters following and including the question mark. Only links to talks were kept and since the first two links always linked to the same talks the first link was deleted from videolist. In order to obtain the URL referring to each video transcript page, 'https://www.ted.com/' and '/transcript' needed to be added to each obtained URI.

The same operation was repeated for all the playlists and the obtained list of all talks was then filtered to only have unique results. A total of 68 links to talks was obtained.

```R
> glimpse(fulllist)
 chr [1:68] "https://www.ted.com//talks/james_b_glattfelder_who_controls_the_world/transcript" ...
```

##### Text captions scraping

Each link was then passed as argument to a custom function to scrape their relative video transcriptions.

```R
# Scrape transcription given a TED website URL (must end with /transcripts)
transcriptTED <- function(pageurl) {
  pagehtml <- pageurl %>% read_html()
  
  transcript <- pagehtml %>% html_nodes('body') %>%
    html_nodes('p') %>%
    html_text(trim = TRUE) %>%
    gsub(pattern = '\t',replacement = '') %>%
    gsub(pattern = '\n', replacement = ' ')
  
  return(transcript)
}
```

 After cleaning the text, a dataframe with lines of text and a number identifying their relative speech was created.

```R
> dfspeech %>% glimpse
Observations: 1,764
Variables: 2
$ text   <chr> "\"When the crisis came, the serious...
$ speech <chr> "1", "1", "1", "1", "1", "1", "1", "...
```

### Keywords selection

...

[1] https://www.ted.com/playlists/browse?topics=climate+change

[2] 