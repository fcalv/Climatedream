# Avaaz comments keywords

#### Comments scraping

Video corpus from Avaaz report "Global Warming" search query misinformation video [1]. Comments were scraped thought the YTDT's Video Info and Comments Module [2] on date 12/03/2020.

The list of filenames for the videos is the following:

```R
> listfiles
 [1] "videoinfo_EhW-B2udhQw_2020_03_12-12_31_32_comments.tab"
 [2] "videoinfo_fA5sGtj7QKQ_2020_03_12-10_13_25_comments.tab"
 [3] "videoinfo_m0sY2tjmr_Y_2020_03_12-11_05_32_comments.tab"
 [4] "videoinfo_NYoOcaqCzxo_2020_03_12-12_22_41_comments.tab"
 [5] "videoinfo_OwqIy8Ikv-c_2020_03_12-10_48_18_comments.tab"
 [6] "videoinfo_oYhCQv5tNsQ_2020_03_12-11_00_10_comments.tab"
 [7] "videoinfo_RkdbSxyXftc_2020_03_12-10_01_08_comments.tab"
 [8] "videoinfo_TCy_UOjEir0_2020_03_12-10_30_25_comments.tab"
 [9] "videoinfo_UGqcweY1a3I_2020_03_12-12_28_50_comments.tab"
[10] "videoinfo_ZDK1aCqqZkQ_2020_03_12-10_42_16_comments.tab" 
```

#### Bigrams creation

All files are then read and a column is added with the video ID to which the comment belongs. Comments text and video ID for all the comments are then saved in the dataframe wddf.

```R
> wddf %>% glimpse()
Observations: 100,492
Variables: 2
$ text  <chr> "Fact 1: By reflecting away 30% of th...
$ video <chr> "EhW-B2udhQw", "EhW-B2udhQw", "EhW-B2...
```

 To obtain the bigrams the function unnest_tokens from the packet tidytext [3] was used:

```R
wddf_bigrams <- wddf %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2)
```

```R
> glimpse(wddf_bigrams)
Observations: 4,914,445
Variables: 2
$ video  <chr> "EhW-B2udhQw", "EhW-B2udhQw", "EhW-B...
$ bigram <chr> "fact 1", "1 by", "by reflecting", "...
```

To filter the stop words, the bigrams pairs were separated and bigrams containing stop words [4] were filtered out:

```R
wddf_bigrams_sep <- wddf_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

wddf_bigrams_fil <- wddf_bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
```

The resulting bigrams were counted, sorted by number of occurrences and saved in a CSV file:

```R
wddf_bigrams_fil %>% 
  count(word1, word2, sort = TRUE) %>%
  write_csv(paste(fold,"bigramsfilter.csv",sep = "/"))
```

After saving the bigrams, some not meaningful bigrams [Table 1] were filtered and plural and singular forms of single bigrams were summarized in a single keyword. The resulting keywords are saved in the file "Top100GlobalWarming.csv"

#### tf-idf

...

###### [Table 1] List of manually filtered keywords from Scraped "GlobalWarming/bigramsfitler.csv"

| word1            | word2            | n    |
| ---------------- | ---------------- | ---- |
| br               | br               | 3960 |
| www.youtube.com  | watch            | 2699 |
| https            | www.youtube.com  | 2474 |
| Ã¢               | Ã¢               | 2288 |
| href             | https            | 1825 |
| https            | youtu.be         | 983  |
| href             | http             | 867  |
| wrong            | wrong            | 740  |
| Ã°Ã¿             | ÂºÃ°Ã¿           | 399  |
| ÂºÃ°Ã¿           | Ã°Ã¿             | 377  |
| quot             | br               | 331  |
| http             | www.youtube.com  | 322  |
| ad               | hominem          | 314  |
| quot             | climate          | 300  |
| ha               | ha               | 280  |
| en.wikipedia.org | wiki             | 259  |
| https            | en.wikipedia.org | 256  |
| change           | quot             | 221  |
| quot             | global           | 217  |
| Ã¢               | Å“the            | 212  |
| decades          | ago              | 205  |
| br               | quot             | 204  |
| br               | Ã¢               | 203  |

[1] https://secure.avaaz.org/campaign/en/youtube_climate_misinformation/

[2] https://tools.digitalmethods.net/netvizz/youtube/mod_video_info.php

[3] https://www.rdocumentation.org/packages/tidytext/versions/0.2.2

[4] https://rdrr.io/cran/tidytext/man/stop_words.html