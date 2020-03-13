library(RSelenium)
library(tidyverse)

driver <- rsDriver(browser=c("chrome"), port = 4444L)
remote_driver <- driver[["client"]]
remote_driver$open()
remote_driver$navigate("https://www.youtube.com/")
