library(tidyverse)
library(rvest)
library(xml2)
library(purrr)

###
#helper functions
###

archiveURL <- "https://flatheadavalanche.org/archive"
webpage <- read_html(archiveURL)

rating <- html_nodes(webpage, "table") %>%
  html_nodes("img") %>%
  html_attr("src")
rating <- rating[2:length(rating)] %>%
  str_extract("[0-9]\\.png") %>%
  str_extract("[0-9]") %>%
  as.numeric()

tbls <- html_nodes(webpage, "table") %>%
  html_table()

df <- tbls[[1]]
colnames(df) <- c("Date", "Summary", "Forecast Region")
df <- df %>%
  mutate(Date = as.Date(str_extract(Date, "[0-9]{4}-[0-9]{2}-[0-9]{2}") ) )%>%
  mutate(Rating = rating, .after = Date)

