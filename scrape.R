library(tidyverse)
library(rvest)
library(rjson)
library(xml2)
library(purrr)

###
#scrape FAC report archive, scrape weather station stuff
#TO DO: Use summary to describe problems in terms of types of slides

#GOAL 1: Predict danger level, avalanche types. GOAL 2: Predict aspect, 
#       elevation of problems from above
# FAC Scrape
###

## Scrape 1, pt 1: only covers until 1-18-21... 

aviScrapePt1 <- function(){
  archiveURL <- "https://flatheadavalanche.org/archive"
  
  webpage <- read_html(archiveURL)
  
  pages <- html_nodes(webpage, ".pager-last") %>%
    html_nodes("a") %>%
    html_attr("href") 
  pages <- pages[1] %>%
    str_extract("[:digit:]+") %>%
    as.integer()
  
  for(i in 0:pages){
    if(i == 0){
      rating <- html_nodes(webpage, "table") %>%
        html_nodes("img") %>%
        html_attr("src")
      rating <- rating[2:length(rating)] %>%
        str_extract("[0-9]\\.png") %>%
        str_extract("[0-9]") %>%
        as.integer()
      tbls <- html_nodes(webpage, "table") %>%
        html_table()
      df <- tbls[[1]]
    }
    if(i > 0){
      url <- str_c( archiveURL, "?page=", as.character(i))
      tPage <- read_html(url)
      tR <- html_nodes(tPage, "table") %>%
        html_nodes("img") %>%
        html_attr("src")
      tR <- tR[2:length(tR)] %>%
        str_extract("[0-9]\\.png") %>%
        str_extract("[0-9]") %>%
        as.integer()
      tTbl <- html_nodes(tPage, "table") %>%
        html_table()
      tDF <- tTbl[[1]]
      rating <- append(rating, tR)
      df <- rbind(df, tDF)
    }
  }
  
  colnames(df) <- c("Date", "Summary", "Region")
  aviDF <- df %>%
    mutate(Date = as.Date(str_extract(Date, "[0-9]{4}-[0-9]{2}-[0-9]{2}") ) )%>%
    mutate(Rating = rating, .after = Date)
  
  write.csv(aviDF, 
            file = str_c( "aviData/", as.character(Sys.Date()), ".csv")
  )
}

# Scrape 2: should cover 1-19-21 until end of 20/21 season and all of
# 21/22 season. 

aviScrapePt2 <- function(startDate, endDate){
  # Dates: Year-Month-Date
  aviData <- tibble(Date = as.Date(character(0)),
                    Rating = numeric(),
                    Summary = character(),
                    Region = character())
  
  urlBase <- "https://api.avalanche.org/v2/public/products?avalanche_center_id=FAC&date_start="
  url <- str_c(urlBase,
               startDate, 
               "&date_end=", 
               endDate)
  
  raw <- read_html(url, options = "HUGE") %>%
    html_elements("body") %>%
    html_text2() %>%
    fromJSON(simplify = TRUE)
  
  for(i in 1:length(raw)){
    if(raw[[i]]$product_type == "forecast"){
      date <- as.Date(raw[[i]]$start_date) 
      danger <- as.numeric(raw[[i]]$danger_rating)
      if(is.null(raw[[i]]$bottom_line)){
        summary <- NA
      } else{
        summary <- raw[[i]]$bottom_line 
      }
      for(j in 1:length(raw[[i]]$forecast_zone)){
        region <- raw[[i]]$forecast_zone[[j]]$name
        aviData <- aviData %>% bind_rows(
          tibble(Date = date, 
                 Rating = danger,
                 Summary = summary,
                 Region = region)
        )
      }
    }
  }
return(aviData)
}


## Scrape 3: format data into seasons to make pairing w weather easier



aviData2 <- aviScrapePt2("2021-01-19", "2022-09-30")

#from scrape pt1, too much data scraped, gives 19 through 21 seasons in one
aviData <- read.csv("./aviData/aviData1.csv")[,2:5] %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Date >= as.Date("2019-10-01")) %>%
  bind_rows(aviData2)

names <- c("whitefish", "flatheadGlacier", "swan" )
regions <- unique(aviData$Region)

for(i in 1:3){
  for(j in 1:3){
    season <- 18 + i
    start <- as.Date( str_c( "20", as.character(season), "-10-01"))
    end <- as.Date( str_c( "20", as.character(season +1), "-09-30"))
    
    fileName <- str_c( "./aviData/" , names[j], "_", as.character(season), 
                      "-", as.character(season+1), "_avi.csv")
    aviData %>%
      filter(Date >= start & Date <= end & Region == regions[j]) %>%
      write_csv(fileName)
  }
  
} 


