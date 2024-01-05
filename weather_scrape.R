library(tidyverse)
library(rvest)
library(xml2)
library(purrr)

###
# Weather scrape
# Goal: get weather data from link below for every day from October 1, 2019 until
#       current day, store in one big data set.  Done as of 2-21-22.
#       Next, read from last day to current day so can update auto at 5AM  
###

names <- c("whitefish", "swan", "flathead", "glacier")

weatherScrape <- function(startDate, endDate){
  date <- startDate
  sU <- "https://looper.avalanche.state.co.us/fac/zones.php?date="
  sE <- "+06&stnlink=hourly&unit=e&flag=on&area=fac&span=6"
  startURL <- str_c(sU, as.character(date), sE)
  
  startPage <- read_html(startURL)
  tables <- html_nodes(startPage, "table")
  
  whitefish <- html_table(tables[[3]])[,1:17] %>% mutate(date = date, .before = Station)
  swan <- html_table(tables[[5]])[,1:17] %>% mutate(date = date, .before = Station)
  flathead <- html_table(tables[[7]])[,1:17] %>% mutate(date = date, .before = Station)
  glacier <- html_table(tables[[9]])[,1:17] %>% mutate(date = date, .before = Station)
  ranges <- list(whitefish, swan, flathead, glacier)
  
  while(date != endDate){
    date <- date + 1
    sU <- "https://looper.avalanche.state.co.us/fac/zones.php?date="
    sE <- "+06&stnlink=hourly&unit=e&flag=on&area=fac&span=6"
    url <- str_c(sU, as.character(date), sE)
    tPage <- read_html(url)
    tTbls <- html_nodes(tPage, "table")
    for(i in 1:4){
      tDF <- html_table(tTbls[[2*i + 1]])[,1:17] %>% mutate(date = date, .before = Station)
      ranges[[i]] <- rbind(ranges[[i]], tDF)
    }
  }

  for( i in 1:4){
    ranges[[i]] <- ranges[[i]] %>%
      mutate(Temp =  as.numeric(na_if(Temp, "-")),
             MxTp = as.numeric(na_if(MxTp, "-")),
             MnTp = as.numeric(na_if(MnTp, "-")),
             DewP = as.numeric(na_if(DewP, "-")),
             RH = as.numeric(na_if(RH, "-")),
             Spd = as.numeric(na_if(Spd, "-")),
             Dir = as.numeric(na_if(Dir, "-")),
             Gst = as.numeric(na_if(Gst, "-")),
             Pcp1 = as.numeric(na_if(Pcp1, "-")),
             Pcp24 = as.numeric(na_if(Pcp24, "-")),
             PcpAc = as.numeric(na_if(PcpAc, "-")),
             Sno24 = as.numeric(na_if(Sno24, "-")),
             SWE24 = as.numeric(na_if(SWE24, "-")),
             SnoHt = as.numeric(na_if(SnoHt, "-")),
             SWE = as.numeric(na_if(SWE, "-")),
      )
    ranges[[i]] <-  ranges[[i]] %>%
      subset( Station != "") %>%
      arrange(Station, date) %>%
      group_by(Station) %>%
      mutate(Sno24 = SnoHt - lag(SnoHt, default = first(SnoHt)),
             SWE24 = SWE - lag(SWE, default = first(SWE))
      )
  }
  return(ranges)
}

if(length(list.files("./weatherData")) == 0){
  for(i in 19:21){
    startDate <- str_c("20",as.character(i),
                       "-10-01")
    endDate <- str_c("20", as.character(i+1),"-05-01")
    ranges <- weatherScrape(as.Date(startDate), as.Date(endDate))
    for(j in 1:3){
      if(j != 3){
        dest <- str_c("weatherData/", names[j], "_", as.character(i), "-",as.character(i+1), "weather.csv", sep = "")
        write_csv(ranges[[j]], file = dest) 
      }else{
        dest <- str_c("weatherData/", "flatheadGlacier_", as.character(i), "-",as.character(i+1), "weather.csv", sep = "")
        flatGlac <- ranges[[j]] %>% 
          bind_rows(ranges[[j+1]]) %>% 
          write_csv(file = dest)
      }
    }
  }
}

