# loop extract North America Data

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)


library(devtools)
library(cli)
library(magrittr)
library(sp)
library(raster)
library(tidyverse)
#interface to the 'Daymet' Web Service
install.packages("daymetr")
library(daymetr)

# Import data ----
d <- read.csv("input/experiment_euro_sites_Jan06.csv", header = TRUE)
# use to experiment with the loop

# empty dataframe
dailytemp <- data.frame(lat = numeric(), long = numeric(), date = as.Date(character()),
                        temp = integer(), year = integer(), month = integer(), 
                        doy = integer(), identifier = character())
# Loop starts here
for (rownum in c(1:nrow(d))){
  la <- d$lat_prov[rownum]
  lo <- d$long_prov[rownum]
  
  # load in daymet data
  dailytempadd <- download_daymet("Daymet",
                                   lat = la,
                                   lon = lo,
                                   start = 2011,
                                   end = 2020,
                                   internal = TRUE)
  # load the tidyverse (install if necessary)
  if(!require(tidyverse)){install.package(tidyverse)}
  library(tidyverse)
  
  # Calculate the mean temperature from min
  # max temperatures and convert the year and doy
  # to a proper date format.
  dailytempadd$data <- dailytempadd$data %>%
    mutate(temp = (tmax..deg.c. + tmin..deg.c.)/2,
           lat = la,
           long = lo,
           date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
  
  #only keep some data from table
  dailytempadd <-
    dailytempadd$data %>%
    dplyr::select(lat,long, year, yday, temp, date)

  dailytempadd$date<-strptime(dailytempadd$date,"%Y-%m-%d", tz="GMT")
#  dailytempadd$year<-as.numeric(format(dailytempadd$date, "%Y"))
  dailytempadd$month = as.numeric(format(dailytempadd$date, "%m"))
  dailytempadd <- dplyr::rename(dailytempadd, doy = yday)
  dailytempadd$identifier <- rep(d$identifier_prov[rownum], times = 3650)# AZ: 3650 instead of 3653 becuz Daymet omits Leap day
  # EMW -- Here you'll need to rbind the old and new data each time
  dailytemp <- rbind(dailytemp, dailytempadd)
}

dailytemp$label <- rep(unique(d$label), nrow(dailytemp))
dailytemp$status <- "provenance"

# export
name<-paste("output/dailyclim/dailytemp","_", unique(dailytemp$status), "_",unique(dailytemp$label), "_2011_2020.csv",sep="")
write.csv(dailytemp,name, row.names = FALSE)
