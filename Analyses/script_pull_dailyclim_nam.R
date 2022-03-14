# script to pull North America climate data
# Jan 16 2022
# by alina zeng

# data download from
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1840

# code adopted from sophia



library(devtools)
library(cli)
library(magrittr)
library(sp)
library(raster)
library(tidyverse)
#interface to the 'Daymet' Web Service
install.packages("daymetr")
library(daymetr)


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

label <- "NA BETUPA Hawkins & Dhar 2012"


# garden 	
la <-  48.48333333	
lo <-	-123.4


# load in daymet data
dailytemp_nam <- download_daymet("Daymet",
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
dailytemp_nam$data <- dailytemp_nam$data %>%
  mutate(temp = (tmax..deg.c. + tmin..deg.c.)/2,
         lat = la,
         long = lo,
         date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))

#only keep some data from table
# dailytemp_nam  <-
#  dailytemp_nam$data %>%
#  dplyr::select(lat, long, year, yday, prcp..mm.day., tmax..deg.c., tmin..deg.c., temp, date)

dailytemp_nam <-
  dailytemp_nam$data %>%
  dplyr::select(lat,long, year, yday, temp, date)

dailytemp_nam$date<-strptime(dailytemp_nam$date,"%Y-%m-%d", tz="GMT")
dailytemp_nam$year<-as.numeric(format(dailytemp_nam$date, "%Y"))
dailytemp_nam$month = as.numeric(format(dailytemp_nam$date, "%m"))
dailytemp_nam$status <- "garden"
dailytemp_nam <- dplyr::rename(dailytemp_nam, doy = yday)
dailytemp_nam$label <- label
dailytemp_nam$identifier <- "R"


name<-paste("output/dailyclim/dailytemp","_garden_",unique(dailytemp_nam$label), "_2011_2020.csv",sep="")
write.csv(dailytemp_nam,name, row.names = FALSE)
