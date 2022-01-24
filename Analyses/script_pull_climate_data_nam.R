# script to pull North America climate data
# Jan 16 2022
# by alina zeng

# data download from
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1840

# code adopted from sophia

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


label <- "NG PICEEN Rehfeldt 1994"
# provenance
la <-33.4
lo <- -105.78
# garden 	
la <-46.7
lo <- -117


# load in daymet data
dailytemp_nam <- download_daymet("Daymet",
                                lat = 33.4,
                                lon = -105.78,
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
         date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))

#only keep some data from table
dailytemp_nam  <-
  dailytemp_nam$data %>%
  select(year, yday, prcp..mm.day., tmax..deg.c., tmin..deg.c., temp, date)


dailytemp_nam$date<-strptime(dailytemp_nam$date,"%Y-%m-%d", tz="GMT")
dailytemp_nam$year<-as.numeric(format(dailytemp_nam$date, "%Y"))
dailytemp_nam$month = as.numeric(format(dailytemp_nam$date, "%m"))
dailytemp_nam$label <- "NG PICEEN Rehfeldt 1994" 
dailytemp_nam$identifier <- "prov59"
dailytemp_nam$status <- "provenance"
dailytemp_nam$doy <- dailytemp_nam$yday
dailytemp_nam$status <- "garden"
dailytemp_nam$identifier <- "garden1"

name<-paste("output/dailyclim/dailytemp","_",label, "_",la,"_",lo,"_2011_2020",".csv",sep="")
# identifier & status
if(length(unique(dailytemp_nam$temp))==1){next}
write.csv(dailytemp_nam,name, row.names = FALSE)