# script to pull europe climate data
# Jan 6 2022
# alina zeng

# data download: https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview

# code adopted from OSPREE
# https://github.com/lizzieinvancouver/ospree/blob/master/analyses/bb_dailyclimate/pull_daily_temp_for_forecasting.R
# ospree/analyses/bb_dailyclimate/pull_daily_temp_for_forecasting.R

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(plyr)
library(dplyr)
library(ncdf4) # to open NC files
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Select the lat/long(s) and years of climate data you'd like
styr<-2011
endyr<-2020
stday <- strptime(paste(styr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
endday <- strptime(paste(endyr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")

# Import data ----
# d <- read.csv("input/experiment_euro_sites_Jan06.csv", header = TRUE)


label <- "NG PSEUME Lavadinovic etal 2013"
# garden 	
la <- 43.783333	
lo <- 18.966667


# read climate file
euro20112020 <- nc_open( "C:/Users/alina/Documents/git/localadaptclim/input/DailyClimRaw/Europe/tg_ens_mean_0.1deg_reg_2011-2020_v23.1e.nc")

#code to get daily climate data for focal lat/long
diff.long.cell <- abs(euro20112020$dim$longitude$vals-as.numeric(lo))
diff.lat.cell <- abs(euro20112020$dim$latitude$vals-as.numeric(la))
long.cell <- which(diff.long.cell==min(diff.long.cell))[1]
lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]

# start and end days of the climate data we need for the lat/long. This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
st <- as.numeric(as.character(stday - strptime("2011-01-01", "%Y-%m-%d", tz = "GMT")))
en <- as.numeric(as.character(endday - strptime("2011-01-01", "%Y-%m-%d", tz = "GMT")))
if(en<st){en=st}
if(endday<stday){endday=stday}

# pull climate data
temp<-ncvar_get(euro20112020,'tg', 
                start=c(long.cell,lat.cell,st+1), 
                count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
)

dailytemp_euro<- data.frame(lat = la,long = lo,date = seq(stday, endday, by = "day"),
                       temp = temp)

dailytemp_euro$date<-strptime(dailytemp_euro$Date,"%Y-%m-%d", tz="GMT")
dailytemp_euro$year<-as.numeric(format(dailytemp_euro$date, "%Y"))
dailytemp_euro$month = as.numeric(format(dailytemp_euro$date, "%m"))
dailytemp_euro$label <- label
dailytemp_euro$identifier <- "garden1"
dailytemp_euro$status <- "garden"
# need to convert everything to doy
dailytemp_euro$doy <- yday(dailytemp_euro$date)
name<-paste("output/dailyclim/dailytemp","_garden_",unique(dailytemp_euro$label),"_",styr,"_",endyr,".csv",sep="")
if(length(unique(dailytemp_euro$Temp))==1){next}
write.csv(dailytemp_euro,name, row.names = FALSE)



