# script to pull europe climate data
# Jan 6 2022
# by alina zeng

# data download: https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview

# code adopted from OSPREE... ugh this is very hard
# https://github.com/lizzieinvancouver/ospree/blob/master/analyses/bb_dailyclimate/pull_daily_temp_for_forecasting.R
# ospree/analyses/bb_dailyclimate/pull_daily_temp_for_forecasting.R

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

install.packages("Interpol.T")
library(plyr)
library(dplyr)
library(ncdf4) # to open NC files

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Select the lat/long(s) and years of climate data you'd like
styr<-2011
endyr<-2020
stday <- strptime(paste(styr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
endday <- strptime(paste(endyr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")

# Import data ----
# d <- read.csv("input/experiment_euro_sites_Jan06.csv", header = TRUE)

# experiment on Jan 6-7 (need to come up with loop later)
la <- 57.484	# provenance location to graph out
lo <- -3.17

la <- 51.9	# garden location to graph out
lo <- -3.8


# need to write a loop -> task for Jan 7 (so that this can be done to multiple locations simultaneously)
# think about how to give identifiers to each site  
for(l in 1:dim(d)[1]){
  la<- d$LAT[l] 
  lo<- d$LON[l] 
}

# read climate file
euro20112020 <- nc_open( "C:/Users/alina/Documents/git/localadaptclim/input/tg_ens_mean_0.1deg_reg_2011-2020_v23.1e.nc")

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

dailytemp<- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                       Temp = temp)

dailytemp$Date<-strptime(dailytemp$Date,"%Y-%m-%d", tz="GMT")
dailytemp$Year<-as.numeric(format(dailytemp$Date, "%Y"))
dailytemp$Month = as.numeric(format(dailytemp$Date, "%m"))
dailytemp$label <- "EA FRAXEX Rosique-Esplugas 2021" # need to think about how to loop this.... label must be present when importing
# need to think about making the column names consistent

# need to convert everything to doy
d$doy <- yday(d$Date)
# identifier & status


# add an identifier so that I can combine all csv into one -> to loop.....


## hmmm need to get rid of 07-01 thru 12-30 in a separate step
dailytemp <- filter(dailytemp, Month < 7) 

name<-paste("output/dailyclim/dailytemp","_",la,"_",lo,"_",styr,"_",endyr,".csv",sep="")
#write out the daily temperature file, in case its useful later
if(length(unique(dailytemp$Temp))==1){next}
write.csv(dailytemp,name, row.names = FALSE)






