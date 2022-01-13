# script for lizzie to figure out the loop
# Jan 12 2022


# data download: https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(plyr)
library(dplyr)
library(ncdf4) # to open NC files


# Select the lat/long(s) and years of climate data you'd like
styr<-2011
endyr<-2020
stday <- strptime(paste(styr, "01-01", sep="-"),"%Y-%m-%d", tz="GMT")
endday <- strptime(paste(endyr, "12-31", sep="-"),"%Y-%m-%d", tz = "GMT")

# start and end days of the climate data we need for the lat/long. This is in days since baseline date (sept 1) Set to GMT to avoid daylight savings insanity
st <- as.numeric(as.character(stday - strptime("2011-01-01", "%Y-%m-%d", tz = "GMT")))
en <- as.numeric(as.character(endday - strptime("2011-01-01", "%Y-%m-%d", tz = "GMT")))
if(en<st){en=st}
if(endday<stday){endday=stday}

# Import data ----
d <- read.csv("input/experiment_euro_sites_Jan06.csv", header = TRUE)
# hi lizzie, this file has the provenance locations for 1 study which you could 
# use to experiment with the loop

# read climate file
euro20112020 <- nc_open( "C:/Users/~/git/localadaptclim/input/tg_ens_mean_0.1deg_reg_2011-2020_v23.1e.nc")

# Loop should start here

# at the moment I have been manually assigning values to lat and long like this 
# but hopefully this can be integrated into a loop
la <- 49.53333333	# provenance location to graph out
lo <- 0.766666667

#code to get daily climate data for focal lat/long
diff.long.cell <- abs(euro20112020$dim$longitude$vals-as.numeric(lo))
diff.lat.cell <- abs(euro20112020$dim$latitude$vals-as.numeric(la))
long.cell <- which(diff.long.cell==min(diff.long.cell))[1] #btw I dont know what "[1]" does in this situation, maybe it is not doing anything
lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]

# pull climate data
temp<-ncvar_get(euro20112020,'tg', 
                start=c(long.cell,lat.cell,st+1), 
                count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
)

# make data frame
dailytemp<- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                       Temp = temp)
dailytemp$Date<-strptime(dailytemp$Date,"%Y-%m-%d", tz="GMT")
dailytemp$Year<-as.numeric(format(dailytemp$Date, "%Y"))
dailytemp$Month = as.numeric(format(dailytemp$Date, "%m"))
dailytemp$doy <- yday(dailytemp$Date)
dailytemp$identifier <- "prov1" # would like for the loop to set the identifier to the same as d$identifier

# thank you so much Lizzie