# script for lizzie to figure out the loop
# Jan 12 2022


# data download: https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview
# EMW says -- I didn't have time (yet) to download these data, but I can later as neededd...

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(plyr)
library(dplyr)
library(lubridate)
library(ncdf4) # to open NC files

# setwd("~/Documents/git/projects/treegarden/misc/localadaptclim")


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
# use to experiment with the loop

# read climate file
euro20112020 <- nc_open( "C:/Users/alina/Documents/git/localadaptclim/Input/DailyClimRaw/Europe/tg_ens_mean_0.1deg_reg_2011-2020_v23.1e.nc")

# set up dataframe to add to in loop ...
# the way I did this may not work as you need the columns to all be the same format (numeric/character/etc.) as what you'll eventually end up with
# the date column might need to be specified more carefully, but this suggests a format with empty vectors could work...
# https://stackoverflow.com/questions/10689055/create-an-empty-data-frame
# You can find out what format a column is through: mode(df$colname)
# dailytemp <- data.frame(Lat = NA, Long = NA, Date = NA,
#                      # Temp = NA, Year = NA, Month = NA, doy = NA, identifier = NA)
# dailytemp <- dailytemp[-1,]
#hmm instead of NAs,

dailytemp <- data.frame(Lat = numeric(), Long = numeric(), Date = as.Date(character()),
                        Temp = integer(), Year = integer(), Month = integer(), 
                        doy = integer(), identifier = character())

# Loop starts here
for (rownum in c(1:nrow(d))){
    la <- d$lat_prov[rownum]
    lo <- d$long_prov[rownum]
  #  print(paste(la, lo)) # EMW says -- you can delete this ... it it just to see what's happening
#code to get daily climate data for focal lat/long
diff.long.cell <- abs(euro20112020$dim$longitude$vals-as.numeric(lo))
diff.lat.cell <- abs(euro20112020$dim$latitude$vals-as.numeric(la))
long.cell <- which(diff.long.cell==min(diff.long.cell))[1] 
lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
#btw I dont know what "[1]" does in this situation, maybe it is not doing anything
# EMW says -- it is taking the FIRST row of the object. So I would check what this looks like
    # which(diff.lat.cell==min(diff.lat.cell))
# And make sure it's okay just to take the first row (in all cases); I suspect it is okay as the command takes the minimum and so that [1] might just cover cases where there are several identical values?
# pull climate data
temp<-ncvar_get(euro20112020,'tg', 
                start=c(long.cell,lat.cell,st+1), 
                count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
)
# perhaps also add label (but this could be mannual since there isn't too much)
# make data frame
dailytempadd<- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                       Temp = temp)
dailytempadd$Date<-strptime(dailytempadd$Date,"%Y-%m-%d", tz="GMT")
dailytempadd$Year<-as.numeric(format(dailytempadd$Date, "%Y"))
dailytempadd$Month = as.numeric(format(dailytempadd$Date, "%m"))
dailytempadd$doy <- yday(dailytempadd$Date)
dailytempadd$identifier <- rep(d$identifier_prov[rownum], nrow(temp)) # EMW -- see if this works, you may need to adjut 'nrow' command
# EMW -- Here you'll need to rbind the old and new data each time
dailytemp <- rbind(dailytemp, dailytempadd)
    }


dailytemp$label <- d$label 
dailytemp$status <- "provenance"
# change column names
 dailytemp <- dplyr::rename(dailytemp, lat=Lat,long=Long,date = Date, 					
             year=Year, month=Month, 					
             temp = Temp)			

# get rid of Date column (its a moot now)
# test<- dplyr::select(test, -Date)

# export
name<-paste("output/dailyclim/dailytemp","_", unique(dailytemp$status), "_",unique(dailytemp$label), "_",styr,"_",endyr,".csv",sep="")
write.csv(dailytemp,name, row.names = FALSE)
