# experiment: extract climate data for a number of sites simultaneously

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
st <- as.numeric(as.character(stday - strptime("2011-01-01", "%Y-%m-%d", tz = "GMT")))
en <- as.numeric(as.character(endday - strptime("2011-01-01", "%Y-%m-%d", tz = "GMT")))
if(en<st){en=st}
if(endday<stday){endday=stday}

# read climate file
euro20112020 <- nc_open( "C:/Users/alina/Documents/git/localadaptclim/input/tg_ens_mean_0.1deg_reg_2011-2020_v23.1e.nc")

# Import data ----
d <- read.csv("input/experiment_euro_sites_Jan06.csv", header = TRUE)



#1 plot raw climate for each year to see interannual variability (entire year)
europeclimatedata_allprovenances <-  
  d  %>%      # the data frame
  group_by(identifier) %>%  #each study needs have different identifier, unless I do each study mannually
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_line(aes(x = doy, y = Temp, color = status))+
       # geom_smooth(aes(x = spring_event, y = diff_LAT))+
       scale_x_continuous(breaks=seq(0,366,15))+			
       guides(col=guide_legend("Type")) + 			# set legend title
       labs(title = paste("Plot of",.$identifier,.$label, "daily temperature in", .$Year, sep = " ")) +
       theme_classic() +  
       theme(axis.text.x = element_text(size = 8, angle = 45),
             axis.text.y = element_text(size = 10, angle = 0),
             plot.title = element_text(size = 9),
             legend.position = "bottom") +
       labs(x = "\n Day of year", 
            y = "Temperature (\u00B0C)\n")
  )



do(tempdata=
     diff.long.cell <- abs(euro20112020$dim$longitude$vals-as.numeric(.$long_prov))
   diff.lat.cell <- abs(euro20112020$dim$latitude$vals-as.numeric($.lat_prov))
   long.cell <- which(diff.long.cell==min(diff.long.cell))[1]
   lat.cell <- which(diff.lat.cell==min(diff.lat.cell))[1]
     
     
   temp<-ncvar_get( .,'tg', 
                   start=c(long.cell,lat.cell,st+1), 
                   count=c(1,1,en-st+1) # this is where we move through the 'cube' to get the one vector of Temp mins
   )
   
   dailytemp<- data.frame(Lat = la,Long = lo,Date = seq(stday, endday, by = "day"),
                          Temp = temp)
   
   dailytemp$Date<-strptime(dailytemp$Date,"%Y-%m-%d", tz="GMT")
   dailytemp$Year<-as.numeric(format(dailytemp$Date, "%Y"))
   dailytemp$Month = as.numeric(format(dailytemp$Date, "%m"))
   dailytemp$label <- "EA FAGUSY Gömöry & Paule 2011" 
   dailytemp$identifier <- "prov1"
   dailytemp$status <- "provenance"
   dailytemp$identifier <- "garden1"
   dailytemp$status <- "garden"
   # need to think about how to loop this.... label must be present when importing
   # need to think about making the column names consistent
   
   # need to convert everything to doy
   dailytemp$doy <- yday(dailytemp$Date)
     
     
     )
#code to get daily climate data for focal lat/long


# You can view the graphs before saving them
oneyear_interannual_variability_plots$plots

# Saving the plots to file

oneyear_interannual_variability_plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotJan7/EA FAGUSY Gömöry & Paule 2011/interannualclimate", "/", "Plot-", .$identifier, .$label,"-", .$Year, "entire_year_temp", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))


