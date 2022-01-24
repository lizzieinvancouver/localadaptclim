#compare the daymet data with the gov of Canada data 
install.packages("devtools")
library(devtools)
install.packages("cli")
library(cli)
library(magrittr)
library(sp)
library(raster)
library(tidyverse)

#interface to the 'Daymet' Web Service
install.packages("daymetr")
library(daymetr)

#set wd
setwd("~/Documents/git")

###load in daymet penticton data
penticton_dm <- download_daymet("penticton",
                                lat = 49.46066,
                                lon = -119.60598,
                                start = 2001,
                                end = 2018,
                                internal = TRUE)

# load the tidyverse (install if necessary)
if(!require(tidyverse)){install.package(tidyverse)}
library(tidyverse)

# Calculate the mean temperature from min
# max temperatures and convert the year and doy
# to a proper date format.
penticton_dm$data <- penticton_dm $data %>%
  mutate(tmean = (tmax..deg.c. + tmin..deg.c.)/2,
         date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))

#only keep some data from table
penticton_dm  <-
  penticton_dm $data %>%
  select(year, yday, prcp..mm.day., tmax..deg.c., tmin..deg.c., tmean, date)


#### load in environment canada data
penticton_ec <- read.csv("environmentcanada_clean.csv")


##############
#### 2001 ####
##############

penticton_ec2001 <- filter(penticton_ec, Year == "2001")

penticton_dm2001 <- filter(penticton_dm, year == "2001")

#make a data set with the t mean for both sets, year, and doy

penticton2001 <- cbind("doydm"=penticton_dm2001$yday, "doydm"=penticton_ec2001$Day,
                       "tmean_dm"= penticton_dm2001$tmean, "tmean_ec"= penticton_ec2001$MeanTemperature 
)

penticton2001 <- data.frame("doydm"=penticton_dm2001$yday, "doydm"=penticton_ec2001$Day,
                            "tmean_dm"= penticton_dm2001$tmean, 
                            "tmean_ec"= penticton_ec2001$MeanTemperature,
                            stringsAsFactors=FALSE)


#create plot

ggplot() +
  geom_point(data =penticton2001, aes(x=tmean_dm, y=tmean_ec))+
  ggtitle("Env.Canada vs Daymet (PetictonA, 2001)")


##############
#### 2009 ####
##############

penticton_ec2009 <- filter(penticton_ec, Year == "2009")

penticton_dm2009 <- filter(penticton_dm, year == "2009")

#make a data set with the t mean for both sets, year, and doy

penticton2009 <- cbind("doydm"=penticton_dm2009$yday, "doydm"=penticton_ec2009$Day,
                       "tmean_dm"= penticton_dm2009$tmean, "tmean_ec"= penticton_ec2009$MeanTemperature 
)

penticton2009 <- data.frame("doydm"=penticton_dm2009$yday, "doydm"=penticton_ec2009$Day,
                            "tmean_dm"= penticton_dm2009$tmean, 
                            "tmean_ec"= penticton_ec2009$MeanTemperature,
                            stringsAsFactors=FALSE)


#create plot

ggplot() +
  geom_point(data =penticton2009, aes(x=tmean_dm, y=tmean_ec))+
  ggtitle("Env.Canada vs Daymet (PetictonA, 2009)")


##############
#### 2018 ####
##############

penticton_ec2018 <- filter(penticton_ec, Year == "2018")

penticton_dm2018 <- filter(penticton_dm, year == "2018")

#make a data set with the t mean for both sets, year, and doy

penticton2018 <- cbind("doydm"=penticton_dm2018$yday, "doydm"=penticton_ec2018$Day,
                       "tmean_dm"= penticton_dm2018$tmean, "tmean_ec"= penticton_ec2018$MeanTemperature 
)

penticton2018 <- data.frame("doydm"=penticton_dm2018$yday, "doydm"=penticton_ec2018$Day,
                            "tmean_dm"= penticton_dm2018$tmean, 
                            "tmean_ec"= penticton_ec2018$MeanTemperature,
                            stringsAsFactors=FALSE)


#create plot

ggplot() +
  geom_point(data =penticton2018, aes(x=tmean_dm, y=tmean_ec))+
  ggtitle("Env.Canada vs Daymet (PetictonA, 2018)")






