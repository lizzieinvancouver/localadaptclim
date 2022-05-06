# new script for doy difference calculation

rm(list=ls()) 
options(stringsAsFactors = FALSE)
# 
 library(Rcpp)
 library(rstanarm)
library(dplyr)
library(bayesplot)
install.packages("geosphere")
 library(geosphere)



# spring

# calculate difference of the doy of the provenance closest to garden and other provenances' doy
# 1. distance by taking the difference between prov lat and garden lat
d$distance_from_garden <- abs(d$lat_prov - d$lat_garden)
# 2. distance by using distm()
for (i in c(1:nrow(d))){
  d$earth_distance_from_garden[i] <- geosphere::distm(c(d$long_prov[i],d$lat_prov[i]), c(d$long_garden[i],d$lat_garden[i]), fun= geosphere::distGeo)
} 


d <- d %>%  dplyr::group_by(garden_identifier, label, year) # important to group by year cuz some studies have multiple years
test <-  dplyr::summarise(d, distance_from_garden_min = min(distance_from_garden, na.rm=TRUE))
test <-  dplyr::summarise(d, earth_distance_from_garden_min = min(earth_distance_from_garden, na.rm=TRUE))
d <- full_join(d, test)
d <- full_join(d, test3)
unique(d$distance_from_garden_min) # 23

# if else
d$spring_event_closest_prov <- ifelse(d$distance_from_garden == d$distance_from_garden_min, d$spring_event, NA)
d$spring_event_closest_prov <- ifelse(d$earth_distance_from_garden == d$earth_distance_from_garden_min, d$spring_event, NA)

# need to count and then divide by length to get the average and add to other columns
# easier to do this in excel.... lemme export and then reimport

write.csv(d,"output/percentage_overlap_modelling_interim.csv", row.names = FALSE)

#reimport   # always reimport from here
# lat difference distance
d <- read.csv("input/percentage_overlap_doy_difference_lat_calculated.csv", header = TRUE)
# earth distance
d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated.csv", header = TRUE)
d$spring_event_difference <- as.numeric(d$spring_event_difference)

# fall event

d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted.csv", header = TRUE)
d$spring_event_difference <- as.numeric(d$spring_event_difference)
d$fall_event <- as.numeric(d$fall_event)

d$fall_event_closest_prov <- ifelse(d$earth_distance_from_garden == d$earth_distance_from_garden_min, d$fall_event, NA)
# need to count and then divide by length to get the average and add to other columns
# easier to do this in excel.... lemme export and then reimport

write.csv(d,"output/percentage_overlap_modelling_interim.csv", row.names = FALSE)


# updated file name: percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included

