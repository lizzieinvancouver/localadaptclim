# script to calculate gdd
# Sept 20, 2022 # update Sept 27
# alinazengziyun@yahoo.com



# combine all the files in the folder
# https://stackoverflow.com/questions/47171374/cursor-sometimes-changes-from-a-vertical-line-to-an-underscore-in-rstudio#:~:text=The%20underscore%20means%20that%20you%20are%20in%20Overwrite,the%20rest%20of%20the%20line%20to%20move%20further.
library(dplyr)
install.packages("readr")
library(readr)
df <- list.files("Output/dailyclim", pattern="*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 


# dataframe to work with
df <- read.csv("Output/dailyclim/all_provenances_dailyclim.csv",header = TRUE)
df$gdd_numbers <- ifelse(df$temp > 0, df$temp, 0) # this step is just to filter out everything that is below 0 celsius degree

# try grouping by studies and year, and provenance


require(dplyr)
df <- df %>% dplyr::group_by(label, year, identifier)

testhhh <- summarize(df, ave_temp = mean(temp)) # can be used to calculate average gdd by day

# proven that groups have been formed


df <- df %>% group_by(label, year, identifier)
df <- df %>% mutate("gdd" = cumsum(gdd_numbers))# worked!!! happy


# now i have gdd across 10 years, I will calculate the average
df <- df %>% group_by(label, doy, identifier)

gdd_10yr_mean <- summarize(df, gdd_10yr_mean = mean(gdd))
gdd_10yr_mean$prov_identifier <- gdd_10yr_mean$identifier

# correct label names
gdd_10yr_mean$label <- ifelse(gdd_10yr_mean$label == "NA BETUPA Hawkins & Dhar 2012 K", "NA BETUPA Hawkins & Dhar 2012",gdd_10yr_mean$label)
gdd_10yr_mean$label <- ifelse(gdd_10yr_mean$label == "ALT EA QUEPET Alberto et al 2011", "EA QUEPET Alberto et al 2011",gdd_10yr_mean$label)

write.csv(gdd_10yr_mean,"Output/gdd_10yr_mean.csv", row.names = FALSE)

# once mean is calculated, join by doy, identifier, label

d<- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_Sept16.csv",header = TRUE)

#hmmm need to first create doy column that is a rounded number of spring_event

d$doy <- round(d$spring_event, digits = 0)
head(d$doy) #worked

d <- left_join(d, gdd_10yr_mean, by = c("doy","prov_identifier", "label"))


# write first so i don't lose progress
name<-"input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_gdd_Sept27.csv"
write.csv(d,name, row.names = FALSE)

#yayyy done!
