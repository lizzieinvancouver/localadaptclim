
library(plyr)	
library(dplyr)	
library(tidyr)	
library(lubridate)	
library(weathermetrics)	
library(ggplot2)	
library(dygraphs)	
library(Cairo)	
library(webshot)	
library(svglite)	
library(viridis)	


setwd("C:/Users/alina/Documents/git/localadaptclim/data/DataScraping")
read.csv("input/datascraping1.csv", header = T)
data <- as.data.frame((read.csv("datascraping1.csv", header = T)))

# descend
data <- data %>% arrange(desc(data$X))

write.csv(data, file = "output/scraped1.csv", row.names = F)
