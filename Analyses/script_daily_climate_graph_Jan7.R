# Script to make graphs over winter break
# Jan 7
# alina zeng

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(plyr)
library(tidyr)
library(Cairo)
library(webshot)
library(ggplot2)
library(RColorBrewer)
library(svglite)
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Import data ----
prov <- read.csv("output/dailyclim/dailytemp_57.484_-3.17_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_51.9_-3.8_2011_2020.csv", header = TRUE)

prov <- read.csv("output/dailyclim/dailytemp_49.53333333_0.766666667_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_48.63333333_19.03333333_2011_2020.csv", header = TRUE)

#hmmmm maybe put them into different folders by study

# merge two dataframes

d <- full_join(garden, prov)
#d$doy <- yday(d$Date)

#1 plot raw climate for each year to see interannual variability (entire year)
oneyear_interannual_variability_plots <-  
  d  %>%      # the data frame
  group_by(Year) %>%  
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

# You can view the graphs before saving them
oneyear_interannual_variability_plots$plots

# Saving the plots to file

oneyear_interannual_variability_plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotJan7/EA FAGUSY Gömöry & Paule 2011/interannualclimate", "/", "Plot-", .$identifier, .$label,"-", .$Year, "entire_year_temp", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))



#2 for just jan to june
  janjune_interannual_variability_plots <-  
  d  %>%      # the data frame
  group_by(Year) %>%  
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_line(aes(x = doy, y = Temp, color = status))+
       # geom_smooth(aes(x = spring_event, y = diff_LAT))+
       scale_x_continuous(breaks=seq(0,180,15))+
       scale_x_continuous(limits = c(0, 180))+
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

# You can view the graphs before saving them
  janjune_interannual_variability_plots$plots

# Saving the plots to file

  janjune_interannual_variability_plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotJan7/EA FAGUSY Gömöry & Paule 2011/interannualclimate/JanJune", "/", "Plot-", .$identifier, .$label,"-", .$Year, "entire_year_temp", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))
  
  
# take the average and compare
MAT20112020 <- d %>% 
  dplyr::group_by(Year, identifier) %>% 
  dplyr::summarize(MAT=mean(Temp))


png(filename="MATofsitesJan7_EA FAGUSY Gömöry & Paule 2011.png", 						
    type="cairo", 						
    units="in", 						
    width=14, 						
    height=10, 						
    res=300)						
ggplot(MAT20112020, aes(x = Year, y = MAT), color = identifier) +  ### will need to see how to plot multiple lines at once
  geom_line(aes(color = identifier)) +
    guides(col=guide_legend("Type")) + 			# set legend title
    labs(title ="Plot of mean annual temperature from 2011-2020 EA FAGUSY Gömöry & Paule 2011
") +
    theme_classic() +  
  scale_x_continuous(breaks=seq(2011,2020,1))+
    theme(axis.text.x = element_text(size = 12, angle = 45),
          axis.text.y = element_text(size = 14, angle = 0),
          plot.title = element_text(size = 15),
          legend.position = "bottom") +
    labs(x = "\n Day of year", 
         y = "Temperature (\u00B0C)\n")
dev.off()						

  
