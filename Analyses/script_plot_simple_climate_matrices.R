# Script to make plots 
# (1)lat difference against doy (both spring and fall events if available)
# (2)MAT difference against doy
# November 04 2021
# alina.zeng@ubc.ca


# hmm I'm just experimenting at the moment


# there are two methods I'd like to try
# I could make a loop and split my dataframe into lists based on the "Label" column
# or I could use the do() function
# Lemme try both


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(svglite)

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/localadaptclim")

#update on Nov-13, 2021
# Import data ----
d <- read.csv("input/data_plot_Nov13_AllStudiesToDate.csv", header = TRUE)
# importing the csv file and calling it "d" for simplicity


# make a column for the LAT differences
d$diff_LAT <- d$lat_garden - d$lat_prov
# make a column for the LAT differences
d$diff_MAT <- d$MAT_garden - d$MAT_prov

# make a column for the min LAT differences hmmm still cant figurue out abline thingy
summarytest <- d %>% group_by(label, year) %>% summarise(min_diff_MAT= min(diff_LAT))
d <- full_join(d, summarytest, by = c("label", "year"))

# scale_y_continuous(breaks=seq(-8,10,2))+
  
  
# method 1
# Lat ----
lat.plots <-  
  d  %>%      # the data frame
  group_by(label,year) %>%  # grouping by Label and year...would this work?
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_point(aes(x = spring_event, y = diff_LAT), alpha = 0.5, color = "#f3730e", fill = "#f3730e") +
      # scale_y_continuous(breaks=seq(-95,20,5))+
        # geom_smooth(aes(x = spring_event, y = diff_LAT))+
      # geom_abline(intercept = d$min_diff_MAT, slope = 1) + 
       labs(title = paste("Plot of", .$label, "showing LAT Difference in", .$year, sep = " ")) +
       theme_bw() +  
       theme(axis.text.x = element_text(size = 14, angle = 0),
             axis.text.y = element_text(size = 10, angle = 0),
             plot.title = element_text(size = 10),
             legend.position = "bottom") +
       labs(x = "\n Spring Event DOY", 
            y = "Difference in Lat between Garden and Prov (Decimal Degree) \n")
  )

# You can view the graphs before saving them
lat.plots$plots

# Saving the plots to file

lat.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotNov13/LAT", "/", "Plot-", .$label,"-", .$year, "LAT", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))

# Mat ----

mat.plots <-  
  d  %>%      # the data frame
  group_by(label,year) %>%  # grouping by Label and year
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_point(aes(x = spring_event, y = diff_MAT), alpha = 0.5, color = "#6b5694", fill = "#6b5694") +
       # geom_abline(intercept = d$min_diff_MAT, slope = 1) + 
       labs(title = paste("Plot of", .$label, "showing MAT Difference in", .$year, sep = " ")) +
       theme_bw() +  
       theme(axis.text.x = element_text(size = 14, angle = 0),
             axis.text.y = element_text(size = 10, angle = 0),
             plot.title = element_text(size = 10),
             legend.position = "bottom") +
       labs(x = "\n Spring Event DOY", 
            y = "Difference in MAT between Garden and Prov (\u00B0C) \n")
  )

# You can view the graphs before saving them
mat.plots$plots

# Saving the plots to file

mat.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotNov13/MAT", "/", "Plot-", .$label,"-", .$year, "MAT", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))


# fall events ----

# subset data to studies with fall events
dfall <- subset(d, d$fall_event != "N.A.")
dfall$fall_event <- as.numeric(dfall$fall_event)
str(dfall)

# Lat ----
fall.lat.plots <-  
  dfall  %>%      # the data frame
  group_by(label,year) %>%  # grouping by Label and year...would this work?
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_point(aes(x = fall_event, y = diff_LAT), alpha = 0.5, color = "#f3730e", fill = "#f3730e") +
       # geom_smooth(aes(x = fall_event, y = diff_LAT))+
       # geom_abline(intercept = d$min_diff_MAT, slope = 1) + 
       labs(title = paste("Plot of", .$label, "showing LAT Difference in", .$year, sep = " ")) +
       theme_bw() +  
       theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
             axis.text.y = element_text(size = 10, angle = 0),
             plot.title = element_text(size = 10),
             legend.position = "bottom") +
       labs(x = "\n Fall Event DOY", 
            y = "Difference in Lat between Garden and Prov (Decimal Degree) \n")
  )

# You can view the graphs before saving them
fall.lat.plots$plots

# Saving the plots to file


fall.lat.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotNov13/fall_events", "/", "Plot-", .$label, .$year, "LAT","-fall", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))


# Mat ----
fall.mat.plots <-  
  dfall  %>%      # the data frame
  group_by(label,year) %>%  # grouping by Label and year
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_point(aes(x = fall_event, y = diff_MAT), alpha = 0.5, color = "#6b5694", fill = "#6b5694") +
       # geom_abline(intercept = d$min_diff_MAT, slope = 1) + 
       labs(title = paste("Plot of", .$label, "showing MAT Difference in", .$year, sep = " ")) +
       theme_bw() +  
       theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
             axis.text.y = element_text(size = 10, angle = 0),
             plot.title = element_text(size = 10),
             legend.position = "bottom") +
       labs(x = "\n Fall Event DOY", 
            y = "Difference in MAT between Garden and Prov (\u00B0C) \n")
  )

# Saving the plots to file
fall.mat.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotNov13/fall_events", "/", "Plot-", .$label, .$year, "MAT","-fall", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))




# need to try loop
# need more european studies
# need to do reading sent by lizzie


ggplot() + 
  geom_bar(mapping = aes(x = dt$when, y = dt$numinter), stat = "identity", fill = "grey") +
  geom_line(mapping = aes(x = dt$when, y = dt$prod*5), size = 2, color = "blue") + 
  scale_x_date(name = "Day", labels = NULL) +
  scale_y_continuous(name = "Interruptions/day", 
                     sec.axis = sec_axis(~./5, name = "Productivity % of best", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))