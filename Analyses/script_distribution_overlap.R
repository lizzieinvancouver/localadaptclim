# Script to calculate overlap of two distributions
# Feb-2, 2022

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

install.packages("overlapping") # need lizzie's help
library(overlapping)
library(dplyr)
library(plyr)
library(tidyr)
library(Cairo)
library(ggplot2)
library(RColorBrewer)

# Import data ----

# EA FRAXEX Rosique-Esplugas 2021
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

test<- garden %>% slice(rep(1:n(), each = length(unique(prov$identifier))))      # repeat by # of provenance times
test$identifier <- rep(1:length(unique(prov$identifier)), times = 3653)  # assign prov# to all garden data for facet loop
test$yay <- "prov"# times = 3650 when working with Daymet data
test$identifier <- paste(test$yay, test$identifier, sep = "")

# merge two dataframes
dailyclim <- full_join(test, prov)

# subset and only focus on March April May
dailyclim <- subset(dailyclim,dailyclim$month != "1")
dailyclim <- subset(dailyclim,dailyclim$month != "2")
dailyclim <- subset(dailyclim,dailyclim$month != "6")
dailyclim <- subset(dailyclim,dailyclim$month != "7")
dailyclim <- subset(dailyclim,dailyclim$month != "8")
dailyclim <- subset(dailyclim,dailyclim$month != "9")
dailyclim <- subset(dailyclim,dailyclim$month != "10")
dailyclim <- subset(dailyclim,dailyclim$month != "11")
dailyclim <- subset(dailyclim,dailyclim$month != "12")



# hmmm first find the way to calculate
# and then loop it

dailyclim %>% group_by(year,identifier,status) %>% overlap(dailyclim$temp)


for 

# or make list: list 1 = garden temp, list 2-1X provenance 
 <- dailyclim %>% group_by (identifier)

G1 <- dailyclim[dailyclim$status"garden"$temp


dataList <- list(G1 = garden, G2 = G2)
overlap(dataList)

# plotting two distribution lines tgt
# group_by(identifier)
# ggplot(dailyclim, aes(x=temp)) + geom_density()

facet_distributions <-  
  dailyclim  %>%      # the data frame
  group_by(identifier) %>%  
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_density(aes(x = temp, color = status))+
       facet_wrap(year~., scales = "fixed") +
       guides(col=guide_legend("Type")) + 			# set legend title
       labs(title = paste("Plot of",.$label, "March April May daily temperature distribution in", .$identifier, sep = " ")) +
       theme_classic() +  
       theme(axis.text.x = element_text(size = 8, angle = 45),
             axis.text.y = element_text(size = 10, angle = 0),
             plot.title = element_text(size = 9),
             legend.position = "bottom") +
       labs(x = "\n Temperature (\u00B0C)", 
            y = "Density \n")
  )

# You can view the graphs before saving them
# facet_all_provenances$plots

# Saving the plots to file
facet_distributions %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plot_distribution/", dailyclim$label, "/", "Plot-facet", .$identifier,"-", dailyclim$label,".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))

