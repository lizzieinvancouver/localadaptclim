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
library(ggplot2)
library(RColorBrewer)
library(svglite)
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Import data ----

# EA FRAXEX Rosique-Esplugas 2021
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv", header = TRUE)

# EA FAGUSY Petkova et al 2017
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FAGUSY Petkova et al 2017_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FAGUSY Petkova et al 2017_2011_2020.csv", header = TRUE)

# EG PICEAB Sogaard et al. 2008
prov <- read.csv("output/dailyclim/dailytemp_provenance_EG PICEAB Sogaard et al. 2008_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EG PICEAB Sogaard et al. 2008_2011_2020.csv", header = TRUE)

# EA FAGUSY Gömöry & Paule 2011

prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FAGUSY Gömöry & Paule 2011_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FAGUSY Gömöry & Paule 2011_2011_2020.csv", header = TRUE)


# NA ALNURU Cannell et al. 1987

prov <- read.csv("output/dailyclim/dailytemp_provenance_NA ALNURU Cannell et al. 1987_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NA ALNURU Cannell et al. 1987_2011_2020.csv", header = TRUE)
# need to get rid of the 366s in european data....
 garden <- subset(garden,garden$doy != "366")

 # NA ALNURU Hamann et al. 1998
 
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NA ALNURU Hamann et al. 1998_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NA ALNURU Hamann et al. 1998_2011_2020.csv", header = TRUE)

 
 # NG PICEEN Rehfeldt 1994
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PICEEN Rehfeldt 1994_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PICEEN Rehfeldt 1994_2011_2020.csv", header = TRUE)
 

 # NG PICESI Mimura & Aitken 2007
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PICESI Mimura & Aitken 2007_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PICESI Mimura & Aitken 2007_2011_2020.csv", header = TRUE)
 
 # NG PINUAL Bower and Aitken 2008
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PINUAL Bower and Aitken 2008_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PINUAL Bower and Aitken 2008_2011_2020.csv", header = TRUE)
 
 # NA POPUTR McKown et al. 2013
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NA POPUTR McKown et al. 2013_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NA POPUTR McKown et al. 2013_2011_2020.csv", header = TRUE)

 
 
 # NG PSEUME Lavadinovic etal 2013
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME Lavadinovic etal 2013_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME Lavadinovic etal 2013_2011_2020.csv", header = TRUE)
 prov <- na.omit(prov)
 garden <- subset(garden,garden$doy != "366")
 
 # NG PSEUME Lavadinovic etal 2018
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME Lavadinovic etal 2018_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME Lavadinovic etal 2018_2011_2020.csv", header = TRUE)
 prov <- na.omit(prov)
 garden <- subset(garden,garden$doy != "366")
 
 # NG PSEUME Sweet 1965
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME Sweet 1965_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME Sweet 1965_2011_2020.csv", header = TRUE)
 # hmmm havnt got daily temp for new zealand yet
 
 # NG TSUGHE Kuser 1980
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG TSUGHE Kuser 1980_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG TSUGHE Kuser 1980_2011_2020.csv", header = TRUE)
 
 # NG PSEUME White et al. 1979
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME White et al. 1979_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME White et al. 1979_2011_2020.csv", header = TRUE)
 
 # NA POPUBA Farmer 1993
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NA POPUBA Farmer 1993_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NA POPUBA Farmer 1993_2011_2020.csv", header = TRUE)
 prov <- na.omit(prov)
 
 # NG TSUGHE Hannerz et al. 1999
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG TSUGHE Hannerz et al. 1999_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG TSUGHE Hannerz et al. 1999_2011_2020.csv", header = TRUE)
 prov <- na.omit(prov)
 
 # NG PINUPO Dixit et al 2020
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PINUPO Dixit et al 2020_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PINUPO Dixit et al 2020_2011_2020.csv", header = TRUE)
 
 # NG PICEMA Guo et al 2021
 prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PICEMA Guo et al 2021_2011_2020.csv", header = TRUE)
 garden <- read.csv("output/dailyclim/dailytemp_garden_NG PICEMA Guo et al 2021_2011_2020.csv", header = TRUE)
 
 
 
 
# add doy for gardern
# garden$doy<-  yday(garden$date)
  # drop Date
# garden <- dplyr::select(garden, -date)

# for facet (see line 134) before merging I need to replicate garden and make 42 copies...
test<- garden %>% slice(rep(1:n(), each = length(unique(prov$identifier))))      # repeat by # of provenance times
test$identifier <- rep(1:length(unique(prov$identifier)), times = 3650)  # assign prov# to all garden data for facet loop
test$yay <- "prov"# times = 3650 when working with Daymet data
test$identifier <- paste(test$yay, test$identifier, sep = "")
# yayy it worked

# merge two dataframes
d <- full_join(test, prov)

# obsolete ----
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



#2 for just jan to june ----
  janjune_interannual_variability_plots <-  
  d  %>%      # the data frame
  group_by(year) %>%  
  do(plots =           # the plotting call within the do function -> plots are generated as lists
       ggplot(data = .) + # the do() function requires that we supply the data as space dot
       geom_line(aes(x = doy, y = temp, color = status))+
       scale_x_continuous(limits = c(0, 180))+
       guides(col=guide_legend("Type")) + 			# set legend title
       labs(title = paste("Plot of",.$identifier,.$label, "daily temperature in", .$year, sep = " ")) +
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
     ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotJan7/NG PICEEN Rehfeldt 1994", "/", "Plot-", .$identifier, .$label,"-", .$year, "entire_year_temp", ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))
  

# facet ----
  png(filename="facet_NG PICEEN Rehfeldt 1994.png",						
      type="cairo", 							
      units="in", 							
      width=14, 							
      height=10, 							
      res=300)							
  ggplot(d, aes(x = doy, y = temp), color = status) +  ### will need to see how to plot multiple lines at once
    geom_line(aes(color = status)) +
    scale_x_continuous(limits = c(0, 180))+
    facet_wrap(year~., scales = "fixed") +   
    guides(col=guide_legend("Type")) + 			# set legend title
    labs(title = paste("Plot of",d$identifier,d$label, "daily temperature", sep = " ")) +
    theme_classic() +  
    theme(axis.text.x = element_text(size = 8, angle = 45),
          axis.text.y = element_text(size = 10, angle = 0),
          plot.title = element_text(size = 9),
          legend.position = "bottom") +
    labs(x = "\n Day of year", 
         y = "Temperature (\u00B0C)\n")
  dev.off()							
  
# facet loop ----
  # okayy, I need to modify garden identifier just for the sake of plotting -> 
  # if you have questions, please contact alina
  # update on Feb-2, 2022, doy 50-150
  facet_all_provenances <-  
    d  %>%      # the data frame
    group_by(identifier) %>%  
    do(plots =           # the plotting call within the do function -> plots are generated as lists
         ggplot(data = .) + # the do() function requires that we supply the data as space dot
         geom_line(aes(x = doy, y = temp, color = status))+
         scale_x_continuous(limits = c(50, 150))+
         facet_wrap(year~., scales = "fixed") +
         guides(col=guide_legend("Type")) + 			# set legend title
         labs(title = paste("Plot of",.$identifier,.$label, "daily temperature in", .$year, sep = " ")) +
         theme_classic() +  
         theme(axis.text.x = element_text(size = 8, angle = 45),
               axis.text.y = element_text(size = 10, angle = 0),
               plot.title = element_text(size = 9),
               legend.position = "bottom") +
         labs(x = "\n Day of year", 
              y = "Temperature (\u00B0C)\n")
    )
  
  # You can view the graphs before saving them
  # facet_all_provenances$plots
  
  # Saving the plots to file
  facet_all_provenances %>%              # the saving call within the do function
    do(.,
       ggsave(.$plots, filename = paste("C:/Users/alina/Documents/git/localadaptclim/Output/plotFeb2/", d$label, "/", "Plot-facet", .$identifier,"-", d$label,".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))
  

  
# take the average and compare
MAT20112020 <- d %>% 
  dplyr::group_by(year, identifier) %>% 
  dplyr::summarize(MAT=mean(temp))


png(filename="MATofsites_NG PICEEN Rehfeldt 1994.png", 						
    type="cairo", 						
    units="in", 						
    width=14, 						
    height=10, 						
    res=300)						
ggplot(MAT20112020, aes(x = year, y = MAT), color = identifier) +  ### will need to see how to plot multiple lines at once
  geom_line(aes(color = identifier)) +
    guides(col=guide_legend("Type")) + 			# set legend title
    labs(title ="Plot of mean annual temperature from 2011-2020 NG PICEEN Rehfeldt 1994
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

  
