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
label <- "EA FRAXEX Rosique-Esplugas 2021"
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "EA FAGUSY Petkova et al 2017"
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FAGUSY Petkova et al 2017_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FAGUSY Petkova et al 2017_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)


#keep only temp and identifier column for March April May 
prov <- subset(prov,prov$month == "3"|prov$month == "4" | prov$month == "5")
temp_prov <- dplyr::select(prov, temp, identifier)
garden <- subset(garden,garden$month == "3"|garden$month == "4" | garden$month == "5")
temp_garden <- dplyr::select(garden, temp)
temp_garden <- temp_garden$temp
  
# split prov df into lists based on identifier
experiment <- split(temp_prov, f = temp_prov$identifier) #omg it worked!! 
# keep only temp in each list
for( i in 1:length(experiment) ){
  experiment[[i]] <- dplyr::select(experiment[[i]], -identifier)
}
# change df in the list to just values
for( i in 1:length(experiment) ){
  experiment[[i]]<- experiment[[i]][,1] #store as values # isn't working...
}

# experiment[["prov10"]]<- experiment[["prov10"]][,1]


#loop overlap()
# empty dataframe
distribution_overlap <- data.frame(identifier = character(), percentage = character())

for( i in 1:length(experiment) ){
  dataList <- list( G1 = experiment[[i]], G2 = temp_garden)
  percentage <- overlap( dataList )$OV * 100
  
  distribution_overlap_add <- data.frame(percentage = percentage)
  distribution_overlap_add$identifier <- names(experiment[[i]]) # identifier didn't get added successfully
  # EMW -- Here you'll need to rbind the old and new data each time
  distribution_overlap <- rbind(distribution_overlap, distribution_overlap_add)
}

# add identifier and label
distribution_overlap$identifier <- names(experiment)
distribution_overlap$label <- label

#save
name<-paste("Output/plot_distribution/",label, "_dailyclim_distribution_overlap.csv",sep="")
write.csv(distribution_overlap,name, row.names = FALSE)



# distribution
ggplot(distribution_overlap, aes(x=percentage)) + geom_histogram(binwidth=.5)


# OG overlap steps
dataList <- list( G1 = temp_garden, G2 = temp_garden)
overlap( dataList )$OV * 100


# for plotting ----
# need to do the following so that lines of both prov and garden show on the same plot in a loop
test<- garden %>% slice(rep(1:n(), each = length(unique(prov$identifier))))      # repeat by # of provenance times
test$identifier <- rep(1:length(unique(prov$identifier)), times = 3653)  # assign prov# to all garden data for facet loop
test$yay <- "prov"# times = 3650 when working with Daymet data
test$identifier <- paste(test$yay, test$identifier, sep = "")

# merge two dataframes
dailyclim <- full_join(test, prov)

# subset and only focus on March April May
dailyclim <- subset(dailyclim,dailyclim$month == "3" |dailyclim$month == "4" |dailyclim$month == "5")

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

