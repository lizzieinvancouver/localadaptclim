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

label <- "EG PICEAB Sogaard et al. 2008"
prov <- read.csv("output/dailyclim/dailytemp_provenance_EG PICEAB Sogaard et al. 2008_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EG PICEAB Sogaard et al. 2008_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "EA FAGUSY Gömöry & Paule 2011"
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA FAGUSY Gömöry & Paule 2011_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA FAGUSY Gömöry & Paule 2011_2011_2020.csv", header = TRUE)
garden<- na.omit(garden)

label <- "NA ALNURU Cannell et al. 1987"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NA ALNURU Cannell et al. 1987_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NA ALNURU Cannell et al. 1987_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NA ALNURU Hamann et al. 1998"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NA ALNURU Hamann et al. 1998_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NA ALNURU Hamann et al. 1998_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PICEEN Rehfeldt 1994"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PICEEN Rehfeldt 1994_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PICEEN Rehfeldt 1994_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PICESI Mimura & Aitken 2007"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PICESI Mimura & Aitken 2007_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PICESI Mimura & Aitken 2007_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PINUAL Bower and Aitken 2008"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PINUAL Bower and Aitken 2008_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PINUAL Bower and Aitken 2008_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <-  "NA POPUTR McKown et al. 2013"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NA POPUTR McKown et al. 2013_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NA POPUTR McKown et al. 2013_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <-  "NG PSEUME Lavadinovic etal 2013"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME Lavadinovic etal 2013_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME Lavadinovic etal 2013_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PSEUME Lavadinovic etal 2018"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME Lavadinovic etal 2018_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME Lavadinovic etal 2018_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

# NG PSEUME Sweet 1965
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME Sweet 1965_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME Sweet 1965_2011_2020.csv", header = TRUE)
# hmmm havnt got daily temp for new zealand yet


label <- "NG TSUGHE Kuser 1980"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG TSUGHE Kuser 1980_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG TSUGHE Kuser 1980_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PSEUME White et al. 1979"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PSEUME White et al. 1979_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PSEUME White et al. 1979_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NA POPUBA Farmer 1993"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NA POPUBA Farmer 1993_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NA POPUBA Farmer 1993_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG TSUGHE Hannerz et al. 1999"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG TSUGHE Hannerz et al. 1999_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG TSUGHE Hannerz et al. 1999_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PINUPO Dixit et al 2020"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PINUPO Dixit et al 2020_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PINUPO Dixit et al 2020_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NG PICEMA Guo et al 2021"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NG PICEMA Guo et al 2021_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NG PICEMA Guo et al 2021_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "NA BETUPA Hawkins & Dhar 2012"
prov <- read.csv("output/dailyclim/dailytemp_provenance_NA BETUPA Hawkins & Dhar 2012_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_NA BETUPA Hawkins & Dhar 2012_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "EA QUEPET Alberto et al 2011 Garden U"
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA QUEPET Alberto et al 2011_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA QUEPET Alberto et al 2011 Garden U_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

label <- "EA QUEPET Alberto et al 2011 Garden V"
prov <- read.csv("output/dailyclim/dailytemp_provenance_EA QUEPET Alberto et al 2011_2011_2020.csv", header = TRUE)
garden <- read.csv("output/dailyclim/dailytemp_garden_EA QUEPET Alberto et al 2011 Garden V_2011_2020.csv", header = TRUE)
prov <- na.omit(prov)
garden<- na.omit(garden)

# combine all distribution overlap files
setwd("C:/Users/alina/Documents/git/localadaptclim/Output/plot_distribution/distribution_overlap_percentage")
file_list <- list.files()
# https://michaelinom.medium.com/how-to-combine-all-csv-files-from-the-same-folder-into-one-data-frame-automatically-with-r-1775876a876c
for (data in list.files()){
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read.csv(data, header=TRUE)
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read.csv(data, header=TRUE)
    dataset <-unique(rbind(dataset, tempory))
    rm(tempory)
  }
}
View(dataset)

# ascend
dataset <- dataset[with(dataset, order(percentage)), ]

# export
# name<-paste("Output/plot_distribution/distribution_overlap_percentage","all_studies_dailyclim_distribution_overlap.csv",sep="")
write.csv(dataset,"all_studies_dailyclim_distribution_overlap.csv", row.names = FALSE)


#keep only temp and identifier column for March April May 
prov <- subset(prov,prov$month == "3"|prov$month == "4" | prov$month == "5")
temp_prov <- dplyr::select(prov, temp, identifier)
garden <- subset(garden,garden$month == "3"|garden$month == "4" | garden$month == "5")
temp_garden <- dplyr::select(garden, temp)
temp_garden <- temp_garden$temp # store as vector
  
# split prov df into lists based on identifier
experiment <- split(temp_prov, f = temp_prov$identifier) #omg it worked!! 
# keep only temp in each list
for( i in 1:length(experiment) ){
  experiment[[i]] <- dplyr::select(experiment[[i]], -identifier)
}
# change df in the list to just values
for( i in 1:length(experiment) ){
  experiment[[i]]<- experiment[[i]][,1] #store as values 
}

# experiment[["prov10"]]<- experiment[["prov10"]][,1]


#loop overlap()
# empty dataframe
distribution_overlap <- data.frame(identifier = character(), percentage = character(), sd = numeric())

for( i in 1:length(experiment) ){
  dataList <- list( G1 = experiment[[i]], G2 = temp_garden)
  percentage <- overlap( dataList )$OV * 100
  sd <- sd(experiment[[i]])
  distribution_overlap_add <- data.frame(percentage = percentage, sd = sd)
  distribution_overlap_add$identifier <- names(experiment[[i]]) # identifier didn't get added successfully
  distribution_overlap <- rbind(distribution_overlap, distribution_overlap_add)
}

# add identifier and label
distribution_overlap$identifier <- names(experiment) # reintroduce the provenance names
distribution_overlap$label <- label

#save
name<-paste("Output/plot_distribution/distribution_overlap_percentage/",label, "_dailyclim_distribution_overlap.csv",sep="")
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

