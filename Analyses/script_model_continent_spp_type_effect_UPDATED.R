# script model for continent and leaf type effects
# updated on March 12, 2023


## Extracting model fits sidebar ...
# Following here: https://mc-stan.org/rstanarm/reference/as.matrix.stanreg.html

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(viridis)
install.packages("bayesplot")

setwd("C:/Users/alina/Documents/git/localadaptclim")
d<- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_gdd_Sept27.csv",header = TRUE)
d$fall_event <- as.numeric(d$fall_event)
d$spring_event <- as.numeric(d$spring_event)

# fall first since we are more interested in those... well I need to tidy up this entire script


# fitC_fall_lat
fitC_fall_lat <- readRDS("output/model_fit/fitC_fall_lat.RDS")
draws_fitC_fall_lat <- as.matrix(fitC_fall_lat)

evergreens <- c("Picea engelmannii B","Picea sitchensis D","Tsuga heterophylla E")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A", "Fagus sylvatica R*","Fraxinus excelsior Q*",
               "Populus trichocarpa D")
deciduous <- str_replace_all(deciduous," ","_")

# Get all the species of one type now
evergreensdraws_fitC_fall_lat <- matrix(data=NA,
                                        nrow=nrow(draws_fitC_fall_lat),
                                        ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws_fitC_fall_lat[,i] <- draws_fitC_fall_lat[,paste("b[lat_prov species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws_fitC_fall_lat <- matrix(data=NA,
                                       nrow=nrow(draws_fitC_fall_lat),
                                       ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws_fitC_fall_lat[,i] <- draws_fitC_fall_lat[,paste("b[lat_prov species_garden:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost_fitC_fall_lat <- rowMeans(evergreensdraws_fitC_fall_lat)
deciduouspost_fitC_fall_lat <- rowMeans(deciduousdraws_fitC_fall_lat)


# And then you can plot them as we did before
png(filename="leaf_effect_lat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost_fitC_fall_lat, deciduouspost_fitC_fall_lat))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()




# for continental effects
northamerican <- c("Alnus rubra A", "Populus trichocarpa D",
                   "Picea engelmannii B","Picea sitchensis D", "Tsuga heterophylla E")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")
european <- c("Fagus sylvatica R*","Fraxinus excelsior Q*")
european<- str_replace_all(european," ","_")

# Get all the species of one type now
northamericandraws_fitC_fall_lat <- matrix(data=NA,
                                           nrow=nrow(draws_fitC_fall_lat),
                                           ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws_fitC_fall_lat[,i] <- draws_fitC_fall_lat[,paste("b[lat_prov species_garden:", northamerican[i], "]", sep="")]
}

europeandraws_fitC_fall_lat <- matrix(data=NA,
                                      nrow=nrow(draws_fitC_fall_lat),
                                      ncol=length(european))
for (i in c(1:length(european))){
  europeandraws_fitC_fall_lat[,i] <- draws_fitC_fall_lat[,paste("b[lat_prov species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws_fitC_fall_lat)
europeanpost <- rowMeans(europeandraws_fitC_fall_lat)

# And then you can plot them as we did before

png(filename="continent_effect_lat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_fall_mat
fitC_fall_mat <- readRDS("output/model_fit/fitC_fall_mat.RDS")
draws_fitC_fall_mat <- as.matrix(fitC_fall_mat)

evergreens <- c("Picea engelmannii B","Picea sitchensis D","Tsuga heterophylla E")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A", "Fagus sylvatica R*","Fraxinus excelsior Q*",
               "Populus trichocarpa D")
deciduous <- str_replace_all(deciduous," ","_")

# Get all the species of one type now
evergreensdraws_fitC_fall_mat <- matrix(data=NA,
                                        nrow=nrow(draws_fitC_fall_mat),
                                        ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws_fitC_fall_mat[,i] <- draws_fitC_fall_mat[,paste("b[MAT_prov species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws_fitC_fall_mat <- matrix(data=NA,
                                       nrow=nrow(draws_fitC_fall_mat),
                                       ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws_fitC_fall_mat[,i] <- draws_fitC_fall_mat[,paste("b[MAT_prov species_garden:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost_fitC_fall_mat <- rowMeans(evergreensdraws_fitC_fall_mat)
deciduouspost_fitC_fall_mat <- rowMeans(deciduousdraws_fitC_fall_mat)

> summary(evergreenspost_fitC_fall_mat)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
4.698   5.905   6.231   6.230   6.567   8.018 

> summary(deciduouspost_fitC_fall_mat)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.381   3.295   3.701   3.692   4.079   5.795 


# And then you can plot them as we did before
png(filename="leaf_effect_mat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost_fitC_fall_mat, deciduouspost_fitC_fall_mat))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Provenance MAT")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()




# for continental effects
northamerican <- c("Alnus rubra A", "Populus trichocarpa D",
                   "Picea engelmannii B","Picea sitchensis D", "Tsuga heterophylla E")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")
european <- c("Fagus sylvatica R*","Fraxinus excelsior Q*")
european<- str_replace_all(european," ","_")

# Get all the species of one type now
northamericandraws_fitC_fall_mat <- matrix(data=NA,
                                           nrow=nrow(draws_fitC_fall_mat),
                                           ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws_fitC_fall_mat[,i] <- draws_fitC_fall_mat[,paste("b[MAT_prov species_garden:", northamerican[i], "]", sep="")]
}

europeandraws_fitC_fall_mat <- matrix(data=NA,
                                      nrow=nrow(draws_fitC_fall_mat),
                                      ncol=length(european))
for (i in c(1:length(european))){
  europeandraws_fitC_fall_mat[,i] <- draws_fitC_fall_mat[,paste("b[MAT_prov species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost_fitC_fall_mat <- rowMeans(northamericandraws_fitC_fall_mat)
europeanpost_fitC_fall_mat <- rowMeans(europeandraws_fitC_fall_mat)

# summary(northamericanpost_fitC_fall_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.147   6.151   6.412   6.411   6.672   7.914 
# summary(europeanpost_fitC_fall_mat)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3.43430 -0.01449  0.68706  0.69925  1.41898  4.06387 

# And then you can plot them as we did before

png(filename="continent_effect_mat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost_fitC_fall_mat, europeanpost_fitC_fall_mat ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Provenance MAT")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()



# fitC_spring_lat
fitC_spring_lat <- readRDS("output/model_fit/fitC_spring_lat.RDS")
fitC_spring_mat <- readRDS("output/model_fit/fitC_spring_mat.RDS")

draws <- as.matrix(fitC_spring_lat)
# the full posterior for Alnus rubra slope:
alnrubslope <- draws[,"b[lat_prov species_garden:Alnus_rubra]"]
hist(alnrubslope)

# To get the estimate for continent or evergreen/deciduous:
# Here I show an incomplete example for evergreen/deciduous,
# but we especially want to do for continent (group species by continent)
# Add the posteriors of all the species in one group ...
# Example ... many ways to do this, I just created different vectors (I made very incomplete lists!)
colnames(draws)

evergreens <- c("Picea abies S*","Picea engelmannii B",
"Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
"Pseudotsuga menziesii H", "Tsuga heterophylla E","Tsuga heterophylla G")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A",
               "Betula papyrifera K",
               "Betula papyrifera L",
               "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
               "Fraxinus excelsior Q*",
               "Populus balsamifera F","Populus trichocarpa D",
               "Quercus petraea U*","Quercus petraea V*")
deciduous <- str_replace_all(deciduous," ","_")

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[lat_prov species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[lat_prov species_garden:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_lat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Spring DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 40))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()

# for continental effects
northamerican <- c("Alnus rubra A","Betula papyrifera K",
"Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
"Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
  "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")

european <- c("Fagus sylvatica R*","Fagus sylvatica T*",
              "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european<- str_replace_all(european," ","_")

#coef()

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[lat_prov species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[lat_prov species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

library(ggplot2)

png(filename="continent_effect_lat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
# plot_title <- ggtitle("Continental Effects on Spring DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  # plot_title +
  theme(axis.text.x = element_text(size = 40))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_spring_mat

fitC_spring_mat <- stan_glmer(spring_event~(MAT_prov|species), data = d)
draws <- as.matrix(fitC_spring_mat)

# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea abies S*","Picea engelmannii B",
                "Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
                "Pseudotsuga menziesii H", "Tsuga heterophylla E","Tsuga heterophylla G")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A",
               "Betula papyrifera K",
               "Betula papyrifera L",
               "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
               "Fraxinus excelsior Q*",
               "Populus balsamifera F","Populus trichocarpa D",
               "Quercus petraea U*","Quercus petraea V*")
deciduous <- str_replace_all(deciduous," ","_")


# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[MAT_prov species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[MAT_prov species_garden:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_mat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Spring DOY in Relation to Provenance Mean Annual Temperature")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects
northamerican <- c("Alnus rubra A","Betula papyrifera K",
                   "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                   "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                   "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")

european <- c("Fagus sylvatica R*","Fagus sylvatica T*",
              "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european<- str_replace_all(european," ","_")



# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[MAT_prov species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[MAT_prov species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_mat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Spring DOY in Relation to Provenance Mean Annual Temperature")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_spring_overlap

fitC_spring_overlap <- stan_glmer(spring_event~(percentage|species), data = d)
draws <- as.matrix(fitC_spring_overlap)
colnames(draws)

# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea abies S*","Picea engelmannii B",
                "Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
                "Pseudotsuga menziesii H", "Tsuga heterophylla E","Tsuga heterophylla G")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A",
               "Betula papyrifera K",
               "Betula papyrifera L",
               "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
               "Fraxinus excelsior Q*",
               "Populus balsamifera F","Populus trichocarpa D",
               "Quercus petraea U*","Quercus petraea V*")
deciduous <- str_replace_all(deciduous," ","_")


# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[percentage species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[percentage species_garden:", deciduous[i], "]", sep="")]
}


# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_percentage_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Spring DOY in Relation to Climate Overlap Percentage")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 40))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects
northamerican <- c("Alnus rubra A","Betula papyrifera K",
                   "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                   "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                   "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")

european <- c("Fagus sylvatica R*","Fagus sylvatica T*",
              "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european<- str_replace_all(european," ","_")

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[percentage species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[percentage species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_percentage_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost))
plot_title <- ggtitle("Continental Effects on Spring DOY in Relation to Climate Overlap Percentage")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 40))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


fitC_spring_sd <- stan_glmer(spring_event~(sd|species), data = d)
draws <- as.matrix(fitC_spring_sd)

# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea abies S*","Picea engelmannii B",
                "Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
                "Pseudotsuga menziesii H", "Tsuga heterophylla E","Tsuga heterophylla G")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A",
               "Betula papyrifera K",
               "Betula papyrifera L",
               "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
               "Fraxinus excelsior Q*",
               "Populus balsamifera F","Populus trichocarpa D",
               "Quercus petraea U*","Quercus petraea V*")
deciduous <- str_replace_all(deciduous," ","_")


# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[sd species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[sd species_garden:", deciduous[i], "]", sep="")]
}


# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_sd_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Spring DOY in Relation to Climate Overlap Standard Deviation")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects
northamerican <- c("Alnus rubra A","Betula papyrifera K",
                   "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                   "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                   "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")

european <- c("Fagus sylvatica R*","Fagus sylvatica T*",
              "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european<- str_replace_all(european," ","_")

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[sd species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[sd species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_sd_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Spring DOY in Relation to Climate Overlap Standard Deviation")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()



# fitC_fall_lat
fitC_fall_lat <- readRDS("output/model_fit/fitC_fall_lat.RDS")
draws <- as.matrix(fitC_fall_lat)

colnames(draws)
evergreens <- c("Picea engelmannii B","Picea sitchensis D","Tsuga heterophylla E")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A", "Fagus sylvatica R*","Fraxinus excelsior Q*",
               "Populus trichocarpa D")
              deciduous <- str_replace_all(deciduous," ","_")

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[lat_prov species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[lat_prov species_garden:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_lat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects
northamerican <- c("Alnus rubra A","Betula papyrifera K",
                   "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                   "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                   "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")

european <- c("Fagus sylvatica R*","Fagus sylvatica T*",
              "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european<- str_replace_all(european," ","_")

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[lat_prov species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[lat_prov species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_lat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea abies S*","Picea engelmannii B",
                "Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
                "Pseudotsuga menziesii H", "Tsuga heterophylla E","Tsuga heterophylla G")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A",
               "Betula papyrifera K",
               "Betula papyrifera L",
               "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
               "Fraxinus excelsior Q*",
               "Populus balsamifera F","Populus trichocarpa D",
               "Quercus petraea U*","Quercus petraea V*")
deciduous <- str_replace_all(deciduous," ","_")

northamerican <- c("Alnus rubra A","Betula papyrifera K",
                   "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                   "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                   "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")

european <- c("Fagus sylvatica R*","Fagus sylvatica T*",
              "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european<- str_replace_all(european," ","_")

# fitC_fall_mat

fitC_fall_mat <- stan_glmer(fall_event~(MAT_prov|species), data = d)
draws <- as.matrix(fitC_fall_mat)

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[MAT_prov species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[MAT_prov species_garden:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_mat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Provenance Mean Annual Temperature")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[MAT_prov species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[MAT_prov species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_mat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Provenance Mean Annual Temperature")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_fall_overlap

fitC_fall_overlap <- stan_glmer(fall_event~(percentage|species), data = d)
draws <- as.matrix(fitC_fall_overlap)

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[percentage species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[percentage species_garden:", deciduous[i], "]", sep="")]
}


# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_percentage_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Climate Overlap Percentage")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[percentage species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[percentage species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_percentage_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Climate Overlap Percentage")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_fall_sd
fitC_fall_sd <- stan_glmer(fall_event~(sd|species), data = d)
draws <- as.matrix(fitC_fall_sd)

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[sd species_garden:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[sd species_garden:", deciduous[i], "]", sep="")]
}


# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_sd_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Climate Overlap Standard Deviation")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()



# for continental effects
# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[sd species_garden:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[sd species_garden:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_sd_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Climate Overlap Standard Deviation")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()