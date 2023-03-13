
# this document acts as a source file which I will read usinng ms.rnw
# it contains

# fitC_fall_lat
# fitC_fall_mat

library(dplyr)
library(tidyr)
library(Rcpp)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(viridis)
library(rstantools)


# fall evergreen vs deciduous
evergreens <- c("Picea engelmannii B","Picea sitchensis D","Tsuga heterophylla E")
library(stringr)
evergreens <- str_replace_all(evergreens," ","_")
deciduous <- c("Alnus rubra A", "Fagus sylvatica R*","Fraxinus excelsior Q*",
               "Populus trichocarpa D")
deciduous <- str_replace_all(deciduous," ","_")

# fall europeans vs north americans
northamerican <- c("Alnus rubra A", "Populus trichocarpa D",
                   "Picea engelmannii B","Picea sitchensis D", "Tsuga heterophylla E")
library(stringr)
northamerican <- str_replace_all(northamerican," ","_")
european <- c("Fagus sylvatica R*","Fraxinus excelsior Q*")
european<- str_replace_all(european," ","_")




# fitC_fall_mat
fitC_fall_mat <- readRDS("../output/model_fit/fitC_fall_mat.RDS")
draws_fitC_fall_mat <- as.matrix(fitC_fall_mat)
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
evergreenspost_fitC_fall_mat <- rowMeans(evergreensdraws_fitC_fall_mat)
deciduouspost_fitC_fall_mat <- rowMeans(deciduousdraws_fitC_fall_mat)

# UI_evergreens_fitC_fall_mat <- round(quantile(evergreenspost_fitC_fall_mat, 0.9), 2)

# summary(evergreenspost_fitC_fall_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.698   5.905   6.231   6.230   6.567   8.018
# 
# summary(deciduouspost_fitC_fall_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.381   3.295   3.701   3.692   4.079   5.795


# And then you can plot them as we did before
png(filename="leaf_effect_mat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot_fitC_fall_mat <- as.matrix(cbind(evergreenspost_fitC_fall_mat, deciduouspost_fitC_fall_mat))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Provenance MAT")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot_fitC_fall_mat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()


# for continental effects
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

# > summary(northamericanpost_fitC_fall_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.147   6.151   6.412   6.411   6.672   7.914 
# > summary(europeanpost_fitC_fall_mat)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3.43430 -0.01449  0.68706  0.69925  1.41898  4.06387 

# And then you can plot them as we did before

png(filename="continent_effect_mat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot_fitC_fall_mat <- as.matrix(cbind(northamericanpost_fitC_fall_mat, europeanpost_fitC_fall_mat ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Provenance MAT")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot_fitC_fall_mat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_fall_lat
fitC_fall_lat <- readRDS("../output/model_fit/fitC_fall_lat.RDS")
draws_fitC_fall_lat <- as.matrix(fitC_fall_lat)

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
leaftypeplot_fitC_fall_lat <- as.matrix(cbind(evergreenspost_fitC_fall_lat, deciduouspost_fitC_fall_lat))
plot_title <- ggtitle("Leaf Type Effects on Fall DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot_fitC_fall_lat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()

# for continental effects
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
northamericanpost_fitC_fall_lat <- rowMeans(northamericandraws_fitC_fall_lat)
europeanpost_fitC_fall_lat <- rowMeans(europeandraws_fitC_fall_lat)

# plot
png(filename="continent_effect_lat_fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)

continentplot_fitC_fall_lat <- as.matrix(cbind(northamericanpost_fitC_fall_lat, europeanpost_fitC_fall_lat ))
plot_title <- ggtitle("Continental Effects on Fall DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot_fitC_fall_lat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


