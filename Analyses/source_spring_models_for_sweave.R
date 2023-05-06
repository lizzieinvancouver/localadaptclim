
# this document acts as a source file for spring models which I will read usinng ms.rnw
# it contains

# fitC_spring_lat
# fitC_spring_mat
# fitC_spring_overlap

library(dplyr)
library(tidyr)
library(Rcpp)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(viridis)
library(rstantools)
library(stringr)

# spring evergreen vs deciduous_spring
evergreens_spring <- c("Picea abies S*","Picea engelmannii B",
                       "Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
                       "Pseudotsuga menziesii H", "Tsuga heterophylla E","Tsuga heterophylla G")
evergreens_spring <- stringr::str_replace_all(evergreens_spring," ","_")
deciduous_spring <-  c("Alnus rubra A",
                       "Betula papyrifera K",
                       "Betula papyrifera L",
                       "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
                       "Fraxinus excelsior Q*",
                       "Populus balsamifera F","Populus trichocarpa D",
                       "Quercus petraea U*","Quercus petraea V*")
deciduous_spring <- stringr::str_replace_all(deciduous_spring," ","_")

# spring europeans vs north americans
northamerican_spring <- c("Alnus rubra A","Betula papyrifera K",
                          "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                          "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                          "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M")
library(stringr)
northamerican_spring <- stringr::str_replace_all(northamerican_spring," ","_")
european_spring <- c("Fagus sylvatica R*","Fagus sylvatica T*",
                     "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
european_spring<- stringr::str_replace_all(european_spring," ","_")


# May 4 across continents All species
all_species_spring <- c("Alnus rubra A","Betula papyrifera K",
                        "Betula papyrifera L", "Picea engelmannii B","Picea sitchensis D", "Pinus albicaulis C",
                        "Populus trichocarpa D", "Tsuga heterophylla E","Tsuga heterophylla G", "Populus balsamifera F",
                        "Picea mariana I","Pinus ponderosa J", "Pseudotsuga menziesii H", "Betula papyrifera M",
                        "Fagus sylvatica R*","Fagus sylvatica T*",
                        "Fraxinus excelsior Q*","Picea abies S*", "Quercus petraea U*","Quercus petraea V*")
library(stringr)
all_species_spring<- stringr::str_replace_all(all_species_spring," ","_")
# all_species_spring




# fitC_spring_mat
fitC_spring_mat <- readRDS("../output/model_fit/fitC_spring_mat.RDS")

# fitC_spring_mat <- readRDS("C:/Users/alina/Documents/git/localadaptclim/Output/model_fit/fitC_spring_mat.RDS")
# summary(fitC_spring_mat)
draws_fitC_spring_mat <- as.matrix(fitC_spring_mat)


# Get all the species of one type now
all_species_springdraws_fitC_spring_mat <- matrix(data=NA,
                                                  nrow=nrow(draws_fitC_spring_mat),
                                                  ncol=length(all_species_spring))
for (i in c(1:length(all_species_spring))){
  all_species_springdraws_fitC_spring_mat[,i] <- draws_fitC_spring_mat[,paste("b[MAT_prov species_garden:", all_species_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
all_species_springpost_fitC_spring_mat <- rowMeans(all_species_springdraws_fitC_spring_mat)






# Get all the species of one type now
evergreens_springdraws_fitC_spring_mat <- matrix(data=NA,
                                        nrow=nrow(draws_fitC_spring_mat),
                                        ncol=length(evergreens_spring))
for (i in c(1:length(evergreens_spring))){
  evergreens_springdraws_fitC_spring_mat[,i] <- draws_fitC_spring_mat[,paste("b[MAT_prov species_garden:", evergreens_spring[i], "]", sep="")]
}

deciduous_springdraws_fitC_spring_mat <- matrix(data=NA,
                                       nrow=nrow(draws_fitC_spring_mat),
                                       ncol=length(deciduous_spring))
for (i in c(1:length(deciduous_spring))){
  deciduous_springdraws_fitC_spring_mat[,i] <- draws_fitC_spring_mat[,paste("b[MAT_prov species_garden:", deciduous_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
evergreens_springpost_fitC_spring_mat <- rowMeans(evergreens_springdraws_fitC_spring_mat)
deciduous_springpost_fitC_spring_mat <- rowMeans(deciduous_springdraws_fitC_spring_mat)

# UI_evergreens_spring_fitC_spring_mat <- round(quantile(evergreens_springpost_fitC_spring_mat, 0.9), 2)

# summary(evergreens_springpost_fitC_spring_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

# 
# summary(deciduous_springpost_fitC_spring_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.



# And then you can plot them as we did before
png(filename="leaf_effect_mat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot_fitC_spring_mat <- as.matrix(cbind(evergreens_springpost_fitC_spring_mat, deciduous_springpost_fitC_spring_mat))
plot_title <- ggtitle("Leaf Type Effects on spring DOY in Relation to Provenance MAT")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot_fitC_spring_mat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 35))+             # x-axis text size
  theme(axis.text.y = element_text(size = 35))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()


# NEW
png(filename="leaf_effect_mat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)

leaftypeplot_fitC_spring_mat <- as.matrix(cbind(evergreens_springpost_fitC_spring_mat, deciduous_springpost_fitC_spring_mat))
plot_title <- ggtitle("Leaf Type Effects on spring DOY in Relation to Provenance MAT")
color_scheme_set("mix-teal-pink") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot_fitC_spring_mat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 35))+             # x-axis text size
  theme(axis.text.y = element_text(size = 35))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()


# for continental effects
# Get all the species of one type now
northamerican_springdraws_fitC_spring_mat <- matrix(data=NA,
                                           nrow=nrow(draws_fitC_spring_mat),
                                           ncol=length(northamerican_spring))
for (i in c(1:length(northamerican_spring))){
  northamerican_springdraws_fitC_spring_mat[,i] <- draws_fitC_spring_mat[,paste("b[MAT_prov species_garden:", northamerican_spring[i], "]", sep="")]
}

european_springdraws_fitC_spring_mat <- matrix(data=NA,
                                      nrow=nrow(draws_fitC_spring_mat),
                                      ncol=length(european_spring))
for (i in c(1:length(european_spring))){
  european_springdraws_fitC_spring_mat[,i] <- draws_fitC_spring_mat[,paste("b[MAT_prov species_garden:", european_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamerican_springpost_fitC_spring_mat <- rowMeans(northamerican_springdraws_fitC_spring_mat)
european_springpost_fitC_spring_mat <- rowMeans(european_springdraws_fitC_spring_mat)

# > summary(northamerican_springpost_fitC_spring_mat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

# > summary(european_springpost_fitC_spring_mat)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 


# And then you can plot them as we did before

png(filename="continent_effect_mat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot_fitC_spring_mat <- as.matrix(cbind(northamerican_springpost_fitC_spring_mat, european_springpost_fitC_spring_mat ))
plot_title <- ggtitle("Continental Effects on spring DOY in Relation to Provenance MAT")
color_scheme_set("mix-teal-pink")
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot_fitC_spring_mat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 35))+             # x-axis text size
  theme(axis.text.y = element_text(size = 35))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()


# fitC_spring_lat
fitC_spring_lat <- readRDS("../output/model_fit/fitC_spring_lat.RDS")
# fitC_spring_lat <- readRDS("C:/Users/alina/Documents/git/localadaptclim/Output/model_fit/fitC_spring_lat.RDS")
draws_fitC_spring_lat <- as.matrix(fitC_spring_lat)


# Get all the species of one type now
all_species_springdraws_fitC_spring_lat <- matrix(data=NA,
                                                  nrow=nrow(draws_fitC_spring_lat),
                                                  ncol=length(all_species_spring))
for (i in c(1:length(all_species_spring))){
  all_species_springdraws_fitC_spring_lat[,i] <- draws_fitC_spring_lat[,paste("b[lat_prov species_garden:", all_species_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
all_species_springpost_fitC_spring_lat <- rowMeans(all_species_springdraws_fitC_spring_lat)


# Get all the species of one type now
evergreens_springdraws_fitC_spring_lat <- matrix(data=NA,
                                        nrow=nrow(draws_fitC_spring_lat),
                                        ncol=length(evergreens_spring))
for (i in c(1:length(evergreens_spring))){
  evergreens_springdraws_fitC_spring_lat[,i] <- draws_fitC_spring_lat[,paste("b[lat_prov species_garden:", evergreens_spring[i], "]", sep="")]
}

deciduous_springdraws_fitC_spring_lat <- matrix(data=NA,
                                       nrow=nrow(draws_fitC_spring_lat),
                                       ncol=length(deciduous_spring))
for (i in c(1:length(deciduous_spring))){
  deciduous_springdraws_fitC_spring_lat[,i] <- draws_fitC_spring_lat[,paste("b[lat_prov species_garden:", deciduous_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreens_springpost_fitC_spring_lat <- rowMeans(evergreens_springdraws_fitC_spring_lat)
deciduous_springpost_fitC_spring_lat <- rowMeans(deciduous_springdraws_fitC_spring_lat)

# And then you can plot them as we did before
png(filename="leaf_effect_lat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot_fitC_spring_lat <- as.matrix(cbind(evergreens_springpost_fitC_spring_lat, deciduous_springpost_fitC_spring_lat))
plot_title <- ggtitle("Leaf Type Effects on spring DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot_fitC_spring_lat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()

# for continental effects
# Get all the species of one type now
northamerican_springdraws_fitC_spring_lat <- matrix(data=NA,
                                           nrow=nrow(draws_fitC_spring_lat),
                                           ncol=length(northamerican_spring))
for (i in c(1:length(northamerican_spring))){
  northamerican_springdraws_fitC_spring_lat[,i] <- draws_fitC_spring_lat[,paste("b[lat_prov species_garden:", northamerican_spring[i], "]", sep="")]
}

european_springdraws_fitC_spring_lat <- matrix(data=NA,
                                      nrow=nrow(draws_fitC_spring_lat),
                                      ncol=length(european_spring))
for (i in c(1:length(european_spring))){
  european_springdraws_fitC_spring_lat[,i] <- draws_fitC_spring_lat[,paste("b[lat_prov species_garden:", european_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
northamerican_springpost_fitC_spring_lat <- rowMeans(northamerican_springdraws_fitC_spring_lat)
european_springpost_fitC_spring_lat <- rowMeans(european_springdraws_fitC_spring_lat)


mean(northamerican_springpost_fitC_spring_lat)

# # taking the average
# all_spp_garden_post <- rowMeans(all_spp_garden_draws)
# 
# quantile(all_spp_garden_post, probs = c(.1, .5, .9))


# plot
png(filename="continent_effect_lat_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)

continentplot_fitC_spring_lat <- as.matrix(cbind(northamerican_springpost_fitC_spring_lat, european_springpost_fitC_spring_lat ))
plot_title <- ggtitle("Continental Effects on spring DOY in Relation to Provenance Latitude")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot_fitC_spring_lat)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()






# fitC_spring_overlap
fitC_spring_overlap <- readRDS("../output/model_fit/fitC_spring_overlap.RDS")

# fitC_spring_overlap <- readRDS("C:/Users/alina/Documents/git/localadaptclim/Output/model_fit/fitC_spring_overlap.RDS")
draws_fitC_spring_overlap <- as.matrix(fitC_spring_overlap)


# Get all the species of one type now
all_species_springdraws_fitC_spring_overlap <- matrix(data=NA,
                                                      nrow=nrow(draws_fitC_spring_overlap),
                                                      ncol=length(all_species_spring))
for (i in c(1:length(all_species_spring))){
  all_species_springdraws_fitC_spring_overlap[,i] <- draws_fitC_spring_overlap[,paste("b[percentage species_garden:", all_species_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
all_species_springpost_fitC_spring_overlap <- rowMeans(all_species_springdraws_fitC_spring_overlap)



# climate overlap update May 05, 2023


# Get all the species of one type now
evergreens_springdraws_fitC_spring_overlap <- matrix(data=NA,
                                                     nrow=nrow(draws_fitC_spring_overlap),
                                                     ncol=length(evergreens_spring))
for (i in c(1:length(evergreens_spring))){
  evergreens_springdraws_fitC_spring_overlap[,i] <- draws_fitC_spring_overlap[,paste("b[percentage species_garden:", evergreens_spring[i], "]", sep="")]
}

deciduous_springdraws_fitC_spring_overlap <- matrix(data=NA,
                                                    nrow=nrow(draws_fitC_spring_overlap),
                                                    ncol=length(deciduous_spring))
for (i in c(1:length(deciduous_spring))){
  deciduous_springdraws_fitC_spring_overlap[,i] <- draws_fitC_spring_overlap[,paste("b[percentage species_garden:", deciduous_spring[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
evergreens_springpost_fitC_spring_overlap <- rowMeans(evergreens_springdraws_fitC_spring_overlap)
deciduous_springpost_fitC_spring_overlap <- rowMeans(deciduous_springdraws_fitC_spring_overlap)




# NEW
png(filename="leaf_effect_overlap_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)

leaftypeplot_fitC_spring_overlap <- as.matrix(cbind(evergreens_springpost_fitC_spring_overlap, deciduous_springpost_fitC_spring_overlap))
plot_title <- ggtitle("Leaf Type Effects on spring DOY in Relation to Provenance MAT")
color_scheme_set("mix-teal-pink") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot_fitC_spring_overlap)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 35))+             # x-axis text size
  theme(axis.text.y = element_text(size = 35))   +          # y-axis text size
  theme(plot.title = element_text(size = 21))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()

