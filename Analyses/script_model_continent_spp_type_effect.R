# script model for continent and leaf type effects
# May-7, 2022


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
d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included.csv", header = TRUE)  #680
d$fall_event <- as.numeric(d$fall_event)
d$spring_event <- as.numeric(d$spring_event)

#fit5 <- stan_glmer(spring_event~(lat_prov|species), data = d) 

# fitA_spring_lat
fitA_spring_lat <- readRDS("output/model_fit/fitA_spring_lat.RDS")
fitA_spring_mat <- readRDS("output/model_fit/fitA_spring_mat.RDS")

draws <- as.matrix(fitA_spring_lat)
# the full posterior for Alnus rubra slope:
alnrubslope <- draws[,"b[lat_prov species:Alnus_rubra]"]
hist(alnrubslope)

# To get the estimate for continent or evergreen/deciduous:
# Here I show an incomplete example for evergreen/deciduous,
# but we especially want to do for continent (group species by continent)
# Add the posteriors of all the species in one group ...
# Example ... many ways to do this, I just created different vectors (I made very incomplete lists!)
colnames(draws)
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis","Pinus_albicaulis" ,
                "Picea_mariana","Pinus_ponderosa" ,
                "Tsuga_heterophylla","Picea_abies",   
                "Pseudotsuga_menziesii") 
deciduous <- c("Alnus_rubra",
               "Betula_papyrifera",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior" ,    
               "Populus_balsamifera",         
               "Quercus_petraea") 

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[lat_prov species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[lat_prov species:", deciduous[i], "]", sep="")]
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
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis","Pinus_albicaulis",     
                   "Populus_trichocarpa","Tsuga_heterophylla", "Populus_balsamifera","Pseudotsuga_menziesii",
                   "Picea_mariana","Pinus_ponderosa","Betula_papyrifera")

european <- c("Fraxinus_excelsior", "Fagus_sylvatica" ,"Picea_abies", "Quercus_petraea")

coef()

# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[lat_prov species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[lat_prov species:", european[i], "]", sep="")]
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


# fitA_spring_mat

fitA_spring_mat <- stan_glmer(spring_event~(MAT_prov|species), data = d)
draws <- as.matrix(fitA_spring_mat)

# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis","Pinus_albicaulis" ,
                "Picea_mariana","Pinus_ponderosa" ,
                "Tsuga_heterophylla","Picea_abies",   
                "Pseudotsuga_menziesii") 
deciduous <- c("Alnus_rubra",
               "Betula_papyrifera",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior" ,    
               "Populus_balsamifera",         
               "Quercus_petraea") 

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[MAT_prov species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[MAT_prov species:", deciduous[i], "]", sep="")]
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
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis","Pinus_albicaulis",     
                   "Populus_trichocarpa","Tsuga_heterophylla", "Populus_balsamifera","Pseudotsuga_menziesii",
                   "Picea_mariana","Pinus_ponderosa","Betula_papyrifera")

european <- c("Fraxinus_excelsior", "Fagus_sylvatica" ,"Picea_abies", "Quercus_petraea")


# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[MAT_prov species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[MAT_prov species:", european[i], "]", sep="")]
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


# fitA_spring_overlap

fitA_spring_overlap <- stan_glmer(spring_event~(percentage|species), data = d)
draws <- as.matrix(fitA_spring_overlap)
colnames(draws)

# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis","Pinus_albicaulis" ,
                "Picea_mariana","Pinus_ponderosa" ,
                "Tsuga_heterophylla","Picea_abies",   
                "Pseudotsuga_menziesii") 
deciduous <- c("Alnus_rubra",
               "Betula_papyrifera",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior" ,    
               "Populus_balsamifera",         
               "Quercus_petraea") 

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[percentage species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[percentage species:", deciduous[i], "]", sep="")]
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
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis","Pinus_albicaulis",     
                   "Populus_trichocarpa","Tsuga_heterophylla", "Populus_balsamifera","Pseudotsuga_menziesii",
                   "Picea_mariana","Pinus_ponderosa","Betula_papyrifera")

european <- c("Fraxinus_excelsior", "Fagus_sylvatica" ,"Picea_abies", "Quercus_petraea")


# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[percentage species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[percentage species:", european[i], "]", sep="")]
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


fitA_spring_sd <- stan_glmer(spring_event~(sd|species), data = d)
draws <- as.matrix(fitA_spring_sd)

# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis","Pinus_albicaulis" ,
                "Picea_mariana","Pinus_ponderosa" ,
                "Tsuga_heterophylla","Picea_abies",   
                "Pseudotsuga_menziesii") 
deciduous <- c("Alnus_rubra",
               "Betula_papyrifera",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior" ,    
               "Populus_balsamifera",         
               "Quercus_petraea") 

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[sd species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[sd species:", deciduous[i], "]", sep="")]
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
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis","Pinus_albicaulis",     
                   "Populus_trichocarpa","Tsuga_heterophylla", "Populus_balsamifera","Pseudotsuga_menziesii",
                   "Picea_mariana","Pinus_ponderosa","Betula_papyrifera")

european <- c("Fraxinus_excelsior", "Fagus_sylvatica" ,"Picea_abies", "Quercus_petraea")


# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[sd species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[sd species:", european[i], "]", sep="")]
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



# fitA_fall_lat
fitA_fall_lat <- stan_glmer(fall_event~(lat_prov|species), data = d)

Warning messages:
  1: There were 33 divergent transitions after warmup. See
http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
to find out why this is a problem and how to eliminate them. 
2: Examine the pairs() plot to diagnose sampling problems

3: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#bulk-ess

draws <- as.matrix(fitA_fall_lat)
# the full posterior for Alnus rubra slope:
alnrubslope <- draws[,"b[lat_prov species:Alnus_rubra]"]
hist(alnrubslope)

# To get the estimate for continent or evergreen/deciduous:
# Here I show an incomplete example for evergreen/deciduous,
# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
# Add the posteriors of all the species in one group ...
# Example ... many ways to do this, I just created different vectors (I made very incomplete lists!)
colnames(draws)
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis",
                "Tsuga_heterophylla")
deciduous <- c("Alnus_rubra",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior")

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[lat_prov species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[lat_prov species:", deciduous[i], "]", sep="")]
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
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis",    
                   "Populus_trichocarpa","Tsuga_heterophylla")
european <- c("Fraxinus_excelsior", "Fagus_sylvatica")


# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[lat_prov species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[lat_prov species:", european[i], "]", sep="")]
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
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis",
                "Tsuga_heterophylla")
deciduous <- c("Alnus_rubra",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior")
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis",    
                   "Populus_trichocarpa","Tsuga_heterophylla")
european <- c("Fraxinus_excelsior", "Fagus_sylvatica")

# fitA_fall_mat

fitA_fall_mat <- stan_glmer(fall_event~(MAT_prov|species), data = d)
draws <- as.matrix(fitA_fall_mat)

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[MAT_prov species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[MAT_prov species:", deciduous[i], "]", sep="")]
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
  northamericandraws[,i] <- draws[,paste("b[MAT_prov species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[MAT_prov species:", european[i], "]", sep="")]
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


# fitA_fall_overlap

fitA_fall_overlap <- stan_glmer(fall_event~(percentage|species), data = d)
draws <- as.matrix(fitA_fall_overlap)

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[percentage species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[percentage species:", deciduous[i], "]", sep="")]
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
  northamericandraws[,i] <- draws[,paste("b[percentage species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[percentage species:", european[i], "]", sep="")]
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


# fitA_fall_sd
fitA_fall_sd <- stan_glmer(fall_event~(sd|species), data = d)
draws <- as.matrix(fitA_fall_sd)

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[sd species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[sd species:", deciduous[i], "]", sep="")]
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
  northamericandraws[,i] <- draws[,paste("b[sd species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[sd species:", european[i], "]", sep="")]
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