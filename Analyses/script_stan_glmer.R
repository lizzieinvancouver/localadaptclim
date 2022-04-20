# script for stan_glmer models
# 22/2/2022
# alinazengziyun@yahoo.com

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)


# Set Working Directory ----

if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/misc/localadaptclim") 
} else
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Import data ----
d <- read.csv("input/data_plot_Nov13_AllStudiesToDate.csv", header = TRUE)  #768
d$fall_event <- as.numeric(d$fall_event)

### Attention: April 16
# need to get rid of studies happening on different continents
d <- subset(d, d$prov_garden_continent != "different")	#680														

length(unique(d$species))  #15 different species, 7 angiosperms and 8 gymnosperms
length(unique(d$label))    #18 studies
length(unique(d$garden_identifier))  #19 gardens -> get rid of diffo continent studies

# count European provenances
test <- subset(d, d$garden_continent != "North America") #151	
# paste tgt and then unique and length
test$uniqueProv<- with(test, paste(prov_identifier, garden_identifier, sep = " "))
length(unique(test$uniqueProv)) #111 -> minus 1 becuz Alberto has two gardens double counted
length(unique(test$garden_identifier)) # 6 gardens

# 101 European provenances
unique(test$species) #4
# "Fraxinus excelsior" "Fagus sylvatica"    "Picea abies"        "Quercus petraea" 

# count North American provenances
test2 <- subset(d, d$garden_continent != "Europe") #529	
# paste tgt and then unique and length
test2$uniqueProv<- with(test2, paste(prov_identifier, label, sep = " "))
length(unique(test2$uniqueProv)) #420 -> minus 36 becuz H & D has 3 gardens double counted
# 384 North American provenances
length(unique(test2$garden_identifier)) #13 gardens

unique(test2$species) #11
# "Alnus rubra"           "Picea engelmannii"     "Picea sitchensis"      "Pinus albicaulis"     
# [5] "Populus trichocarpa"   "Tsuga heterophylla"    "Populus balsamifera"   "Pseudotsuga menziesii"
# [9] "Picea mariana"         "Pinus ponderosa"       "Betula papyrifera"

# doy ----

#1 ----
fit1_lat <- stan_glmer(spring_event~lat_prov + (1|species)+ (1|garden_identifier), data = d)
fit1_mat <- stan_glmer(spring_event~MAT_prov + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_lat <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat <- stan_glmer(spring_event~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for latitude model ... can you repeat for MAT model?
fit2_latsum <- summary(fit2_lat)
fit2_latsum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_lat)
plot(spring_event~lat_prov, data=d, type="n", xlab = "Provenance latitude", ylab = "Spring DOY")
points(spring_event~lat_prov, data=subset(d, prov_continent=="Europe"))
points(spring_event~lat_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_latsum["(Intercept)", "mean"], fit2_latsum["lat_prov", "mean"]) # line for Europe
abline((fit2_latsum["(Intercept)", "mean"]+fit2_latsum["prov_continentNorth America", "mean"]),
    (fit2_latsum["lat_prov", "mean"] + fit2_latsum["lat_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_lat)

mcmc_areas(posterior,
           pars = c("lat_prov", "lat_prov:prov_continentNorth America"),
           prob = 0.8)

# But we also want to look at the estimates for each species and garden -- these values are high, so likely explain much of the variation #how?

# MAT ----
fit2_matsum <- summary(fit2_mat)
fit2_matsum["(Intercept)", "mean"] # Intercept value for Europe  #125.4658

# coef(fit2_mat)

plot(spring_event~MAT_prov, data=d, type="n",xlab = "Provenance mean annual temperature (\u00B0C)", ylab = "Spring DOY")
points(spring_event~MAT_prov, data=subset(d, prov_continent=="Europe"))
points(spring_event~MAT_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_matsum["(Intercept)", "mean"], fit2_matsum["MAT_prov", "mean"]) # line for Europe
abline((fit2_matsum["(Intercept)", "mean"]+fit2_matsum["prov_continentNorth America", "mean"]),
       (fit2_matsum["MAT_prov", "mean"] + fit2_matsum["MAT_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_mat)

mcmc_areas(posterior,
           pars = c("MAT_prov", "MAT_prov:prov_continentNorth America"),
           prob = 0.8)


#2alt ----
# Slow ... and 279 divergent transitions, but thats look okay #alina says: I had 1141 divergent transitions after warmup
# fit2_latalt <- stan_glmer(spring_event~((lat_prov*prov_continent)|species)+ (1|garden_identifier), data = d)
# Warning messages:
# 1: There were 1141 divergent transitions after warmup. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them. 
# 2: Examine the pairs() plot to diagnose sampling problems
# 3: The largest R-hat is 1.6, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#r-hat 
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess 
# 6: Markov chains did not converge! Do not analyze results! 
  
#3 ---- 
fit3_lat <- stan_glmer(spring_event~(lat_prov|species)+(1|garden_identifier), data = d)
fit3_mat <- stan_glmer(spring_event~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_lat <- stan_glmer(spring_event~lat_prov + lat_garden + (1|species), data = d)
fit4_mat <- stan_glmer(spring_event~MAT_prov + MAT_garden + (1|species), data = d)


# histograms

# lat ----
posterior <- as.matrix(fit1_lat)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)


posterior <- as.matrix(fit4_lat)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)

# MAT ----
posterior <- as.matrix(fit1_mat)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)


posterior <- as.matrix(fit4_mat)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)