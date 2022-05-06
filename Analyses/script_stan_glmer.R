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
d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included.csv", header = TRUE)  #768
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


### New bits by Lizzie on 1 May 2022
fit1 <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species), data = d)
fit2 <- stan_glmer(spring_event~(lat_prov|species)*prov_continent, data = d)
fit3 <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species) + (1|garden_identifier), data = d)
fit4 <- stan_glmer(spring_event~(lat_prov|species)*prov_continent + (1|garden_identifier), data = d)
fit5 <- stan_glmer(spring_event~(lat_prov|species), data = d) 
fit6 <- stan_glmer(spring_event~(lat_prov|species) + (1|garden_identifier), data = d) # if you want to do loo, I would compare models like fit5 to models like this, which includes a different intercept for each garden as well


# Understanding the output  ...
summary(fit1)
# 'Model info' is just that, it tells up what model we fit and how many observations, groups etc... You'll see here that it uses 'stan_lmer.' You should always give this a quick glance to make sure it sounds right to you.   
# Below are the estimates, these are what we need to plot, understand and report to our audiences (people at talks, eventually the readers and reviewers of papers)
# (Intercept) is just that -- so for this model the intercept is 90.9 day of year.
# lat_prov  is the effect of latitude of provenance (in degrees of latitude). So for every 1 degree north the day of year gets 0.7 days later ... but since we have an effect of continent this estimate is for EUROPE. This is a small effect but the uncertainty interavals for 2.5 to 97.5 are 0.2 to 1.1, meaning there is likely a small effect
# prov_continentNorth America  gives us the effect for NA, it says for every 1 degree north the day of year gets 15.7 days later
# Next, it gives us the species offsets, so Alnus rubra has an intercept of (Intercept) + -28.4 roughly and so on ... rstanarm will actually calculate these for you. Check out ...
coef(fit1)
# sigma is the 'measurement error' meaning that after accounting for everything else in our model there is about 8.2 days of 'natural' or 'unexplained' variation

# These estimates represent mean, 2.5% etc. estimates from the posteriors and you can use the posteriors themselves to further summarize the models, which I think we may want to do. 

# For fit2, you get different intercepts and slopes for each species, based on the new plots you've made this looks like what your data needs to me (there is not ONE slope across all the data that I see) 

# For fit3, you can see many of the garden_identifier effects are small but some like for Q and R are large; you also get a sigma associated with garden and species which tells us the overall variation in each... so Sigma[garden_identifier:(Intercept),(Intercept)] is about 2.2 (this is a sigma value so does NOT mean +2.2 days, it's better to think of these as relative to one another) and the species is 3.9, suggesting species explains about 2X variation as the gardens. 

# For plotting, if we wanted for something like fit1 (random intercepts only):
# each species has a unique intercept that you can extract from coef(fit1) and the slopes are the same so you'd plot the overall slope of 0.7 for all species (we sort of want to plot the European slope (0.7 here) for European species and the NA one for NA species, but the model is not set up that way). We'd want one line per species, color-coded to match the species dots. (We could also plot the overall effects as thicker lines, but I think it might be better to show those effects as we have been the posterior density plots.)
# And for fit2 you'd have a unique slope and intercept to plot for each species, which you can get in:
coef(fit2)

# comparing some output ...
plot(coef(fit5)$species["lat_prov"][,]~coef(fit4)$species["lat_prov"][,]) # species estimates are relatively similar with or without continent


### End of new bits by Lizzie on 1 May 2022


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
