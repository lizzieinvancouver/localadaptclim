# script for stan_glmer models
# 22/2/2022
# alinazengziyun@yahoo.com

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)


# Set Working Directory ----

if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/misc/localadaptclim") 
} else
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Import data ----
d <- read.csv("input/data_plot_Nov13_AllStudiesToDate.csv", header = TRUE)

# length(unique(d$lat_garden)) #20
# length(unique(d$garden_identifier)) #20


#1 ----
fit1_lat <- stan_glmer(spring_event~lat_prov+ + (1|species)+ (1|garden_identifier), data = d)
fit1_mat <- stan_glmer(spring_event~MAT_prov+ + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_lat <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat <- stan_glmer(spring_event~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for latitude model ... can you repeat for MAT model?
fit2_latsum <- summary(fit2_lat)
fit2_latsum["(Intercept)", "mean"] # Intercept value for Europe 

# coef(fit2_lat)

plot(spring_event~lat_prov, data=d, type="n")
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

# But we also want to look at the estimates for each species and garden -- these values are high, so likely explain much of the variation

#2alt ___
# Slow ... amd 279 divergent transitions, but Rhats look okay
# fit2_latalt <- stan_glmer(spring_event~((lat_prov*prov_continent)|species)+ (1|garden_identifier), data = d)


#3 ---- 
fit3_lat <- stan_glmer(spring_event~(lat_prov|species)+(1|garden_identifier), data = d)
fit3_mat <- stan_glmer(spring_event~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_lat <- stan_glmer(spring_event~lat_prov + lat_garden + (1|species), data = d)
fit4_mat <- stan_glmer(spring_event~MAT_prov + MAT_garden + (1|species), data = d)
