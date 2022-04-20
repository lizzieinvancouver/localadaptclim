# April 16, 2022

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)


# doy difference ~ LAT/MAT----

#1 ----
fit1_lat <- stan_glmer(spring_event_difference~lat_prov + (1|species)+ (1|garden_identifier), data = d)
fit1_mat <- stan_glmer(spring_event_difference~MAT_prov + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_lat <- stan_glmer(spring_event_difference~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat <- stan_glmer(spring_event_difference~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for latitude model ... can you repeat for MAT model?
fit2_latsum <- summary(fit2_lat)
fit2_latsum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_lat)
plot(spring_event_difference~lat_prov, data=d, type="n", xlab = "Provenance latitude", ylab = "Spring DOY")
points(spring_event_difference~lat_prov, data=subset(d, prov_continent=="Europe"))
points(spring_event_difference~lat_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

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

plot(spring_event_difference~MAT_prov, data=d, type="n",xlab = "Provenance mean annual temperature (\u00B0C)", ylab = "Spring DOY")
points(spring_event_difference~MAT_prov, data=subset(d, prov_continent=="Europe"))
points(spring_event_difference~MAT_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

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
# fit2_latalt <- stan_glmer(spring_event_difference~((lat_prov*prov_continent)|species)+ (1|garden_identifier), data = d)
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
fit3_lat <- stan_glmer(spring_event_difference~(lat_prov|species)+(1|garden_identifier), data = d)
fit3_mat <- stan_glmer(spring_event_difference~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_lat <- stan_glmer(spring_event_difference~lat_prov + lat_garden + (1|species), data = d)
fit4_mat <- stan_glmer(spring_event_difference~MAT_prov + MAT_garden + (1|species), data = d)


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