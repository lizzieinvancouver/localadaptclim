# percentage ~ distance

library(dplyr)
library(tidyr)
library(Rcpp)
library(rstanarm)
library(bayesplot)


d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated.csv", header = TRUE)
d$percentage_difference <- as.numeric(d$percentage_difference)



# percentage ~ lat difference

#1 ----
fit1 <- stan_glmer(percentage~distance_from_garden + (1|species)+ (1|garden_identifier), data = d)
#2 ----
fit2 <- stan_glmer(percentage~distance_from_garden*prov_continent + (1|species)+ (1|garden_identifier), data = d)

#3 ---- 
fit3 <- stan_glmer(percentage~(distance_from_garden|species)+(1|garden_identifier), data = d)

#4 ----
fit4 <- stan_glmer(percentage~distance_from_garden + lat_garden + (1|species), data = d)


fit2sum <- summary(fit2)
fit2sum["(Intercept)", "mean"] # Intercept value 71.42834

# coef(fit2)
plot(percentage~distance_from_garden, data=d, type="n", xlab = "Provenance latitude differences", ylab = "Climate overlap percentage")
points(percentage~distance_from_garden, data=subset(d, prov_continent=="Europe"))
points(percentage~distance_from_garden, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2sum["(Intercept)", "mean"], fit2sum["distance_from_garden", "mean"]) # line for Europe
abline((fit2sum["(Intercept)", "mean"]+fit2sum["prov_continentNorth America", "mean"]),
       (fit2sum["distance_from_garden", "mean"] + fit2sum["distance_from_garden:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2)

mcmc_areas(posterior,
           pars = c("distance_from_garden", "distance_from_garden:prov_continentNorth America"),
           prob = 0.8)


# lat ----
posterior <- as.matrix(fit1)

mcmc_areas(posterior,
           pars = c("distance_from_garden"),
           prob = 0.8)


posterior <- as.matrix(fit4)

mcmc_areas(posterior,
           pars = c("distance_from_garden"),
           prob = 0.8)



# percentage ~ earth distance

#1 ----
fit_earth_dist_1 <- stan_glmer(percentage~earth_distance_from_garden + (1|species)+ (1|garden_identifier), data = d)
#2 ----
fit_earth_dist_2 <- stan_glmer(percentage~earth_distance_from_garden*prov_continent + (1|species)+ (1|garden_identifier), data = d)

#3 ---- 
fit_earth_dist_3 <- stan_glmer(percentage~(earth_distance_from_garden|species)+(1|garden_identifier), data = d)

#4 ----
fit_earth_dist_4 <- stan_glmer(percentage~earth_distance_from_garden + lat_garden + (1|species), data = d)


fit_earth_dist_2sum <- summary(fit_earth_dist_2)
fit_earth_dist_2sum["(Intercept)", "mean"] # Intercept value 71.42834

# coef(fit_earth_dist_2)
plot(percentage~earth_distance_from_garden, data=d, type="n", xlab = "Geodesic distance", ylab = "Climate overlap percentage")
points(percentage~earth_distance_from_garden, data=subset(d, prov_continent=="Europe"))
points(percentage~earth_distance_from_garden, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit_earth_dist_2sum["(Intercept)", "mean"], fit_earth_dist_2sum["earth_distance_from_garden", "mean"]) # line for Europe
abline((fit_earth_dist_2sum["(Intercept)", "mean"]+fit_earth_dist_2sum["prov_continentNorth America", "mean"]),
       (fit_earth_dist_2sum["earth_distance_from_garden", "mean"] + fit_earth_dist_2sum["earth_distance_from_garden:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit_earth_dist_2)

mcmc_areas(posterior,
           pars = c("earth_distance_from_garden", "earth_distance_from_garden:prov_continentNorth America"),
           prob = 0.8)


# lat ----
posterior <- as.matrix(fit_earth_dist_1)

mcmc_areas(posterior,
           pars = c("earth_distance_from_garden"),
           prob = 0.8)


posterior <- as.matrix(fit_earth_dist_4)

mcmc_areas(posterior,
           pars = c("earth_distance_from_garden"),
           prob = 0.8)



