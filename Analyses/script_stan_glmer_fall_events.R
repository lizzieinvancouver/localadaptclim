# fall events

library(dplyr)
library(tidyr)
library(Rcpp)
library(rstanarm)
library(bayesplot)

d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated.csv", header = TRUE)
d$spring_event_difference <- as.numeric(d$spring_event_difference)
#get rid of NAs
d_fall <- subset(d, d$fall_event != "N.A.")
d_fall$fall_event <- as.numeric(d_fall$fall_event)

# d <- read.csv("input/data_plot_Nov13_AllStudiesToDate.csv", header = TRUE)


#1 ----
fit_fall_1_lat <- stan_glmer(fall_event~lat_prov + (1|species)+ (1|garden_identifier), data = d_fall)
fit_fall_1_mat <- stan_glmer(fall_event~MAT_prov + (1|species)+ (1|garden_identifier), data = d_fall)

#2 ---- can't plot this yet becuz no fall events for european provenances..... need to scrap, but ImageJ is broken :((
fit_fall_2_lat <- stan_glmer(fall_event~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d_fall)
fit_fall_2_mat <- stan_glmer(fall_event~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d_fall)


#3 ---- 
fit_fall_3_lat <- stan_glmer(fall_event~(lat_prov|species)+(1|garden_identifier), data = d_fall)
fit_fall_3_mat <- stan_glmer(fall_event~(MAT_prov|species)+(1|garden_identifier), data = d_fall)

#4 ----
fit_fall_4_lat <- stan_glmer(fall_event~lat_prov + lat_garden + (1|species), data = d_fall)
fit_fall_4_mat <- stan_glmer(fall_event~MAT_prov + MAT_garden + (1|species), data = d_fall)



# plot fit 2
# Here's an example of how to plot for latitude model ... can you repeat for MAT model?
fit_fall_2_latsum <- summary(fit_fall_2_lat)
fit_fall_2_latsum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit_fall_2_lat)
plot(fall_event~lat_prov, data=d, type="n", xlab = "Provenance latitude", ylab = "fall DOY")
points(fall_event~lat_prov, data=subset(d, prov_continent=="Europe"))
points(fall_event~lat_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit_fall_2_latsum["(Intercept)", "mean"], fit_fall_2_latsum["lat_prov", "mean"]) # line for Europe
abline((fit_fall_2_latsum["(Intercept)", "mean"]+fit_fall_2_latsum["prov_continentNorth America", "mean"]),
       (fit_fall_2_latsum["lat_prov", "mean"] + fit_fall_2_latsum["lat_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit_fall_2_lat)

mcmc_areas(posterior,
           pars = c("lat_prov", "lat_prov:prov_continentNorth America"),
           prob = 0.8)

# But we also want to look at the estimates for each species and garden -- these values are high, so likely explain much of the variation #how?

# MAT ----
fit_fall_2_matsum <- summary(fit_fall_2_mat)
fit_fall_2_matsum["(Intercept)", "mean"] # Intercept value for Europe  #125.4658

# coef(fit_fall_2_mat)

plot(fall_event~MAT_prov, data=d, type="n",xlab = "Provenance mean annual temperature (\u00B0C)", ylab = "fall DOY")
points(fall_event~MAT_prov, data=subset(d, prov_continent=="Europe"))
points(fall_event~MAT_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit_fall_2_matsum["(Intercept)", "mean"], fit_fall_2_matsum["MAT_prov", "mean"]) # line for Europe
abline((fit_fall_2_matsum["(Intercept)", "mean"]+fit_fall_2_matsum["prov_continentNorth America", "mean"]),
       (fit_fall_2_matsum["MAT_prov", "mean"] + fit_fall_2_matsum["MAT_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit_fall_2_mat)

mcmc_areas(posterior,
           pars = c("MAT_prov", "MAT_prov:prov_continentNorth America"),
           prob = 0.8)



# histograms

# lat ----
posterior <- as.matrix(fit_fall_1_lat)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)


posterior <- as.matrix(fit_fall_4_lat)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)

# MAT ----
posterior <- as.matrix(fit_fall_1_mat)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)


posterior <- as.matrix(fit_fall_4_mat)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)
