# script_stan_glimer_altitudinal_studies
# March 30, 2022
# alinazengziyun@yahoo.com


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)


# Import data ----
d <- read.csv("input/data_plot_altitudinal_studies.csv", header = TRUE)
d$fall_event <- as.numeric(d$fall_event)

# length(unique(d$elev_garden)) #20
# length(unique(d$garden_identifier)) #20

#1 ----
fit1_elev <- stan_glmer(spring_event~elev_prov + (1|species)+ (1|garden_identifier), data = d)
fit1_mat <- stan_glmer(spring_event~MAT_prov + (1|species)+ (1|garden_identifier), data = d)

#2 ---- can't plot this becuz only have european studies atm
fit2_elev <- stan_glmer(spring_event~elev_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat <- stan_glmer(spring_event~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)


fit2_elevsum <- summary(fit2_elev)
fit2_elevsum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_elev)
plot(spring_event~elev_prov, data=d, type="n", xlab = "Provenance elevitude", ylab = "Spring DOY")
points(spring_event~elev_prov, data=subset(d, prov_continent=="Europe"))
points(spring_event~elev_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_elevsum["(Intercept)", "mean"], fit2_elevsum["elev_prov", "mean"]) # line for Europe
abline((fit2_elevsum["(Intercept)", "mean"]+fit2_elevsum["prov_continentNorth America", "mean"]),
       (fit2_elevsum["elev_prov", "mean"] + fit2_elevsum["elev_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_elev)

mcmc_areas(posterior,
           pars = c("elev_prov", "elev_prov:prov_continentNorth America"),
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



#3 ---- 
fit3_elev <- stan_glmer(spring_event~(elev_prov|species)+(1|garden_identifier), data = d)
fit3_mat <- stan_glmer(spring_event~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_elev <- stan_glmer(spring_event~elev_prov + elev_garden + (1|species), data = d)
fit4_mat <- stan_glmer(spring_event~MAT_prov + MAT_garden + (1|species), data = d)



# histograms

# elev ----
posterior <- as.matrix(fit1_elev)

mcmc_areas(posterior,
           pars = c("elev_prov"),
           prob = 0.8)


posterior <- as.matrix(fit4_elev)

mcmc_areas(posterior,
           pars = c("elev_prov"),
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