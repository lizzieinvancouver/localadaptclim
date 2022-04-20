# script for stan_glmer models percentage overlap
# March 30, 2022
# alinazengziyun@yahoo.com

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(dplyr)
library(bayesplot)
install.packages("geosphere")
library(geosphere)


# Import data ----
d <- read.csv("input/data_plot_Nov13_AllStudiesToDate.csv", header = TRUE)
d$fall_event <- as.numeric(d$fall_event)
### Attention: April 16
# need to get rid of studies happening on different continents
d <- subset(d, d$prov_garden_continent != "different")	#680		

# d <- d[,1:21]
percentage <- read.csv("Output/plot_distribution/distribution_overlap_percentage/all_studies_dailyclim_distribution_overlap.csv", header = TRUE)

# first need to link overlap percentage to data
d <- full_join(d, percentage)
d <- unique(d)
d <- d[!is.na(d$lat_prov), ] #get rid of rows with NAs

# calculate difference of the doy of the provenance closest to garden and other provenances' doy
# 1. distance by taking the difference between prov lat and garden lat
d$distance_from_garden <- abs(d$lat_prov - d$lat_garden)
# 2. distance by using distm()
for (i in c(1:nrow(d))){
        d$earth_distance_from_garden[i] <- geosphere::distm(c(d$long_prov[i],d$lat_prov[i]), c(d$long_garden[i],d$lat_garden[i]), fun= geosphere::distGeo)
} 


d <- d %>%  dplyr::group_by(garden_identifier, label, year) # important to group by year cuz some studies have multiple years
test <-  dplyr::summarise(d, distance_from_garden_min = min(distance_from_garden, na.rm=TRUE))
test <-  dplyr::summarise(d, earth_distance_from_garden_min = min(earth_distance_from_garden, na.rm=TRUE))
d <- full_join(d, test)
d <- full_join(d, test3)
unique(d$distance_from_garden_min) # 23

# if else
d$spring_event_closest_prov <- ifelse(d$distance_from_garden == d$distance_from_garden_min, d$spring_event, NA)
d$spring_event_closest_prov <- ifelse(d$earth_distance_from_garden == d$earth_distance_from_garden_min, d$spring_event, NA)

# need to count and then divide by length to get the average and add to other columns
# easier to do this in excel.... lemme export and then reimport

write.csv(d,"output/percentage_overlap_modelling_interim.csv", row.names = FALSE)

#reimport   # always reimport from here
# lat difference distance
d <- read.csv("input/percentage_overlap_doy_difference_lat_calculated.csv", header = TRUE)
# earth distance
d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated.csv", header = TRUE)
d$spring_event_difference <- as.numeric(d$spring_event_difference)

# model series A: doy~percentage and doy~sd ----
#1 ----
fit1_percentage <- stan_glmer(spring_event~percentage + (1|species)+ (1|garden_identifier), data = d)
fit1_sd <- stan_glmer(spring_event~sd + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_percentage <- stan_glmer(spring_event~percentage*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_sd <- stan_glmer(spring_event~sd*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for percentageitude model ... can you repeat for sd model?
fit2_percentagesum <- summary(fit2_percentage)
fit2_percentagesum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_percentage)
plot(spring_event~percentage, data=d, type="n", xlab = "Climate percentage overlap", ylab = "Spring DOY")
points(spring_event~percentage, data=subset(d, prov_continent=="Europe"))
points(spring_event~percentage, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_percentagesum["(Intercept)", "mean"], fit2_percentagesum["percentage", "mean"]) # line for Europe
abline((fit2_percentagesum["(Intercept)", "mean"]+fit2_percentagesum["prov_continentNorth America", "mean"]),
       (fit2_percentagesum["percentage", "mean"] + fit2_percentagesum["percentage:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_percentage)

mcmc_areas(posterior,
           pars = c("percentage", "percentage:prov_continentNorth America"),
           prob = 0.8)

# But we also want to look at the estisdes for each species and garden -- these values are high, so likely explain much of the variation #how?

# sd ----
fit2_sdsum <- summary(fit2_sd)
fit2_sdsum["(Intercept)", "mean"] # Intercept value for Europe  #125.4658

# coef(fit2_sd)

plot(spring_event~sd, data=d, type="n",xlab = "Provenance mean annual temperature (\u00B0C)", ylab = "Spring DOY")
points(spring_event~sd, data=subset(d, prov_continent=="Europe"))
points(spring_event~sd, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_sdsum["(Intercept)", "mean"], fit2_sdsum["sd", "mean"]) # line for Europe
abline((fit2_sdsum["(Intercept)", "mean"]+fit2_sdsum["prov_continentNorth America", "mean"]),
       (fit2_sdsum["sd", "mean"] + fit2_sdsum["sd:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_sd)

mcmc_areas(posterior,
           pars = c("sd", "sd:prov_continentNorth America"),
           prob = 0.8)


#2alt ----
# Slow ... and 279 divergent transitions, but thats look okay #alina says: I had 1141 divergent transitions after warmup
# fit2_percentagealt <- stan_glmer(spring_event~((percentage*prov_continent)|species)+ (1|garden_identifier), data = d)
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
fit3_percentage <- stan_glmer(spring_event~(percentage|species)+(1|garden_identifier), data = d)
fit3_sd <- stan_glmer(spring_event~(sd|species)+(1|garden_identifier), data = d)

#4 ---- # no need to run fit 4... or need to change it 
fit4_percentage <- stan_glmer(spring_event~percentage + percentage_garden + (1|species), data = d)
fit4_sd <- stan_glmer(spring_event~sd + sd_garden + (1|species), data = d)


# histograms

# percentage ----
posterior <- as.matrix(fit1_percentage)

mcmc_areas(posterior,
           pars = c("percentage"),
           prob = 0.8)


posterior <- as.matrix(fit4_percentage)

mcmc_areas(posterior,
           pars = c("percentage"),
           prob = 0.8)

# sd ----
posterior <- as.matrix(fit1_sd)

mcmc_areas(posterior,
           pars = c("sd"),
           prob = 0.8)


posterior <- as.matrix(fit4_sd)

mcmc_areas(posterior,
           pars = c("sd"),
           prob = 0.8)


# model series B: doy_difference~percentage and doy_difference~sd ----
#1 ----
fit1_percentage_doy_diffo <- stan_glmer(spring_event_difference~percentage + (1|species)+ (1|garden_identifier), data = d)
fit1_sd_doy_diffo <- stan_glmer(spring_event_difference~sd + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_percentage_doy_diffo <- stan_glmer(spring_event_difference~percentage*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_sd_doy_diffo <- stan_glmer(spring_event_difference~sd*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for percentageitude model ... can you repeat for sd model?
fit2_percentage_doy_diffosum <- summary(fit2_percentage_doy_diffo)
fit2_percentage_doy_diffosum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_percentage_doy_diffo)
plot(spring_event_difference~percentage, data=d, type="n", xlab = "Daily climate overlap percentage", ylab = "Spring DOY difference")
points(spring_event_difference~percentage, data=subset(d, prov_continent=="Europe"))
points(spring_event_difference~percentage, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_percentage_doy_diffosum["(Intercept)", "mean"], fit2_percentage_doy_diffosum["percentage", "mean"]) # line for Europe
abline((fit2_percentage_doy_diffosum["(Intercept)", "mean"]+fit2_percentage_doy_diffosum["prov_continentNorth America", "mean"]),
       (fit2_percentage_doy_diffosum["percentage", "mean"] + fit2_percentage_doy_diffosum["percentage:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_percentage_doy_diffo)

mcmc_areas(posterior,
           pars = c("percentage", "percentage:prov_continentNorth America"),
           prob = 0.8)

# But we also want to look at the estisdes for each species and garden -- these values are high, so likely explain much of the variation #how?

# sd ----
fit2_sd_doy_diffosum <- summary(fit2_sd_doy_diffo)
fit2_sd_doy_diffosum["(Intercept)", "mean"] # Intercept value for Europe  #125.4658

# coef(fit2_sd_doy_diffo)

plot(spring_event_difference~sd, data=d, type="n",xlab = "Daily climate overlap standard deviation", ylab = "Spring DOY difference")
points(spring_event_difference~sd, data=subset(d, prov_continent=="Europe"))
points(spring_event_difference~sd, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_sd_doy_diffosum["(Intercept)", "mean"], fit2_sd_doy_diffosum["sd", "mean"]) # line for Europe
abline((fit2_sd_doy_diffosum["(Intercept)", "mean"]+fit2_sd_doy_diffosum["prov_continentNorth America", "mean"]),
       (fit2_sd_doy_diffosum["sd", "mean"] + fit2_sd_doy_diffosum["sd:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_sd_doy_diffo)

mcmc_areas(posterior,
           pars = c("sd", "sd:prov_continentNorth America"),
           prob = 0.8)

#3 ---- 
fit3_percentage_doy_diffo <- stan_glmer(spring_event_difference~(percentage|species)+(1|garden_identifier), data = d)
fit3_sd_doy_diffo <- stan_glmer(spring_event_difference~(sd|species)+(1|garden_identifier), data = d)

#4 ---- # no need to run fit 4... or need to change it 
fit4_percentage_doy_diffo <- stan_glmer(spring_event_difference~percentage + percentage_garden + (1|species), data = d)
fit4_sd_doy_diffo <- stan_glmer(spring_event_difference~sd + sd_garden + (1|species), data = d)


# histograms

# percentage ----
posterior <- as.matrix(fit1_percentage_doy_diffo)

mcmc_areas(posterior,
           pars = c("percentage"),
           prob = 0.8)


posterior <- as.matrix(fit4_percentage_doy_diffo)

mcmc_areas(posterior,
           pars = c("percentage"),
           prob = 0.8)

# sd ----
posterior <- as.matrix(fit1_sd_doy_diffo)

mcmc_areas(posterior,
           pars = c("sd"),
           prob = 0.8)


posterior <- as.matrix(fit4_sd_doy_diffo)

mcmc_areas(posterior,
           pars = c("sd"),
           prob = 0.8)


# model series C: percentage~lat/mat and sd~lat/mat ----

#percentage
#1 ----
fit1_lat_percentage <- stan_glmer(percentage~lat_prov + (1|species)+ (1|garden_identifier), data = d)
fit1_mat_percentage <- stan_glmer(percentage~MAT_prov + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_lat_percentage <- stan_glmer(percentage~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat_percentage <- stan_glmer(percentage~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for latitude model ... can you repeat for MAT model?
fit2_lat_percentagesum <- summary(fit2_lat_percentage)
fit2_lat_percentagesum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_lat_percentage)
plot(percentage~lat_prov, data=d, type="n", xlab = "Provenance latitude", ylab = "Daily climate overlap percentage")
points(percentage~lat_prov, data=subset(d, prov_continent=="Europe"))
points(percentage~lat_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_lat_percentagesum["(Intercept)", "mean"], fit2_lat_percentagesum["lat_prov", "mean"]) # line for Europe
abline((fit2_lat_percentagesum["(Intercept)", "mean"]+fit2_lat_percentagesum["prov_continentNorth America", "mean"]),
       (fit2_lat_percentagesum["lat_prov", "mean"] + fit2_lat_percentagesum["lat_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_lat_percentage)

mcmc_areas(posterior,
           pars = c("lat_prov", "lat_prov:prov_continentNorth America"),
           prob = 0.8)

# But we also want to look at the estimates for each species and garden -- these values are high, so likely explain much of the variation #how?

# MAT ----
fit2_mat_percentagesum <- summary(fit2_mat_percentage)
fit2_mat_percentagesum["(Intercept)", "mean"] # Intercept value for Europe  #125.4658

# coef(fit2_mat_percentage)

plot(percentage~MAT_prov, data=d, type="n",xlab = "Provenance mean annual temperature (\u00B0C)", ylab = "Daily climate overlap percentage")
points(percentage~MAT_prov, data=subset(d, prov_continent=="Europe"))
points(percentage~MAT_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_mat_percentagesum["(Intercept)", "mean"], fit2_mat_percentagesum["MAT_prov", "mean"]) # line for Europe
abline((fit2_mat_percentagesum["(Intercept)", "mean"]+fit2_mat_percentagesum["prov_continentNorth America", "mean"]),
       (fit2_mat_percentagesum["MAT_prov", "mean"] + fit2_mat_percentagesum["MAT_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_mat_percentage)

mcmc_areas(posterior,
           pars = c("MAT_prov", "MAT_prov:prov_continentNorth America"),
           prob = 0.8)

#3 ---- 
fit3_lat_percentage <- stan_glmer(percentage~(lat_prov|species)+(1|garden_identifier), data = d)
fit3_mat_percentage <- stan_glmer(percentage~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_lat_percentage <- stan_glmer(percentage~lat_prov + lat_garden + (1|species), data = d)
fit4_mat_percentage <- stan_glmer(percentage~MAT_prov + MAT_garden + (1|species), data = d)


# histograms

# percentage ----
posterior <- as.matrix(fit1_lat_percentage)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)


posterior <- as.matrix(fit1_mat_percentage)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)


posterior <- as.matrix(fit4_lat_percentage)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)

posterior <- as.matrix(fit4_mat_percentage)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)


#sd
#1 ----
fit1_lat_sd <- stan_glmer(sd~lat_prov + (1|species)+ (1|garden_identifier), data = d)
fit1_mat_sd <- stan_glmer(sd~MAT_prov + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_lat_sd <- stan_glmer(sd~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat_sd <- stan_glmer(sd~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)

# Here's an example of how to plot for latitude model ... can you repeat for MAT model?
fit2_lat_sdsum <- summary(fit2_lat_sd)
fit2_lat_sdsum["(Intercept)", "mean"] # Intercept value for Europe #112.9426

# coef(fit2_lat_sd)
plot(sd~lat_prov, data=d, type="n", xlab = "Provenance latitude", ylab = "Daily climate overlap sd")
points(sd~lat_prov, data=subset(d, prov_continent=="Europe"))
points(sd~lat_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_lat_sdsum["(Intercept)", "mean"], fit2_lat_sdsum["lat_prov", "mean"]) # line for Europe
abline((fit2_lat_sdsum["(Intercept)", "mean"]+fit2_lat_sdsum["prov_continentNorth America", "mean"]),
       (fit2_lat_sdsum["lat_prov", "mean"] + fit2_lat_sdsum["lat_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_lat_sd)

mcmc_areas(posterior,
           pars = c("lat_prov", "lat_prov:prov_continentNorth America"),
           prob = 0.8)

# But we also want to look at the estimates for each species and garden -- these values are high, so likely explain much of the variation #how?

# MAT ----
fit2_mat_sdsum <- summary(fit2_mat_sd)
fit2_mat_sdsum["(Intercept)", "mean"] # Intercept value for Europe  #125.4658

# coef(fit2_mat_sd)

plot(sd~MAT_prov, data=d, type="n",xlab = "Provenance mean annual temperature (\u00B0C)", ylab = "Daily climate overlap sd")
points(sd~MAT_prov, data=subset(d, prov_continent=="Europe"))
points(sd~MAT_prov, data=subset(d, prov_continent=="North America"), col="dodgerblue")

abline(fit2_mat_sdsum["(Intercept)", "mean"], fit2_mat_sdsum["MAT_prov", "mean"]) # line for Europe
abline((fit2_mat_sdsum["(Intercept)", "mean"]+fit2_mat_sdsum["prov_continentNorth America", "mean"]),
       (fit2_mat_sdsum["MAT_prov", "mean"] + fit2_mat_sdsum["MAT_prov:prov_continentNorth America", "mean"]), col="dodgerblue") # line for N America

# We're initially most interested in these 'across garden and species' effects shown above ... so following...
# https://mc-stan.org/bayesplot/
posterior <- as.matrix(fit2_mat_sd)

mcmc_areas(posterior,
           pars = c("MAT_prov", "MAT_prov:prov_continentNorth America"),
           prob = 0.8)

#3 ---- 
fit3_lat_sd <- stan_glmer(sd~(lat_prov|species)+(1|garden_identifier), data = d)
fit3_mat_sd <- stan_glmer(sd~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_lat_sd <- stan_glmer(sd~lat_prov + lat_garden + (1|species), data = d)
fit4_mat_sd <- stan_glmer(sd~MAT_prov + MAT_garden + (1|species), data = d)


# histograms

# sd ----
posterior <- as.matrix(fit1_lat_sd)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)


posterior <- as.matrix(fit1_mat_sd)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)


posterior <- as.matrix(fit4_lat_sd)

mcmc_areas(posterior,
           pars = c("lat_prov"),
           prob = 0.8)

posterior <- as.matrix(fit4_mat_sd)

mcmc_areas(posterior,
           pars = c("MAT_prov"),
           prob = 0.8)



# plot percentage against sd


library(ggplot2)
library(Cairo)
# Basic scatter plot

png(filename="percentage~sd.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(x=sd, y=percentage, color = prov_continent)) + geom_point(size=3)+
theme(axis.text.x = element_text(size = 14, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap	
      axis.text.y = element_text(size = 14),	
      axis.title = element_text(size = 18, face = "plain"),                      	
      panel.grid = element_blank(),                                          	
      legend.position = "none" ,	
      plot.margin = unit(c(1,1,1,1), units = , "cm"))+	
        guides(col=guide_legend("Continent")) + 			
        ylab("Climate overlap percentage\n") +                             		
        xlab("\n Standard deviation")+
        theme_classic() 
dev.off()   
        
        
