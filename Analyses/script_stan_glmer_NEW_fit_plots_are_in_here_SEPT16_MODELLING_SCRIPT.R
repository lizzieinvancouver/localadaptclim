# script_stan_glmer_NEW

# 4/May/2022
# alinazengziyun@yahoo.com


# updated Nov-13, 2022 to try out model outputs

install.packages('modelsummary')
library(modelsummary)

# test 
fitA_spring_lat <- readRDS("output/model_fit/fitA_spring_lat.RDS")
fitA_fall_lat <- readRDS("output/model_fit/fitA_fall_lat.RDS")
modelsummary(fitA_spring_lat, statistic = "conf.int")


# two models side by side
models <- list(fitA_spring_lat, fitA_fall_lat)
modelsummary(models,statistic = "conf.int")

# this looks very good
modelsummary::datasummary(fitA_spring_lat)
datasummary_skim(fitA_spring_lat)

#experiment
datasummary_skim( (b[(Intercept) species:Alnus_rubra])~Mean + SD + P5 + P50 + P95, data = fitA_spring_lat)



# another method https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-rstanarm.html

install.packages("ggdist","tidybayes")
install.packages("knitr")
library(ggdist)
library(tidybayes)
library(tidyr)
library(knitr)
m = fitA_spring_lat
get_variables(m) #  get a list of raw model variables names so that we know what variables we can extract from the model:

m %>%   # tidy up the names for each row
  spread_draws(b[term,group]) %>%
  head(10)

m %>%
  spread_draws(b[,group]) %>%  #no need to have the term column
  head(10)

# m %>% 
# spread_draws(b[,group]) %>%   # separate out species
# separate(group, c("group", "species"), ":") %>%
# head(10)

#perfect!!!
m %>%
  spread_draws(b[,group]) %>% 
  summarise_draws()     # https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-rstanarm.html
                        # https://mc-stan.org/posterior/reference/draws_summary.html


library(flextable)
modelsummary(models,statistic = "conf.int",output = "tabletest.tex")

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(viridisLite)
library(viridis)
library(svglite)

d<- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_gdd_Sept27.csv",header = TRUE)

d$fall_event <- as.numeric(d$fall_event)
d$spring_event <- as.numeric(d$spring_event)

#export d cuz i have information abt intercept and slope

name<-paste("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_gdd_Sept27",sep="")
write.csv(d,name, row.names = FALSE)

### New bits by Lizzie on 1 May 2022
fit1 <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species), data = d)
fit2 <- stan_glmer(spring_event~(lat_prov|species)*prov_continent, data = d)
fit3 <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species) + (1|garden_identifier), data = d)
fit4 <- stan_glmer(spring_event~(lat_prov|species)*prov_continent + (1|garden_identifier), data = d)
fit5 <- stan_glmer(spring_event~(lat_prov|species), data = d) # main model
fit6 <- stan_glmer(spring_event~(lat_prov|species) + (1|garden_identifier), data = d) # if you want to do loo, I would compare models like fit5 to models like this, which includes a different intercept for each garden as well


# Understanding the output  ...
summary(fit1)
# 'Model info' is just that, it tells up what model we fit and how many observations, groups etc... You'll see here that it uses 'stan_lmer.' You should always give this a quick glance to make sure it sounds right to you.   
# Below are the estimates, these are what we need to plot, understand and report to our audiences (people at talks, eventually the readers and reviewers of papers)
# (Intercept) is just that -- so for this model the intercept is 114.2 day of year.
# lat_prov  is the effect of latitude of provenance (in degrees of latitude). So for every 1 degree north the day of year gets 0.7 days later ... but since we have an effect of continent this estimate is for EUROPE. This is a small effect but the uncertainty interavals for 2.5 to 97.5 are 0.2 to 1.1, meaning there is likely a small effect
# prov_continentNorth America  gives us the effect for NA, it says for every 1 degree north the day of year gets 16.8 days later
# Next, it gives us the species offsets, so Alnus rubra has an intercept of (Intercept) + -28.3 roughly and so on ... rstanarm will actually calculate these for you. Check out ...
coef(fit1)
# sigma is the 'measurement error' meaning that after accounting for everything else in our model there is about 8.2 days of 'natural' or 'unexplained' variation

# These estimates represent mean, 2.5% etc. estimates from the posteriors and you can use the posteriors themselves to further summarize the models, which I think we may want to do. 

# For fit2, you get different intercepts and slopes for each species, based on the new plots you've made this looks like what your data needs to me (there is not ONE slope across all the data that I see) 

# For fit3, you can see many of the garden_identifier effects are small but some like for Q and R are large; you also get a sigma associated with garden and species which tells us the overall variation in each... so Sigma[garden_identifier:(Intercept),(Intercept)] is about 2.2 (this is a sigma value so does NOT mean +2.2 days, it's better to think of these as relative to one another) and the species is 3.9, suggesting species explains about 2X variation as the gardens. 

# For plotting, if we wanted for something like fit1 (random intercepts only):
# each species has a unique intercept that you can extract from coef(fit1) and the slopes are the same so you'd plot the overall slope of 0.6766682 for all species (we sort of want to plot the European slope (0.7 here) for European species and the NA one for NA species, but the model is not set up that way). We'd want one line per species, color-coded to match the species dots. (We could also plot the overall effects as thicker lines, but I think it might be better to show those effects as we have been the posterior density plots.)
# And for fit2 you'd have a unique slope and intercept to plot for each species, which you can get in:
coef(fit2)

# comparing some output ...
# species estimates are relatively similar with or without continent
plot(coef(fit5)$species["lat_prov"][,]~coef(fit4)$species["lat_prov"][,]) 
# compare species effect and garden effect..?

### End of new bits by Lizzie on 1 May 2022


# do fit 5 for MAT, overlap, sd, and then for fall events, and then 
# do a fit with multiple predictors

# let me name these two fitA and fitB



# spring doy models
fitA_spring_lat <- stan_glmer(spring_event~(lat_prov|species), data = d)
fitA_spring_mat <- stan_glmer(spring_event~(MAT_prov|species), data = d)
fitA_spring_overlap <- stan_glmer(spring_event~(percentage|species), data = d)
fitA_spring_sd <- stan_glmer(spring_event~(sd|species), data = d)

# spring doy diffo models
fitA_spring_diffo_lat <- stan_glmer(spring_event_difference~(lat_prov|species), data = d)
fitA_spring_diffo_mat <- stan_glmer(spring_event_difference~(MAT_prov|species), data = d)
fitA_spring_diffo_overlap <- stan_glmer(spring_event_difference~(percentage|species), data = d)
fitA_spring_diffo_sd <- stan_glmer(spring_event_difference~(sd|species), data = d)

# fall 
fitA_fall_lat <- stan_glmer(fall_event~(lat_prov|species), data = d)
fitA_fall_mat <- stan_glmer(fall_event~(MAT_prov|species), data = d)
fitA_fall_overlap <- stan_glmer(fall_event~(percentage|species), data = d)
fitA_fall_sd <- stan_glmer(fall_event~(sd|species), data = d)

# fall doy diffo models
fitA_fall_diffo_lat <- stan_glmer(fall_event_difference~(lat_prov|species), data = d)
fitA_fall_diffo_mat <- stan_glmer(fall_event_difference~(MAT_prov|species), data = d)
fitA_fall_diffo_overlap <- stan_glmer(fall_event_difference~(percentage|species), data = d)
fitA_fall_diffo_sd <- stan_glmer(fall_event_difference~(sd|species), data = d)


# plotting for two predictors
fitB_spring_percentage_sd <- stan_glmer(spring_event~((percentage*sd)|species), data = d)
fitB_fall_percentage_sd <- stan_glmer(fall_event~((percentage*sd)|species), data = d)

# new addition Sept 16, 2022
fitA_spring_lat_diffo <- stan_glmer(spring_event~(distance_from_garden|Species), data = d)
fitA_fall_lat_diffo <- stan_glmer(fall_event~(distance_from_garden|Species), data = d)
fitA_spring_earth_distance <- stan_glmer(spring_event~(earth_distance_from_garden|Species), data = d)
fitA_fall_earth_distance <- stan_glmer(fall_event~(earth_distance_from_garden|Species), data = d)

fitA_spring_mat_diffo <- stan_glmer(spring_event~(MAT_diffo|species), data = d)
fitA_fall_mat_diffo <- stan_glmer(fall_event~(MAT_diffo|species), data = d)


fitC_spring_lat <- stan_glmer(spring_event~(lat_prov|species_garden), data = d)
fitC_fall_lat <- stan_glmer(fall_event~(lat_prov|species_garden), data = d)

fitC_spring_mat <- stan_glmer(spring_event~(MAT_prov|species_garden), data = d)
fitC_fall_mat <- stan_glmer(fall_event~(MAT_prov|species_garden), data = d)

fitD_spring_lat <- stan_glmer(gdd_10yr_mean~(lat_prov|species), data = d)
fitD_spring_mat <- stan_glmer(gdd_10yr_mean~(MAT_prov|species), data = d)

#save fit Sept 16, 2022
saveRDS(fitA_spring_lat_diffo, file = "output/model_fit/fitA_spring_lat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_lat_diffo, file = "output/model_fit/fitA_fall_lat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(fitA_spring_earth_distance, file = "output/model_fit/fitA_spring_earth_distance.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_earth_distance, file = "output/model_fit/fitA_fall_earth_distance.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(fitA_spring_mat_diffo, file = "output/model_fit/fitA_spring_mat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_mat_diffo, file = "output/model_fit/fitA_fall_mat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)



saveRDS(fitC_spring_lat, file = "output/model_fit/fitC_spring_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(fitC_fall_lat, file = "output/model_fit/fitC_fall_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)



# saving fit
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata
saveRDS(fitA_spring_lat, file = "output/model_fit/fitA_spring_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_spring_mat, file = "output/model_fit/fitA_spring_mat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_spring_overlap, file = "output/model_fit/fitA_spring_overlap.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_spring_sd, file = "output/model_fit/fitA_spring_sd.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_lat, file = "output/model_fit/fitA_fall_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_mat, file = "output/model_fit/fitA_fall_mat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_overlap, file = "output/model_fit/fitA_fall_overlap.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitA_fall_sd, file = "output/model_fit/fitA_fall_sd.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(fitB_spring_percentage_sd , file = "output/model_fit/fitB_spring_percentage_sd.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitB_fall_percentage_sd , file = "output/model_fit/fitB_fall_percentage_sd.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


saveRDS(fitD_spring_lat, file = "output/model_fit/fitD_spring_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(fitD_spring_mat, file = "output/model_fit/fitD_spring_mat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


# or
# save(fitA_fall_lat, file = "fitA_fall_lat.RData")
# load("fitA_fall_lat.RData")


# restore
fitA_spring_lat <- readRDS("output/model_fit/fitA_spring_lat.RDS")
fitA_spring_mat <- readRDS("output/model_fit/fitA_spring_mat.RDS")
fitA_spring_overlap <- readRDS("output/model_fit/fitA_spring_overlap.RDS")
fitA_spring_sd <- readRDS("output/model_fit/fitA_spring_sd.RDS")
fitA_fall_lat <- readRDS("output/model_fit/fitA_fall_lat.RDS")
fitA_fall_mat <- readRDS("output/model_fit/fitA_fall_mat.RDS")
fitA_fall_overlap <- readRDS("output/model_fit/fitA_fall_overlap.RDS")
fitA_fall_sd <- readRDS("output/model_fit/fitA_fall_sd.RDS")

fitB_spring_percentage_sd<- readRDS("output/model_fit/fitB_spring_percentage_sd.RDS")
fitB_fall_percentage_sd <- readRDS("output/model_fit/fitB_fall_percentage_sd.RDS")

fitD_spring_mat <- readRDS("output/model_fit/fitD_spring_mat.RDS")
fitD_spring_lat<- readRDS("output/model_fit/fitD_spring_lat.RDS")

fitA_spring_lat_diffo <- readRDS("output/model_fit/fitA_spring_lat_diffo.RDS")
fitA_fall_lat_diffo <- readRDS("output/model_fit/fitA_fall_lat_diffo.RDS")
fitA_spring_earth_distance <- readRDS("output/model_fit/fitA_spring_earth_distance.RDS")
fitA_fall_earth_distance <- readRDS("output/model_fit/fitA_fall_earth_distance.RDS")
fitA_spring_mat_diffo <- readRDS("output/model_fit/fitA_spring_mat_diffo.RDS")
fitA_fall_mat_diffo <- readRDS("output/model_fit/fitA_fall_mat_diffo.RDS")
fitC_spring_lat <- readRDS("output/model_fit/fitC_spring_lat.RDS")
fitC_fall_lat <- readRDS("output/model_fit/fitC_fall_lat.RDS")




# extract slope and intercept
# spring_lat
coef(fitA_spring_lat)$species
fitA_spring_lat_slope_intercept_df <- as.data.frame(coef(fitA_spring_lat)$species)

#rename columns
fitA_spring_lat_slope_intercept_df <- rename(fitA_spring_lat_slope_intercept_df, 
                                             fitA_spring_lat_slope=lat_prov, 
                                             fitA_spring_lat_intercept="(Intercept)")
fitA_spring_lat_slope_intercept_df$species <- row.names(fitA_spring_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_lat_slope_intercept_df)

# spring_lat_diffo
coef(fitA_spring_lat_diffo)$Species
fitA_spring_lat_diffo_slope_intercept_df <- as.data.frame(coef(fitA_spring_lat_diffo)$Species)

#rename columns
fitA_spring_lat_diffo_slope_intercept_df <- rename(fitA_spring_lat_diffo_slope_intercept_df, 
                                             fitA_spring_lat_diffo_slope=distance_from_garden, 
                                             fitA_spring_lat_diffo_intercept="(Intercept)")
fitA_spring_lat_diffo_slope_intercept_df$Species <- row.names(fitA_spring_lat_diffo_slope_intercept_df)
# try joining
d$Species<- d$species
d <- full_join(d,fitA_spring_lat_diffo_slope_intercept_df)


# fall_lat_diffo
coef(fitA_fall_lat_diffo)$Species
fitA_fall_lat_diffo_slope_intercept_df <- as.data.frame(coef(fitA_fall_lat_diffo)$Species)

#rename columns
fitA_fall_lat_diffo_slope_intercept_df <- rename(fitA_fall_lat_diffo_slope_intercept_df, 
                                                 fitA_fall_lat_diffo_slope=distance_from_garden, 
                                                 fitA_fall_lat_diffo_intercept="(Intercept)")
fitA_fall_lat_diffo_slope_intercept_df$Species <- row.names(fitA_fall_lat_diffo_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_lat_diffo_slope_intercept_df)



# spring_gdd_lat
coef(fitD_spring_lat)$species
fitD_spring_lat_slope_intercept_df <- as.data.frame(coef(fitD_spring_lat)$species)

#rename columns
fitD_spring_lat_slope_intercept_df <- rename(fitD_spring_lat_slope_intercept_df, 
                                             fitD_spring_lat_slope=lat_prov, 
                                             fitD_spring_lat_intercept="(Intercept)")
fitD_spring_lat_slope_intercept_df$species <- row.names(fitD_spring_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitD_spring_lat_slope_intercept_df)


# spring_gdd_mat
coef(fitD_spring_mat)$species
fitD_spring_mat_slope_intercept_df <- as.data.frame(coef(fitD_spring_mat)$species)

#rename columns
fitD_spring_mat_slope_intercept_df <- rename(fitD_spring_mat_slope_intercept_df, 
                                             fitD_spring_mat_slope=MAT_prov, 
                                             fitD_spring_mat_intercept="(Intercept)")
fitD_spring_mat_slope_intercept_df$species <- row.names(fitD_spring_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitD_spring_mat_slope_intercept_df)






# spring_earth_distance
coef(fitA_spring_earth_distance)$Species
fitA_spring_earth_distance_slope_intercept_df <- as.data.frame(coef(fitA_spring_earth_distance)$Species)

#rename columns
fitA_spring_earth_distance_slope_intercept_df <- rename(fitA_spring_earth_distance_slope_intercept_df, 
                                                        fitA_spring_earth_distance_slope=earth_distance_from_garden, 
                                                        fitA_spring_earth_distance_intercept="(Intercept)")
fitA_spring_earth_distance_slope_intercept_df$Species <- row.names(fitA_spring_earth_distance_slope_intercept_df)

d<- 
# try joining
d <- full_join(d,fitA_spring_earth_distance_slope_intercept_df)


# fall_earth_distance 
coef(fitA_fall_earth_distance)$Species
fitA_fall_earth_distance_slope_intercept_df <- as.data.frame(coef(fitA_fall_earth_distance)$Species)

#rename columns
fitA_fall_earth_distance_slope_intercept_df <- rename(fitA_fall_earth_distance_slope_intercept_df, 
                                                      fitA_fall_earth_distance_slope=earth_distance_from_garden, 
                                                      fitA_fall_earth_distance_intercept="(Intercept)")
fitA_fall_earth_distance_slope_intercept_df$species <- row.names(fitA_fall_earth_distance_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_earth_distance_slope_intercept_df)

# spring_mat_diffo
coef(fitA_spring_mat_diffo)$species
fitA_spring_mat_diffo_slope_intercept_df <- as.data.frame(coef(fitA_spring_mat_diffo)$species)

#rename columns
fitA_spring_mat_diffo_slope_intercept_df <- rename(fitA_spring_mat_diffo_slope_intercept_df, 
                                                   fitA_spring_mat_diffo_slope=MAT_diffo, 
                                                   fitA_spring_mat_diffo_intercept="(Intercept)")
fitA_spring_mat_diffo_slope_intercept_df$Species <- row.names(fitA_spring_mat_diffo_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_mat_diffo_slope_intercept_df)


# fall_mat_diffo
coef(fitA_fall_mat_diffo)$species
fitA_fall_mat_diffo_slope_intercept_df <- as.data.frame(coef(fitA_fall_mat_diffo)$species)

#rename columns
fitA_fall_mat_diffo_slope_intercept_df <- rename(fitA_fall_mat_diffo_slope_intercept_df, 
                                                 fitA_fall_mat_diffo_slope=MAT_diffo, 
                                                 fitA_fall_mat_diffo_intercept="(Intercept)")
fitA_fall_mat_diffo_slope_intercept_df$Species <- row.names(fitA_fall_mat_diffo_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_mat_diffo_slope_intercept_df)




# spring_mat
coef(fitA_spring_mat)$species
fitA_spring_mat_slope_intercept_df <- as.data.frame(coef(fitA_spring_mat)$species)

#rename columns
fitA_spring_mat_slope_intercept_df <- rename(fitA_spring_mat_slope_intercept_df, 
                                             fitA_spring_mat_slope=MAT_prov, 
                                             fitA_spring_mat_intercept="(Intercept)")
fitA_spring_mat_slope_intercept_df$species <- row.names(fitA_spring_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_mat_slope_intercept_df)



# spring_lat_spp_garden
coef(fitC_spring_lat)$species
fitC_spring_lat_slope_intercept_df <- as.data.frame(coef(fitC_spring_lat)$species)

#rename columns
fitC_spring_lat_slope_intercept_df <- rename(fitC_spring_lat_slope_intercept_df, 
                                             fitC_spring_lat_slope=lat_prov, 
                                             fitC_spring_lat_intercept="(Intercept)")
fitC_spring_lat_slope_intercept_df$species_garden <- row.names(fitC_spring_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitC_spring_lat_slope_intercept_df)



# fall_lat_spp_garden
coef(fitC_fall_lat)$species
fitC_fall_lat_slope_intercept_df <- as.data.frame(coef(fitC_fall_lat)$species)

#rename columns
fitC_fall_lat_slope_intercept_df <- rename(fitC_fall_lat_slope_intercept_df, 
                                           fitC_fall_lat_slope=lat_prov, 
                                           fitC_fall_lat_intercept="(Intercept)")
fitC_fall_lat_slope_intercept_df$species_garden <- row.names(fitC_fall_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_lat_slope_intercept_df)





# spring_overlap
coef(fitA_spring_overlap)$species
fitA_spring_overlap_slope_intercept_df <- as.data.frame(coef(fitA_spring_overlap)$species)

#rename columns
fitA_spring_overlap_slope_intercept_df <- rename(fitA_spring_overlap_slope_intercept_df, 
                                                 fitA_spring_overlap_slope=percentage, 
                                                 fitA_spring_overlap_intercept="(Intercept)")
fitA_spring_overlap_slope_intercept_df$species <- row.names(fitA_spring_overlap_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_overlap_slope_intercept_df)


# spring_sd
coef(fitA_spring_sd)$species
fitA_spring_sd_slope_intercept_df <- as.data.frame(coef(fitA_spring_sd)$species)

#rename columns
fitA_spring_sd_slope_intercept_df <- rename(fitA_spring_sd_slope_intercept_df, 
                                            fitA_spring_sd_slope=sd, 
                                            fitA_spring_sd_intercept="(Intercept)")
fitA_spring_sd_slope_intercept_df$species <- row.names(fitA_spring_sd_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_sd_slope_intercept_df)


# spring_diffo_lat
coef(fitA_spring_diffo_lat)$species
fitA_spring_diffo_lat_slope_intercept_df <- as.data.frame(coef(fitA_spring_diffo_lat)$species)

#rename columns
fitA_spring_diffo_lat_slope_intercept_df <- rename(fitA_spring_diffo_lat_slope_intercept_df, 
                                                   fitA_spring_diffo_lat_slope=lat_prov, 
                                                   fitA_spring_diffo_lat_intercept="(Intercept)")
fitA_spring_diffo_lat_slope_intercept_df$species <- row.names(fitA_spring_diffo_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_diffo_lat_slope_intercept_df)

# spring_diffo_mat
coef(fitA_spring_diffo_mat)$species
fitA_spring_diffo_mat_slope_intercept_df <- as.data.frame(coef(fitA_spring_diffo_mat)$species)

#rename columns
fitA_spring_diffo_mat_slope_intercept_df <- rename(fitA_spring_diffo_mat_slope_intercept_df, 
                                                   fitA_spring_diffo_mat_slope=MAT_prov, 
                                                   fitA_spring_diffo_mat_intercept="(Intercept)")
fitA_spring_diffo_mat_slope_intercept_df$species <- row.names(fitA_spring_diffo_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_diffo_mat_slope_intercept_df)



# spring_diffo_overlap
coef(fitA_spring_diffo_overlap)$species
fitA_spring_diffo_overlap_slope_intercept_df <- as.data.frame(coef(fitA_spring_diffo_overlap)$species)

#rename columns
fitA_spring_diffo_overlap_slope_intercept_df <- rename(fitA_spring_diffo_overlap_slope_intercept_df, 
                                                       fitA_spring_diffo_overlap_slope=percentage, 
                                                       fitA_spring_diffo_overlap_intercept="(Intercept)")
fitA_spring_diffo_overlap_slope_intercept_df$species <- row.names(fitA_spring_diffo_overlap_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_diffo_overlap_slope_intercept_df)


# spring_diffo_sd
coef(fitA_spring_diffo_sd)$species
fitA_spring_diffo_sd_slope_intercept_df <- as.data.frame(coef(fitA_spring_diffo_sd)$species)

#rename columns
fitA_spring_diffo_sd_slope_intercept_df <- rename(fitA_spring_diffo_sd_slope_intercept_df, 
                                                  fitA_spring_diffo_sd_slope=sd, 
                                                  fitA_spring_diffo_sd_intercept="(Intercept)")
fitA_spring_diffo_sd_slope_intercept_df$species <- row.names(fitA_spring_diffo_sd_slope_intercept_df)
# try joining
d <- full_join(d,fitA_spring_diffo_sd_slope_intercept_df)


# fall_lat
coef(fitA_fall_lat)$species
fitA_fall_lat_slope_intercept_df <- as.data.frame(coef(fitA_fall_lat)$species)

#rename columns
fitA_fall_lat_slope_intercept_df <- rename(fitA_fall_lat_slope_intercept_df, 
                                           fitA_fall_lat_slope=lat_prov, 
                                           fitA_fall_lat_intercept="(Intercept)")
fitA_fall_lat_slope_intercept_df$species <- row.names(fitA_fall_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_lat_slope_intercept_df)

# fall_mat
coef(fitA_fall_mat)$species
fitA_fall_mat_slope_intercept_df <- as.data.frame(coef(fitA_fall_mat)$species)

#rename columns
fitA_fall_mat_slope_intercept_df <- rename(fitA_fall_mat_slope_intercept_df, 
                                           fitA_fall_mat_slope=MAT_prov, 
                                           fitA_fall_mat_intercept="(Intercept)")
fitA_fall_mat_slope_intercept_df$species <- row.names(fitA_fall_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_mat_slope_intercept_df)



# fall_overlap
coef(fitA_fall_overlap)$species
fitA_fall_overlap_slope_intercept_df <- as.data.frame(coef(fitA_fall_overlap)$species)

#rename columns
fitA_fall_overlap_slope_intercept_df <- rename(fitA_fall_overlap_slope_intercept_df, 
                                               fitA_fall_overlap_slope=percentage, 
                                               fitA_fall_overlap_intercept="(Intercept)")
fitA_fall_overlap_slope_intercept_df$species <- row.names(fitA_fall_overlap_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_overlap_slope_intercept_df)


# fall_sd
coef(fitA_fall_sd)$species
fitA_fall_sd_slope_intercept_df <- as.data.frame(coef(fitA_fall_sd)$species)

#rename columns
fitA_fall_sd_slope_intercept_df <- rename(fitA_fall_sd_slope_intercept_df, 
                                          fitA_fall_sd_slope=sd, 
                                          fitA_fall_sd_intercept="(Intercept)")
fitA_fall_sd_slope_intercept_df$species <- row.names(fitA_fall_sd_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_sd_slope_intercept_df)


# fall_diffo_lat
coef(fitA_fall_diffo_lat)$species
fitA_fall_diffo_lat_slope_intercept_df <- as.data.frame(coef(fitA_fall_diffo_lat)$species)

#rename columns
fitA_fall_diffo_lat_slope_intercept_df <- rename(fitA_fall_diffo_lat_slope_intercept_df, 
                                                 fitA_fall_diffo_lat_slope=lat_prov, 
                                                 fitA_fall_diffo_lat_intercept="(Intercept)")
fitA_fall_diffo_lat_slope_intercept_df$species <- row.names(fitA_fall_diffo_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_diffo_lat_slope_intercept_df)

# fall_diffo_mat
coef(fitA_fall_diffo_mat)$species
fitA_fall_diffo_mat_slope_intercept_df <- as.data.frame(coef(fitA_fall_diffo_mat)$species)

#rename columns
fitA_fall_diffo_mat_slope_intercept_df <- rename(fitA_fall_diffo_mat_slope_intercept_df, 
                                                 fitA_fall_diffo_mat_slope=MAT_prov, 
                                                 fitA_fall_diffo_mat_intercept="(Intercept)")
fitA_fall_diffo_mat_slope_intercept_df$species <- row.names(fitA_fall_diffo_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_diffo_mat_slope_intercept_df)



# fall_diffo_overlap
coef(fitA_fall_diffo_overlap)$species
fitA_fall_diffo_overlap_slope_intercept_df <- as.data.frame(coef(fitA_fall_diffo_overlap)$species)

#rename columns
fitA_fall_diffo_overlap_slope_intercept_df <- rename(fitA_fall_diffo_overlap_slope_intercept_df, 
                                                     fitA_fall_diffo_overlap_slope=percentage, 
                                                     fitA_fall_diffo_overlap_intercept="(Intercept)")
fitA_fall_diffo_overlap_slope_intercept_df$species <- row.names(fitA_fall_diffo_overlap_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_diffo_overlap_slope_intercept_df)


# fall_diffo_sd
coef(fitA_fall_diffo_sd)$species
fitA_fall_diffo_sd_slope_intercept_df <- as.data.frame(coef(fitA_fall_diffo_sd)$species)

#rename columns
fitA_fall_diffo_sd_slope_intercept_df <- rename(fitA_fall_diffo_sd_slope_intercept_df, 
                                                fitA_fall_diffo_sd_slope=sd, 
                                                fitA_fall_diffo_sd_intercept="(Intercept)")
fitA_fall_diffo_sd_slope_intercept_df$species <- row.names(fitA_fall_diffo_sd_slope_intercept_df)
# try joining
d <- full_join(d,fitA_fall_diffo_sd_slope_intercept_df)



# plotting
# d <- rename(d, species=Species)
d <- rename(d, Species=species)
d <- rename(d, Species=species, "Provenance_continent" =prov_continent)
# color by garden, symbol by species

# lat~spring
png(filename="lat~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_lat_slope,intercept=fitA_spring_lat_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Latitude")+				
 scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()

ggsave(file="lat~spring_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# mat~spring
png(filename="mat~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_mat_slope,intercept=fitA_spring_mat_intercept,
                         linetype = Species,
                         color = garden_identifier))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance MAT (\u00B0C)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Mean Annual Temperature")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="mat~spring_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# overlap~spring
png(filename="overlap~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(percentage, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_overlap_slope,intercept=fitA_spring_overlap_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance climate overlap percentage")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Climate Overlap Percentage")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="overlap~spring_lines_added_fitA.svg", width=12, height=12)	


# sd~spring
png(filename="sd~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(sd, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_sd_slope,intercept=fitA_spring_sd_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance climate overlap standard deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Climate Overlap Standard Deviation")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="sd~spring_lines_added_fitA.svg", width=12, height=12)	


# lat~spring_diffo
png(filename="lat~spring_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_diffo_lat_slope,intercept=fitA_spring_diffo_lat_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Corrected spring event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Spring Event Day of Year (CDOY) ~ Provenance Latitude")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~spring_diffo_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# mat~spring_diffo
png(filename="mat~spring_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_diffo_mat_slope,intercept=fitA_spring_diffo_mat_intercept,
                         linetype = Species,
                         color = garden_identifier))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Corrected spring event day of year\n") +                             		
  xlab("\n Provenance MAT (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Spring Event Day of Year (CDOY) ~ Provenance Mean Annual Temperature")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="mat~spring_diffo_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# overlap~spring_diffo
png(filename="overlap~spring_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(percentage, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_diffo_overlap_slope,intercept=fitA_spring_diffo_overlap_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Corrected spring event day of year\n") +                             		
  xlab("\n Provenance climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Spring Event Day of Year (CDOY) ~ Provenance Climate Overlap Percentage")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="overlap~spring_diffo_lines_added_fitA.svg", width=12, height=12)	


# sd~spring_diffo
png(filename="sd~spring_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(sd, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_diffo_sd_slope,intercept=fitA_spring_diffo_sd_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Corrected spring event day of year\n") +                             		
  xlab("\n Provenance climate overlap standard deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Spring Event Day of Year (CDOY) ~ Provenance Climate Overlap Standard Deviation")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="sd~spring_diffo_lines_added_fitA.svg", width=12, height=12)	




# lat~fall
png(filename="lat~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_lat_slope,intercept=fitA_fall_lat_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ Provenance Latitude")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~fall_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# mat~fall
png(filename="mat~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_mat_slope,intercept=fitA_fall_mat_intercept,
                         linetype = Species,
                         color = garden_identifier))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance MAT (\u00B0C)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ Provenance Mean Annual Temperature")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="mat~fall_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# overlap~fall
png(filename="overlap~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(percentage, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_overlap_slope,intercept=fitA_fall_overlap_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance climate overlap percentage")+
  theme(axis.text.x = element_text(size = 40))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 40))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ Provenance Climate Overlap Percentage")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="overlap~fall_lines_added_fitA.svg", width=12, height=12)	


# sd~fall
png(filename="sd~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(sd, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_sd_slope,intercept=fitA_fall_sd_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance climate overlap standard deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ Provenance Climate Overlap Standard Deviation")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="sd~fall_lines_added_fitA.svg", width=12, height=12)	


# lat~fall_diffo
png(filename="lat~fall_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, fall_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_diffo_lat_slope,intercept=fitA_fall_diffo_lat_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Corrected Fall Event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Fall Event Day of Year (CDOY) ~ Provenance Latitude")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~fall_diffo_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# mat~fall_diffo
png(filename="mat~fall_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, fall_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_diffo_mat_slope,intercept=fitA_fall_diffo_mat_intercept,
                         linetype = Species,
                         color = garden_identifier))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Corrected Fall Event day of year\n") +                             		
  xlab("\n Provenance MAT (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Fall Event Day of Year (CDOY) ~ Provenance Mean Annual Temperature")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="mat~fall_diffo_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# overlap~fall_diffo
png(filename="overlap~fall_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(percentage, fall_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_diffo_overlap_slope,intercept=fitA_fall_diffo_overlap_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Corrected Fall Event day of year\n") +                             		
  xlab("\n Provenance climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Fall Event Day of Year (CDOY) ~ Provenance Climate Overlap Percentage")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="overlap~fall_diffo_lines_added_fitA.svg", width=12, height=12)	


# sd~fall_diffo
png(filename="sd~fall_diffo_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(sd, fall_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_diffo_sd_slope,intercept=fitA_fall_diffo_sd_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Corrected Fall Event day of year\n") +                             		
  xlab("\n Provenance climate overlap standard deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Corrected Fall Event Day of Year (CDOY) ~ Provenance Climate Overlap Standard Deviation")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="sd~fall_diffo_lines_added_fitA.svg", width=12, height=12)	