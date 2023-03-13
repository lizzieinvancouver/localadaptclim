# This script documents updated fits that we need to run
# All results are for unique sp x garden (not just unique spp)
# Dec 13, 2022


# for previous fits, refer to script_rstanarm::stan_glmer_NEW_fit_plots_are_in_here



#most updated version
d<- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_gdd_Sept27.csv",header = TRUE)


# write first so i don't lose progress
name<-"input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_gdd_Sept27.csv"
write.csv(d,name, row.names = FALSE)


library(dplyr)
library(tidyr)
library(Rcpp)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(viridis)
library(rstantools)


# attempting to make tables March 7
summary(fitC_spring_mat)
coef(fitC_spring_mat)$species
posterior_interval(fitC_spring_mat, prob=0.80) # not like this...
# following Issue # 16 to sum and average posteriors of one model

draws <- as.matrix(fitC_spring_mat)
all_spp_garden <- c("Alnus rubra A",
                "Betula papyrifera K",
                "Betula papyrifera L",
                "Betula papyrifera M","Fagus sylvatica R*","Fagus sylvatica T*",
                "Fraxinus excelsior Q*","Picea abies S*","Picea engelmannii B",
                "Picea mariana I","Picea sitchensis D","Pinus albicaulis C","Pinus ponderosa J",
                "Populus balsamifera F","Populus trichocarpa D","Pseudotsuga menziesii H",
                "Quercus petraea U*","Quercus petraea V*","Tsuga heterophylla E","Tsuga heterophylla G") # put the rest of the species names in ... 

library(stringr)
all_spp_garden <- str_replace_all(all_spp_garden," ","_")

# Get all the species of one type now
all_spp_garden_draws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(all_spp_garden))

for (i in c(1:length(all_spp_garden))){
  all_spp_garden_draws[,i] <- draws[,paste("b[MAT_prov species_garden:", all_spp_garden[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
pe -- you need to take the rowMeans beacause the samples are related and so you keep the rows together
all_spp_garden_post <- rowMeans(all_spp_garden_draws)

quantile(all_spp_garden_post, probs = c(.1, .5, .9))

10%         50%         90% 
-0.28666551 -0.10987028  0.07123643 

# spring or fall DOY ~ lat or mat
fitC_spring_lat <- rstanarm::stan_glmer(spring_event~(lat_prov|species_garden), data = d)
fitC_fall_lat <- rstanarm::stan_glmer(fall_event~(lat_prov|species_garden), adapt_delta = 0.99, iter = 3000, warmup=2000, data = d)  # one divergent transition
fitC_spring_mat <- rstanarm::stan_glmer(spring_event~(MAT_prov|species_garden), data = d)

fitC_fall_mat <- rstanarm::stan_glmer(fall_event~(MAT_prov|species_garden), adapt_delta = 0.99, data = d)
:stan_glmer(fall_event~(MAT_prov|species_garden), iter=4000,warmup=2000, data = d)

# climate overlap
fitC_spring_overlap <- rstanarm::stan_glmer(spring_event~(percentage|species_garden), data = d)
fitC_fall_overlap <- rstanarm::stan_glmer(fall_event~(percentage|species_garden), data = d)

# still need to run the below
# lat diffo
fitC_spring_lat_diffo <- rstanarm::stan_glmer(spring_event~(distance_from_garden|species_garden), data = d)
fitC_fall_lat_diffo <- rstanarm::stan_glmer(fall_event~(distance_from_garden|species_garden), data = d)
fitC_spring_earth_distance <- rstanarm::stan_glmer(spring_event~(earth_distance_from_garden|species_garden), data = d)
fitC_fall_earth_distance <- rstanarm::stan_glmer(fall_event~(earth_distance_from_garden|species_garden), data = d)

# mat diffo
fitC_spring_mat_diffo <- rstanarm::stan_glmer(spring_event~(MAT_diffo|species_garden), data = d)
fitC_fall_mat_diffo <- rstanarm::stan_glmer(fall_event~(MAT_diffo|species_garden), data = d)

# gdd
fitD_spring_lat <- rstanarm::stan_glmer(gdd_10yr_mean~(lat_prov|species_garden), data = d)
fitD_spring_mat <- rstanarm::stan_glmer(gdd_10yr_mean~(MAT_prov|species_garden), data = d)



#save fit Dec 13, 2022
saveRDS(fitC_spring_lat, file = "output/model_fit/fitC_spring_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_fall_lat, file = "output/model_fit/fitC_fall_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_spring_mat, file = "output/model_fit/fitC_spring_mat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_fall_mat, file = "output/model_fit/fitC_fall_mat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_spring_overlap, file = "output/model_fit/fitC_spring_overlap.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_fall_overlap, file = "output/model_fit/fitC_fall_overlap.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_spring_lat_diffo, file = "output/model_fit/fitC_spring_lat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_fall_lat_diffo, file = "output/model_fit/fitC_fall_lat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_spring_earth_distance, file = "output/model_fit/fitC_spring_earth_distance.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_fall_earth_distance, file = "output/model_fit/fitC_fall_earth_distance.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


saveRDS(fitC_spring_mat_diffo, file = "output/model_fit/fitC_spring_mat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitC_fall_mat_diffo, file = "output/model_fit/fitC_fall_mat_diffo.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(fitD_spring_lat, file = "output/model_fit/fitD_spring_lat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(fitD_spring_mat, file = "output/model_fit/fitD_spring_mat.RDS", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


# restore
fitC_spring_lat <- readRDS("output/model_fit/fitC_spring_lat.RDS")
fitC_spring_mat <- readRDS("output/model_fit/fitC_spring_mat.RDS")
fitC_spring_overlap <- readRDS("output/model_fit/fitC_spring_overlap.RDS")
fitC_fall_lat <- readRDS("output/model_fit/fitC_fall_lat.RDS")
fitC_fall_mat <- readRDS("output/model_fit/fitC_fall_mat.RDS")
fitC_fall_overlap <- readRDS("output/model_fit/fitC_fall_overlap.RDS")
fitC_spring_lat_diffo <- readRDS("output/model_fit/fitC_spring_lat_diffo.RDS")
fitC_fall_lat_diffo <- readRDS("output/model_fit/fitC_fall_lat_diffo.RDS")
fitC_spring_earth_distance <- readRDS("output/model_fit/fitC_spring_earth_distance.RDS")
fitC_fall_earth_distance <- readRDS("output/model_fit/fitC_fall_earth_distance.RDS")
fitC_spring_mat_diffo <- readRDS("output/model_fit/fitC_spring_mat_diffo.RDS")
fitC_fall_mat_diffo <- readRDS("output/model_fit/fitC_fall_mat_diffo.RDS")

fitD_spring_mat <- readRDS("output/model_fit/fitD_spring_mat.RDS")
fitD_spring_lat<- readRDS("output/model_fit/fitD_spring_lat.RDS")



# extract slope and intercept
# 1. spring_lat
coef(fitC_spring_lat)$species
fitC_spring_lat_slope_intercept_df <- as.data.frame(coef(fitC_spring_lat)$species)

#rename columns
fitC_spring_lat_slope_intercept_df <- rename(fitC_spring_lat_slope_intercept_df, 
                                             fitC_spring_lat_slope=lat_prov, 
                                             fitC_spring_lat_intercept="(Intercept)")
fitC_spring_lat_slope_intercept_df$species <- row.names(fitC_spring_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitC_spring_lat_slope_intercept_df)

# 2. spring_lat_diffo
coef(fitC_spring_lat_diffo)$Species
fitC_spring_lat_diffo_slope_intercept_df <- as.data.frame(coef(fitC_spring_lat_diffo)$Species)

#rename columns
fitC_spring_lat_diffo_slope_intercept_df <- rename(fitC_spring_lat_diffo_slope_intercept_df, 
                                                   fitC_spring_lat_diffo_slope=distance_from_garden, 
                                                   fitC_spring_lat_diffo_intercept="(Intercept)")
fitC_spring_lat_diffo_slope_intercept_df$Species <- row.names(fitC_spring_lat_diffo_slope_intercept_df)
# try joining
d$Species<- d$species
d <- full_join(d,fitC_spring_lat_diffo_slope_intercept_df)


# 3. fall_lat_diffo
coef(fitC_fall_lat_diffo)$Species
fitC_fall_lat_diffo_slope_intercept_df <- as.data.frame(coef(fitC_fall_lat_diffo)$Species)

#rename columns
fitC_fall_lat_diffo_slope_intercept_df <- rename(fitC_fall_lat_diffo_slope_intercept_df, 
                                                 fitC_fall_lat_diffo_slope=distance_from_garden, 
                                                 fitC_fall_lat_diffo_intercept="(Intercept)")
fitC_fall_lat_diffo_slope_intercept_df$Species <- row.names(fitC_fall_lat_diffo_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_lat_diffo_slope_intercept_df)



# 4. spring_gdd_lat
coef(fitD_spring_lat)$species
fitD_spring_lat_slope_intercept_df <- as.data.frame(coef(fitD_spring_lat)$species)

#rename columns
fitD_spring_lat_slope_intercept_df <- rename(fitD_spring_lat_slope_intercept_df, 
                                             fitD_spring_lat_slope=lat_prov, 
                                             fitD_spring_lat_intercept="(Intercept)")
fitD_spring_lat_slope_intercept_df$species <- row.names(fitD_spring_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitD_spring_lat_slope_intercept_df)


# 5. spring_gdd_mat
coef(fitD_spring_mat)$species
fitD_spring_mat_slope_intercept_df <- as.data.frame(coef(fitD_spring_mat)$species)

#rename columns
fitD_spring_mat_slope_intercept_df <- rename(fitD_spring_mat_slope_intercept_df, 
                                             fitD_spring_mat_slope=MAT_prov, 
                                             fitD_spring_mat_intercept="(Intercept)")
fitD_spring_mat_slope_intercept_df$species <- row.names(fitD_spring_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitD_spring_mat_slope_intercept_df)


# 6. spring_earth_distance
coef(fitC_spring_earth_distance)$Species
fitC_spring_earth_distance_slope_intercept_df <- as.data.frame(coef(fitC_spring_earth_distance)$Species)

#rename columns
fitC_spring_earth_distance_slope_intercept_df <- rename(fitC_spring_earth_distance_slope_intercept_df, 
                                                        fitC_spring_earth_distance_slope=earth_distance_from_garden, 
                                                        fitC_spring_earth_distance_intercept="(Intercept)")
fitC_spring_earth_distance_slope_intercept_df$Species <- row.names(fitC_spring_earth_distance_slope_intercept_df)

d<- 
  # try joining
  d <- full_join(d,fitC_spring_earth_distance_slope_intercept_df)


# 7. fall_earth_distance 
coef(fitC_fall_earth_distance)$Species
fitC_fall_earth_distance_slope_intercept_df <- as.data.frame(coef(fitC_fall_earth_distance)$Species)

#rename columns
fitC_fall_earth_distance_slope_intercept_df <- rename(fitC_fall_earth_distance_slope_intercept_df, 
                                                      fitC_fall_earth_distance_slope=earth_distance_from_garden, 
                                                      fitC_fall_earth_distance_intercept="(Intercept)")
fitC_fall_earth_distance_slope_intercept_df$species <- row.names(fitC_fall_earth_distance_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_earth_distance_slope_intercept_df)

# 8. spring_mat_diffo
coef(fitC_spring_mat_diffo)$species
fitC_spring_mat_diffo_slope_intercept_df <- as.data.frame(coef(fitC_spring_mat_diffo)$species)

#rename columns
fitC_spring_mat_diffo_slope_intercept_df <- rename(fitC_spring_mat_diffo_slope_intercept_df, 
                                                   fitC_spring_mat_diffo_slope=MAT_diffo, 
                                                   fitC_spring_mat_diffo_intercept="(Intercept)")
fitC_spring_mat_diffo_slope_intercept_df$Species <- row.names(fitC_spring_mat_diffo_slope_intercept_df)
# try joining
d <- full_join(d,fitC_spring_mat_diffo_slope_intercept_df)


# 9. fall_mat_diffo
coef(fitC_fall_mat_diffo)$species
fitC_fall_mat_diffo_slope_intercept_df <- as.data.frame(coef(fitC_fall_mat_diffo)$species)

#rename columns
fitC_fall_mat_diffo_slope_intercept_df <- rename(fitC_fall_mat_diffo_slope_intercept_df, 
                                                 fitC_fall_mat_diffo_slope=MAT_diffo, 
                                                 fitC_fall_mat_diffo_intercept="(Intercept)")
fitC_fall_mat_diffo_slope_intercept_df$Species <- row.names(fitC_fall_mat_diffo_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_mat_diffo_slope_intercept_df)


# 10. spring_mat
coef(fitC_spring_mat)$species
fitC_spring_mat_slope_intercept_df <- as.data.frame(coef(fitC_spring_mat)$species)

#rename columns
fitC_spring_mat_slope_intercept_df <- rename(fitC_spring_mat_slope_intercept_df, 
                                             fitC_spring_mat_slope=MAT_prov, 
                                             fitC_spring_mat_intercept="(Intercept)")
fitC_spring_mat_slope_intercept_df$species <- row.names(fitC_spring_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitC_spring_mat_slope_intercept_df)


# 11. spring_overlap
coef(fitC_spring_overlap)$species
fitC_spring_overlap_slope_intercept_df <- as.data.frame(coef(fitC_spring_overlap)$species)

#rename columns
fitC_spring_overlap_slope_intercept_df <- rename(fitC_spring_overlap_slope_intercept_df, 
                                                 fitC_spring_overlap_slope=percentage, 
                                                 fitC_spring_overlap_intercept="(Intercept)")
fitC_spring_overlap_slope_intercept_df$species <- row.names(fitC_spring_overlap_slope_intercept_df)
# try joining
d <- full_join(d,fitC_spring_overlap_slope_intercept_df)


# 12. fall_lat
coef(fitC_fall_lat)$species
fitC_fall_lat_slope_intercept_df <- as.data.frame(coef(fitC_fall_lat)$species)

#rename columns
fitC_fall_lat_slope_intercept_df <- rename(fitC_fall_lat_slope_intercept_df, 
                                           fitC_fall_lat_slope=lat_prov, 
                                           fitC_fall_lat_intercept="(Intercept)")
fitC_fall_lat_slope_intercept_df$species <- row.names(fitC_fall_lat_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_lat_slope_intercept_df)

# 13. fall_mat
coef(fitC_fall_mat)$species
fitC_fall_mat_slope_intercept_df <- as.data.frame(coef(fitC_fall_mat)$species)

#rename columns
fitC_fall_mat_slope_intercept_df <- rename(fitC_fall_mat_slope_intercept_df, 
                                           fitC_fall_mat_slope=MAT_prov, 
                                           fitC_fall_mat_intercept="(Intercept)")
fitC_fall_mat_slope_intercept_df$species <- row.names(fitC_fall_mat_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_mat_slope_intercept_df)



# 14. fall_overlap
coef(fitC_fall_overlap)$species
fitC_fall_overlap_slope_intercept_df <- as.data.frame(coef(fitC_fall_overlap)$species)

#rename columns
fitC_fall_overlap_slope_intercept_df <- rename(fitC_fall_overlap_slope_intercept_df, 
                                               fitC_fall_overlap_slope=percentage, 
                                               fitC_fall_overlap_intercept="(Intercept)")
fitC_fall_overlap_slope_intercept_df$species <- row.names(fitC_fall_overlap_slope_intercept_df)
# try joining
d <- full_join(d,fitC_fall_overlap_slope_intercept_df)




# plotting
# d <- rename(d, species=Species)
d <- rename(d, Species=species)
d <- rename(d, Species=species, "Provenance_continent" = prov_continent)
# color by garden, symbol by species


# change color?shape?fitAtoC, 
# depending on newly added spp... need to add more shape
# hmmm since the model fits are spp/garden.... i might need to plot that way too
# so shape would equal species_garden....or color that way
# need to edit the green part

# after I finalized how one plot looks, I can produce other plots as well. Maybe even group them (panel display)

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


