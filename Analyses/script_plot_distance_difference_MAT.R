# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

install.packages("ggplot2")
install.packages("viridis")
install.packages("svglite")
install.packages("dplyr")
install.packages("bayesplot")
install.packages("rstanarm")
install.packages("Rcpp")

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(viridis)
library(svglite)


d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included.csv", header = TRUE)  #680
d$fall_event <- as.numeric(d$fall_event)
d$spring_event <- as.numeric(d$spring_event)



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



# need to calculate MAT_difference

# plotting
# d <- rename(d, species=Species)
d <- rename(d, Species=species)
d <- rename(d, Species=species, "Provenance_continent" =prov_continent)
# color by garden, symbol by species

# lat~spring_distance
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
  theme(axis.text.x = element_text(size = 40))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 40))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Latitude")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()

ggsave(file="lat~spring_lines_added_fitA.svg", width=12, height=12)														# saving svg.
