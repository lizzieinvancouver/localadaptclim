# percentage ~ distance

library(dplyr)
library(tidyr)
library(Rcpp)
library(rstanarm)
library(bayesplot)
library(ggplot2)



#d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated.csv", header = TRUE)
#d$percentage_difference <- as.numeric(d$percentage_difference)

#d<- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_May5.csv",header = TRUE)
d<- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_Sept16.csv",header = TRUE)


# write first so i don't lose progress
name<-"input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_Sept16.csv"
write.csv(d,name, row.names = FALSE)


test <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included_slope_intercept_Sept16.csv", header = T)

d$MAT_diffo <- abs(d$MAT_prov - d$MAT_garden)


# plotting
# d <- rename(d, species=Species)
d <- dplyr::rename(d, Species=species)
d <- dplyr::rename(d, "Provenance_continent" =prov_continent)
# color by garden, symbol by species

# hmmmm need to change slope and intercept first

# lat_diffo~spring
png(filename="lat_diffo~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(distance_from_garden, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_lat_diffo_slope,intercept=fitA_spring_lat_diffo_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance latitude difference (decimal degrees)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Latitude Difference")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()

ggsave(file="lat_diffo~spring_lines_added_fitA.svg", width=12, height=12)														# saving svg.


# lat_diffo~fall
png(filename="lat_diffo~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(distance_from_garden, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_lat_diffo_slope,intercept=fitA_fall_lat_diffo_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance latitude difference (decimal degrees)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ Provenance Latitude Difference")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()


# earth_distance~spring 
png(filename="earth_distance~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(earth_distance_from_garden, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_earth_distance_slope,intercept=fitA_spring_earth_distance_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n  Spherical distance (meters)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Earth Surface Distance")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()




# earth_distance~fall 
png(filename="earth_distance~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(earth_distance_from_garden, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_earth_distance_slope,intercept=fitA_fall_earth_distance_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n  Spherical distance (meters)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ Earth Surface Distance")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()


# lat_diffo~spring
png(filename="lat_diffo~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(distance_from_garden, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_lat_diffo_slope,intercept=fitA_spring_lat_diffo_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance latitude difference (decimal degrees)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Latitude Difference")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()


# mat_diffo~spring
png(filename="mat_diffo~spring_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_diffo, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_spring_mat_diffo_slope,intercept=fitA_spring_mat_diffo_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance MAT difference (\u00B0C)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ MAT Difference")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()



# mat_diffo~fall
png(filename="mat_diffo~fall_lines_added_fitA.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_diffo, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitA_fall_mat_diffo_slope,intercept=fitA_fall_mat_diffo_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance MAT difference (\u00B0C)")+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Fall Event Day of Year (DOY) ~ MAT Difference")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()




d$species_garden <- paste(d$species, d$garden_identifier, sep = " ")



#fitA
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


#fitC

# lat~spring_spp_garden
png(filename="lat~spring_spp_garden_lines_added_fitC.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitC_spring_lat_slope,intercept=fitC_spring_lat_intercept,linetype = Species, color = garden_identifier))+
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
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Latitude  **spp&garden")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()

# lat~fall_spp_garden
png(filename="lat~fall_spp_garden_lines_added_fitC.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitC_fall_lat_slope,intercept=fitC_fall_lat_intercept,linetype = Species, color = garden_identifier))+
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
  labs(title = "Fall Event Day of Year (DOY) ~ Provenance Latitude  **spp&garden")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()




# histogram
# MAT_continent
png(filename="histogram_MAT_continent.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(x=MAT_prov, color=prov_continent)) + 
  geom_histogram(fill="white")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Provenance continent")) + 
  ylab("Count\n") +                             		
  xlab("\n Provenance MAT (\u00B0C)")
  
dev.off()


# LAT_continent
png(filename="histogram_LAT_continent.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(x=lat_prov, color=prov_continent)) + 
  geom_histogram(fill="white")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Provenance continent")) + 
  ylab("Count\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")

dev.off()


# MAT_species_type
png(filename="histogram_MAT_species_type.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(x=MAT_prov, color=species_type)) + 
  geom_histogram(fill="white")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Species type")) + 
  ylab("Count\n") +                             		
  xlab("\n Provenance MAT (\u00B0C)")

dev.off()


# LAT_species_type
png(filename="histogram_LAT_species_type.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(x=lat_prov, color=species_type)) + 
  geom_histogram(fill="white")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 25))+             # x-axis text size
  theme(axis.text.y = element_text(size = 40))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 30))    +        # x-axis title
  theme(axis.title.y = element_text(size = 40)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Species type")) + 
  ylab("Count\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")

dev.off()



