# script_plot_for_manuscript
# Alina Zeng
# April 25
# alinazengziyun@yahoo.com



# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(svglite)


#import

# earth distance
d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included.csv", header = TRUE)
d$spring_event_difference <- as.numeric(d$spring_event_difference)
d$fall_event_difference <- as.numeric(d$fall_event_difference)
d <- rename(d, Species=species, "Provenance_continent" =prov_continent)



# plot 1 (4 panels or 8) ----
# panel (1) Spring event DOY versus MAT, 
# panel (2) Garden-corrected spring event day versus MAT
# the same two (panel 3-4) but using latitude. 
# For now, different symbols for species and make a different color for each garden 
# (or vice versa ... likely easiest to do in base R graphics); if 
# possible make all the NA gardens similar symbols and all the European ones similar 
# symbols (or you could just plot numbers instead of symbols? Or! It could be that 
#        the best option is to plot Europe and NA on separate panels, but make 
#         sure the xlim and ylim are the same). perhaps 
# use viridis color package instead of colorbrewer.


# color by garden, symbol by species
# plot1 part 1 ----
  png(filename="lat~spring.png", 
      type="cairo", 
      units="in", 
      width=14, 
      height=10, 
      res=300)

  ggplot(d, aes(lat_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
    ylab("Spring event day of year\n") +                             		
    xlab("\n Provenance latitude (decimal degrees)")+
    theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
    theme(axis.title.x = element_text(size = 20))    +        # x-axis title
    theme(axis.title.y = element_text(size = 20)) +           # y-axis title
    theme(legend.title = element_text(size = 15))       +     # Legend title
    theme(legend.text = element_text(size = 10))      +       # Legend text
    guides(col=guide_legend("Garden ID")) + 
    scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~spring.svg", width=12, height=12)														# saving svg.


# plot1 part 2 ----
png(filename="mat~spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance mean annual temperature (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="mat~spring.svg", width=12, height=12)		


# plot1 part 3 ----
png(filename="lat~spring_diffo.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Difference of spring event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="lat~spring_diffo.svg", width=12, height=12)		


# plot1 part 3 ----
png(filename="mat~spring_diffo.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Difference of spring event day of year\n") +                             		
  xlab("\n Provenance mean annual temperature (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="mat~spring_diffo.svg", width=12, height=12)		





# Plot 2 same at plot1 but for fall ----
# color by garden, symbol by species
# plot2 part 1 ----
png(filename="lat~fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~fall.svg", width=12, height=12)														# saving svg.


# plot2 part 2 ----
png(filename="mat~fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n Provenance mean annual temperature (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="mat~fall.svg", width=12, height=12)		



# plot 3 part A elevation~doys -> latitudinal studies

png(filename="elev_latitudestudies~spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(elev_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance elevation (m)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="elev_latitudestudies~spring.svg", width=12, height=12)														# saving svg.


# plot3 part 1 ----
png(filename="elev_latitudestudies~spring_diffo.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(elev_prov, spring_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Difference of spring event day of year\n") +                             		
  xlab("\n Provenance elevation (m)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="elev_latitudestudies~spring_diffo.svg", width=12, height=12)		


#  for fall ----
# color by garden, symbol by species

png(filename="elev_latitudestudies~fall.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(elev_prov, fall_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Fall event day of year\n") +                             		
  xlab("\n PProvenance elevation (m)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="elev_latitudestudies~fall.svg", width=12, height=12)														# saving svg.



# Finally ----
# Make some correlation plots of predictors (multi-panel figures: different 
# symbols for NA versus Europe (prov_continent), and color code by garden ID): 
# MAT versus latitude, 
# % overlap versus SD, 
# % overlap versus MAT, 
# SD of overlap versus MAT, 
# % overlap versus lat, SD of overlap versus lat, MAT versus elevation, 
# lat versus elevation.


# MAT versus latitude
png(filename="lat~mat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, MAT_prov, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  ylab("Provenance mean annual temperature (\u00B0C)\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Latitude VS. Mean annual temperature")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~mat.svg", width=12, height=12)														# saving svg.


# % overlap versus SD
png(filename="percentage~sd.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(sd, percentage, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Standard deviation\n") +                             		
  ylab("\n Climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(title = "Standard deviation VS. Climate overlap percentage")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="percentage~sd.svg", width=12, height=12)														# saving svg.




# % overlap versus MAT

png(filename="percentage~mat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, percentage, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Provenance mean annual temperature (\u00B0C)\n") +                             		
  ylab("\n Climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Mean annual temperature VS. Climate overlap percentage")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="percentage~mat.svg", width=12, height=12)														# saving svg.


# SD of overlap versus MAT
png(filename="sd~mat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, sd, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Provenance mean annual temperature (\u00B0C)\n") +                             		
  ylab("\n Standard deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Mean annual temperature VS. Standard deviation")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="sd~mat.svg", width=12, height=12)														# saving svg.


# % overlap versus lat, 

png(filename="percentage~lat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, percentage, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Provenance latitude (decimal degrees)\n") +                             		
  ylab("\n Climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Latitude VS. Climate overlap percentage")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="percentage~lat.svg", width=12, height=12)														# saving svg.


# SD of overlap versus lat

png(filename="sd~lat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, sd, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Provenance latitude (decimal degrees)\n") +                             		
  ylab("\n Standard deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Latitude VS. Standard deviation")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="sd~lat.svg", width=12, height=12)														# saving svg.

# MAT versus elevation
png(filename="elev_latitudestudies~mat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(elev_prov, MAT_prov, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Provenance elevation (m))\n") +                             		
  ylab("\n Provenance mean annual temperature (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Elevation VS. Mean annual temperature")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="elev_latitudestudies~mat.svg", width=12, height=12)														# saving svg.




# lat versus elevation

png(filename="elev_latitudestudies~lat.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(elev_prov, lat_prov, colour = garden_identifier, shape = Provenance_continent)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(15,2))+
  theme_classic()+
  xlab("Provenance elevation (m))\n") +                             		
  ylab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  labs(	title = "Elevation VS. Latitude")+
  theme(plot.title = element_text(size = 22))     +         # Plot title size
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="elev_latitudestudies~lat.svg", width=12, height=12)														# saving svg.





# plot1 part ... dk what number ----
png(filename="lat~fall_diffo.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, fall_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Difference of fall event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="lat~fall_diffo.svg", width=12, height=12)		


# plot1 part.. dk what number  ----
png(filename="mat~fall_diffo.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(MAT_prov, fall_event_difference, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  theme_classic()+
  ylab("Difference of fall event day of year\n") +                             		
  xlab("\n Provenance mean annual temperature (\u00B0C)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  guides(col=guide_legend("Garden ID")) + 
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful

dev.off()
ggsave(file="mat~fall_diffo.svg", width=12, height=12)		











# values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,10,18) # this so that European and North American species look distinctive
# "Alnus rubra"   "Betula papyrifera" "Fagus sylvatica" "Fraxinus excelsior" "Picea abies" 
# "Picea engelmannii" "Picea mariana" "Picea sitchensis" "Pinus albicaulis" "Pinus ponderosa"                       
# "Populus balsamifera" "Populus trichocarpa"  "Pseudotsuga menziesii"              
# "Quercus petraea"  "Tsuga heterophylla"
