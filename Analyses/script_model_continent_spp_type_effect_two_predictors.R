# script model for two predictors and in relation to continent and leaf type effects
# May-18, 2022


## Extracting model fits sidebar ...
# Following here: https://mc-stan.org/rstanarm/reference/as.matrix.stanreg.html

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)

d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included.csv", header = TRUE)  #680
d$fall_event <- as.numeric(d$fall_event)
d$spring_event <- as.numeric(d$spring_event)


# restore
fitB_spring_percentage_sd<- readRDS("output/model_fit/fitB_spring_percentage_sd.RDS")


draws <- as.matrix(fitB_spring_percentage_sd)
# the full posterior for Alnus rubra slope:
alnrubslope <- draws[,"b[percentage:sd species:Alnus_rubra]"]
hist(alnrubslope)



# To get the estimate for continent or evergreen/deciduous:
# Here I show an incomplete example for evergreen/deciduous,
# but we especially want to do for continent (group species by continent)
d <- dplyr::group_by(d, prov_continent)
# Add the posteriors of all the species in one group ...
# Example ... many ways to do this, I just created different vectors (I made very incomplete lists!)
colnames(draws)
evergreens <- c("Picea_engelmannii",  "Picea_sitchensis","Pinus_albicaulis" ,
                "Picea_mariana","Pinus_ponderosa" ,
                "Tsuga_heterophylla","Picea_abies",   
                "Pseudotsuga_menziesii") 
deciduous <- c("Alnus_rubra",
               "Betula_papyrifera",
               "Fagus_sylvatica","Populus_trichocarpa","Fraxinus_excelsior" ,    
               "Populus_balsamifera",         
               "Quercus_petraea") 

# Get all the species of one type now
evergreensdraws <- matrix(data=NA,
                          nrow=nrow(draws),
                          ncol=length(evergreens))
for (i in c(1:length(evergreens))){
  evergreensdraws[,i] <- draws[,paste("b[percentage:sd species:", evergreens[i], "]", sep="")]
}

deciduousdraws <- matrix(data=NA,
                         nrow=nrow(draws),
                         ncol=length(deciduous))
for (i in c(1:length(deciduous))){
  deciduousdraws[,i] <- draws[,paste("b[percentage:sd species:", deciduous[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
evergreenspost <- rowMeans(evergreensdraws)
deciduouspost <- rowMeans(deciduousdraws)

# And then you can plot them as we did before
png(filename="leaf_effect_percentage_sd_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
leaftypeplot <- as.matrix(cbind(evergreenspost, deciduouspost))
plot_title <- ggtitle("Leaf Type Effects on Spring DOY in Relation to Climate Overlap Percentage and Standard Deviation")
color_scheme_set("viridis") 
new_labels_leaftype <- c("Angiosperm","Gymnosperm")
mcmc_areas(leaftypeplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 15))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_leaftype))
dev.off()






# for continental effects
northamerican <- c("Alnus_rubra","Picea_engelmannii","Picea_sitchensis","Pinus_albicaulis",     
                   "Populus_trichocarpa","Tsuga_heterophylla", "Populus_balsamifera","Pseudotsuga_menziesii",
                   "Picea_mariana","Pinus_ponderosa","Betula_papyrifera")

european <- c("Fraxinus_excelsior", "Fagus_sylvatica" ,"Picea_abies", "Quercus_petraea")


# Get all the species of one type now
northamericandraws <- matrix(data=NA,
                             nrow=nrow(draws),
                             ncol=length(northamerican))
for (i in c(1:length(northamerican))){
  northamericandraws[,i] <- draws[,paste("b[percentage:sd species:", northamerican[i], "]", sep="")]
}

europeandraws <- matrix(data=NA,
                        nrow=nrow(draws),
                        ncol=length(european))
for (i in c(1:length(european))){
  europeandraws[,i] <- draws[,paste("b[percentage:sd species:", european[i], "]", sep="")]
}

# Now you can manipulate these posteriors with basic math ...  
# We want the mean across all of each type -- you need to take the rowMeans because the samples are related and so you keep the rows together
northamericanpost <- rowMeans(northamericandraws)
europeanpost <- rowMeans(europeandraws)

# And then you can plot them as we did before

png(filename="continent_effect_percentage_sd_spring.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=8, 
    res=300)
continentplot <- as.matrix(cbind(northamericanpost, europeanpost ))
plot_title <- ggtitle("Continental Effects on Spring DOY in Relation to Climate Overlap Percentage and Standard Deviation")
color_scheme_set("viridis") 
new_labels_continent <- c("Europe","North America")
mcmc_areas(continentplot)+ 
  plot_title +
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(plot.title = element_text(size = 15))  +            # plot title
  theme(text=element_text(family="sans"))+
  scale_y_discrete(labels = rev(new_labels_continent))
dev.off()





# extract slope and intercept
# spring_percentage_sd (two predictors)
coef(fitB_spring_percentage_sd)$species
fitB_spring_percentage_sd_slope_intercept_df <- as.data.frame(coef(fitB_spring_percentage_sd)$species)


#rename columns
fitB_spring_percentage_sd_slope_intercept_df <- rename(fitB_spring_percentage_sd_slope_intercept_df, 
                                             "fitB_spring_percentage_sd_percentage_slope"= "percentage",
                                             "fitB_spring_percentage_sd_sd_slope"="sd",
                                             "fitB_spring_percentage_sd_percentage_sd_slope"="percentage:sd",
                                             "fitB_spring_percentage_sd_intercept"="(Intercept)")
fitB_spring_percentage_sd_slope_intercept_df$species <- row.names(fitB_spring_percentage_sd_slope_intercept_df)
# try joining
d <- full_join(d,fitB_spring_percentage_sd_slope_intercept_df)

# plotting
# d <- rename(d, species=Species)
d <- rename(d, Species=species)
d <- rename(d, Species=species, "Provenance_continent" =prov_continent)
# color by garden, symbol by species


# percentage:sd~spring
png(filename="percentage_sd_percentage_lines_added_fitB.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(spring_event,fitB_spring_percentage_sd_percentage_sd_slope, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
 # geom_abline(data=d,aes(slope="fitB_spring_percentage_sd_percentage_slope",intercept=fitB_spring_percentage_sd_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
 # scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                       #            "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                        #           "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                         #          "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                          #         "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Climate Overlap Percentage")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="lat~spring_lines_added_fitA.svg", width=12, height=12)														# saving svg.




# percentage:sd~spring
png(filename="percentage_sd_lines_added_fitB.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(lat_prov, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope="fitB_spring_percentage_sd_percentage_sd_slope",intercept="fitB_spring_percentage_sd_intercept",linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Provenance latitude (decimal degrees)")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Provenance Latitude")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()