# script_2_predictors


# 13/May/2022
# alinazengziyun@yahoo.com

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(Rcpp)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(viridis)
library(svglite)

library(sjPlot)
library(sjmisc)
library(ggplot2)

fitB_spring_percentage_sd <- stan_glmer(spring_event~((percentage*sd)|species), data = d)
fitB_fall_percentage_sd <- stan_glmer(fall_event~((percentage*sd)|species), data = d)

d <- read.csv("input/percentage_overlap_doy_difference_earth_calculated_garden_identifier_adjusted_fall_diffo_included.csv", header = TRUE)  #680
d$fall_event <- as.numeric(d$fall_event)
d$spring_event <- as.numeric(d$spring_event)

# restore
fitB_spring_percentage_sd<- readRDS("output/model_fit/fitB_spring_percentage_sd.RDS")
fitB_fall_percentage_sd <- readRDS("output/model_fit/fitB_fall_percentage_sd.RDS")

fitB_spring_percentage_sd_intercept_df <- as.data.frame(coef(fitB_spring_percentage_sd)$species)


# Steps for plotting your own interactions:
  
# First, there is no easy way to know if you should subset percentage or sd to high 
# and low values. I generally just try both. I will call these just x1 and x2 below.

# x1 = percentage
# x2 = sd

# subseting sd first
top25sd <- head(sort(d$sd,decreasing=TRUE),n=170) # 680*0.25 = 170
bot25sd <- head(sort(d$sd,decreasing=FALSE),n=170) # 680*0.25 = 170
  
mean_top25sd <- mean(head(sort(d$sd,decreasing=TRUE),n=170)) # 680*0.25 = 170
mean_bot25sd <- mean(head(sort(d$sd,decreasing=FALSE),n=170))

sd_high <- mean_top25sd 
sd_low <- mean_bot25sd 

ypredicted = intercept + b1x1 + b2x2 + b3(x1*x2) 
ypredicted = fitB_spring_percentage_sd_intercept_df$`(Intercept)` + b1*fitB_spring_percentage_sd_intercept_df$percentage 
+ b2*fitB_spring_percentage_sd_intercept_df$sd + b3*(spring_percentage_sd_intercept_df$percentage)*(spring_percentage_sd_intercept_df$sd) 

# min(d$percentage)= 11.17418
# max(d$percentage)= 100

percentage_seq <- seq(11.17418, 100, length.out = 300)

# Select which you will subset (x1 or x2). For our example, we pick x2.
# Label -- say -- the top 25% of x2 as 'high' and low 25% as 'low' (make a new column or such).
# Take the mean of low and high, we call them x2high and x2low.

# Now you have do the math of your equation: ypredicted = intercept + b1x1 + b2x2 + b3(x1*x2) ... 
# make a set of x1 data from min(x1) to max(x1) with small steps -- 
# so if my min was 5 and max was 25 I might do x1seq <- seq(5, 25, length.out=300)
# Now you can calculate what your equation PREDICTS for the sequence of x1 and for high and low x2 .... 
# (you need a loop or apply command). 

# For the first value of x1seq at low x2 it would be: 
# ypredicted = intercept + b1x1[1] + b2x2[x2low] + b3(x1[1]*x2[x2low]) ... 
# you have to extract from your model the intercept, b1, b2, b3. 
# And then you need to plot your ypred for high and low x2 across the data 
# (ypredicted = ypred = predicted y, 
# so you can plot ypredicted ~ x ... with small enough divisions of x1, it will plot as a line).















plot_model(fitB_spring_percentage_sd, type = "int")

plot_model(fitB_spring_percentage_sd, type = "pred", terms = c("percentage", "sd"),mdrt.values = "minmax")

plot_model(fitB_fall_percentage_sd, type = "pred", terms = c("percentage", "sd"))

plot_model(fitB_spring_percentage_sd, type = "pred", terms = c("sd","percentage"))

# extract slope and intercept
# spring_percentage_sd
coef(fitB_spring_percentage_sd)$species
fitB_spring_percentage_sd_intercept_df <- as.data.frame(coef(fitB_spring_percentage_sd)$species)

colnames(fitB_spring_percentage_sd_intercept_df)
#rename columns
fitB_spring_percentage_sd_intercept_df <- rename(fitB_spring_percentage_sd_intercept_df, 
                                                 fitB_spring_percentage_sd_slope="percentage:sd", 
                                                 fitB_spring_percentage_sd_intercept="(Intercept)")
fitB_spring_percentage_sd_intercept_df$species <- row.names(fitB_spring_percentage_sd_intercept_df)
# try joining
d <- full_join(d,fitB_spring_percentage_sd_intercept_df)


coef(fitB_fall_percentage_sd)$species
fitB_fall_percentage_sd_intercept_df <- as.data.frame(coef(fitB_fall_percentage_sd)$species)

colnames(fitB_fall_percentage_sd_intercept_df)
#rename columns
fitB_fall_percentage_sd_intercept_df <- rename(fitB_fall_percentage_sd_intercept_df, 
                                               fitB_fall_percentage_sd_slope="percentage:sd", 
                                               fitB_fall_percentage_sd_intercept="(Intercept)")
fitB_fall_percentage_sd_intercept_df$species <- row.names(fitB_fall_percentage_sd_intercept_df)
# try joining
d <- full_join(d,fitB_fall_percentage_sd_intercept_df)


# percentage:sd~spring
png(filename="percentage.sd~spring_lines_added_fitB.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)

ggplot(d, aes(percentage:sd, spring_event, colour = garden_identifier, shape = Species)) +
  geom_point(size = 3.5)+
  geom_abline(data=d,aes(slope=fitB_spring_percentage_sd_slope,intercept=fitB_spring_percentage_sd_intercept,linetype = Species, color = garden_identifier))+
  scale_shape_manual(values = c(0,1,15,16,17,2,3,4,5,6,7,8,9,18,10))+
  scale_linetype_manual(values = c("Alnus rubra" = "solid", "Betula papyrifera" ="dashed", "Fagus sylvatica" = "dotted", 
                                   "Fraxinus excelsior" = "dotdash", "Picea abies" = "longdash", "Picea engelmannii"="twodash", 
                                   "Picea mariana"="1F", "Picea sitchensis"="F1", "Pinus albicaulis" = "4C88C488", 
                                   "Populus balsamifera" = "solid", "Pinus ponderosa" = "12345678","Populus trichocarpa" = "dashed",
                                   "Pseudotsuga menziesii" = "dotdash", "Quercus petraea"="dashed", "Tsuga heterophylla" ="4C88C488"))+
  theme_classic()+
  ylab("Spring event day of year\n") +                             		
  xlab("\n Percentage:Standard Deviation")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))      +       # Legend text
  theme(plot.title = element_text(size = 21))  +            # plot title
  guides(col=guide_legend("Garden ID")) + 
  labs(title = "Spring Event Day of Year (DOY) ~ Climate Overlap Percentage:Standard Deviation")+				
  scale_color_viridis(discrete = TRUE) # I dont think viridis is very helpful
dev.off()
ggsave(file="percentage.sd~spring_lines_added_fitB.svg", width=12, height=12)														# saving svg.


