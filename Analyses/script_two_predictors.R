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

# fit model
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

# Select which you will subset (x1 or x2). For our example, we pick x2.
# Label -- say -- the top 25% of x2 as 'high' and low 25% as 'low' (make a new column or such).
# Take the mean of low and high, we call them x2high and x2low.

# subseting sd 
top25sd <- head(sort(d$sd,decreasing=TRUE),n=170) # 680*0.25 = 170
bot25sd <- head(sort(d$sd,decreasing=FALSE),n=170)

sd_high <- mean(head(sort(d$sd,decreasing=TRUE),n=170)) # 6.472353
sd_low <- mean(head(sort(d$sd,decreasing=FALSE),n=170))  # 3.551351

# Now you have do the math of your equation: ypredicted = intercept + b1x1 + b2x2 + b3(x1*x2) 
# make a set of x1 data from min(x1) to max(x1) with small steps -- 
# so if my min was 5 and max was 25 I might do x1seq <- seq(5, 25, length.out=300)

# min(d$percentage)= 11.17418 
# max(d$percentage)= 100
percentage_seq <- seq(11.17418, 100, length.out = 300)

# Now you can calculate what your equation PREDICTS for the sequence of x1 and for high and low x2 .... 
# (you need a loop or apply command).
# ypredicted = intercept + b1x1 + b2x2 + b3(x1*x2) 
# ypredicted = intercept + b1*percentage + b2*sd + b3(percentage*sd) 

# b1 -> slope of percentage
# b2 -> slope of sd
# b3 -> slope of percentage:sd 

intercept <- mean(fitB_spring_percentage_sd_intercept_df$`(Intercept)`) # 111.5683
percentage_slope <- mean(fitB_spring_percentage_sd_intercept_df$percentage) # 0.1024583
sd_slope <- mean(fitB_spring_percentage_sd_intercept_df$sd) # -0.006339267
percentage_sd_slope <- mean(fitB_spring_percentage_sd_intercept_df$`percentage:sd`) # -0.01322158

# low sd -> bottom 25% first
# need to make an empty df
ypredicted_sd_low <- data.frame(y = numeric())
for (i in c(1:length(percentage_seq))){
    y <- intercept + percentage_slope*percentage_seq[[i]]+ sd_slope*sd_low
    + percentage_sd_slope*percentage_seq[[i]]*sd_low
ypredicted_sd_low_add<- data.frame(y=y)
ypredicted_sd_low <- rbind(ypredicted_sd_low, ypredicted_sd_low_add)
}

# high sd -> top 25% 
ypredicted_sd_high <- data.frame(y = numeric())
for (i in c(1:length(percentage_seq))){
  y <- intercept + percentage_slope*percentage_seq[[i]]+ sd_slope*sd_high
  + percentage_sd_slope*percentage_seq[[i]]*sd_high
  ypredicted_sd_high_add<- data.frame(y=y)
  ypredicted_sd_high <- rbind(ypredicted_sd_high, ypredicted_sd_high_add)
}

# merge ypredicted_sd_high and ypredicted_sd_low and the percentage sequence
ypredicted_sd_high$percentage <- percentage_seq
ypredicted_sd_low$percentage <- percentage_seq
ypredicted_sd_high$status <- "sd_high"
ypredicted_sd_low$status <- "sd_low"

ypredicted_sd <- rbind(ypredicted_sd_high, ypredicted_sd_low)
# so you can plot ypredicted ~ x ... with small enough divisions of x1, it will plot as a line).

png(filename="spring_sd_highlow_fitB.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(ypredicted_sd, aes(percentage, y, colour = status)) +
  geom_line() +
    ylab("Climate overlap standard deviation")+
    xlab("Climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))           # Legend text
dev.off()

# subseting percentage
top25percentage <- head(sort(d$percentage,decreasing=TRUE),n=170) # 680*0.25 = 170
bot25percentage <- head(sort(d$percentage,decreasing=FALSE),n=170)
percentage_high <- mean(head(sort(d$percentage,decreasing=TRUE),n=170)) # 87.22572
percentage_low <- mean(head(sort(d$percentage,decreasing=FALSE),n=170)) # 30.96112

# Now you have do the math of your equation: ypredicted = intercept + b1x1 + b2x2 + b3(x1*x2) 

# make a set of x1 data from min(x1) to max(x1) with small steps -- 
# so if my min was 5 and max was 25 I might do x1seq <- seq(5, 25, length.out=300)

# min(d$sd)= 2.455242
# max(d$sd)= 9.733365
sd_seq <- seq(2.455242, 9.733365, length.out = 300)

# ypredicted = intercept + b1x1 + b2x2 + b3(x1*x2) 
# ypredicted = intercept + b1*percentage + b2*sd + b3(percentage*sd) 

# b1 -> slope of percentage
# b2 -> slope of sd
# b3 -> slope of percentage:sd ??

intercept <- mean(fitB_spring_percentage_sd_intercept_df$`(Intercept)`) # 111.5683
percentage_slope <- mean(fitB_spring_percentage_sd_intercept_df$percentage) # 0.1024583
sd_slope <- mean(fitB_spring_percentage_sd_intercept_df$sd) # -0.006339267
percentage_sd_slope <- mean(fitB_spring_percentage_sd_intercept_df$`percentage:sd`) # -0.01322158

# low percentage -> bottom 25% first
# need to make an empty df
ypredicted_percentage_low <- data.frame(y = numeric())
for (i in c(1:length(sd_seq))){
  y <- intercept + percentage_slope*percentage_low+ sd_slope*sd_seq[[i]]
  + percentage_sd_slope*percentage_low*sd_seq[[i]]
  ypredicted_percentage_low_add<- data.frame(y=y)
  ypredicted_percentage_low <- rbind(ypredicted_percentage_low, ypredicted_percentage_low_add)
}

# high percentage -> top 25% 
ypredicted_percentage_high <- data.frame(y = numeric())
for (i in c(1:length(sd_seq))){
  y <- intercept + percentage_slope*percentage_high+ sd_slope*sd_seq[[i]]
  + percentage_sd_slope*percentage_high*sd_seq[[i]]
  ypredicted_percentage_high_add<- data.frame(y=y)
  ypredicted_percentage_high <- rbind(ypredicted_percentage_high, ypredicted_percentage_high_add)
}

# merge ypredicted_sd_high and ypredicted_sd_low and the percentage sequence
ypredicted_percentage_high$sd <- sd_seq
ypredicted_percentage_low$sd <- sd_seq
ypredicted_percentage_high$status <- "percentage_high"
ypredicted_percentage_low$status <- "percentage_low"

ypredicted_percentage <- rbind(ypredicted_percentage_high, ypredicted_percentage_low)
# so you can plot ypredicted ~ x ... with small enough divisions of x1, it will plot as a line).

png(filename="spring_percentage_highlow_fitB.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(ypredicted_percentage, aes(sd, y, colour = status)) +
  geom_line() +
  xlab("Climate overlap standard deviation")+
  ylab("Climate overlap percentage")+
  theme(axis.text.x = element_text(size = 15))+             # x-axis text size
  theme(axis.text.y = element_text(size = 15))   +          # y-axis text size
  theme(axis.title.x = element_text(size = 20))    +        # x-axis title
  theme(axis.title.y = element_text(size = 20)) +           # y-axis title
  theme(legend.title = element_text(size = 15))       +     # Legend title
  theme(legend.text = element_text(size = 10))           # Legend text
dev.off()


