# script for stan_glmer models
# 22/2/2022
# alinazengziyun@yahoo.com

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

install.packages("Rcpp")
install.packages("sjPlot")
install.packages("bayesplot")
library(Rcpp)
library(rstanarm)
library(bayesplot)
# library(sjPlot)
# library(sjmisc)

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/localadaptclim")

# Import data ----
d <- read.csv("input/data_plot_Nov13_AllStudiesToDate.csv", header = TRUE)

# length(unique(d$lat_garden)) #20
# length(unique(d$garden_identifier)) #20


#1 ----
fit1_lat <- stan_glmer(spring_event~lat_prov+ + (1|species)+ (1|garden_identifier), data = d)
fit1_mat <- stan_glmer(spring_event~MAT_prov+ + (1|species)+ (1|garden_identifier), data = d)

#2 ----
fit2_lat <- stan_glmer(spring_event~lat_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)
fit2_mat <- stan_glmer(spring_event~MAT_prov*prov_continent + (1|species)+ (1|garden_identifier), data = d)

#3 ---- 
fit3_lat <- stan_glmer(spring_event~(lat_prov|species)+(1|garden_identifier), data = d)
fit3_mat <- stan_glmer(spring_event~(MAT_prov|species)+(1|garden_identifier), data = d)

#4 ----
fit4_lat <- stan_glmer(spring_event~lat_prov + lat_garden + (1|species), data = d)
fit4_mat <- stan_glmer(spring_event~MAT_prov + MAT_garden + (1|species), data = d)