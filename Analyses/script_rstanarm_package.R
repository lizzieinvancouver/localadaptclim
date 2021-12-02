# How to Use the rstanarm Package
# December 2, 2021
# alina.zeng@ubc.ca


install.packages("rstanarm")
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())

data("womensrole", package = "HSAUR3")
womensrole$total <- womensrole$agree + womensrole$disagree
womensrole_glm_1 <- glm(cbind(agree, disagree) ~ education + gender,
                        data = womensrole, family = binomial(link = "logit"))
round(coef(summary(womensrole_glm_1)), 3) # wow interesting



#Bayesian
library(rstanarm)
womensrole_bglm_1 <- stan_glm(cbind(agree, disagree) ~ education + gender,
                              data = womensrole,
                              family = binomial(link = "logit"),
                              prior = student_t(df = 7, 0, 5),
                              prior_intercept = student_t(df = 7, 0, 5),
                              cores = 2, seed = 12345)
