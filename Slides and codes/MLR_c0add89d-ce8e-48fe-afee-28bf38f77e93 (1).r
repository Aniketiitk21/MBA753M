# Call the packages that are used in the analysis
library(tidyverse)
library(datarium)
library(broom)
library(ggplot2)
library(car)
library(sjPlot)
library(sjmisc)


####### Dummy Variable Regression #######
# Load data
data(Duncan)

# Data documentation
# https://www.rdocumentation.org/packages/car/versions/2.1-6/topics/Duncan

# Summarize data
summary(Duncan)
table(Duncan$type)

# Default method of dummy coding
contrasts(Duncan$type)

# You can change the reference category based on your preference and need
type2 <- relevel(Duncan$type, ref = "wc")
contrasts(type2)

# Fit the linear model
mod1<-lm(prestige ~ income + education + type, data = Duncan)
summary(mod1)
plot_model(mod1, type = "pred", terms = c("income", "type"))

mod2<-lm(prestige ~ income + education + type2, data=Duncan)
summary(mod2)
plot_model(mod1, type = "pred", terms = c("income", "type"))

# Fit model with interaction effects
# Two-way interactions
mod3<-lm(prestige ~ education + income * type, data=Duncan)
summary(mod3)
plot_model(mod3, type = "pred", terms = c("income", "type"))

mod4<-lm(prestige ~ income + education * type, data=Duncan)
summary(mod4)
plot_model(mod4, type = "pred", terms = c("education", "type"))

# Three way interactions
mod5<-lm(prestige ~ income * education * type, data=Duncan)
summary(mod5)
plot_model(mod5, type = "pred", 
           terms = c("education", "income", "type"))
