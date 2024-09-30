library(tidyverse)
remotes::install_github('NickCH-K/causaldata/R/')
library(fixest)

### Card & Krueger, 1994 ###
data <- read.csv("Card_Krueger.csv")

summary(data)
reg <- lm( Empl ~ Group + Treatment + Treatment * Group + 
             C.Owned + Hours.Opening + 
             Soda + Fries +  as.factor(Chain) +  
             SouthJ +  CentralJ +  NorthJ +  
             PA1 +  PA2 +  Shore, data = data)
summary(reg)

### Falsification test ###
od <- causaldata::organ_donations

# Treatment variable
od <- od %>%
  mutate(Treated = State == 'California' & 
           Quarter %in% c('Q32011','Q42011','Q12012'))
clfe <- feols(Rate ~ Treated | State + Quarter,
              data = od)
summary(clfe)

od <- causaldata::organ_donations %>%
  # Use only pre-treatment data
  filter(Quarter_Num <= 3)

# Create our fake treatment variables
od <- od %>%
  mutate(FakeTreat1 = State == 'California' & 
           Quarter %in% c('Q12011','Q22011'),
         FakeTreat2 = State == 'California' &
           Quarter == 'Q22011')
# Run the same model we did before but with our fake treatment
clfe1 <- feols(Rate ~ FakeTreat1 | State + Quarter,
               data = od)
clfe2 <- feols(Rate ~ FakeTreat2 | State + Quarter,
               data = od)
summary(clfe1)
summary(clfe2)
