library(tidyverse)
library(wooldridge)
library(ivreg)
library(lmtest)
library(sandwich)

### Load Data ###
data("card")
summary(card)

plot( density(card$lwage), 
      xlab = "Wage", main = "Dependent variable", ylab = "Frequency",
      bty="n", cex=1.3 )

plot( density(card$educ), 
      xlab = "Education", main = "Endogenous variable", ylab = "Frequency",
      bty="n", cex=1.3 )

barplot( table(card$nearc4), xlab = "Living near a four-year college", 
         ylab = "Frequency", main = "Instrumental variable")

### Regression ###

naive <- lm(lwage ~
              educ +
              nearc4 +
              smsa66 +          
              exper +          
              expersq +        
              black +         
              south66, data = card)
summary(naive)

# Using TSLS 

first.stage = lm(educ ~ 
                   nearc4 +  
                   smsa66 +          
                   exper +          
                   expersq +        
                   black +         
                   south66, data = card)

x1_hat <- fitted( first.stage ) #Save predicted values for education

second.stage <- lm(lwage ~
                     x1_hat +
                     smsa66 +          
                     exper +          
                     expersq +        
                     black +         
                     south66, data = card)
summary(second.stage)

## Corrected SE
m_iv <- ivreg(lwage ~ smsa66 + south66 + black + exper + expersq | 
                educ |
                nearc4, data = card)
summary(m_iv)


