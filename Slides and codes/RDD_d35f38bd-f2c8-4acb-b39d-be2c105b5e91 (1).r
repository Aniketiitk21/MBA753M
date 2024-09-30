library(tidyverse)
library(rdd)
library(ggplot2)
library(ggthemes)
library(lemon)

## Data ##
data <- read.csv("alcohol_mortality.csv", stringsAsFactors=F)

summary(data)

## Create treatment variable ##
data$Treatment <- ifelse( data$agecell > 21, 1, 0 )

## Centered age ##
data$age_c <- data$agecell - 21

reg1 <- lm( mva ~ Treatment*age_c, data = data )

summary(reg1)

# We first predict the final exam grades based on our regression discontinuity model. 
predfit <- predict( reg1, data ) 

# We set colors for the graph
palette( c( adjustcolor( "steelblue", alpha.f=1 ), 
            adjustcolor( "darkred", alpha.f=1)  ) )

# To differently color the treatment and control group 
# we need to create a new Treatment variable that where the Control group = 2. 
data$Treatment2 <- ifelse(data$Treatment == 0, 2, 1)

# Plot our observed value
plot(data$age_c, data$mva, col = data$Treatment2, pch=19, bty="n",
     xlab="Age (centered)", ylab="Car accidents",  cex=2, ylim = c(28, 40))

# Plot a line at the cutoff point
abline( v=0, col="red", lwd=2, lty=2 )

# Plot our predicted values
points( data$age_c, predfit, col="black", lwd=5, pch = 20 )

## Add a legend
points( 1, 39, col="steelblue", pch=19, cex=2 )
text( 1, 39, "Treatment group", col="steelblue", pos=4 )
points( 1, 37, col="darkred", pch=19, cex=2 )
text( 1, 37, "Control group", col="darkred", pos=4 )

## RDD with bandwidth ##
optimal_bandwidth <- IKbandwidth(X = data$agecell,
                                 Y = data$mva,
                                 cutpoint = 21)
optimal_bandwidth

rd_est <- RDestimate(mva ~ agecell,
                     cutpoint = 21,
                     bw = optimal_bandwidth,
                     data = data)
rd_est
summary(reg1)
rd_est

bandwidths <- c(seq(0.25,1.75,0.01))
rd_est <- RDestimate(mva ~ agecell,
                     cutpoint=21,
                     bw=bandwidths,
                     data=data)
rd_est

results <- data.frame(bw = bandwidths,
                      est = rd_est$est,
                      se = rd_est$se,
                      lo = rd_est$est - 1.96*rd_est$se,
                      hi = rd_est$est + 1.96*rd_est$se,
                      opt = bandwidths==round(optimal_bandwidth,2))

ggplot(results, aes(x= bw, y= est, color=opt)) +
  geom_point() +
  geom_linerange(aes(ymin=lo,ymax=hi)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_x_continuous("Bandwidth",breaks = seq(0.25,1.75,.1)) +
  scale_color_manual(values = c("black","red")) +
  ylab("Estimate") +
  theme_clean() +
  lemon::coord_capped_cart(bottom="both",left="both") +
  theme(plot.background = element_rect(color=NA),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        axis.ticks.length = unit(2,"mm"))
