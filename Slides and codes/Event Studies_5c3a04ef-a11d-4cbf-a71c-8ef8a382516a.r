devtools::install_github( repo="OuhscBbmc/Wats" )

library(scales)        
library(stargazer)     
library(dplyr)         
library(pander)        
library(Wats) 
library(lmtest)
library(sandwich)

# load data from GitHub
URL <- "https://github.com/DS4PS/pe4ps-textbook/blob/master/data/time-series-example.rds?raw=true"
dataTS <- readRDS(gzcon(url( URL )))

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 300), xlim=c(0,400),
      xlab = "Time (days)", 
      ylab = "Wellbeing Index" )

# Line marking the interruption
abline( v=200, col="firebrick", lty=2 )
text( 200, 300, "Start of Wellbeing Classes", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=dataTS )
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

# Fitting the segmented regression 
regTS = lm ( Y ~ T + D + P, data = dataTS)  # Our time series model
summary(regTS)

# Plotting results
# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 201, D = 1, P = 1 )) 

# We use the function predict to 
# (1) take the coefficients estimated in regTS and 
# (2) calculate the outcome Y based on the values we set in the new dataset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( dataTS$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 365), 
      ylim = c(0, 300),
      xlab = "Time (days)", 
      ylab = "Wellbeing Index")

# We add a point showing the level of wellbeing at time = 201)
points( 201, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 201, y1, labels = "t = 201", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=200, col="red", lty=2 )

# Add a new point
data2 <- as.data.frame( cbind( T = 230, D = 1, P = 30 )) # New data

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( dataTS$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 365), 
      ylim = c(0, 300),
      xlab = "Time (days)", 
      ylab = "Wellbeing Index")

# We add a point showing the level of wellbeing at time = 201
points(201, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of wellbeing at time = 230)
points(230, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(201, y1, labels = "t = 201", pos = 4, cex = 1)

#Label for the counterfactual 
text(230, y2, labels = "t = 230", pos = 4, cex = 1)

# Line marking the interruption
abline( v=200, col="red", lty=2 )

# Plotting counterfactuals
data3 <- as.data.frame(cbind( T= 230, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( dataTS$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 365), 
      ylim = c(0, 300),
      xlab = "Time (days)", 
      ylab = "Wellbeing Index")

# We add a  point showing the level of wellbeing at time = 230
points(230, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(230, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(230, y2, labels = "Y at t = 230", pos = 4, cex = 1)

#Label for the counterfactual 
text(230, y3, labels = "C at t = 230", pos = 4, cex = 1)

# Line marking the interruption
abline( v=200, col="red", lty=2 )


# Multiple counterfactuals to draw the trend
data4 <- as.data.frame(cbind( T = 320, D = 1, P = 80)) 
data5 <- as.data.frame(cbind( T = 320, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( dataTS$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 365), 
      ylim = c(0, 300),
      xlab = "Time (days)", 
      ylab = "Wellbeing index")

# Wellbeing at time = 230
points(230, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = 230
points(230, y3, col = "darkorange2", pch = 19, cex = 2)

# Wellbeing at time = 320
points(320, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = 320
points(320, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(320, y4, labels = "Y at t = 320", pos = 4, cex = 1)
text(320, y5, labels = "C at t = 320", pos = 4, cex = 1)
text(230, y2, labels = "Y at at = 230", pos = 4, cex = 1)
text(230, y3, labels = "C at t = 230", pos = 4, cex = 1)

# Line marking the interruption
abline( v=200, col="red", lty=2 )

# To estimate all predicted values of Y, we just use our dataset
pred1 <- predict(regTS, dataTS) 

# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.
datanew <- as.data.frame(cbind(T = rep(1 : 365), D = rep(0), P = rep(0))) 

# Predict the counterfactuals
pred2 <- predict(regTS, datanew) 

plot( dataTS$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 365), 
      ylim = c(0, 400),
      xlab = "Time (days)", 
      ylab = "Wellbeing index")

lines( rep(1:199), pred1[1:199], col="dodgerblue4", lwd = 3 )
lines( rep(201:365), pred1[201:365], col="dodgerblue4", lwd = 3 )
lines( rep(200:365), pred2[200:365], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 45, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(300, 95, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=200, col="darkorange2", lty=2 )


### Autocorrelation ###
# Plot the residuals
plot(resid( regTS ))

# Durbin-Watson test
# Significant p-value implies autocorrelation is an issue
dwtest(dataTS$Y ~ dataTS$T)

# Robust SE
summary(regTS)
coeftest(regTS, vcov = vcovHC(regTS, type = "HC1"))

# More information for self practice
# https://rpubs.com/arda_yalcin/550917













