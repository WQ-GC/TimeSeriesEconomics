#This project implements a random simulated AR(1) data
#AR(1): Y[t] = beta0 + beta1Y[t_1] + err[t]

library(ggplot2)
library(reshape2)

set.seed(88)  #Used for Random Number Generation
TOTAL_COUNT <- 10000     #Number of simulated trials
beta0    <- 0.25
beta1    <- 0.7  #Note: beta1 between -1 and 1
err_mean <- 0    #For generating White Noise (WN)
err_sd   <- 1   #For generating WN
Y        <- rep(NA, TOTAL_COUNT)  #Univariate RV
err      <- rep(NA, TOTAL_COUNT)  #White Noise RV
Y[1]     <- 0       #initialise 1st term
err[1]   <- 0       #initialise 1st term

#Simulate random values
for(t in 2:TOTAL_COUNT) {
  err[t] <- rnorm(n = TOTAL_COUNT, mean = err_mean, sd = err_sd)
  Y[t]   <- beta0 + (beta1*Y[t-1]) + err[t]
}

#View(Y)
#View(err)
#summary(err)
plot(1:TOTAL_COUNT, err, "l")   #Plot simulated WN
plot(1:TOTAL_COUNT, Y, "l")     #Plot simulated Univariate

#Plot 1st order Lag Scatterplot
plot(x = Y[2:TOTAL_COUNT], y = Y[1:TOTAL_COUNT-1])
abline(lm(Y[2:(TOTAL_COUNT)] ~ Y[1:(TOTAL_COUNT-1)]))

MAX_LAG = 100
#Plot ACF for first 100 lags
acf(Y, type = "correlation", lag.max = MAX_LAG)

#Note: Observations that for AR(1), 
#The AR(1) ACF = beta1 power k
#The ACF thus decays with k

Plot_AR1 <- function(set_MAX_Observations = 10, set_MAX_Simulations = 10) {
  set.seed(99)  #Used for Random Number Generation
  i = 0
  beta0    <- 0.25
  beta1    <- 0.7  #Note: beta1 between -1 and 1
  err_mean <- 0    #For generating White Noise (WN)
  err_sd   <- 1   #For generating WN
  Y   <- rep(NA, set_MAX_Observations)  #Univariate RV
  err <- rep(NA, set_MAX_Observations)  #White Noise RV
  Y[1]   <- 0      #initialise 1st term
  err[1] <- 0      #initialise 1st term
  
  count <- c(1:set_MAX_Observations)
  dataset <- data.frame(count)
  
  #Simulate random values for MA(1)
  for(i in 1:set_MAX_Simulations) {
    for(t in 2:set_MAX_Observations) {
      err[t] <- rnorm(n = set_MAX_Observations, mean = err_mean, sd = err_sd)
      Y[t]   <- beta0 + (beta1*Y[t-1]) + err[t]
    }
    dataset <- data.frame(dataset,Y)
  }  
  dataset_long <- melt(dataset, id="count")  # convert to long format
  
  ggplot(data = dataset_long,
         aes(x = count, y=value, colour = variable)) +
    geom_line(size = 1)

#  acf(Y, type = "correlation", lag.max = MAX_LAG)
}
#Plot_AR1(set_MAX_Observations = 20, set_MAX_Simulations = 100)
Plot_AR1(set_MAX_Observations = 100, set_MAX_Simulations = 100)
#Plot_AR1(set_MAX_Observations = 10000, set_MAX_Simulations = 10)










