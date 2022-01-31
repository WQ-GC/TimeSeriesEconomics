#This project implements a random simulated AR(1) data
#AR(1): Y[t] = beta0 + beta1Y[t_1] + err[t]

set.seed(99)  #Used for Random Number Generation
              #99 is my lucky number
TOTAL_COUNT <- 10000     #Number of simulated trials
beta0    <- 0
beta1    <- 0.7  #Note: beta1 between -1 and 1
err_mean <- 0    #For generating White Noise (WN)
err_sd   <- 10   #For generating WN
Y        <- rep(NA, TOTAL_COUNT)  #Univariate RV
err      <- rep(NA, TOTAL_COUNT)  #White Noise RV
Y[1]     <- beta0   #initialise 1st term
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
