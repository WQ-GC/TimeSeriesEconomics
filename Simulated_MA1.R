#Simple application for Moving Average 1
#MA(1): Y[t] = MA_C + err[t] + theta1*err[t-1]

library(ggplot2)
library(reshape2)

#Create a random set of data
set.seed(99)  #Used for Random Number Generation
TOTAL_COUNT <- 100   #Number of simulated trials
MA_C     <- 1    #MA_C is constant in MA(1)
THETA_1  <- 2    #THETA_1 is another constant in MA(1)
err_mean <- 0    #For generating White Noise (WN)
err_sd   <- 32   #For generating WN
Y   <- rep(NA, TOTAL_COUNT)  #Univariate RV
err <- rep(NA, TOTAL_COUNT)  #White Noise RV
Y[1]   <- MA_C    #initialise 1st term
err[1] <- 0       #initialise 1st term

#Simulate random values for MA(1)
for (t in 2:TOTAL_COUNT) {
  err[t] <- rnorm(n = TOTAL_COUNT, mean = err_mean, sd = err_sd)
  Y[t]   <- (MA_C + err[t] + (THETA_1*err[t-1]))
}

PLOT_COUNT <- TOTAL_COUNT   #Set to only plot first 50 values
plot(x = 1:PLOT_COUNT, y = Y[1:PLOT_COUNT], "l")

#Verify Properties of MA(1)
MA1_mean <- mean(Y)  #Mean of MA(1)
print(MA1_mean)      #close to MA_C == 1

MA1_variance <-var(Y)  #Variance of MA(1)
print(MA1_variance)

MA1_err_variance <- var(err)   #WN variance
print(MA1_err_variance)

#verify var(MA(1)) is (1 + THETA_1^2)sigma^2
verify_MA1_variance <- (1 + (THETA_1^2))*MA1_err_variance
print(verify_MA1_variance)


#verify var(MA(1)) is (1 + THETA_1^2)sigma^2
verify_MA1_covariance <- (THETA_1)/(1 + (THETA_1^2))
print(verify_MA1_covariance)

#summary(Y)
#summary(err)

#Compare 1st Lag vs 2nd Lag and 3rd Lag for MA(1)
#Scatterplot for 1st Lag
plot(x = Y[2:(TOTAL_COUNT)], y = Y[1:(TOTAL_COUNT-1)])
abline(lm(Y[2:(TOTAL_COUNT)] ~ Y[1:(TOTAL_COUNT-1)]))

#Scatterplot for 2nd Lag
#The slope for 2nd Lag is almost flat (visually)
plot(x = Y[3:(TOTAL_COUNT)], y = Y[1:(TOTAL_COUNT-2)])
abline(lm(Y[3:(TOTAL_COUNT)] ~ Y[1:(TOTAL_COUNT-2)]))

#Scatterplot for 3rd Lag
#The slope for 3rd Lag is almost flat (visually)
plot(x = Y[4:(TOTAL_COUNT)], y = Y[1:(TOTAL_COUNT-3)])
abline(lm(Y[4:(TOTAL_COUNT)] ~ Y[1:(TOTAL_COUNT-3)]))

#MAX_LAG <- TOTAL_COUNT
MAX_LAG <- 50
acf(x = Y, lag.max = MAX_LAG, type = "correlation")


Plot_MA1 <- function(set_MAX_Observations = 10, set_MAX_Simulations = 10) {
  set.seed(99)  #Used for Random Number Generation
  i = 0
  err_mean <- 0    #For generating White Noise (WN)
  err_sd   <- 32   #For generating WN
  set.seed(99)  #Used for Random Number Generation
  MA_C     <- 1    #MA_C is constant in MA(1)
  THETA_1  <- 2    #THETA_1 is another constant in MA(1)
  err_mean <- 0    #For generating White Noise (WN)
  err_sd   <- 32   #For generating WN
  Y   <- rep(NA, set_MAX_Observations)  #Univariate RV
  err <- rep(NA, set_MAX_Observations)  #White Noise RV
  Y[1]   <- MA_C    #initialise 1st term
  err[1] <- 0       #initialise 1st term
  
  count <- c(1:set_MAX_Observations)
  dataset <- data.frame(count)
  
  #Simulate random values for MA(1)
  for(i in 1:set_MAX_Simulations) {
    for (t in 2:set_MAX_Observations) {
      err[t] <- rnorm(n = set_MAX_Observations, mean = err_mean, sd = err_sd)
      Y[t]   <- (MA_C + err[t] + (THETA_1*err[t-1]))
    }
    dataset <- data.frame(dataset,Y)
  }  
  dataset_long <- melt(dataset, id="count")  # convert to long format
  
  ggplot(data = dataset_long,
         aes(x = count, y=value, colour = variable)) +
    geom_line(size = 1)
  
#  acf(Y, type = "correlation", lag.max = MAX_LAG)
}
#Plot_MA1(set_MAX_Observations = 20, set_MAX_Simulations = 100)
Plot_MA1(set_MAX_Observations = 100, set_MAX_Simulations = 100)
#Plot_MA1(set_MAX_Observations = 10000, set_MAX_Simulations = 1)




  
