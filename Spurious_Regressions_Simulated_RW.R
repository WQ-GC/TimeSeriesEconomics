#Spurious Regressions is a problem
#When 2 UNRELATED time series that are trending are regressed
#The regressor's coefficient is tested and will be significant

library(forecast)
library(ggplot2)
library(gridExtra)
library(fpp2)
library(car)        
library(lmtest)

MAX_TIME_SERIES_SET <- 10    #Number of simulated trials
MAX_TIME_POINTS     <- 1000   #Number of time points in each time series set

set.seed(99)  #Used for Random Number Generation

Generate_RW_data <- function(set_MAX_Time_Points = MAX_TIME_POINTS, drift = 0) {
  Y <- arima.sim(model = list(order = c(0, 1, 0)), 
                 mean = drift,
                 n = set_MAX_Time_Points)
  return (Y)
}


#Simulate for 100 pairs
MAX_SIMULATION_PAIRS = 1000
MAX_TIME_DATA_POINTS = 1000
tstats      <- rep(NA, MAX_SIMULATION_PAIRS)  # vector to store t-stats
coeff_data2 <- rep(NA, MAX_SIMULATION_PAIRS)
se_data2    <- rep(NA, MAX_SIMULATION_PAIRS)

for(i in 1:MAX_SIMULATION_PAIRS) {
  data_1 <- Generate_RW_data(set_MAX_Time_Points = MAX_TIME_DATA_POINTS, drift = 0)
  data_2 <- Generate_RW_data(set_MAX_Time_Points = MAX_TIME_DATA_POINTS, drift = 0)
  #p1 <- autoplot(data_1)
  #p2 <- autoplot(data_2)
  print(autoplot(ts.intersect(data_1, data_2), size = 1))  
  
  model <- tslm(data_1 ~ data_2, lambda = NULL)
  tstats[i] = coef(summary(model))["data_2","t value"]
  coeff_data2[i] = coef(model)["data_2"]
  
  se_data2[i] <- coef(summary(model))["data_2","Std. Error"]
  Sys.sleep(0.5)
}


#View(tstats)
hist(tstats, breaks = 20)

#View(coeff_data2)
hist(coeff_data2, breaks = 20)

hist(se_data2, breaks = 20)
