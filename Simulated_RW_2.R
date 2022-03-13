#Simulated Random Walk
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
  
Generate_RW <- function(plot = "FULL", set_MAX_Time_Points = MAX_TIME_POINTS) {
  Y <- Generate_RW_data(set_MAX_Time_Points)
  str = paste("RW")
  p_plot_Y <- autoplot(Y, size = 1) +
    ggtitle(str)

  ##########################################################################3
  str = paste("ACF: RW")

  p_acf <- ggAcf(Y, lag = 30, size = 2) +
    scale_y_continuous(limits=c(-1,1)) +
    ggtitle(str)

  ##########################################################################3
  str = paste("PACF: RW")
  p_pacf <- ggPacf(Y, lag = 30, size = 2) +
    scale_y_continuous(limits=c(-1,1)) +
    ggtitle(str)

  
  if(plot == "FULL") {
    p_plot = grid.arrange(p_plot_Y, p_acf, p_pacf)  
  }    
  
  else if(plot == "Y") {
    p_plot = p_plot_Y
  }    
  
  else if(plot == "ACF") {
    p_plot = p_acf
  }
  
  else if(plot == "PACF") {
    p_plot = p_pacf
  }
  
  print(p_plot)
  return (p_plot)
}

# p_RW1 = Generate_RW()
# p_RW2 = Generate_RW()
# p_RW3 = Generate_RW()
# p_RW4 = Generate_RW()
# print(grid.arrange(p_RW1, p_RW2, p_RW3, p_RW4))  


# p_RW1 = Generate_RW("Y")
# p_RW2 = Generate_RW("Y")
# p_RW3 = Generate_RW("Y")
# p_RW4 = Generate_RW("Y")
# print(grid.arrange(p_RW1, p_RW2, p_RW3, p_RW4))  

# p_RW1 = Generate_RW("ACF")
# p_RW2 = Generate_RW("ACF")
# p_RW3 = Generate_RW("ACF")
# p_RW4 = Generate_RW("ACF")
# print(grid.arrange(p_RW1, p_RW2, p_RW3, p_RW4))  


# p_RW1 = Generate_RW("PACF")
# p_RW2 = Generate_RW("PACF")
# p_RW3 = Generate_RW("PACF")
# p_RW4 = Generate_RW("PACF")
# print(grid.arrange(p_RW1, p_RW2, p_RW3, p_RW4))  

RandomWalkSimulations <- function(set_MAX_Time_Points = MAX_TIME_POINTS, 
                                  set_MAX_SIMULATIONS = MAX_TIME_SERIES_SET,
                                  Simulation_Delay = FALSE,
                                  drift = 0) {
  data_full <- Generate_RW_data(set_MAX_Time_Points, drift)
  for(i in 1:set_MAX_SIMULATIONS) {
    data_full <- data.frame(data_full, Generate_RW_data(set_MAX_Time_Points, drift))
    names(data_full)[names(data_full) == "Generate_RW_data.set_MAX_Time_Points..drift."] <- paste("Y",as.numeric(i))

    if(Simulation_Delay == TRUE) {
      print(autoplot(ts(data_full), size = 1))
      Sys.sleep(0.5)
    }
    g_data_full <<- data_full
  }
  names(data_full)[names(data_full) == "data_full"] <- paste("Y")

  if(Simulation_Delay == FALSE) {
    print(autoplot(ts(data_full), size = 1))
  }
}

#RandomWalkSimulations()
RandomWalkSimulations(set_MAX_Time_Points = 100, set_MAX_SIMULATIONS = 30)
RandomWalkSimulations(drift = 0.1, set_MAX_Time_Points = 1000, set_MAX_SIMULATIONS = 20)
RandomWalkSimulations(drift = -0.1, set_MAX_Time_Points = 1000, set_MAX_SIMULATIONS = 20)

RandomWalkSimulations(drift = 1, set_MAX_Time_Points = 100, set_MAX_SIMULATIONS = 30)
RandomWalkSimulations(drift = -1, set_MAX_Time_Points = 100, set_MAX_SIMULATIONS = 30)


data_1 <- Generate_RW_data(set_MAX_Time_Points = 100, drift = 0)
autoplot(data_1)

p_RW1 = Generate_RW()
