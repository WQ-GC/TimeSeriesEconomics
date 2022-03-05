#This project implements a random simulated AR(p) data
#AR(p): Y[t] = beta0 + beta1Y[t_1] + beta@Y[t_2] + ... + err[t]
library(forecast)
library(ggplot2)
library(gridExtra)
library(fpp2)
library(car)        
library(lmtest)

TOTAL_COUNT <- 1000     #Number of simulated trials

#AR(order) is only up to 6
#Beta is currently fixed for Stationarity
Generate_AR <- function(order = 1, plot = "PACF") {
  set.seed(99)  #Used for Random Number Generation

  #Tested Stationary Betas
  if(order == 1)  
    beta <- c(0.3)

  else if(order == 2)
    beta <- c(0.3, 0.4)

  else if(order == 3)
    beta <- c(0.3, 0.2, 0.3)
  
  else if(order == 4)
    beta <- c(0.1, 0.1, 0.1, 0.3)

  else if(order == 5)  
    beta <- c(0.1, 0.1, 0.1, 0.2, 0.3)
  
  else if(order == 6)
    beta <- c(0.1, 0.1, 0.1, 0.2, 0.1, 0.3)  
  
  Y <- arima.sim(model = list(order = c(order, 0, 0), ar = beta[1:order]), n = TOTAL_COUNT)

  if(plot == "Y") {
    str = paste("AR(",toString(order),")")
    str = paste(str," beta[")
    for(i in (1:order)) {
      str = paste(str,paste(toString(beta[i]))," ")    
    }
    str = paste(str,"]")
    p_plot <- autoplot(Y) +
      ggtitle(str)
  }
  
  else if(plot == "ACF") {
    str = paste("ACF: AR(",toString(order),")")
    str = paste(str," beta[")
    for(i in (1:order)) {
      str = paste(str,paste(toString(beta[i]))," ")    
    }
    str = paste(str,"]")
    
    p_plot <- ggAcf(Y, lag = 30) +
      scale_y_continuous(limits=c(-1,1)) +
      ggtitle(str)
  }
    
  else if(plot == "PACF") {
    str = paste("PACF: AR(",toString(order),")")
    str = paste(str," beta[")
    for(i in (1:order)) {
      str = paste(str,paste(toString(beta[i]))," ")    
    }
    str = paste(str,"]")
    
    p_plot <- ggPacf(Y, lag = 30) +
      scale_y_continuous(limits=c(-1,1)) +
      ggtitle(str)
  }
  
  #print(grid.arrange(p1, p_acf, p_pacf))  
  #print(grid.arrange(p_acf, p_pacf))  

  return (p_plot)
}
p_AR1 = Generate_AR(1, "Y")#beta <- c(0.7)
p_AR2 = Generate_AR(2, "Y")#beta <- c(0.3, 0.4)
p_AR3 = Generate_AR(3, "Y")#beta <- c(0.3, 0.2, 0.3)
p_AR4 = Generate_AR(4, "Y")#beta <- c(0.1, 0.1, 0.1, 0.3)
print(grid.arrange(p_AR1, p_AR2, p_AR3, p_AR4))  

AR1_acf = Generate_AR(1, "ACF")#beta <- c(0.7)
AR2_acf = Generate_AR(2, "ACF")#beta <- c(0.3, 0.4)
AR3_acf = Generate_AR(3, "ACF")#beta <- c(0.3, 0.2, 0.3)
AR4_acf = Generate_AR(4, "ACF")#beta <- c(0.1, 0.1, 0.1, 0.3)
print(grid.arrange(AR1_acf, AR2_acf, AR3_acf, AR4_acf))  


AR1_pacf = Generate_AR(1, "PACF")#beta <- c(0.7)
AR2_pacf = Generate_AR(2, "PACF")#beta <- c(0.3, 0.4)
AR3_pacf = Generate_AR(3, "PACF")#beta <- c(0.3, 0.2, 0.3)
AR4_pacf = Generate_AR(4, "PACF")#beta <- c(0.1, 0.1, 0.1, 0.3)
print(grid.arrange(AR1_pacf, AR2_pacf, AR3_pacf, AR4_pacf))  
