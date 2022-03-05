#First Differencing AR(1)
#First Differencing AR(2)
library(forecast)
library(ggplot2)
library(gridExtra)
library(fpp2)
library(car)        
library(lmtest)

Generate_Non_Stationary_AR <- function(order = 1, unitRoots = 1, 
                                       diffOrder = 0, plot = "Y", diff = 1) {
  set.seed(99)  #Used for Random Number Generation
  TOTAL_COUNT = 1000
  str = "I("
  str = paste(str, toString(unitRoots), ")  ")
  str = paste(str, "+  AR(",toString(order - unitRoots),"):")
  
  if(order < unitRoots) {
    return("Error: Unit roots must be less than order")
  }

  else if (order == unitRoots) {
    Y <- arima.sim(model = list(order = c(0, unitRoots, 0)), n = TOTAL_COUNT)
  }
  
  else {
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
    
    else if(order == 7)
      beta <- c(0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1)  
    
    Y <- arima.sim(model = list(order = c(order-unitRoots, unitRoots, 0), 
                                ar = beta[1:(order-unitRoots)]), n = TOTAL_COUNT)


    str = paste(str, "beta[")
    betaStr = ""
    for(i in (1:(order-unitRoots))) {
      betaStr = paste(betaStr, toString(beta[i]),",")
    }
    str = paste(str, betaStr)
    str = paste(str, "]")
  }
  
  diffStr = "   DIFF Order: 0"
  if(diffOrder == 1) {
    Y <- diff(Y)
    diffStr = "   DIFF Order: 1"
  }
  
  if(diffOrder == 2) {
    Y <- diff(Y)
    Y <- diff(Y)
    diffStr = "   DIFF Order: 2"
  }
  
  str = paste(str, diffStr)
  #######################################################################
  if(plot == "Y") {
    str = paste("Y: ", str)
    p_plot <- autoplot(Y, size = 1) +
      ggtitle(str)
  }

  if(plot == "FIRST_DIFF") {
    str = paste("First Diff: ", str)
    Y_firstDiff <- diff(Y)
    p_plot <- autoplot(Y_firstDiff, size = 1) +
      ggtitle(str)
  }
  
  if(plot == "TWICE_DIFF") {
    str = paste("Twice Diff: ", str)
    Y_twiceDiff <- diff(diff(Y))
    p_plot <- autoplot(Y_twiceDiff, size = 1) +
      ggtitle(str)
  }
  
    
  if(plot == "ACF") {
    str = paste("ACF: ", str)
    p_plot <- ggAcf(Y, lag = 50, size = 2) +
      scale_y_continuous(limits=c(-1,1)) +
      ggtitle(str)
  }
  
  if(plot == "PACF") {
    str = paste("PACF: ", str)
    p_plot <- ggPacf(Y, lag = 50, size = 2) +
      scale_y_continuous(limits=c(-1,1)) +
      ggtitle(str)
  }
  
  #print(grid.arrange(p_plot, p_plot_FD))  #Internal Plots  
  #print(p_plot)
  # return (data.frame(Y, Y_Firstdiff[]))
  # return (data.frame(Y[2:length(Y)], Y_Firstdiff))
  return (p_plot)  
}  

#Plot AR(1) to AR(5)
#Unit Root == 1
Plot_AR_6_UR1_No_Diff <- function(unitRootsOrder = 0 , diffOrder = 0,
                                  setPlot = "Y") {
  UR_ORDER = unitRootsOrder
  SET_PLOT = setPlot 
  DIFF_ORDER = diffOrder

  p_I_AR0 = Generate_Non_Stationary_AR(order = (unitRootsOrder + 0), unitRoots = UR_ORDER, diffOrder = DIFF_ORDER, plot = SET_PLOT)
  p_I_AR1 = Generate_Non_Stationary_AR(order = (unitRootsOrder + 1), unitRoots = UR_ORDER, diffOrder = DIFF_ORDER, plot = SET_PLOT)
  p_I_AR2 = Generate_Non_Stationary_AR(order = (unitRootsOrder + 2), unitRoots = UR_ORDER, diffOrder = DIFF_ORDER, plot = SET_PLOT)
  p_I_AR3 = Generate_Non_Stationary_AR(order = (unitRootsOrder + 3), unitRoots = UR_ORDER, diffOrder = DIFF_ORDER, plot = SET_PLOT)
  p_I_AR4 = Generate_Non_Stationary_AR(order = (unitRootsOrder + 4), unitRoots = UR_ORDER, diffOrder = DIFF_ORDER, plot = SET_PLOT)
  p_I_AR5 = Generate_Non_Stationary_AR(order = (unitRootsOrder + 5), unitRoots = UR_ORDER, diffOrder = DIFF_ORDER, plot = SET_PLOT)
  print(grid.arrange(p_I_AR0, p_I_AR1, p_I_AR2, 
                     p_I_AR3, p_I_AR4, p_I_AR5))  

}

Plot_AR_6_UR1_No_Diff(unitRootsOrder = 0 , diffOrder = 0, setPlot = "Y")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 0 , diffOrder = 0, setPlot = "ACF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 0 , diffOrder = 0, setPlot = "PACF")

#order: the total order of the TSE
#unitRoots: number of unit roots in TSE
#Stationary AR order: (order - unitRoots)
#e.g AR(2) with 1 RW 
#      order = 2
#      unitRoots = 1

#RW(1) + AR(p-1) - No first Differencing (diffOrder == 0)
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 0, setPlot = "Y")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 0, setPlot = "FIRST_DIFF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 0, setPlot = "ACF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 0, setPlot = "PACF")


#RW(1) + AR(p-1) - WITH first Differencing (diffOrder == 1)
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 1, setPlot = "Y")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 1, setPlot = "ACF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 1 , diffOrder = 1, setPlot = "PACF")


#############################################################################
#############################################################################
#############################################################################
#RW(2) + AR(p-2) - No first Differencing (diffOrder == 0)
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 0, setPlot = "Y")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 0, setPlot = "FIRST_DIFF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 0, setPlot = "TWICE_DIFF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 0, setPlot = "ACF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 0, setPlot = "PACF")

#RW(2) + AR(p-2) - With first Differencing (diffOrder == 1)
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 1, setPlot = "Y")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 1, setPlot = "ACF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 1, setPlot = "PACF")

#RW(2) + AR(p-2) - With second Differencing (diffOrder == 2)
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 2, setPlot = "Y")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 2, setPlot = "ACF")
Plot_AR_6_UR1_No_Diff(unitRootsOrder = 2 , diffOrder = 2, setPlot = "PACF")


#############################################################################
#############################################################################
#############################################################################
#Plot
# UR_ORDER = 1    #x1 Unit Root
# UR_ORDER = 2    #x2 Unit Root
# SET_PLOT = "Y"  #"FIRST_DIFF"   "TWICE_DIFF"   "ACF"  "PACF"
# p_I1_AR0 = Generate_Non_Stationary_AR(order = 1, unitRoots = UR_ORDER, plot = SET_PLOT)
# p_I1_AR1 = Generate_Non_Stationary_AR(order = 2, unitRoots = UR_ORDER, plot = SET_PLOT)
# p_I1_AR2 = Generate_Non_Stationary_AR(order = 3, unitRoots = UR_ORDER, plot = SET_PLOT)
# p_I1_AR3 = Generate_Non_Stationary_AR(order = 4, unitRoots = UR_ORDER, plot = SET_PLOT)
# p_I1_AR4 = Generate_Non_Stationary_AR(order = 5, unitRoots = UR_ORDER, plot = SET_PLOT)
# p_I1_AR5 = Generate_Non_Stationary_AR(order = 6, unitRoots = UR_ORDER, plot = SET_PLOT)
# print(grid.arrange(p_I1_AR0, p_I1_AR1, p_I1_AR2, 
#                    p_I1_AR3, p_I1_AR4, p_I1_AR5))  

