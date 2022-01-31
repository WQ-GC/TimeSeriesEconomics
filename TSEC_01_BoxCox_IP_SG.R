library(readr)
library(forecast)
library(ggplot2)
library(gridExtra)

rm(list=ls()) #clear/remove environment
set.seed(99) # Used for Random Number Generation

SET_COLOUR = "red"
SET_SIZE = 1.0
# TS_01.csv contains four series, including SG_IP
# sample period Jan 1983 to Dec 2017
#SP_IP: SG Industrial production
## Read in IP_SG

dat1.df <- read.csv("TS_01.csv")
IP_SG <- dat1.df[,'IP_SG']
IP_SG.ts <- ts(IP_SG, start=c(1983,1), frequency=12)
#plot(IP_SG.ts)
#class(IP_SG.ts)
#attributes(IP_SG.ts)

#in_data is a tibble dataframe
#in_data <- read_csv("TS_01.csv")
#IP_SG.ts <- ts(in_data, start=c(1983,1), frequency=12)
p0 <- autoplot(IP_SG.ts)
# p1 <- ggplot(data = IP_SG.ts,
#              aes(x = 1:length(IP_SG.ts), y = IP_SG.ts)) + 
#              geom_line(color = SET_COLOUR, size = SET_SIZE)

ln_IP_SG.ts <- log(IP_SG.ts) 
lnp0 <- autoplot(ln_IP_SG.ts) + geom_smooth(method = lm, size = 0.5)
# p2 <- ggplot(data = ln_IP_SG.ts,
#              aes(x = 1:length(ln_IP_SG.ts), y = ln_IP_SG.ts)) + 
#              geom_line(color = SET_COLOUR, size = SET_SIZE)


#Box-Cox for different Lambda
lambda = -5
Y_5 <- (((IP_SG.ts)^lambda)-1) / lambda
p_5 <- autoplot(Y_5) + geom_smooth(method = lm, size = 0.5)

lambda = -4
Y_4 <- (((IP_SG.ts)^lambda)-1) / lambda
p_4 <- autoplot(Y_4) + geom_smooth(method = lm, size = 0.5)

lambda = -3
Y_3 <- (((IP_SG.ts)^lambda)-1) / lambda
p_3 <- autoplot(Y_3) + geom_smooth(method = lm, size = 0.5)

lambda = -2
Y_2 <- (((IP_SG.ts)^lambda)-1) / lambda
p_2 <- autoplot(Y_2) + geom_smooth(method = lm, size = 0.5)

lambda = -1
Y_1 <- (((IP_SG.ts)^lambda)-1) / lambda
p_1 <- autoplot(Y_1) + geom_smooth(method = lm, size = 0.5)

lambda = 1
Y1 <- (((IP_SG.ts)^lambda)-1) / lambda
p1 <- autoplot(Y1) + geom_smooth(method = lm, size = 0.5)

lambda = 2
Y2 <- (((IP_SG.ts)^lambda)-1) / lambda
p2 <- autoplot(Y2) + geom_smooth(method = lm, size = 0.5)

lambda = 3
Y3 <- (((IP_SG.ts)^lambda)-1) / lambda
p3 <- autoplot(Y3) + geom_smooth(method = lm, size = 0.5)

lambda = 4
Y4 <- (((IP_SG.ts)^lambda)-1) / lambda
p4 <- autoplot(Y4) + geom_smooth(method = lm, size = 0.5)

lambda = 5
Y5 <- (((IP_SG.ts)^lambda)-1) / lambda
p5 <- autoplot(Y5) + geom_smooth(method = lm, size = 0.5)

grid.arrange(p_5, p_4, p_3, p_2, p_1,
             lnp0,
             p1, p2, p3, p4, p5,
             nrow=3)

