###############################################################################################
##  ARIMA(0, 2, 1) ### (2 differencing; not very nice)
######################
library(dplyr)
library(magrittr)
library(tseries)
library(forecast)
Y <- read.csv("gdp_data.csv")
Y_gdp <- Y
Y_gdp <- as.data.frame(Y_gdp)
Y_gdp %<>% select(real_gdp)
Y_gdp <- ts(Y_gdp, frequency=4, start=c(1970,1))
Y_gdp <- as.data.frame(Y_gdp)
Y_gdp %<>% select(real_gdp)

#UK GDP before Covid
Y_gdp[1:200,]
#UK GDP after Covid:
Y_gdp[201:210,]

#Forecast for 10 quarters ahead (in log) (Before Covid)
forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10)

#Check residual (it is not a good fit)
forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10) %>% checkresiduals()
#plot
plot(forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10))

#Area of the forecasting trend without Covid:
10 * data.frame(forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]] %>% sum()
#Area of the realistic trend with Covid:
Y_gdp[201:210,] %>% sum() * 10

#GDP lost due to Covid: (losing  -236,204.9 Million)
Y_gdp[201:210,] - data.frame(forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]]
(Y_gdp[201:210,] - data.frame(forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]] )[1:7] %>% sum()

#plot of two graphs to show the Synthetic Counterfactual:
f_Y_gdp_post_covid <-data.frame(forecast(auto.arima((Y_gdp[1:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]] %>% ts(frequency = 4, start = c(2020, 1)) 
x <- Y_gdp[1:200,]
x <- ts(x, frequency = 4, start = c(1970, 1))
fore_gdp <- coalesce(ts.union(x, f_Y_gdp_post_covid)[,1], ts.union(x, f_Y_gdp_post_covid)[,2])
plot.ts((Y_gdp))
lines(data.frame(fore_gdp), col = "red")

################################################################################
##  Using only from Q4 1999 ## This has the benefit of better fitting line ###
##  ARIMA(5, 1, 0) #####
#############################################################################
## https://www.cityam.com/brexit-and-pandemic-have-cost-uk-businesses-250bn-each-but-eu-departure-tally-now-rising-faster-than-covid-disruption/

forecast(auto.arima((Y_gdp[120:200,]), seasonal = TRUE), h = 10) %>% checkresiduals()
##Plot of ARIMA(5, 1, 0)
plot(forecast(auto.arima((Y_gdp[120:200,]), seasonal = TRUE), h = 10))

#Area of the forecasting trend without Covid:
data.frame(forecast(auto.arima((Y_gdp[120:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]] %>% sum()
#Area of the realistic trend with Covid:
Y_gdp[201:210,] %>% sum() 

#GDP lost due to Covid: (losing  -205,631.7 Million)
Y_gdp[201:210,] - data.frame(forecast(auto.arima((Y_gdp[120:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]]
(Y_gdp[201:210,] - data.frame(forecast(auto.arima((Y_gdp[120:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]])[1:7] %>% sum()


#plot of two graphs to show the Synthetic Counterfactual:
f_Y_gdp_post_covid <-data.frame(forecast(auto.arima((Y_gdp[120:200,]), seasonal = TRUE), h = 10))[["Point.Forecast"]] %>% ts(frequency = 4, start = c(2020, 1)) 
x <- Y_gdp[120:200,]
x <- ts(x, frequency = 4, start = c(1999, 4))
fore_gdp <- coalesce(ts.union(x, f_Y_gdp_post_covid)[,1], ts.union(x, f_Y_gdp_post_covid)[,2])
plot.ts((Y_gdp[120:210,]))
lines(data.frame(fore_gdp), col = "red")