#Code Book:
  
#Reading the data
  unemp<-read.csv("unemploy.csv")
  unemp_ts<- ts(unemp$UNEMPLOY, frequency=12,start=c(1948,1))
  unemp_numeric<-as.numeric(unemp_ts)
  
# Plot of the data Time Series
  library(astsa)
  dev.new(width=8, height=6)
  tsplot(unemp_ts, ylab="Unemployment Rate", xlab="Year", type="l", main="Monthly US Economic Data")

# ACF and PACF of x_t
  dev.new(width=8, height=6)
  acf2(unemp_numeric, max.lag=72, main="Monthly US Economic Data")
  #seasonal and non seasonal pattern there
  #Differencing of the Seasonal

# Plot of (1-B^12)*x_t 
  dev.new(width=8, height=6)
  tsplot(diff(unemp_ts, lag=12, differences=1), ylab=expression((1-B^12)*x[t]), xlab="Year", type="o", main=expression(paste("Plot of ", (1-B^12)*x[t])))
  #Diff only in seasonal doesnt work
  #Differencing of Both Seasonal and Non Seasonal - Variation , considerable

# Plot of (1-B)(1-B^12)*x_t 
  dev.new(width=8, height=6)
  tsplot(diff(diff(unemp_ts, lag=12, differences=1)), ylab=expression((1-B)(1-B^12)*x[t]), xlab="Year", type="l", main=expression(paste("Plot of ", (1-B)(1-B^12)*x[t])))
  #Differencing both seasonal and non seasonal works, with some considerable variations
  #There is a slight big vairation at 2021, during Pandemic, which is obvious
  #Differencing of Both Seasonal and Non Seasonal - Variation , considerable

# Plot of (1-B)(1-B^12)*x_t 
  dev.new(width=8, height=6)
  tsplot(diff(diff(unemp_ts, lag=12, differences=1)), ylab=expression((1-B)(1-B^12)*x[t]), xlab="Year", type="l", main=expression(paste("Plot of ", (1-B)(1-B^12)*x[t])))
  #Differencing both seasonal and non seasonal works, with some considerable variations
  #There is a slight big vairation at 2021, during Pandemic, which is obvious

# Plot of (1-B)(1-B^12)*x_t 
  dev.new(width=8, height=6)
  tsplot(diff(diff(sqrt(unemp_numeric), lag=12, differences=1)), ylab=expression((1-B)(1-B^12)*x[t]), xlab="Year", type="l", main=expression(paste("Plot of ", (1-B)(1-B^12)*x[t])))
  #Differencing both seasonal and non seasonal works, with some considerable variations
  #There is a slight big vairation at 2021, during Pandemic, which is obvious
  #using the transformation, still there is no much contraints on the variation, it remains the same

# ACF and PACF of (1-B)(1-B^12)*x_t
  dev.new(width=8, height=6)
  acf2(diff(diff(unemp_numeric, lag=12, differences=1)), max.lag=132, main=expression(paste("Est. ACF & PACF for ", (1-B)(1-B^12)*x[t], " data")))
  #good

# ACF and PACF of (1-B)(1-B^12)*x_t
  dev.new(width=8, height=6)
  acf2(diff(unemp_numeric, lag=12, differences=1), max.lag=132, main=expression(paste("Est. ACF & PACF for ", (1-B)(1-B^12)*x[t], " data")))
  #Initial Thoughts
  #Q=1, P=10
  #q= 3, p= 3
  #Final Models

#8
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 4, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.410 <- sarima(unemp_numeric, p=0, d=1, q=0, P=4, D=1, Q=0, S=12)
  mod.fit.010.410
  examine.mod(mod.fit.010.410, 0,1,0, 4,1,0, 12, lag.max=36)
  #good

#9
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 5, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.510 <- sarima(unemp_numeric, p=0, d=1, q=0, P=5, D=1, Q=0, S=12)
  mod.fit.010.510
  examine.mod(mod.fit.010.510, 0,1,0, 5,1,0, 12, lag.max=36)
  #good

#10
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 6, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.610 <- sarima(unemp_numeric, p=0, d=1, q=0, P=6, D=1, Q=0, S=12)
  mod.fit.010.610
  examine.mod(mod.fit.010.610, 0,1,0, 6,1,0, 12, lag.max=36)
  #good

#17
  # Fit models and examine diagnostics
  # initial thought: p = 2, q = 2, P = 0, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.212.011 <- sarima(unemp_numeric, p=2, d=1, q=2, P=0, D=1, Q=1, S=12)
  mod.fit.212.011
  examine.mod(mod.fit.212.011, 2,1,2, 0,1,1, 12, lag.max=36)
  #good model

#18
  # Fit models and examine diagnostics
  # initial thought: p = 1, q = 1, P = 0, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.111.011 <- sarima(unemp_numeric, p=1, d=1, q=1, P=0, D=1, Q=1, S=12)
  mod.fit.111.011
  examine.mod(mod.fit.111.011, 1,1,1, 0,1,1, 12, lag.max=36)
  #Okay except for LungBox

# Create data frame with model information  mod.fit.212.011 mod.fit.111.011
  df <- data.frame(mod.name=c("ARIMA(2,1,2)x(0,1,1)_12","ARIMA(1,1,1)x(0,1,1)_12","ARIMA(0,1,0)x(4,1,0)_12","ARIMA(0,1,0)x(5,1,0)_12","ARIMA(0,1,0)x(6,1,0)_12"),
                   AIC = c(mod.fit.212.011$ICs["AIC"], mod.fit.111.011$ICs["AIC"], mod.fit.010.410$ICs["AIC"], mod.fit.010.510$ICs["AIC"], mod.fit.010.610$ICs["AIC"]),
                   AICc = c(mod.fit.212.011$ICs["AICc"], mod.fit.111.011$ICs["AICc"],mod.fit.010.410$ICs["AICc"], mod.fit.010.510$ICs["AICc"], mod.fit.010.610$ICs["AICc"]),
                   BIC = c(mod.fit.212.011$ICs["BIC"], mod.fit.111.011$ICs["BIC"],mod.fit.010.410$ICs["BIC"], mod.fit.010.510$ICs["BIC"], mod.fit.010.610$ICs["BIC"]))

# Print the data frame
  print(df)

# Forecasts 24 time periods into the future
  dev.new(width=8, height=6)
  fore.mod <- sarima.for(unemp_ts, n.ahead=24, p=2, d=1, q=2, P=0, D=1, Q=1, S=12, plot.all=TRUE)
  fore.mod
  pred.mod <- unemp_ts - ts(mod.fit.212.011$fit$residuals, frequency=12, start=c(1948,1))
  dev.new(width=8, height=6)
  tsplot(unemp_ts, ylab="unemp_ts", xlab="Year", type="o", main="Monthly US Economic Data")
  lines(pred.mod, col="red", type="o", pch=17) 
  legend("topright", legend=c("Observed", "Forecast"), lty=c("solid", "solid"), col=c("black", "red"), pch=c(1, 17), bty="n")

#Extras:

#3
    # Fit models and examine diagnostics
    # initial thought: p = 0, q = 0, P = 1, Q = 0
    source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.110 <- sarima(unemp_numeric, p=0, d=1, q=0, P=1, D=1, Q=0, S=12)
  mod.fit.010.110
  examine.mod(mod.fit.010.110, 0,1,0, 1,1,0, 12, lag.max=60)
  #outliers, LS 

#4
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 0, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.011 <- sarima(unemp_numeric, p=0, d=1, q=0, P=0, D=1, Q=1, S=12)
  mod.fit.010.011
  examine.mod(mod.fit.010.011, 0,1,0, 0,1,1, 12, lag.max=36)
  #outliers, LS 

#6
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 2, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.210 <- sarima(unemp_numeric, p=0, d=1, q=0, P=2, D=1, Q=0, S=12)
  mod.fit.010.210
  examine.mod(mod.fit.010.210, 0,1,0, 2,1,0, 12, lag.max=36)
  #outliers, LS 

#7
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 3, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.310 <- sarima(unemp_numeric, p=0, d=1, q=0, P=3, D=1, Q=0, S=12)
  mod.fit.010.310
  examine.mod(mod.fit.010.310, 0,1,0, 3,1,0, 12, lag.max=36)
  #outliers, LS 
  #Not significant

# Fit models and examine diagnostics
# initial thought: p = 0, q = 0, P = 10, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.1011 <- sarima(unemp_numeric, p=0, d=1, q=0, P=10, D=1, Q=1, S=12)
  mod.fit.010.1011
  examine.mod(mod.fit.010.1011, 0,1,0, 10,1,1, 12, lag.max=60)

#2
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 10, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.1010 <- sarima(unemp_numeric, p=0, d=1, q=0, P=10, D=1, Q=0, S=12)
  mod.fit.010.1010
  examine.mod(mod.fit.010.1010, 0,1,0, 10,1,0, 12, lag.max=60)

#5
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 1, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.111 <- sarima(unemp_numeric, p=0, d=1, q=0, P=1, D=1, Q=1, S=12)
  mod.fit.010.111
  examine.mod(mod.fit.010.111, 0,1,0, 1,1,1, 12, lag.max=36)
  #not Significant ar1

#11
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 7, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.710 <- sarima(unemp_numeric, p=0, d=1, q=0, P=7, D=1, Q=0, S=12)
  mod.fit.010.710
  examine.mod(mod.fit.010.710, 0,1,0, 7,1,0, 12, lag.max=36)

#12
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 8, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.810 <- sarima(unemp_numeric, p=0, d=1, q=0, P=8, D=1, Q=0, S=12)
  mod.fit.010.810
  examine.mod(mod.fit.010.810, 0,1,0, 8,1,0, 12, lag.max=36)

#13
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 9, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.910 <- sarima(unemp_numeric, p=0, d=1, q=0, P=9, D=1, Q=0, S=12)
  mod.fit.010.910
  examine.mod(mod.fit.010.910, 0,1,0, 9,1,0, 12, lag.max=36)

#14
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 0, P = 2, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.010.211 <- sarima(unemp_numeric, p=0, d=1, q=0, P=2, D=1, Q=1, S=12)
  mod.fit.010.211
  examine.mod(mod.fit.010.211, 0,1,0, 2,1,1, 12, lag.max=36)

#15
  # Fit models and examine diagnostics
  # initial thought: p = 1, q = 1, P = 1, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.111.111 <- sarima(unemp_numeric, p=1, d=1, q=1, P=1, D=1, Q=1, S=12)
  mod.fit.111.111
  examine.mod(mod.fit.111.111, 1,1,1, 1,1,1, 12, lag.max=36)
  #SAR1 not sig
  #P=10,Q=1,p=2,q=2

#16
  # Fit models and examine diagnostics
  # initial thought: p = 2, q = 2, P = 1, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.212.111 <- sarima(unemp_numeric, p=2, d=1, q=2, P=1, D=1, Q=1, S=12)
  mod.fit.212.111
  examine.mod(mod.fit.212.111, 2,1,2, 1,1,1, 12, lag.max=36)
  #SAR1 not sig

#19
  # Fit models and examine diagnostics
  # initial thought: p = 1, q = 0, P = 0, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.110.011 <- sarima(unemp_numeric, p=1, d=1, q=0, P=0, D=1, Q=1, S=12)
  mod.fit.110.011
  examine.mod(mod.fit.110.011, 1,1,1, 0,1,1, 12, lag.max=36)
  #ar1 not significant

#20
  # Fit models and examine diagnostics
  # initial thought: p = 0, q = 1, P = 1, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.011.111 <- sarima(unemp_numeric, p=0, d=1, q=1, P=1, D=1, Q=1, S=12)
  mod.fit.011.111
  examine.mod(mod.fit.011.111, 0,1,1, 1,1,1, 12, lag.max=36)
  #ma1 AND sar1 not sig

#21
  # Fit models and examine diagnostics
  # initial thought: p = 1, q = 0, P = 1, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.110.111 <- sarima(unemp_numeric, p=1, d=1, q=0, P=1, D=1, Q=1, S=12)
  mod.fit.110.111
  examine.mod(mod.fit.110.111, 1,1,0, 1,1,1, 12, lag.max=36)
  #ar1 and sar1 not sig

#21
  # Fit models and examine diagnostics
  # initial thought: p = 1, q = 0, P = 1, Q = 0
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.110.110 <- sarima(unemp_numeric, p=1, d=1, q=0, P=1, D=1, Q=0, S=12)
  mod.fit.110.110
  examine.mod(mod.fit.110.111, 1,1,0, 1,1,0, 12, lag.max=36)
  #ar1 not sig
  # Create data frame with model information  mod.fit.212.011 mod.fit.111.011

#19
  # Fit models and examine diagnostics
  # initial thought: p = 3, q = 3, P = 0, Q = 1
  source("examine.mod.R")
  dev.new(width=8, height=6)
  mod.fit.313.011 <- sarima(unemp_numeric, p=3, d=1, q=3, P=0, D=1, Q=1, S=12)
  mod.fit.313.011
  examine.mod(mod.fit.313.011, 3,1,3, 0,1,1, 12, lag.max=36)
  #good model
  df <- data.frame(mod.name=c("ARIMA(2,1,2)x(0,1,1)_12","ARIMA(1,1,1)x(0,1,1)_12","ARIMA(0,1,0)x(1,1,0)_12", "ARIMA(0,1,0)x(0,1,1)_12","ARIMA(0,1,0)x(2,1,0)_12","ARIMA(0,1,0)x(3,1,0)_12","ARIMA(0,1,0)x(4,1,0)_12","ARIMA(0,1,0)x(5,1,0)_12","ARIMA(0,1,0)x(6,1,0)_12"),
                   AIC = c(mod.fit.212.011$ICs["AIC"], mod.fit.111.011$ICs["AIC"], mod.fit.010.110$ICs["AIC"], mod.fit.010.011$ICs["AIC"], mod.fit.010.210$ICs["AIC"], mod.fit.010.310$ICs["AIC"], mod.fit.010.410$ICs["AIC"], mod.fit.010.510$ICs["AIC"], mod.fit.010.610$ICs["AIC"]),
                   AICc = c(mod.fit.212.011$ICs["AICc"], mod.fit.111.011$ICs["AICc"],mod.fit.010.110$ICs["AICc"], mod.fit.010.011$ICs["AICc"],mod.fit.010.210$ICs["AICc"],mod.fit.010.310$ICs["AICc"],mod.fit.010.410$ICs["AICc"], mod.fit.010.510$ICs["AICc"], mod.fit.010.610$ICs["AICc"]),
                   BIC = c(mod.fit.212.011$ICs["BIC"], mod.fit.111.011$ICs["BIC"],mod.fit.010.110$ICs["BIC"], mod.fit.010.011$ICs["BIC"],mod.fit.010.210$ICs["BIC"],mod.fit.010.310$ICs["BIC"],mod.fit.010.410$ICs["BIC"], mod.fit.010.510$ICs["BIC"], mod.fit.010.610$ICs["BIC"]))'

# Print the data frame
print(df)
#leasst AIC, BIC, AICc 2,1,2 0,1,1 and 1,1,1 0,1,1

# Forecasts 24 time periods into the future
dev.new(width=8, height=6)
fore.mod <- sarima.for(unemp_ts, n.ahead=24, p=2, d=1, q=2, P=0, D=1, Q=1, S=12, plot.all=TRUE)
fore.mod

# Forecasts 24 time periods into the future
dev.new(width=8, height=6)
fore.mod <- sarima.for(unemp_ts, n.ahead=24, p=1, d=1, q=1, P=0, D=1, Q=1, S=12, plot.all=TRUE)
fore.mod

pred.mod <- unemp_ts - ts(mod.fit.212.011$fit$residuals, frequency=12, start=c(1948,1))
pred.mod <- unemp_ts - ts(mod.fit.111.011$fit$residuals, frequency=12, start=c(1948,1))
dev.new(width=8, height=6)

tsplot(unemp_ts, ylab="unemp_ts", xlab="Year", type="o", main="US UnEmployment Rate")
lines(pred.mod, col="red", type="o", pch=17) 
legend("topright", legend=c("Observed", "Forecast"), lty=c("solid", "solid"), col=c("black", "red"), pch=c(1, 17), bty="n")

