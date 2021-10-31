#package load
install.packages("read.xlsx")
pacman::p_load(read.xl,ggplot2,forecast,grid,Amelia,tseries,scales,gridExtra,lmtest,Rcpp,knitr)

#read the data
data = read_xlsx("C:/Users/Alec Snipes/Downloads/Historical Crude Price Data.xlsx")
summary(data)

#Missing data check
sum(is.na(data))
missmap(data ,main = "Missing Values",col = c('light blue','Blue'),x.cex = 1.5)


#convert the date field from character to date type
data$Period <- as.Date(data$Date,format = "%Y/%m/%d")
data$Value <- as.numeric(gsub(",","",data$`Brent M1`))
head(data)

#Plot sales data
ts_plot <- ggplot(data, aes(Period,Value)) + geom_line(na.rm=TRUE) + 
  xlab("Year") + ylab("Oil Prices") + 
  scale_x_date(labels = date_format(format= "%Y"),breaks = date_breaks("2 year")) + 
  stat_smooth(colour = "green")

ts_plot
class(data)

#Converting data into a time series object
data_ts <-ts(data[,c('Value')])
class(data_ts)

#Plot time series with trend line
plot(data_ts, col = "blue", main = "Oil Time Series Data")
abline(reg=lm(data_ts~time(data_ts)), col="lightgray") #plotting the trend line

#Autocorrelation and Partial Autocorrelation Plots
Acf(data_ts)
Pacf(data_ts)

#Lag plot of Data
gglagplot(data_ts, set.lags=1:16)

#Ljung-Box test on the first 24 Lag autocorrelations

Box.test(data_ts, lag=24, fitdf=0, type="Lj")

#The small p-value than it indicates the data are probably not white noise.

###DECOMPOSING THE TIME SERIES (Multi)

#Converting data into a time series object by year
data_tsd <-ts(data[,c('Value')],start=c(1992,1), frequency=12)
class(data_tsd)
component.ts = decompose(data_tsd, type = "multi")
component.ts$figure
plot(component.ts$figure, type="o")
component.ts$trend
plot(component.ts$trend[1:500])


#STL
data_tsd %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

#If seasonal variation is proportional to the level of the time series (if the variation changes over time) then use multi
#Variation does change over time so we use multi decomp

#Using the ts() command to create a time series object to pass to tsclean()
data$Value <-tsclean(data_ts)

#Plot the cleaned data
c_ts_plot <- ggplot(data, aes(Period,Value)) + geom_line(na.rm=TRUE) + 
  xlab("Year") + ylab("Oil Prices") + 
  scale_x_date(labels = date_format(format= "%Y"),breaks = date_breaks("2 year")) + 
  stat_smooth(colour="red")
c_ts_plot

#Compare both cleaned and uncleaned plots
grid.arrange(ts_plot,c_ts_plot,ncol=1, top = textGrob("Uncleaned vs Cleaned Series"))

#Cleaning the data removes the outliers to smooth the trend

#Smoothing the time series and looking at the decomposed time series again
my_ts <- ts(na.omit(data$Value), frequency = 12)
plot(my_ts)

component.ts2 = decompose(my_ts, type = "add")
plot(component.ts2)

#Staying with multi because the variation in the random component of is much smaller than the variation in the additive

#Naive Forecasting Method (for the next 24 months after observed data)
naive_forecast <-naive(data_ts, 24)
summary(naive_forecast)
autoplot(naive_forecast)

#Check for fitted values and residuals
checkresiduals(naive_forecast)

#Smoothing the Series to uncover patterns in data
#Moving Averages
#MA of order 5 (generally of odd numbers)

data_ma<-ma(data_ts, 5)
autoplot(data_ts, series="Data") +
  autolayer(ma(data_ts,5), series="5-MA") +
  xlab("Year") + ylab("Prices") +
  ggtitle("Oil Prices Moving Average - 5 months") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

#MA of order 3
autoplot(data_ts, series="Data") +
  autolayer(ma(data_ts,3), series="3-MA") +
  xlab("Year") + ylab("Sales") +
  ggtitle("Sales Moving Average - 3 months") +
  scale_colour_manual(values=c("Data"="grey50","3-MA"="red"),
                      breaks=c("Data","3-MA"))

#MA of order 9
autoplot(data_ts, series="Data") +
  autolayer(ma(data_ts,9), series="9-MA") +
  xlab("Year") + ylab("Price") +
  ggtitle("Oil Price Moving Average - 9 months") +
  scale_colour_manual(values=c("Data"="grey50","9-MA"="red"),
                      breaks=c("Data","9-MA"))

#Moving Average of Moving Averages (only for even order moving average to make them symmetric)
#A 2x4 moving average

autoplot(data_ts, series = "Data") + 
  autolayer(ma(data_ts, order = 4, centre = TRUE), series = "2x4-MA") +
  labs(x = "Year", y = "Price") + 
  ggtitle("2x4 moving average of oil price")

#Removing Seasonal effects (if it is there- say a 1 year seasonal variation)
autoplot(data_ts, series = "Data") + 
  autolayer(ma(data_ts, 12), series = "12-MA") +
  labs(x = "Months", y = "Oil Prices") + 
  ggtitle("12-month moving average of sales") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA"))

#Making the series stationary (identify level of differencing required) 
#remove trend by using appropriate order of difference and make the series stationary. 

#DICKEY FULLER TEST 
#(We have to test if Rho - 1 is significantly different than zero or not. 

#The time series is stationary because the p-value of .04 is low enough to reject the null hypothesis of a non-stationary times series

adf.test(my_ts)


#Look at ACF (autocorrelation plot for the series to identify the order of differencing required)
Acf(my_ts)
Pacf(my_ts)

#Forecasting with ARIMA Model
#using differencing: try order 1 difference
#ARIMA(0,d,0)(0,D,0)[12] models 
#verify acf residuals to find which 'd' or 'D' order of differencing is appropriate in our case.
#Applying only one order of difference i.e ARIMA(0,1,0)(0,0,0)

dfit1 <-arima(my_ts, order=c(0,1,0))
plot(residuals(dfit1))

Acf(residuals(dfit1))
Pacf(residuals(dfit1))

#Try only with one seasonal difference i.e ARIMA(0,0,0)(0,1,0)

dfit2 <- arima(my_ts, order =c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit2))
Acf(residuals(dfit2))
Pacf(residuals(dfit2))

#Apply both seasonal and non-seasonal differencing, ARIMA(0,1,0)(0,1,0)[12]
dfit3 <- arima(my_ts, order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit3))
Acf(residuals(dfit3))
Pacf(residuals(dfit3))

#check the standard deviation of the models (sd=RMSE) 
summary(dfit1)
summary(dfit2)
summary(dfit3)

#Look for which model has the lowest standard deviation(RMSE) and AIC. 
#DFit3 has the lowest std dev and AIC, it is the correct order of differencing.


#Identify AR/MA and SAR/SMA values and fit the model

dfit4 <- arima(my_ts, order =c(0,1,1), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit4))
Acf(residuals(dfit4))
Pacf(residuals(dfit4))

#Add a one-order MA component to the seasonal part and see what we get
dfit5 <- arima(my_ts, order =c(0,1,0), seasonal = list(order = c(0,1,1), period = 12))
plot(residuals(dfit5))
Acf(residuals(dfit5))
Pacf(residuals(dfit5))

#combine a MA component to non-seasonal and one to seasonal
dfit6 <- arima(my_ts, order =c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
plot(residuals(dfit6))
Acf(residuals(dfit6))
Pacf(residuals(dfit6))

#Pending statistically significant MA coefficient and low AIC the model seems a good fit
summary(dfit4)
summary(dfit5)
summary(dfit6)

#The coeftest() function in lmtest package can help us in getting the p-values of coefficients.
coeftest(dfit6)

#Check Minimum AIC and Iterate
#Use the auto.arima() function to let R build our model with least AIC

dfit7 <- auto.arima(my_ts, seasonal = TRUE)
plot(residuals(dfit7))
Acf(residuals(dfit7))
Pacf(residuals(dfit7))

summary(dfit7)
coeftest(dfit7)

#Model Validation (n-fold holdout method)
hold <- window(ts(my_ts), start =868)

#Forecast data for the last two years (month = 328 to 351)
fit_predicted <- arima(ts(my_ts[-c(868:1109)]), order =c(0,1,1), seasonal = list(order = c(0,1,2), period = 12))

#use the model to forecast values for last 24 months. 
#Specify forecast horizon h periods ahead of prediction to be made 
#and use the fitted model to generate those predictions

forecast_pred <- forecast(fit_predicted,h=24)
plot(forecast_pred, main="")
lines(ts(my_ts))

#Forecast the sales for another 24 months ahead of time. 
f_values <-forecast(dfit7, h=24)
plot(f_values, main="")

