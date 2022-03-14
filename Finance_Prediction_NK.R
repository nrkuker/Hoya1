# WEATHER TIME SERIES


pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, tidyquant, lubridate,
  stargazer, data.table, gridExtra,
  forecast, scales, tseries, tibbletime,
  lmtest, itsmr, here, fpp2,
  vars, MTS, car, caret,
  MLmetrics, imputeTS,
  psych,        # EDA
  visdat,       # missingness
  corrplot,     # correlation plot
  FactoMineR,   # EDA, PCA, MFA
  factoextra    # extract and visualize PCA/MFA
)


financial <- import("data/Crude3.csv", na.strings = "") %>% clean_names()
financial$date %<>% lubridate::as_date(format = "%m/%d/%Y")
financial$usd_fx_index %<>% as.numeric()

# Date order
financial %<>% arrange(date)

# Interpolate missing
financial$s_p_close %<>% na_interpolation(option = "linear")
financial$usd_fx_index %<>% na_interpolation(option = "linear")
financial$dow_dji_close %<>% na_interpolation(option = "linear")
financial$emerging_market_etf %<>% na_interpolation(option = "linear")


# VISUALIZE

# scale and graph
financial %>% mutate(across(avg_price_all:emerging_market_etf, scale)) %>% 
  pivot_longer(cols = avg_price_all:emerging_market_etf, 
               names_to = "index", values_to = "price") %>% 
  ggplot(aes(date, price, color = index)) + 
  geom_line()

# scale and graph, faceted
financial %>% mutate(across(avg_price_all:emerging_market_etf, scale)) %>% 
  pivot_longer(cols = avg_price_all:emerging_market_etf, 
               names_to = "index", values_to = "price") %>% 
  ggplot(aes(date, price, color = index)) + 
  geom_line() + facet_wrap(~index)

# graph, no DOW/S&P
financial %>% #mutate(across(avg_price_all:emerging_market_etf, scale)) %>% 
  pivot_longer(cols = avg_price_all:emerging_market_etf, 
               names_to = "index", values_to = "price") %>% 
  filter(!index %in% c("dow_dji_close", "s_p_close")) %>%
  ggplot(aes(date, price, color = index)) + 
  geom_line()


# NOTES:
  # Futures index doesn't correlate with the other 4
  # Other 4 indices seem to move together



#ANALYZE TIME SERIES DATA




#2. Plot monthly sales data
ggplot(financial, aes(date, avg_price_all)) +
  geom_line(na.rm = T) + # identical to na.rm=F
  xlab("Date") + ylab("Price") + labs(title = "Avg Price All") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("3 month")) +
  stat_smooth(colour = "green")

ggplot(financial, aes(date, s_p_close)) +
  geom_line(na.rm = T) + # identical to na.rm=F
  xlab("Date") + ylab("Price") + labs(title = "S&P Close") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("3 month")) +
  stat_smooth(colour = "green")

ggplot(financial, aes(date, dow_dji_close)) +
  geom_line(na.rm = T) + # identical to na.rm=F
  xlab("Date") + ylab("Price") + labs(title = "DOW Close") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("3 month")) +
  stat_smooth(colour = "green")

ggplot(financial, aes(date, emerging_market_etf)) +
  geom_line(na.rm = T) + # identical to na.rm=F
  xlab("Date") + ylab("Price") + labs(title = "Emerging Market ETF") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("3 month")) +
  stat_smooth(colour = "green")

ggplot(financial, aes(date, usd_fx_index)) +
  geom_line(na.rm = T) + # identical to na.rm=F
  xlab("Date") + ylab("Price") + labs(title = "Future Index") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("3 month")) +
  stat_smooth(colour = "green")

# all
financial %>% mutate(across(avg_price_all:emerging_market_etf, scale)) %>% 
  pivot_longer(cols = avg_price_all:emerging_market_etf, 
               names_to = "index", values_to = "price") %>% 
  # filter(index %in% c("dow_dji_close", "s_p_close")) %>%
  ggplot(aes(date, price, group = index)) + 
  geom_line(aes(color = index), alpha = 0.4) +
  xlab("Date") + ylab("Price (Z Scaled)") + labs(title = "Financial Indices Over Time") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("4 month")) +
  stat_smooth(aes(colour = index))








financial_diffs <- data.frame(financial[, 2] %>% diff(lag = 1) %>% quantile(probs = seq(0, 1, 0.05)),
                              financial[, 3] %>% diff(lag = 1) %>% quantile(probs = seq(0, 1, 0.05)),
                              financial[, 4] %>% diff(lag = 1) %>% quantile(probs = seq(0, 1, 0.05)),
                              financial[, 5] %>% diff(lag = 1) %>% quantile(probs = seq(0, 1, 0.05)),
                              financial[, 6] %>% diff(lag = 1) %>% quantile(probs = seq(0, 1, 0.05)))
colnames(financial_diffs) <- colnames(financial)[-1]
# write.csv(financial_diffs, file = "financial_diffs.csv")





#Converting data into a time series object
financial_ts <-ts(financial[,-1])
# only include the values, but doesn't know the interval

head(financial_ts)

#Other Time Series Plots
#Plot time series with trend line
plot(financial_ts, col = "blue", main = "Financial Time Series Data")
# abline(reg=lm(financial_ts~time(financial_ts)), col="lightgray") #plotting the trend line

#Autocorrelation and Partial Autocorrelation Plots
Acf(financial_ts)
Pacf(financial_ts)
#Lag plot of Data
gglagplot(financial_ts[,1], set.lags=1:16)
gglagplot(financial_ts[,2], set.lags=1:16)
gglagplot(financial_ts[,3], set.lags=1:16)
gglagplot(financial_ts[,4], set.lags=1:16)
gglagplot(financial_ts[,5], set.lags=1:16)
# what does this tell us?
# tighter to the diag means more correlation?
# tightness appears to decrease with increasing lags
# no apparent multiples where tightness increases again

#The ACF plots test if an individual lag autocorrelation is different than zero. 
#An alternative approach is to use the Ljung-Box test, 
#which tests whether any of a group of autocorrelations of a time series are different from zero. 
#In essence it tests the “overall randomness” based on a number of lags.
#If the result is a small p-value than it indicates the data are probably not white noise.
#Ljung-Box test on the first 24 Lag autocorrelations

Box.test(financial_ts[,1], lag=24, fitdf=0, type="Lj")
Box.test(financial_ts[,2], lag=24, fitdf=0, type="Lj")
Box.test(financial_ts[,3], lag=24, fitdf=0, type="Lj")
Box.test(financial_ts[,4], lag=24, fitdf=0, type="Lj")
Box.test(financial_ts[,5], lag=24, fitdf=0, type="Lj")
# pval signif so data not white noise

###DECOMPOSING THE TIME SERIES (additive)
#Converting data into a time series object by year
financial_tsd <-ts(financial_ts[,1:5], frequency=365)
# 12 obs per unit time makes the unit time 1 year
plot(financial_tsd, col = "blue", main = "Financial Time Series Data")

class(financial_tsd)
component.ts <- decompose(financial_tsd[,1])
plot(component.ts)

#Or use the following
component.tsa  <- decompose(financial_tsd[,1], type="additive", filter=NULL)
plot(component.tsa)

#######################################
# TESTING DIFFERENT SEASONALITY
financial_ts <-ts(financial_tsd, frequency=365)

Acf(financial_ts)
Pacf(financial_ts)
gglagplot(financial_ts[,1], set.lags=1:16)





#######################################
##EXTRA CODE
#For multiplicative decomposition use the following code
component.tsm2 = decompose(financial_tsd[,1], type="multiplicative", filter=NULL)
plot(component.tsm2)

#OR

component.tsm2 = decompose(log(financial_tsd[,1]))
plot(component.tsm2)

#If you are using multiplicative decomposition
#In all subsequent modeling purposes, use the log of the variable
######################################

#We can also use STL (Seasonal and Trend Decomposition using LOESS)
#LOESS is a method for estimating nonlinear relationships
#Advantage of LOESS: handle any type of seasonality, seasonal component changes over time, 
#smoothness of trend can be controlled by user and can be robust to outliers

financial_tsd[,1] %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

financial_tsd[,2] %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

financial_tsd[,3] %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

financial_tsd[,4] %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

financial_tsd[,5] %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

# But ARE you actually seasonal?

# #tsclean() identifies and replaces outliers using series smoothing and decomposition.
# #tsclean() can also impute missing values in the series if there are any
# #We are using the ts() command to create a time series object to pass to tsclean()
# auto$csales <-tsclean(auto_ts)
# 
# #Plot the cleaned data
# c_ts_plot <- ggplot(auto, aes(DATE,csales)) + geom_line(na.rm=TRUE) + 
#   xlab("Month") + ylab("Auto Sales in Thousands") + 
#   scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + 
#   stat_smooth(colour="green")
# c_ts_plot
# 
# #Lets compare both cleaned and uncleaned plots
# grid.arrange(ts_plot,c_ts_plot,ncol=1, top = textGrob("Uncleaned vs Cleaned Series"))
# 
# #Smoothing the time series and looking at the decomposed time series again
# my_ts <- ts(na.omit(auto$csales), frequency = 12)
# plot(my_ts)
# 
# component.ts2 = decompose(my_ts)
# plot(component.ts2)
# 
# #3. Naive Forecasting Method (for the next 24 months after observed data)
# naive_forecast <-naive(auto_ts, 24)
# summary(naive_forecast)
# autoplot(naive_forecast)
# 
# #Check for fitted values and residuals
# checkresiduals(naive_forecast)
# 
# #4. Smoothing the Series to uncover patterns in data
# #4.1 Moving Averages
# #MA of order 5 (generally of odd numbers)
# 
# auto_ma<-ma(auto_ts, 5)
# autoplot(auto_ts, series="Data") +
#   autolayer(ma(auto_ts,5), series="5-MA") +
#   xlab("Year") + ylab("Sales") +
#   ggtitle("Auto Sales Moving Average - 5 months") +
#   scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
#                       breaks=c("Data","5-MA"))
# 
# #MA of order 3
# autoplot(auto_ts, series="Data") +
#   autolayer(ma(auto_ts,3), series="3-MA") +
#   xlab("Year") + ylab("Sales") +
#   ggtitle("Auto Sales Moving Average - 3 months") +
#   scale_colour_manual(values=c("Data"="grey50","3-MA"="red"),
#                       breaks=c("Data","3-MA"))
# 
# #MA of order 9
# autoplot(auto_ts, series="Data") +
#   autolayer(ma(auto_ts,9), series="9-MA") +
#   xlab("Year") + ylab("Sales") +
#   ggtitle("Auto Sales Moving Average - 9 months") +
#   scale_colour_manual(values=c("Data"="grey50","9-MA"="red"),
#                       breaks=c("Data","9-MA"))
# 
# #Moving Average of Moving Averages (only for even order moving average to make them symmetric)
# #A 2x4 moving average
# 
# autoplot(auto_ts, series = "Data") + 
#   autolayer(ma(auto_ts, order = 4, centre = TRUE), series = "2x4-MA") +
#   labs(x = "Year", y = "Sales") + 
#   ggtitle("2x4 moving average of autosales")
# 
# #Removing Seasonal effects (if it is there- say a 1 year seasonal variation)
# autoplot(auto_ts, series = "Data") + 
#   autolayer(ma(auto_ts, 12), series = "12-MA") +
#   labs(x = "Year", y = "Sales") + 
#   ggtitle("12-month moving average of autosales") +
#   scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
#                       breaks=c("Data","12-MA"))
# 
# #4.2 Exponential Smoothing Models
# #Simple exponential smoothing used only for models that dont have any trend or Seasonality
# auto_ses <-ses(auto_ts, alpha=0.2, h=24)
# autoplot(auto_ses)
# 
# #We can remove the trend simply by differencing the data
# auto_dif <-diff(auto_ts)
# autoplot(auto_dif)
# 
# #Once we’ve differenced we’ve effectively removed the trend from our data and can reapply the SES model
# auto_ses2 <-ses(auto_dif, alpha=0.2, h=24)
# autoplot(auto_ses2)

my_ts <- ts(financial_tsd[,1])

#5. Making the series stationary (identify level of differencing required) 
#we need to remove trend by using appropriate order of difference and make the series stationary. 
#We do this by looking at acf, Dickey-Fuller Test and standard deviation.
#DICKEY FULLER TEST 
#(We have to test if Rho - 1 is significantly different than zero or not. 
#If the null hypothesis gets rejected, we’ll get a stationary time series.)
#First, confirm that the series is non-stationary using augmented DF test
adf.test(my_ts)

#To convert series to stationary, we need to know the level of differencing required
#Look at ACF (autocorrelation plot for the series to identify the order of differencing required)
Acf(my_ts)
Pacf(my_ts)

#6. Forecasting with ARIMA Model
#using differencing: lets try order 1 difference
#We will fit ARIMA(0,d,0)(0,D,0)[12] models 
#and verify acf residuals to find which ‘d’ or ‘D’ order of differencing is appropriate in our case.
#Applying only one order of difference i.e ARIMA(0,1,0)(0,0,0)
dfit1 <-arima(my_ts, order=c(0,1,0))
plot(residuals(dfit1))

Acf(residuals(dfit1))
Pacf(residuals(dfit1))

#Because the seasonal pattern is strong and stable, 
#we will want to use an order of seasonal differencing in the model. 
#Before that let’s try only with one seasonal difference i.e ARIMA(0,0,0)(0,1,0)

dfit2 <- arima(my_ts, order =c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit2))
Acf(residuals(dfit2))
Pacf(residuals(dfit2))

#lets try and apply both seasonal and non-seasonal differencing, ARIMA(0,1,0)(0,1,0)[12]
dfit3 <- arima(my_ts, order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit3))
Acf(residuals(dfit3))
Pacf(residuals(dfit3))

#Since first ACF is -ve and most of the positive correlations are now negative (series is overdifferenced)
#we should add an MA term to the model but to know what order of MA we need,
#check the standard deviation of the models (sd=RMSE) 
summary(dfit1)
summary(dfit2)
summary(dfit3)

#We have over-differencing, so we will stop here, 
#Out of the above, dfit3 model, i.e., ARIMA(0,1,0)(0,1,0)12 has the lowest standard deviation(RMSE) and AIC. 
#Therefore, it is the correct order of differencing.
#Now, we need to identify AR/MA and SAR/SMA values and fit the model

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


financial_ts <-ts(financial[,-1])
my_ts <- ts(financial_ts)

adf.test(my_ts)
Acf(my_ts)
Pacf(my_ts)



#Check Minimum AIC and Iterate
#We use the auto.arima() function to let R build our model with least AIC
#this function will search through combination of order parameters and provide best set
#by default it looks at maximum order of size 5 
dfit1 <- auto.arima(financial_ts[,1], seasonal = FALSE, trace = TRUE, stepwise = FALSE)
plot(residuals(dfit1))
adf.test(residuals(dfit1))
Acf(residuals(dfit1))
Pacf(residuals(dfit1))
summary(dfit1)
coeftest(dfit1)

dfit2 <- auto.arima(financial_ts[,2], seasonal = FALSE, trace = TRUE, stepwise = FALSE)
plot(residuals(dfit2))
adf.test(residuals(dfit2))
Acf(residuals(dfit2))
Pacf(residuals(dfit2))
summary(dfit2)
coeftest(dfit2)

dfit3 <- auto.arima(financial_ts[,3], seasonal = FALSE, trace = TRUE, stepwise = FALSE)
plot(residuals(dfit3))
adf.test(residuals(dfit3))
Acf(residuals(dfit3))
Pacf(residuals(dfit3))
summary(dfit3)
coeftest(dfit3)

dfit4 <- auto.arima(financial_ts[,4], seasonal = FALSE, trace = TRUE, stepwise = FALSE)
plot(residuals(dfit4))
adf.test(residuals(dfit4))
Acf(residuals(dfit4))
Pacf(residuals(dfit4))
summary(dfit4)
coeftest(dfit4)

dfit5 <- auto.arima(financial_ts[,5], seasonal = FALSE, trace = TRUE, stepwise = FALSE)
plot(residuals(dfit5))
adf.test(residuals(dfit5))
Acf(residuals(dfit5))
Pacf(residuals(dfit5))
summary(dfit5)
coeftest(dfit5)

# 2 & 4 (S&P and Dow) have much higher error metrics


#7. Model Validation (n-fold holdout method)
hold <- window(ts(financial_ts), start = 856)

#we will forecast data for the last two years (month = 233 to 256)
fit_predicted1 <- arima(ts(financial_ts[-c(856:866),1]), order = c(dfit1$arma[1], dfit1$arma[6], dfit1$arma[2]))
fit_predicted2 <- arima(ts(financial_ts[-c(856:866),2]), order = c(dfit2$arma[1], dfit2$arma[6], dfit2$arma[2]))
fit_predicted3 <- arima(ts(financial_ts[-c(856:866),3]), order = c(dfit3$arma[1], dfit3$arma[6], dfit3$arma[2]))
fit_predicted4 <- arima(ts(financial_ts[-c(856:866),4]), order = c(dfit4$arma[1], dfit4$arma[6], dfit4$arma[2]))
fit_predicted5 <- arima(ts(financial_ts[-c(856:866),5]), order = c(dfit5$arma[1], dfit5$arma[6], dfit5$arma[2]))

#use the model to forecast values for last 24 months. 
#Specify forecast horizon h periods ahead of prediction to be made 
#and use the fitted model to generate those predictions

forecast_pred1 <- forecast::forecast(fit_predicted1, h=11, level=c(75,95))
plot(forecast_pred1, main="Financial ARIMA Model", xlim=c(767,867)) #
lines(ts(financial_ts[,1]))

forecast_pred2 <- forecast::forecast(fit_predicted2, h=11, level=c(75,95))
plot(forecast_pred2, main="Financial ARIMA Model", xlim=c(767,867)) #
lines(ts(financial_ts[,2]))

forecast_pred3 <- forecast::forecast(fit_predicted3, h=11, level=c(75,95))
plot(forecast_pred3, main="Financial ARIMA Model", xlim=c(767,867)) #
lines(ts(financial_ts[,3]))

forecast_pred4 <- forecast::forecast(fit_predicted4, h=11, level=c(75,95))
plot(forecast_pred4, main="Financial ARIMA Model", xlim=c(767,867)) #
lines(ts(financial_ts[,4]))

forecast_pred5 <- forecast::forecast(fit_predicted5, h=11, level=c(75,95))
plot(forecast_pred5, main="Financial ARIMA Model", xlim=c(767,867)) #
lines(ts(financial_ts[,5]))







#8. Forecasting ####
#Next step is to forecast the sales for another 24 months ahead of time. 

forecast_pred1 <- forecast::forecast(dfit1, h=14, level=c(75,99))
forecast_pred2 <- forecast::forecast(dfit2, h=14, level=c(75,99))
forecast_pred3 <- forecast::forecast(dfit3, h=14, level=c(75,99))
forecast_pred4 <- forecast::forecast(dfit4, h=14, level=c(75,99))
forecast_pred5 <- forecast::forecast(dfit5, h=14, level=c(75,99))



finance_pred1 <-
  data.frame(
    date = seq.Date(from = tail(financial$date, 1) + 1, by = 1, length.out = 14),
    mean = forecast_pred1$mean %>% as.numeric(),
    lower75 = forecast_pred1$lower[, "75%"] %>% as.numeric(),
    lower99 = forecast_pred1$lower[, "99%"] %>% as.numeric(),
    upper75 = forecast_pred1$upper[, "75%"] %>% as.numeric(),
    upper99 = forecast_pred1$upper[, "99%"] %>% as.numeric()
  ) %>%
  mutate(
    mean_diff = c(NA_real_, diff(mean)),
    lower75_diff = c(NA_real_, diff(lower75)),
    lower99_diff = c(NA_real_, diff(lower99)),
    upper75_diff = c(NA_real_, diff(upper75)),
    upper99_diff = c(NA_real_, diff(upper99))
  )

finance_pred2 <-
  data.frame(
    date = seq.Date(from = tail(financial$date, 1) + 1, by = 1, length.out = 14),
    mean = forecast_pred2$mean %>% as.numeric(),
    lower75 = forecast_pred2$lower[, "75%"] %>% as.numeric(),
    lower99 = forecast_pred2$lower[, "99%"] %>% as.numeric(),
    upper75 = forecast_pred2$upper[, "75%"] %>% as.numeric(),
    upper99 = forecast_pred2$upper[, "99%"] %>% as.numeric()
  ) %>%
  mutate(
    mean_diff = c(NA_real_, diff(mean)),
    lower75_diff = c(NA_real_, diff(lower75)),
    lower99_diff = c(NA_real_, diff(lower99)),
    upper75_diff = c(NA_real_, diff(upper75)),
    upper99_diff = c(NA_real_, diff(upper99))
  )

finance_pred3 <-
  data.frame(
    date = seq.Date(from = tail(financial$date, 1) + 1, by = 1, length.out = 14),
    mean = forecast_pred3$mean %>% as.numeric(),
    lower75 = forecast_pred3$lower[, "75%"] %>% as.numeric(),
    lower99 = forecast_pred3$lower[, "99%"] %>% as.numeric(),
    upper75 = forecast_pred3$upper[, "75%"] %>% as.numeric(),
    upper99 = forecast_pred3$upper[, "99%"] %>% as.numeric()
  ) %>%
  mutate(
    mean_diff = c(NA_real_, diff(mean)),
    lower75_diff = c(NA_real_, diff(lower75)),
    lower99_diff = c(NA_real_, diff(lower99)),
    upper75_diff = c(NA_real_, diff(upper75)),
    upper99_diff = c(NA_real_, diff(upper99))
  )

finance_pred4 <-
  data.frame(
    date = seq.Date(from = tail(financial$date, 1) + 1, by = 1, length.out = 14),
    mean = forecast_pred4$mean %>% as.numeric(),
    lower75 = forecast_pred4$lower[, "75%"] %>% as.numeric(),
    lower99 = forecast_pred4$lower[, "99%"] %>% as.numeric(),
    upper75 = forecast_pred4$upper[, "75%"] %>% as.numeric(),
    upper99 = forecast_pred4$upper[, "99%"] %>% as.numeric()
  ) %>%
  mutate(
    mean_diff = c(NA_real_, diff(mean)),
    lower75_diff = c(NA_real_, diff(lower75)),
    lower99_diff = c(NA_real_, diff(lower99)),
    upper75_diff = c(NA_real_, diff(upper75)),
    upper99_diff = c(NA_real_, diff(upper99))
  )

finance_pred5 <-
  data.frame(
    date = seq.Date(from = tail(financial$date, 1) + 1, by = 1, length.out = 14),
    mean = forecast_pred5$mean %>% as.numeric(),
    lower75 = forecast_pred5$lower[, "75%"] %>% as.numeric(),
    lower99 = forecast_pred5$lower[, "99%"] %>% as.numeric(),
    upper75 = forecast_pred5$upper[, "75%"] %>% as.numeric(),
    upper99 = forecast_pred5$upper[, "99%"] %>% as.numeric()
  ) %>%
  mutate(
    mean_diff = c(NA_real_, diff(mean)),
    lower75_diff = c(NA_real_, diff(lower75)),
    lower99_diff = c(NA_real_, diff(lower99)),
    upper75_diff = c(NA_real_, diff(upper75)),
    upper99_diff = c(NA_real_, diff(upper99))
  )

# write.csv(finance_pred1, file = "avgprice_pred.csv", row.names = F)
# write.csv(finance_pred2, file = "s&p_pred.csv", row.names = F)
# write.csv(finance_pred3, file = "fx_pred.csv", row.names = F)
# write.csv(finance_pred4, file = "dow_pred.csv", row.names = F)
# write.csv(finance_pred5, file = "emergingmarketetf_pred.csv", row.names = F)



############################################################
#1. To improve the forecast, take only the most recent data from 2011 onwards
#2. Add covariates into the model
############################################################