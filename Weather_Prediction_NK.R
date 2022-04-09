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


weather1 <- import("data/Historical Data 16-20.xlsx", 
                   col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "text")) %>% clean_names()
weather1 %<>% mutate(date_time = lubridate::as_date(date_time, format = "%m/%d/%Y"))

weather2 <- import("data/OPEC Weather Data.xlsx", 
                   col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "text")) %>% clean_names()
weather2 %<>% mutate(date_time = lubridate::as_date(date_time, format = "%m/%d/%Y"))

weather3 <- import("data/North Sea Weather Data - Bergen Norway.csv") %>% clean_names()
weather3 %<>% 
  dplyr::select(name, datetime, tempmax, tempmin, temp, precipprob, feelslike, 
                precip, snow, snowdepth, windspeed, winddir, windgust, visibility,
                cloudcover, humidity, conditions)
weather3 %<>% mutate(datetime = lubridate::as_date(datetime, format = "%m/%d/%y"))
# weather3 %<>% mutate(across(tempmax:temp, ~ (.x * 1.8)+32))
weather3[,3:16] <- sapply(weather3[,3:16], function(x) as.numeric(x))
colnames(weather3) <- colnames(weather1)


weather <- rbind(weather1, weather2, weather3) %>% 
  filter(name != "Asia, Lima, Perú") %>%    # filtering out fully blank rows
  filter(!is.na(maximum_temperature)) %>% 
  mutate(name = as.factor(name),
         conditions = as.factor(conditions))

weather %<>% 
  mutate(region = case_when(
    name == "Australia" ~ "AUS",
    name %like% "United States" ~ "NA",
    name == "South America- Ecuador" ~ "SA",
    name %in% c("Africa-Angola", "Africa-Nigera") ~ "AFR",
    name %in% c("Europe, Paris, Île-de-France, France", "Africa, Nocera Umbra, Umbria, Italia", "bergen, norway") ~ "EUR",
    name %in% c("Middle East-Saudi Arabia", "Middle East-Iran/Iraq") ~ "ME",
    TRUE ~ NA_character_
  ))

weather %<>% 
  mutate(weathersit = case_when(
    conditions %in% c("Clear") ~ "Clear",
    conditions %in% c("Overcast", "Partially cloudy") ~ "Cloudy",
    conditions %in% c("Rain", "Rain, Overcast", "Rain, Partially cloudy") ~ "Rain",
    conditions %in% c("Snow", "Snow, Overcast", "Snow, Partially cloudy") ~ "Snow",
    TRUE ~ NA_character_
  ))


wMin <- weather %>% 
  group_by(date_time) %>% 
  summarize(min = min(minimum_temperature))

wMinName <- left_join(
  wMin %>% mutate(index = str_c(date_time, min)),
  weather %>% 
    mutate(index = str_c(date_time, minimum_temperature)) %>% 
    dplyr::select(index, name),
  by = "index"
) %>% 
  # filter(between(date_time, lower = as.Date("2015-10-20"), upper = as.Date("2019-07-01"))) %>%
  filter(between(date_time, lower = as.Date("2019-04-25"), upper = as.Date("2019-05-14"))) %>%
  dplyr::select(-index)

# wMinName <- weather %>%
#   filter(name %in% c("South St W, Norwood Young America, MN 55368, United States")) %>%
#   dplyr::select(name, date_time, min = minimum_temperature)




#ANALYZE TIME SERIES DATA


#read the data
str(wMinName)
summary(wMinName)

#1. Missing data check
sum(is.na(wMinName))

Amelia::missmap(wMinName, main = "Missing Values",col = c('light blue','Blue'),x.cex = 1.5)


#first convert the date field from character to date type
wMinName$date_time <- as.Date(wMinName$date_time, "%m/%d/%Y")
head(wMinName)

#2. Plot monthly sales data
ts_plot <- 
  ggplot(wMinName, aes(date_time, min)) + 
  geom_line(na.rm = T) + # identical to na.rm=F
  xlab("Date") + ylab("Min Temp") + 
  scale_x_date(labels = date_format(format= "%b-%Y"), breaks = date_breaks("3 month")) + 
  stat_smooth(colour = "green")

ts_plot

wMinName %>% 
  # pivot_longer(cols = avg_price_all:emerging_market_etf, 
  #              names_to = "index", values_to = "price") %>% 
  # filter(index %in% c("dow_dji_close", "s_p_close")) %>%
  ggplot(aes(date_time, min)) + 
  geom_line(alpha = 0.4) +
  xlab("Date") + ylab("Minimum Temperature (Z Scaled)") + 
  labs(title = "Weather Over Time") +
  scale_x_date(labels = date_format(format = "%b-%Y"),
               breaks = date_breaks("4 month")) +
  stat_smooth()



class(wMinName)
#[1] "data.frame"

#Converting data into a time series object
weather_ts <-ts(wMinName[,c("min")])
# only include the values, but doesn't know the interval
class(weather_ts)
#[1] "ts"

head(weather_ts)

#Other Time Series Plots
#Plot time series with trend line
plot(weather_ts, col = "blue", main = "Weather Time Series Data")
abline(reg=lm(weather_ts~time(weather_ts)), col="lightgray") #plotting the trend line

#Autocorrelation and Partial Autocorrelation Plots
Acf(weather_ts)
Pacf(weather_ts)
#Lag plot of Data
gglagplot(weather_ts, set.lags=1:16)
# what does this tell us?
# tighter to the diag means more correlation?

#The ACF plots test if an individual lag autocorrelation is different than zero. 
#An alternative approach is to use the Ljung-Box test, 
#which tests whether any of a group of autocorrelations of a time series are different from zero. 
#In essence it tests the “overall randomness” based on a number of lags.
#If the result is a small p-value than it indicates the data are probably not white noise.
#Ljung-Box test on the first 24 Lag autocorrelations

Box.test(weather_ts, lag=24, fitdf=0, type="Lj")
# pval signif so data not white noise

###DECOMPOSING THE TIME SERIES (additive)
#Converting data into a time series object by year
weather_tsd <-ts(wMinName[,c("min")], frequency=365)
# 12 obs per unit time makes the unit time 1 year
plot(weather_tsd, col = "blue", main = "Weather Time Series Data")

class(weather_tsd)
component.ts <- decompose(weather_tsd)
plot(component.ts)

#Or use the following
component.tsa  <- decompose(weather_tsd, type="additive", filter=NULL)
plot(component.tsa)

#######################################
# TESTING DIFFERENT SEASONALITY
weather_ts <-ts(wMinName[,c("min")], frequency=365)

Acf(weather_ts)
Pacf(weather_ts)
gglagplot(weather_ts, set.lags=1:16)





#######################################
##EXTRA CODE
#For multiplicative decomposition use the following code
component.tsm2 = decompose(auto_tsd, type="multiplicative", filter=NULL)
plot(component.tsm2)

#OR

component.tsm2 = decompose(log(auto_tsd))
plot(component.tsm2)

#If you are using multiplicative decomposition
#In all subsequent modeling purposes, use the log of the variable
######################################

#We can also use STL (Seasonal and Trend Decomposition using LOESS)
#LOESS is a method for estimating nonlinear relationships
#Advantage of LOESS: handle any type of seasonality, seasonal component changes over time, 
#smoothness of trend can be controlled by user and can be robust to outliers

weather_tsd[,"min"] %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

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

my_ts <- ts(weather_tsd[,"min"], frequency = 365)

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

#Check Minimum AIC and Iterate
#We use the auto.arima() function to let R build our model with least AIC
#this function will search through combination of order parameters and provide best set
#by default it looks at maximum order of size 5 
my_ts <- ts(wMinName[,c("min")], frequency=365)

dfit7 <- auto.arima(my_ts, seasonal = TRUE, trace = TRUE, stepwise = FALSE)
plot(residuals(dfit7))
Acf(residuals(dfit7))
Pacf(residuals(dfit7))

summary(dfit7)
coeftest(dfit7)

dfit303 <- arima(my_ts, order = c(3,0,3), seasonal = list(order = c(0,1,0), period = 365))
dfit302 <- arima(my_ts, order = c(3,0,2), seasonal = list(order = c(0,1,0), period = 365))

summary(dfit303)
summary(dfit302)
coeftest(dfit303)
coeftest(dfit302)

plot(residuals(dfit303))
adf.test(residuals(dfit303))
Acf(residuals(dfit303))
Pacf(residuals(dfit303))
plot(residuals(dfit302))
adf.test(residuals(dfit302))
Acf(residuals(dfit302))
Pacf(residuals(dfit302))


#7. Model Validation (n-fold holdout method)
hold <- window(ts(my_ts), start = 1212)

#we will forecast data for the last two years (month = 233 to 256)
fit_predicted <- arima(ts(my_ts[-c(1212:1278)]), order =c(3,0,2), seasonal = list(order = c(0,1,0), period = 365))

#use the model to forecast values for last 24 months. 
#Specify forecast horizon h periods ahead of prediction to be made 
#and use the fitted model to generate those predictions

forecast_pred <- forecast::forecast(fit_predicted, h=67, level=c(75,95))
plot(forecast_pred, main="Weather ARIMA Model", xlim=c(1100,1278))
lines(ts(my_ts))

forecast_pred$mean
forecast_pred$lower %>% head()
forecast_pred$upper %>% head()

weather_pred <- data.frame(date = wMinName$date_time[1212:1278],
                           mean = forecast_pred$mean %>% as.numeric(),
                           lower75 = forecast_pred$lower[, "75%"] %>% as.numeric(),
                           lower95 = forecast_pred$lower[, "95%"] %>% as.numeric(),
                           upper75 = forecast_pred$upper[, "75%"] %>% as.numeric(),
                           upper95 = forecast_pred$upper[, "95%"] %>% as.numeric())
# write.csv(weather_pred, file = "weather_pred.csv", row.names = F)


#8. Forecasting
#Next step is to forecast the sales for another 24 months ahead of time. 
f_values <-forecast(dfit6, h=24)
plot(f_values, main="")

############################################################
#1. To improve the forecast, take only the most recent data from 2011 onwards
#2. Add covariates into the model
############################################################