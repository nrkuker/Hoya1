# Capstone EDA

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect,
  stargazer, data.table,
  forecast, scales, tseries,
  lmtest
)

data <- import("Capstone/data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated")
str(data)
colnames(data)
# limiting vars
data2 <- data[,c(1, 80:81, 87:101, 132:137)]
colnames(data2)




# UNIVARIATE TIME SERIES DECOMPOSITION ####
    # Reference Crude #

# plotting reference crude over time
ggplot(data, aes(Date, `M#DATED BRENT`)) + geom_line()

t0 <- data$`M#DATED BRENT`
t1 <- c(NA_real_, diff(t0, lag = 1))

# plotting 1st lag of reference crude over time
data.frame(cbind(Date = data$Date, diff = t1)) %>% 
  ggplot(aes(Date, diff)) + geom_line()


# data2[,4:18]
ref_ts <- ts(data2[,4], frequency = 365, start = c(2015, 299))
plot(ref_ts)

dfit7 <- auto.arima(ref_ts, seasonal = TRUE)
dfit7   # 1 order seasonal diff and non-seasonal diff
plot(residuals(dfit7))
Acf(residuals(dfit7))
Pacf(residuals(dfit7))
summary(dfit7)
coeftest(dfit7)



#we will forecast data for the last two years (month = 233 to 256)
fit_predicted <- arima(ts(ref_ts[-c(233:256)]), order =c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

#use the model to forecast values for last 24 months. 
#Specify forecast horizon h periods ahead of prediction to be made 
#and use the fitted model to generate those predictions

forecast_pred <- forecast(fit_predicted,h=24)
plot(forecast_pred, main="")
lines(ts(my_ts))






# 


