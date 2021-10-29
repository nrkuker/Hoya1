# Capstone EDA

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, lubridate,
  stargazer, data.table,
  forecast, scales, tseries,
  lmtest,
  vars, MTS
)

data <- import("Capstone/data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated") %>% clean_names()
str(data)
colnames(data)
# limiting vars
data2 <- data[,c(1, 80:81, 87:101, 132:137)]
colnames(data2)


djia <- import("Capstone/data/djia.csv", skip = 15, colClasses = c("POSIXct", "numeric"))
str(djia)
djia$date[1:10]
djia$date <- force_tz(djia$date, "UTC")
djia$date[1:10]


# UNIVARIATE TIME SERIES DECOMPOSITION ####
    # Reference Crude #

# plotting reference crude over time
ggplot(data, aes(date, m_number_dated_brent)) + geom_line()

t0 <- data$m_number_dated_brent
t1 <- c(NA_real_, diff(t0, lag = 1))

# plotting 1st lag of reference crude over time
data.frame(cbind(date = data$date, diff = t1)) %>% 
  ggplot(aes(date, diff)) + geom_line()

# data2[,4:18]
ref_ts <- ts(data2[,4], frequency = 365, start = c(2015, 299))
plot(ref_ts)

dfit7 <- auto.arima(ref_ts, seasonal = TRUE)
dfit7   # 1 order seasonal diff and non-seasonal diff
plot(residuals(dfit7))
Acf(residuals(dfit7))
Pacf(residuals(dfit7))
adf.test(residuals(dfit7))
summary(dfit7)
coeftest(dfit7)


# use lagged values
ref_lag <- data2[,4] %>% diff(1)
ref_lag_ts <- ts(ref_lag, frequency = 365, start = c(2015, 299))
# TS not useful here








# holding out the last 20% of dates
fit_predicted <- arima(ts(ref_ts[-c(694:866)]), order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 365))

forecast_pred <- forecast(fit_predicted, h = 173)
plot(forecast_pred, main = "")
lines(ts(ref_ts))
# just doing the reference crude isn't enough
# add in DJIA

data2_djia <- left_join(data2, djia, by = "date") %>% 
  rename(djia = value)
sum(is.na(data2_djia))
visdat::vis_miss(data2_djia)

data2_djia[,c(1,4,25)] %>% filter(is.na(djia))

ref_ts <- ts(data2_djia[,c(4,25)], frequency = 365, start = c(2015, 299))
plot(ref_ts)
sum(is.na(ref_ts))



ts.d <- diffM(ref_ts, d = 1)
var.a <- vars::VAR(ts.d,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)










# MULTIVARIATE TIME SERIES DECOMPOSITION ####
    # Reference Crude + DJIA #


