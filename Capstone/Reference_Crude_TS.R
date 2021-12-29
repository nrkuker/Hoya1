# PREDICT REFERENCE

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, tidyquant, lubridate,
  stargazer, data.table, gridExtra,
  forecast, scales, tseries, tibbletime,
  lmtest, itsmr, here, fpp2,
  vars, MTS
)

data <- import("Capstone/data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated") %>% clean_names()
str(data)
colnames(data)

djia <- import("Capstone/data/djia.csv", skip = 15, colClasses = c("POSIXct", "numeric"))
str(djia)
djia$date[1:10]
djia$date <- force_tz(djia$date, "UTC")
djia$date[1:10]



data2 <- data %>% 
  dplyr::select(date, m_number_dated_brent, dollar_euros)

# # brent <- ts(data$m_number_dated_brent, frequency = 30)
# # plot(brent)
# 
# # start_date <- data$date[1]
# # end_date <- data$date[nrow(data)]
# # dates <- seq(start_date, end_date, by = "day")
# 
# # left_join(dates, data[,c("date", "m_number_dated_brent")], by = "date")








# WITH DJIA ####
data2_djia <- left_join(data2, djia, by = "date") %>% 
  rename(djia = value)
sum(is.na(data2_djia))
visdat::vis_miss(data2_djia)

data2_djia %<>% filter(!is.na(djia))

# brent <- ts(data2_djia[,-1], frequency = 30)
# plot(brent)




refdj_ts <- ts(data2_djia[,-1], frequency = 1)
plot(refdj_ts)
sum(is.na(refdj_ts))

#####
# ts.d <- diffM(refdj_ts, d = 1)
# var.a <- vars::VAR(ts.d,
#                    lag.max = 10, #highest lag order for lag length selection according to the choosen ic
#                    ic = "AIC", #information criterion
#                    type = "none") #type of deterministic regressors to include
# summary(var.a)
# 
# 
# 
# autoplot(refdj_ts, facets = TRUE) +
#   xlab("") + ylab("") +
#   ggtitle("")
# 
# # plot the ACF
# ggAcf(refdj_ts)
# 
# # plot the ACF
# ggPacf(refdj_ts)
# 
# brent_freq30 <- refdj_ts[,"m_number_dated_brent"] %>% ts(., frequency = 30)
# brent_stl <- stl(brent_freq30, s.window = "periodic")
# autoplot(brent_stl)
# 
# djia_freq30 <- refdj_ts[,"djia"] %>% ts(., frequency = 30)
# djia_stl <- stl(djia_freq30, s.window = "periodic")
# autoplot(djia_stl)
#####

exog_reg <- as.matrix(refdj_ts[,-1])
arimax1 <- auto.arima(refdj_ts[,"m_number_dated_brent"],
                      xreg = refdj_ts2[,"dollar_euros"],
                      trace = T, seasonal = F, stepwise = F, approximation = F)
summary(arimax1)


refdj_ts2 <- ts(data2_djia[,-1], frequency = 365)
arimax2 <- auto.arima(refdj_ts2[,"m_number_dated_brent"],
                      xreg = refdj_ts2[,"dollar_euros"],
                      trace = T, seasonal = T, stepwise = T, approximation = F)
summary(arimax2)

autoplot(arimax1)
checkresiduals(arimax1) # residuals are not normal
test(resid(arimax1)) # from package itsmr
