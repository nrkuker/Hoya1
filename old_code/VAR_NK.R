# VECTOR AUTOREGRESSION

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, lubridate,
  stargazer, data.table,
  forecast, scales, tseries,
  lmtest,
  vars, MTS
)

data <- import("data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated") %>% clean_names()
str(data)
colnames(data)
crudes_to_predict <- colnames(data)[87:101]


par(mfrow = c(4,4))
for (i in crudes_to_predict) {
  plot(data[[i]])
}

crudes_no_flat <- colnames(data[c(87:95,97)])
  # omitting: grane, alvheim, asgard, wti_midlands, eagleford_45
par(mfrow = c(3,3))
for (i in crudes_no_flat) {
  plot(data[[i]])
}



brent <- ts(data$m_number_dated_brent, frequency = 30)
plot(brent)
# note that some dates are missing, impute?

forties <- ts(data$forties, frequency = 30)
oseberg <- ts(data$oseberg, frequency = 30)
ekofisk <- ts(data$ekofisk, frequency = 30)
troll <- ts(data$troll, frequency = 30)
north_sea_basket <- ts(data$north_sea_basket, frequency = 30)
statfjord <- ts(data$statfjord, frequency = 30)
flotta_gold <- ts(data$flotta_gold, frequency = 30)
duc_dansk <- ts(data$duc_dansk, frequency = 30)
grane <- ts(data$grane, frequency = 30)
gulfaks <- ts(data$gulfaks, frequency = 30)
alvheim <- ts(data$alvheim, frequency = 30)
asgard <- ts(data$asgard, frequency = 30)
wti_midlands <- ts(data$wti_midlands, frequency = 30)
eagleford_45 <- ts(data$eagleford_45, frequency = 30)


# stationarity
pp.test(brent)
pp.test(forties)
pp.test(oseberg)
pp.test(ekofisk)
pp.test(troll)
pp.test(north_sea_basket)
pp.test(statfjord)
pp.test(flotta_gold)
pp.test(duc_dansk)
pp.test(grane)
pp.test(gulfaks)
pp.test(alvheim)
pp.test(asgard)
pp.test(wti_midlands)
pp.test(eagleford_45)
# all non-stationary

v1 <- cbind(brent, forties, oseberg, ekofisk, troll, north_sea_basket, statfjord,
            flotta_gold, duc_dansk, grane, gulfaks, alvheim, asgard, wti_midlands,
            eagleford_45)
head(v1)

lagselect <- VARselect(v1, lag.max = 15, type = "const") # can vary type
lagselect$selection

lagselect <- VARselect(v1, lag.max = 15, type = "trend") # can vary type
lagselect$selection

lagselect <- VARselect(v1, lag.max = 15, type = "both") # can vary type
lagselect$selection

lagselect <- VARselect(v1, lag.max = 15, type = "none") # can vary type
lagselect$selection

model1 <- vars::VAR(v1, p = 2, type = "const", season = NULL, exogen = NULL)
summary(model1)


# diagnostics
serial1 <- serial.test(model1, lags.pt = 5, type = "PT.asymptotic")
serial1
# null is no serial correlation aka autocorrelation so "we can reject it with extreme prejudice"
serial.test(model1, lags.pt = 5, type = "PT.asymptotic")
serial.test(model1, lags.pt = 5, type = "PT.adjusted")
serial.test(model1, lags.pt = 5, type = "BG")
serial.test(model1, lags.pt = 5, type = "ES")


arch1 <- arch.test(model1, lags.multi = 15, multivariate.only = T)
arch1
# fail to reject, signify no degree of heteroscedasticity





# Forecasting
forecast <- predict(model1, n.ahead = 7, ci = 0.95)

fanchart(forecast, names = "brent")
fanchart(forecast, names = "forties")
fanchart(forecast, names = "eagleford_45")









#### WITH DIFFERENTIALS ####
data2 <- data[,c(1,87:95,97)]
glimpse(data2)

data_diff <- data2 %>% 
  mutate(
    diff_brent = m_number_dated_brent - m_number_dated_brent,
    diff_forties = forties - m_number_dated_brent,
    diff_oseberg = oseberg - m_number_dated_brent,
    diff_ekofisk = ekofisk - m_number_dated_brent,
    diff_troll = troll - m_number_dated_brent,
    diff_north_sea_basket = north_sea_basket - m_number_dated_brent,
    diff_statfjord = statfjord - m_number_dated_brent,
    diff_flotta_gold = flotta_gold - m_number_dated_brent,
    diff_duc_dansk = duc_dansk - m_number_dated_brent,
    diff_gulfaks = gulfaks - m_number_dated_brent
  )
glimpse(data_diff)

brent <- ts(data_diff$diff_brent, frequency = 30)
forties <- ts(data_diff$diff_forties, frequency = 30)
oseberg <- ts(data_diff$diff_oseberg, frequency = 30)
ekofisk <- ts(data_diff$diff_ekofisk, frequency = 30)
troll <- ts(data_diff$diff_troll, frequency = 30)
north_sea_basket <- ts(data_diff$diff_north_sea_basket, frequency = 30)
statfjord <- ts(data_diff$diff_statfjord, frequency = 30)
flotta_gold <- ts(data_diff$diff_flotta_gold, frequency = 30)
duc_dansk <- ts(data_diff$diff_duc_dansk, frequency = 30)
gulfaks <- ts(data_diff$diff_gulfaks, frequency = 30)

pp.test(brent)
pp.test(forties)
pp.test(oseberg)
pp.test(ekofisk)
pp.test(troll)
pp.test(north_sea_basket)
pp.test(statfjord)
pp.test(flotta_gold)
pp.test(duc_dansk)
pp.test(gulfaks)

v2 <- cbind(forties, oseberg, ekofisk, troll, north_sea_basket, statfjord,
            flotta_gold, duc_dansk, gulfaks)
head(v2)


lagselect <- VARselect(v2, lag.max = 15, type = "const") # can vary type
lagselect$selection

model2 <- vars::VAR(v2, p = 2, type = "const", season = 30, exogen = NULL)

# diagnostics
serial2 <- serial.test(model2, lags.pt = 5, type = "PT.asymptotic")
serial2
# null is no serial correlation aka no autocorrelation. p-val low so "we can reject it with extreme prejudice"

arch2 <- arch.test(model2, lags.multi = 15, multivariate.only = T)
arch2
# fail to reject, signify no degree of heteroscedasticity


VARselect(v2[-(860:866),], lag.max = 15, type = "const")$selection # can vary type
model_forecast <- vars::VAR(v2[-(860:866),], p = 2, type = "const", season = 30, exogen = NULL)
forecast2 <- predict(model_forecast, n.ahead = 7, ci = 0.95)

plot(data_diff$diff_forties[860:866], type = "l", col = "black", main = "Forties - Actual (black) vs Predicted (red)")
lines(forecast2$fcst$forties[,"fcst"], type = "l", col = "red")

plot(data_diff$diff_oseberg[860:866], type = "l", col = "black", main = "Oseberg - Actual (black) vs Predicted (red)")
lines(forecast2$fcst$oseberg[,"fcst"], type = "l", col = "red")

plot(data_diff$diff_ekofisk[860:866], type = "l", col = "black", main = "Ekofisk - Actual (black) vs Predicted (red)")
lines(forecast2$fcst$ekofisk[,"fcst"], type = "l", col = "red")

plot(data_diff$diff_troll[860:866], type = "l", col = "black", main = "Troll - Actual (black) vs Predicted (red)")
lines(forecast2$fcst$troll[,"fcst"], type = "l", col = "red")
