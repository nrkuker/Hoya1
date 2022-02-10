# Capstone EDA

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor, tidyselect,
  tidyselect, lubridate,
  stargazer, data.table,
  forecast, scales, tseries,
  lmtest, urca, egcm, 
  vars, MTS
)

data <- import("Capstone/data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated") %>% clean_names()
str(data)
colnames(data)
# limiting vars
# data2 <- data[,c(1, 80, 87:101, 132:137)]
# colnames(data2)


djia <- import("Capstone/data/djia.csv", skip = 15, colClasses = c("POSIXct", "numeric"))
str(djia)
djia$date[1:10]
djia$date <- force_tz(djia$date, "UTC")
djia$date[1:10]




# GRAPHING ####
data2 <- data[,c(1,87:95,97)]

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
# stationarity
pp.test(data_diff$diff_brent)
pp.test(data_diff$diff_duc_dansk)
pp.test(data_diff$diff_ekofisk)
pp.test(data_diff$diff_flotta_gold)
pp.test(data_diff$diff_forties)
pp.test(data_diff$diff_gulfaks)
pp.test(data_diff$diff_north_sea_basket)
pp.test(data_diff$diff_oseberg)
pp.test(data_diff$diff_statfjord)
pp.test(data_diff$diff_troll)



data_long <- data2 %>% pivot_longer(names_to = "crude", values_to = "price", cols = m_number_dated_brent:gulfaks)

data_long %>% 
  # filter(crude %in% c("gulfaks", "flotta_gold")) %>%
  ggplot(aes(date, price)) +
  geom_line(aes(color = crude))


data_diff_long <- data_diff %>% dplyr::select(date, diff_brent:diff_gulfaks) %>% 
  pivot_longer(names_to = "crude", values_to = "price", cols = diff_brent:diff_gulfaks)

data_diff_long %>% 
  # filter(between(date, "2018-02-20", "2018-07-01")) %>%
  filter(crude %in% c("diff_brent", "diff_gulfaks", "diff_flotta_gold")) %>%
  ggplot(aes(date, price)) + 
  geom_line(aes(color = crude))
  # geom_line(aes(color = reorder(crude, price, median))) + facet_wrap(~reorder(crude, price, median))

# observations:
  # flotta gold generally less than brent
  # statfjord, duc dansk, troll, gulfaks have similar trends and similar peaks/valleys (Jan 2017 ish and Feb? 2019)
    # big drop Q2-ish 2018 observed in these 4 but not in the others
    # lies, also in flotta gold
  


  # what happened on Dec 27 2016 that everything spiked?
  # what happened on Apr 17 2017 that everything dropped?
  # Feb 25 2019 drop, Feb 26 2019 spike (higher than)
  # recession (?) Apr-Jun 2018


# FOR PRESENTATION
data_long %>% 
  filter(crude %in% c("m_number_dated_brent")) %>%
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(date, price)) +
  geom_line(color = "#2c86cb") + 
  labs(title = "Reference Crude Over Time") + xlab("Date") + ylab("Price (USD)") +
  scale_y_continuous(labels = scales::dollar_format(), breaks = seq(0,100,10)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  ggthemes::theme_clean() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

data_diff_long %>% 
  filter(crude %in% c("diff_brent", "diff_gulfaks", "diff_flotta_gold")) %>%
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(date, price)) +
  geom_line(aes(color = crude)) + 
  labs(title = "Example of Crude Differential Over Time", 
       subtitle = "Price difference from reference for two crudes of interest",
       color = "Crude") + 
  xlab("Date") + ylab("Price Differential (USD)") +
  scale_y_continuous(labels = scales::dollar_format(), breaks = seq(-3,4,1)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  scale_color_manual(labels = c("Reference", "Flotta Gold", "Gulfaks"), 
                     values = c("#bba0d8", "#003e7e", "#6caddf")) + 
  ggthemes::theme_clean() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))








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
ref_ts <- ts(data2[,3], frequency = 365, start = c(2015, 299))
plot(ref_ts)

ref_ts_add <- decompose(ref_ts, "additive")
ref_ts_mult <- decompose(ref_ts, "multiplicative")

plot(ref_ts_add)
plot(ref_ts_mult)

# why the NAs?
# testing with marketing dataset
usliquorsales <- read_csv("~/Dropbox/_GEORGETOWN Dropbox/MARK 601 Strategic Marketing Analytics/MarketingAnalytics/data/usliquorsales.csv")
usliquorsales %<>% drop_na()
liq_ts <- ts(usliquorsales[,2], frequency = 12, start = c(1992, 2))
plot(liq_ts)

liq_ts_add <- decompose(liq_ts, "additive")
liq_ts_mult <- decompose(liq_ts, "multiplicative")

plot(liq_ts_add)
plot(liq_ts_mult)

# aha! NAs because of frequency
weekly <- ts(data2[,3], frequency = 7)
weekly.tsd <- decompose(weekly, "multiplicative")
plot(weekly.tsd) + title(sub = "Weekly Frequency")

month <- ts(data2[,3:11], frequency = 30)
month.tsd <- decompose(month, "multiplicative")
plot(month.tsd) + title(sub = "Monthly Frequency")

quarter <- ts(data2[,3], frequency = 120)
quarter.tsd <- decompose(quarter, "multiplicative")
plot(quarter.tsd) + title(sub = "Quarterly Frequency")

yearly <- ts(data2[,3], frequency = 365)
yearly.tsd <- decompose(yearly, "multiplicative")
plot(yearly.tsd) + title(sub = "Yearly Frequency")



dfit7 <- auto.arima(ref_ts, seasonal = TRUE)
dfit7   # 1 order seasonal diff and non-seasonal diff
plot(residuals(dfit7))
Acf(residuals(dfit7))
Pacf(residuals(dfit7))
adf.test(residuals(dfit7))
summary(dfit7)

dfit.m <- auto.arima(month, seasonal = T)
summary(dfit.m)
plot(residuals(dfit.m))
Acf(residuals(dfit.m))
Pacf(residuals(dfit.m))
adf.test(residuals(dfit.m))

dfit.q <- auto.arima(quarter, seasonal = T)
summary(dfit.q)
plot(residuals(dfit.q))
Acf(residuals(dfit.q))
Pacf(residuals(dfit.q))
adf.test(residuals(dfit.q))





# use lagged values
ref_lag <- data2[,3] %>% diff(1)
ref_lag_ts <- ts(ref_lag, frequency = 365, start = c(2015, 299))
# TS not useful here








# holding out the last 20% of dates
fit_predicted <- arima(ts(ref_ts[-c(694:866)]), order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 365))

forecast_pred <- forecast(fit_predicted, h = 173)
plot(forecast_pred, main = "")
lines(ts(ref_ts))
# just doing the reference crude isn't enough
# add in DJIA?

# monthly data
month <- ts(data2[,3], frequency = 30)
month_short <- ts(data2[-c(694:866), 3], frequency = 30)
fit.m_predicted <- arima(month_short,
                         order =c(0,1,0),
                         seasonal = list(order = c(0,1,0), period = 30))

forecast.m_pred <- forecast(fit.m_predicted, h = 173)
plot(forecast.m_pred, ylim = c(25,90))
lines(month)





# WITH DJIA ####
data2_djia <- left_join(data2, djia, by = "date") %>% 
  rename(djia = value)
sum(is.na(data2_djia))
visdat::vis_miss(data2_djia)

data2_djia[,c(1,3,24)] %>% filter(is.na(djia))

dj_ts <- ts(data2_djia$djia, frequency = 365, start = c(2015, 299))
plot(dj_ts)

# drop NAs
data2_djia$djia[is.na(data2_djia$djia) == F]
dj_ts <- ts(data2_djia$djia[is.na(data2_djia$djia) == F], 
            frequency = 365, start = c(2015, 299))
plot(dj_ts)


dj_ts_add <- decompose(dj_ts, "additive")
dj_ts_mult <- decompose(dj_ts, "multiplicative")

plot(dj_ts_add)
plot(dj_ts_mult)




refdj_ts <- ts(data2_djia[,c(3,24)], frequency = 365, start = c(2015, 299))
plot(refdj_ts)
sum(is.na(refdj_ts))



ts.d <- diffM(refdj_ts, d = 1)
var.a <- vars::VAR(ts.d,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)










################################################################################

# RUN VAR_NK_2.R THRU CREATING data_diff AND join_data AND IMPUTING

crudes_to_predict <- colnames(data)[87:101]
crudes_no_flat <- colnames(data[c(87:95,97)])
# omitting: grane,     alvhiem,   asgard,    wti_midlands, eagleford_45
# dates:    2016-04-04 2017-04-28 2017-04-28 2018-09-20    2018-09-20      one day before
# dates:    2016-04-01 2017-04-27 2017-04-27 2018-09-19    2018-09-19      where to start

data %>% 
  # mutate(same = (grane == lag(grane))) %>% 
  # filter(same == FALSE) %>% 
  filter(date < as.Date("2018-09-20")) %>% 
  dplyr::select(date) %>% tail()

# getting rid of repeats
join_data2 <- join_data
join_data2$grane[join_data2$date < as.Date("2016-04-01")] <- NA_real_
join_data2$alvhiem[join_data2$date < as.Date("2017-04-27")] <- NA_real_
join_data2$asgard[join_data2$date < as.Date("2017-04-27")] <- NA_real_
join_data2$wti_midlands[join_data2$date < as.Date("2018-09-19")] <- NA_real_
join_data2$eagleford_45[join_data2$date < as.Date("2018-09-19")] <- NA_real_



boxplot(join_data2[,3:17])
# observe smaller range on the crudes without earlier prices

boxplot(join_data2[-c(1:726),3:17])
# observe more similar ranges when dates limited





# stationarity
CheckStationarity <- function(vector, signif = 0.05, name = "") {
  output <- tseries::adf.test(vector)

  print(paste0("Augmented Dickey-Fuller Test for variable: ", name))
  print("Null Hypothesis: Data has unit root. Non-Stationary.")
  print(paste0("Significance Level = ", signif))
  print(paste0("Test Statistic = ", output$statistic))
  print(paste0("No. Lags Chosen = ", output$parameter))
  
  if (output$p.value <= signif) {
    print(paste0(" => P-Value = ", output$p.value, ". Rejecting Null Hypothesis."))
    print(" => Series is Stationary.")
  } else {
    print(paste0(" => P-Value = ", output$p.value, ". Fail to Reject Null Hypothesis."))
    print(" => Series is Non-Stationary.")
  }
  print("---------------")
}


CheckStationarity(join_data2$forties, name = "forties")

for (i in crudes_no_flat) {
  CheckStationarity(join_data2[[i]], name = i)
}



# WHAT THE ACTUAL EVERLIVING FUCK
# test for nonstationarity of 3-month treasury bills using ADF test
ur.df(join_data2$forties, 
      lags = 9, 
      selectlags = "AIC", 
      type = "drift")
#> 
#> ############################################################### 
#> # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -2.1004 2.2385

# test for nonstationarity of 10-years treasury bonds using ADF test
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")
#> 
#> ############################################################### 
#> # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -1.0079 0.5501

# test for nonstationarity of 3-month treasury bills using DF-GLS test
ur.ers(window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)
#> 
#> ############################################################### 
#> # Elliot, Rothenberg and Stock Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -1.8042

# test for nonstationarity of 10-years treasury bonds using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)
#> 
#> ############################################################### 
#> # Elliot, Rothenberg and Stock Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -0.942
#> 
#> 
#> 
#> 
#> 
#> 

for (i in 4:17) {
  print(egcm(join_data2[,c(3,i)]))
  
}
egcm(join_data2[,c(3,4)])










# FITTING OLS - IF RESID STATIONARY, THEN COINTEGRATED ####
ols <- lm(north_sea_basket ~ m_number_dated_brent + forties + oseberg + ekofisk + troll +
            statfjord + flotta_gold + duc_dansk + grane + gulfaks + alvhiem + asgard +
            wti_midlands + eagleford_45
          , data = join_data2)
summary(ols)
CheckNormal(ols)
plot(ols)
plot(ols$residuals, type = "l")  # doesn't look stationary?

CheckStationarity(ols$residuals, name = "OLS Resid")   # but apparently is!
pp.test(ols$residuals)
# "If residuals are stationary, we can safely assume that, the given series are really cointegrated."

