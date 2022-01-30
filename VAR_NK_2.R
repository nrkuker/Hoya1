# VECTOR AUTOREGRESSION TAKE 2

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

data <- import("data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated") %>% clean_names()
financial <- import("data/Crude3.csv", na.strings = "") %>% clean_names()

financial$date %<>% lubridate::as_date(format = "%m/%d/%Y")
financial$usd_fx_index %<>% as.numeric()





crudes_to_predict <- colnames(data)[87:101]
crudes_no_flat <- colnames(data[c(87:95,97)])
  # omitting: grane, alvheim, asgard, wti_midlands, eagleford_45



# STATIONARITY TEST
pp.test_results <- data.frame(crude = rep(NA_character_,length(crudes_to_predict)),
                              p_value = rep(NA_real_,length(crudes_to_predict)))
for (i in 87:101) {
  pp.test_results[i-86,1] <- colnames(data)[i]
  pp.test_results[i-86,2] <- pp.test(data[,i])$p.value
}
pp.test_results %>% 
  mutate(stationary = if_else(p_value<0.05, TRUE, FALSE)) %>% 
  print()




# CREATING DIFFERENCED DATA
data2 <- data[,c(1, 80, 87:101)]
join_data <- left_join(data2, financial, by = "date")
str(join_data)


data_diff <- join_data[-1,]

data_diff[3:17] <- sapply(data2[, 3:17], function(x) {
  diff(x, lag = 1)
})




# REPEATING STATIONARITY FOR DIFFERENCED DATA
pp.test_results_diff <- data.frame(crude = rep(NA_character_, length(crudes_to_predict)),
                                   p_value = rep(NA_real_, length(crudes_to_predict)))
for (i in 3:17) {
  pp.test_results_diff[i-2,1] <- colnames(data_diff)[i]
  pp.test_results_diff[i-2,2] <- pp.test(data_diff[,i])$p.value
}
pp.test_results_diff %>% 
  mutate(stationary = if_else(p_value<0.05, TRUE, FALSE)) %>% 
  print()
# data is stationary




# HANDLING MISSING

# VARselect doesn't like NAs, how many do I have?
sum(is.na(join_data)); sum(is.na(data_diff))  # the same

vis_miss(join_data)   # all in financial data

statsNA(join_data$s_p_close)            # 17 missing, only ever 1 in a row
statsNA(join_data$usd_fx_index)         # 123 missing, mostly 1 in a row but a couple 2 and one 3
statsNA(join_data$dow_dji_close)        # 17 missing, only ever 1 in a row
statsNA(join_data$emerging_market_etf)  # 17 missing, only ever 1 in a row


truth <- join_data$usd_fx_index
pred <- na_interpolation(join_data$usd_fx_index, option = "linear")

plot(pred-truth, ylim = c(-mean(truth, na.rm=T), mean(truth, na.rm=T)))

for (i in which(is.na(truth)==T)[1:5]) {
  print(pred[(i-1):(i+1)])
}

data.frame(date = join_data$date,
           pred = pred,
           truth = truth) %>% 
  filter(date < "2017-01-01") %>% 
  ggplot(aes(x=date)) + geom_line(aes(y = pred)) + geom_line(aes(y=truth), color = "red")

# ok linear interpolation seems valid, going for it

join_data$s_p_close %<>% na_interpolation(option = "linear")
join_data$usd_fx_index %<>% na_interpolation(option = "linear")
join_data$dow_dji_close %<>% na_interpolation(option = "linear")
join_data$emerging_market_etf %<>% na_interpolation(option = "linear")

# have to redefine data_diff:
data_diff <- join_data[-1,]

data_diff[3:22] <- sapply(join_data[, 3:22], function(x) {
  diff(x, lag = 1)
})

sum(is.na(join_data)); sum(is.na(data_diff))  # cool cool cool





# CHOOSING LAG LENGTH
  # where join_data is not differenced (non-stationary) 
  # and data_diff is differenced (stationary)

VARselect(join_data, lag.max = 10,
          type = "const")
# 1/29 19:04 getting errors. It's Saturday and dinner time and I'm done.










