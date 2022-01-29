# VECTOR AUTOREGRESSION TAKE 2

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, tidyquant, lubridate,
  stargazer, data.table, gridExtra,
  forecast, scales, tseries, tibbletime,
  lmtest, itsmr, here, fpp2,
  vars, MTS, car, caret,
  MLmetrics,
  psych,        # EDA
  visdat,       # missingness
  corrplot,     # correlation plot
  FactoMineR,   # EDA, PCA, MFA
  factoextra    # extract and visualize PCA/MFA
)

data <- import("Capstone/data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated") %>% clean_names()


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
pp.test_results




# CREATING DIFFERENCED DATA
data2 <- data[,c(1, 80, 87:101)]

data_diff <- data2[-1,]

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
pp.test_results_diff
# data is stationary
