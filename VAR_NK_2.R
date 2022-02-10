# VECTOR AUTOREGRESSION TAKE 2

# PREP ####
pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, tidyquant, lubridate,
  stargazer, data.table, gridExtra,
  forecast, scales, tseries, tibbletime,
  lmtest, itsmr, here, fpp2, urca, egcm, 
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

weather1 <- import("data/Historical Data 16-20.xlsx", 
                   col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "text")) %>% clean_names()
weather1 %<>% mutate(name = as.factor(name),
                     conditions = as.factor(conditions),
                     date_time = lubridate::as_date(date_time, format = "%m/%d/%Y"))

weather2 <- import("data/OPEC Weather Data.xlsx", 
                   col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "text")) %>% clean_names()
weather2 %<>% mutate(name = as.factor(name),
                     conditions = as.factor(conditions),
                     date_time = lubridate::as_date(date_time, format = "%m/%d/%Y"))

weather <- rbind(weather1, weather2) %>% 
  filter(name != "Asia, Lima, Perú") %>%    # filtering out fully blank rows
  filter(!is.na(maximum_temperature))








crudes_to_predict <- colnames(data)[87:101]
crudes_no_flat <- colnames(data[c(87:95,97)])
  # omitting: grane, alvheim, asgard, wti_midlands, eagleford_45



# CREATING DIFFERENCED DATA ####
data2 <- data[,c(1, 80, 87:101)]
join_data <- left_join(data2, financial, by = "date")


data_diff <- join_data[-1,]

data_diff[3:17] <- sapply(data2[, 3:17], function(x) {
  diff(x, lag = 1)
})




# STATIONARITY TEST ####
pp.test_results <- data.frame(crude = rep(NA_character_,length(crudes_to_predict)),
                              p_value = rep(NA_real_,length(crudes_to_predict)))
for (i in 87:101) {
  pp.test_results[i-86,1] <- colnames(data)[i]
  pp.test_results[i-86,2] <- pp.test(data[,i])$p.value
}
pp.test_results %>% 
  mutate(stationary = if_else(p_value<0.05, TRUE, FALSE)) %>% 
  print()




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




# HANDLING MISSING ####

# VARselect doesn't like NAs, how many do I have?
# sum(is.na(join_data)); sum(is.na(data_diff))  # the same

# vis_miss(join_data)   # all in financial data

# statsNA(join_data$s_p_close)            # 17 missing, only ever 1 in a row
# statsNA(join_data$usd_fx_index)         # 123 missing, mostly 1 in a row but a couple 2 and one 3
# statsNA(join_data$dow_dji_close)        # 17 missing, only ever 1 in a row
# statsNA(join_data$emerging_market_etf)  # 17 missing, only ever 1 in a row


# truth <- join_data$usd_fx_index
# pred <- na_interpolation(join_data$usd_fx_index, option = "linear")
# 
# plot(pred-truth, ylim = c(-mean(truth, na.rm=T), mean(truth, na.rm=T)))
# 
# for (i in which(is.na(truth)==T)[1:5]) {
#   print(pred[(i-1):(i+1)])
# }
# 
# data.frame(date = join_data$date,
#            pred = pred,
#            truth = truth) %>% 
#   filter(date < "2017-01-01") %>% 
#   ggplot(aes(x=date)) + geom_line(aes(y = pred)) + geom_line(aes(y=truth), color = "red")
# 
# # ok linear interpolation seems valid, going for it

join_data$s_p_close %<>% na_interpolation(option = "linear")
join_data$usd_fx_index %<>% na_interpolation(option = "linear")
join_data$dow_dji_close %<>% na_interpolation(option = "linear")
join_data$emerging_market_etf %<>% na_interpolation(option = "linear")

# have to redefine data_diff:
data_diff <- join_data[-1,]

data_diff[3:22] <- sapply(join_data[, 3:22], function(x) {
  diff(x, lag = 1)
})

# sum(is.na(join_data)); sum(is.na(data_diff))  # cool cool cool





# PARTITION
end <- nrow(join_data)
start <- end-6
train <- join_data[-c(start:end),]
test <- join_data[c(start:end),]

end <- nrow(data_diff)
start <- end-6
train_diff <- data_diff[-c(start:end),]
test_diff <- data_diff[c(start:end),]


# CHOOSING LAG LENGTH
  # where join_data is not differenced (non-stationary) 
  # and data_diff is differenced (stationary)

VARselect(train[, colnames(train) %in% crudes_to_predict],
          lag.max = 10, 
          type = "both",
          season = 250,
          exogen = train[,19:22]) #2

VARselect(train_diff[, colnames(train_diff) %in% crudes_to_predict],
          lag.max = 10,
          type = "both",
          season = 30,
          exogen = train_diff[,19:22]) #1




# ESTIMATING MODEL ####

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "both",
                       season = 20,
                       exogen = train_diff[, 19:22])
# summary(var.model)

summary(var.model)$covres # var-covar matrix
# off-diag != 0 which indicates there is contemporaneous correlation b/t vars

summary(var.model)$corres # correlation matrix
# CAVEAT correlations do not imply any direction in the underlying causality

t(chol(summary(var.model)$covres))


var.irf <- irf(var.model)
plot(var.irf)

var.fevd <- fevd(var.model)
plot(var.fevd)











# PREDICTING ####
var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, 19:22])
# x11(); par(mai=rep(0.4, 4)); plot(var.pred, xlim = c(800,866))
# x11(); fanchart(var.pred, xlim = c(800,866))

# var.pred$fcst

# var.pred.df <- data.frame(crude = rep(names(var.pred$fcst), 7),
#                           index = rep(1:7, each = 15),
#                           fcst = rep(NA_real_, 7*length(names(var.pred$fcst))))
# count = 0
# for (each in (var.pred$fcst)) {
#   count = 7*count + 1
#   end = count+6
#   var.pred.df[c(count:end), 3] <- each[,"fcst"]
#   # print(names(var.pred$fcst)[each])
# }
# 
# var.pred$fcst[[1]][,"fcst"]


# var.pred1 <- var.pred$fcst[[1]][,"fcst"]
# var.truth1 <- test_diff$m_number_dated_brent
# 
# df <- data.frame(index = seq(1:7),
#            pred = var.pred1,
#            actual = var.truth1)
# 
# ggplot(df) + geom_line(aes(index, pred)) + geom_line(aes(index, actual), color = "red") + 
#   labs(title = "Brent - Differenced Data - Predicted (Black) vs Actual (Red)", 
#        subtitle = "Season = 62 (working days in a quarter)")

var.pred

train %>% tail(1)











