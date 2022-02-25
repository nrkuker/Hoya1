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

opec <- import("data/OPEC_market_indicators.xlsx", sheet = "Indicators")
opec2 <- opec %>% 
  mutate(dateindex = paste0(month, "-", year)) %>% 
  dplyr::select(-c("date", "month", "year"))


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

weather <- rbind(weather1, weather2) %>% 
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
    name %in% c("Europe, Paris, Île-de-France, France", "Africa, Nocera Umbra, Umbria, Italia") ~ "EUR",
    name %in% c("Middle East-Saudi Arabia", "Middle East-Iran/Iraq") ~ "ME",
    TRUE ~ NA_character_
  )) %>% 
  mutate(region = as.factor(region))

weather %<>% 
  mutate(weathersit = case_when(
    conditions == "Clear" ~ "Clear",
    conditions %in% c("Overcast", "Partially cloudy") ~ "Cloudy",
    conditions %in% c("Rain", "Rain, Overcast", "Rain, Partially cloudy") ~ "Rain",
    conditions %in% c("Snow", "Snow, Overcast", "Snow, Partially cloudy") ~ "Snow",
    TRUE ~ NA_character_
  )) %>% 
  mutate(weathersit = as.factor(weathersit))

weather_small <- weather %>% 
  filter(name %in% c("Europe, Paris, Île-de-France, France", "United States-Houston")) %>% 
  dplyr::select(name, date_time, conditions, weathersit)

weather_small %<>% pivot_wider(names_from = name, values_from = c(conditions, weathersit)) %>% 
  rename(conditions_EUR = `conditions_Europe, Paris, Île-de-France, France`,
         conditions_USA = `conditions_United States-Houston`,
         weathersit_EUR = `weathersit_Europe, Paris, Île-de-France, France`,
         weathersit_USA = `weathersit_United States-Houston`)








crudes_to_predict <- colnames(data)[87:101]
crudes_no_flat <- colnames(data[c(87:95,97)])
  # omitting: grane, alvheim, asgard, wti_midlands, eagleford_45



# CREATING DIFFERENCED DATA ####
data2 <- data[,c(1, 80, 87:101)] %>% 
  mutate(dateindex = paste0(lubridate::month(date), "-", lubridate::year(date)))
join_data <- left_join(data2, financial, by = "date") %>% 
  left_join(opec2, by = "dateindex") %>% 
  left_join(weather_small, by = c("date" = "date_time")) %>% 
  dplyr::select(-dateindex)






# HANDLING MISSING ####

join_data$s_p_close %<>% na_interpolation(option = "linear")
join_data$usd_fx_index %<>% na_interpolation(option = "linear")
join_data$dow_dji_close %<>% na_interpolation(option = "linear")
join_data$emerging_market_etf %<>% na_interpolation(option = "linear")

# # IF SCALING FINANCIAL DATA:
join_data[,c(2,18:22)] %<>% scale()




# have to redefine data_diff:
data_diff <- join_data[-1,]

data_diff[2:24] <- sapply(join_data[, 2:24], function(x) {
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

# # CHOOSING LAG LENGTH
#   # where join_data is not differenced (non-stationary) 
#   # and data_diff is differenced (stationary)
# 
# VARselect(train[, colnames(train) %in% crudes_to_predict],
#           lag.max = 10,
#           type = "both",
#           season = 20,
#           exogen = train[,19:24]) #2
# 
# VARselect(train_diff[, colnames(train_diff) %in% crudes_to_predict],
#           lag.max = 10,
#           type = "both",
#           season = 20,
#           exogen = train_diff[,19:24]) #1




# ESTIMATING MODEL ####


SEASONALITY <- 62

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "both",
                       season = SEASONALITY,
                       exogen = train_diff[, c(2,18:24)]
                       )







# PREDICTING ####
lastday <- train %>% tail(1)

pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)

lower <- pred
upper <- pred


var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:24)], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
    lower[(j+1),i] <- var.pred$fcst[[i]][j,2] + pred[(j),i]
    upper[(j+1),i] <- var.pred$fcst[[i]][j,3] + pred[(j),i]
  }
  # print(pred[,i])
}
pred$date[-1] <- test$date
# pred


truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)
# truth

CRUDE <- "wti_midlands"
var.pred2 <- pred[[CRUDE]]
var.truth2 <- c(lastday[[CRUDE]], test[[CRUDE]])
pred.lower <- lower[[CRUDE]]
pred.upper <- upper[[CRUDE]]

df2 <- data.frame(index = seq(1:8),
                  pred = var.pred2,
                  actual = var.truth2,
                  lower = pred.lower,
                  upper = pred.upper)

ggplot(df2) + geom_line(aes(index-1, pred)) + geom_line(aes(index-1, actual), color = "red") +
  geom_line(aes(index-1, lower), color = "darkblue", linetype = "dashed") +
  geom_line(aes(index-1, upper), color = "darkblue", linetype = "dashed") +
  labs(title = paste0(CRUDE, " - Predicted (Black) vs Actual (Red)"),
       subtitle = paste0("With 95% confidence interval, season = ", SEASONALITY),
       x = "Number of obs. ahead", y = "Price ($)")





# FORECAST EVALUATION ####

resid <- matrix(nrow = nrow(truth)-1, ncol = ncol(truth)-1) %>% as.data.frame()
colnames(resid) <- colnames(truth)[-1]

# for (i in 2:nrow(truth)) {
#   resid[]
# }

# rmse <- matrix(nrow = 15, ncol = 10) %>% as.data.frame()
# rownames(rmse) <- colnames(truth)[-1]


pred_long <- pred[-1,] %>% 
  pivot_longer(names_to = "crude", cols = m_number_dated_brent:eagleford_45, values_to = "price_pred") %>% 
  mutate(index = paste0(lubridate::day(date),crude))

truth_long <- truth[-1,] %>% 
  pivot_longer(names_to = "crude", cols = m_number_dated_brent:eagleford_45, values_to = "price_truth") %>% 
  mutate(index = paste0(lubridate::day(date),crude))

resid_long <- left_join(pred_long, truth_long[,c("price_truth", "index")], by = "index") %>% 
  dplyr::select(-index) %>% 
  mutate(diff = price_pred - price_truth,
         diff2 = diff^2)

resid_long %>% 
  summarise(sum_resid = sum(diff),
            sum_sq_err = sum(diff2)) %>% view()














# DUMMY DATA
train_diff[,c(2,18:22)] %>% summary()
lastday <- train %>% tail(1)


pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)
lower <- pred
upper <- pred



avg_mkt <- import("data/exog_for_pred.xlsx", sheet = 1) %>% clean_names()
bear <- import("data/exog_for_pred.xlsx", sheet = 2) %>% clean_names()
bull <- import("data/exog_for_pred.xlsx", sheet = 3) %>% clean_names()
neg_shock <- import("data/exog_for_pred.xlsx", sheet = 4) %>% clean_names()
pos_shock <- import("data/exog_for_pred.xlsx", sheet = 5) %>% clean_names()

pred_outputname <- "pos_shock"
var.pred <- predict(var.model, n.ahead = 7, dumvar = pos_shock[,-1], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
    lower[(j+1),i] <- var.pred$fcst[[i]][j,2] + pred[(j),i]
    upper[(j+1),i] <- var.pred$fcst[[i]][j,3] + pred[(j),i]
  }
}
pred$date[-1] <- test$date
# pred

# write.csv(pred, str_c(pred_outputname, ".csv"))
