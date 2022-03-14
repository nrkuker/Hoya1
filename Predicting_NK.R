# GENERATING PREDICTIONS

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

weather3 <- import("data/North Sea Weather Data - Bergen Norway.csv") %>% clean_names()
weather3 %<>% 
  dplyr::select(name, datetime, tempmax, tempmin, temp, precipprob, feelslike, 
                precip, snow, snowdepth, windspeed, winddir, windgust, visibility,
                cloudcover, humidity, conditions)
weather3 %<>% mutate(datetime = lubridate::as_date(datetime, format = "%m/%d/%y"))
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

weather_small <- left_join(
  weather %>% group_by(date_time) %>%
    summarize(min = min(minimum_temperature)) %>% mutate(index = str_c(date_time, min)),
  weather %>%
    mutate(index = str_c(date_time, minimum_temperature)) %>%
    dplyr::select(index, name),
  by = "index"
) %>%
  dplyr::select(-index)








crudes_to_predict <- colnames(data)[87:101]
crudes_no_flat <- colnames(data[c(87:95,97)])
# omitting: grane, alvheim, asgard, wti_midlands, eagleford_45



# CREATING DIFFERENCED DATA ####
data2 <- data[,c(1, 80, 87:101)] %>% 
  mutate(dateindex = paste0(lubridate::month(date), "-", lubridate::year(date)))
data3 <- left_join(data2, financial, by = "date") %>% 
  left_join(opec2, by = "dateindex") %>% 
  left_join(weather_small, by = c("date" = "date_time")) %>% 
  dplyr::select(-c("dateindex", "name"))






# HANDLING MISSING ####

data3$s_p_close %<>% na_interpolation(option = "linear")
data3$usd_fx_index %<>% na_interpolation(option = "linear")
data3$dow_dji_close %<>% na_interpolation(option = "linear")
data3$emerging_market_etf %<>% na_interpolation(option = "linear")

# Weather missing
join_data <- data3[complete.cases(data3)==T, ]

# SCALING FINANCIAL DATA:
join_data[,c(2,18:24)] %<>% scale()


# have to redefine data_diff:
data_diff <- join_data[-1,]

data_diff[c(2:22,25)] <- sapply(join_data[, c(2:22,25)], function(x) {
  diff(x, lag = 1)
})









# ESTIMATING MODEL ####


SEASONALITY <- 62

var.model <- vars::VAR(data_diff[, colnames(data_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "both",
                       season = SEASONALITY,
                       exogen = data_diff[, c(2,18:25)]
)



# EXOGENOUS REGRESSOR SCENARIOS  ####

weather_pred <- import("data/exog_for_pred.xlsx", sheet = "weather_pred")
financial_pred <- import("data/exog_for_pred.xlsx", sheet = "financial_pred", na = "NA")
misc_pred <- import("data/exog_for_pred.xlsx", sheet = "misc_pred", na = "NA")


w_avg <- data.frame(min = weather_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(mean))
w_warm <- data.frame(min = weather_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(upper75))
w_cool <- data.frame(min = weather_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(lower75))

f_avg <- data.frame(financial_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(date, index, mean)) %>% 
  pivot_wider(names_from = index, values_from = mean)
f_bull <- data.frame(financial_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(date, index, upper75)) %>% 
  pivot_wider(names_from = index, values_from = upper75)
f_bear <- data.frame(financial_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(date, index, lower75)) %>% 
  pivot_wider(names_from = index, values_from = lower75)
f_neg <- data.frame(financial_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(date, index, neg_shock)) %>% 
  pivot_wider(names_from = index, values_from = neg_shock)
f_pos <- data.frame(financial_pred %>% filter(date %in% misc_pred$date) %>% dplyr::select(date, index, pos_shock)) %>% 
  pivot_wider(names_from = index, values_from = pos_shock)

f_list <- list(f_avg, f_bull, f_bear, f_neg, f_pos)

scenarios <- expand.grid(w = c("w_avg", "w_warm", "w_cool"),
                         f = c("f_avg", "f_bull", "f_bear", "f_neg", "f_pos")) %>%
  mutate(name = str_c(w, ", ", f))

exog_reg <- vector("list", length = 15)
names(exog_reg) <- scenarios$name

count <- 0
for (f in f_list) {
  for (w in c(w_avg, w_warm, w_cool)) {
    count <- count + 1
    exog_reg[[count]] <- cbind(date = misc_pred$date,
                               dollar_euros = misc_pred$dollar_euros, 
                               f[,-1], 
                               opec_prod = misc_pred$opec_prod, 
                               world_demand = misc_pred$world_demand, 
                               min = w)
  }
}



# PREPROCESSING FOR REGRESSORS
exog_reg_proc <- exog_reg

# scaling
centers <- attr(scale(data3[complete.cases(data3) == T,][, c(2, 18:24)]), "scaled:center")
scales <- attr(scale(data3[complete.cases(data3) == T,][, c(2, 18:24)]), "scaled:scale")
centers; scales

exog_reg_proc <- 
  lapply(exog_reg_proc, function(x) {
    cbind(
      date = x[,1],
      scale(
        x[, c(2:9)],
        center = centers,
        scale = scales
      ) %>% as.data.frame(),
      min = x[,10]
    )
  })

# differencing
exog_reg_proc[[1]][,c(2:7, 10)]

exog_reg_proc[[10]][, c(2:7, 10)] %>% as.matrix() %>% diff(lag = 1)
exog_reg_proc[[1]][, c(2:7, 10)] %>% as.matrix() %>% diff(lag = 1)
exog_reg_proc[[13]][, c(2:7, 10)] %>% as.matrix() %>% diff(lag = 1)

exog_reg_proc[[1]][, c(2:7, 10)] %>% as.matrix() %>% diff()

lapply(exog_reg_proc, function(x) {
  c(NA_real_,
    diff(x[, c(2:7, 10)],
         lag = 1))
})


data_diff <- join_data[-1,]

data_diff[c(2:22,25)] <- sapply(join_data[, c(2:22,25)], function(x) {
  diff(x, lag = 1)
})


exog_reg_proc[[1]]


# GENERATING PREDICTIONS ####


pred <- join_data %>% tail(1) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)
lower <- pred
upper <- pred



avg_mkt <- import("data/exog_for_pred.xlsx", sheet = 1) %>% clean_names()
bear <- import("data/exog_for_pred.xlsx", sheet = 2) %>% clean_names()
bull <- import("data/exog_for_pred.xlsx", sheet = 3) %>% clean_names()
neg_shock <- import("data/exog_for_pred.xlsx", sheet = 4) %>% clean_names()
pos_shock <- import("data/exog_for_pred.xlsx", sheet = 5) %>% clean_names()

pred_outputname <- "pos_shock"







var.pred <- predict(var.model, n.ahead = 14, dumvar = exog_reg[[1]][,-1], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
    lower[(j+1),i] <- var.pred$fcst[[i]][j,2] + pred[(j),i]
    upper[(j+1),i] <- var.pred$fcst[[i]][j,3] + pred[(j),i]
  }
}
pred$date[-1] <- misc_pred$date
pred

# write.csv(pred, str_c(pred_outputname, ".csv"))






lastday <- train %>% tail(1)

pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)

lower <- pred
upper <- pred


var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:25)], ci = 0.95)

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
