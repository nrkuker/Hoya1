# FORECAST EVALUATION

# do everything up to running model from VAR_NK_3.R

# SET UP
seasonal_options <- c(5, 20, 30, 62, 90, 125, 180, 250, 365, NULL)
lag_order <- 1
# seasonal_options <- c(62)
count <- 0 
# outputname <- "mae_wti&brent_m1m2"
mae <- matrix(nrow = 15, ncol = 10) %>% as.data.frame()
rownames(mae) <- colnames(train_diff)[colnames(train_diff) %in% crudes_to_predict]


# LOOP THRU SEASONALITY
for (i in seasonal_options) {

SEASONALITY <- i
count <- count+1
modname <- paste0("season", i)

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = lag_order,
                       type = "both",
                       season = i,
                       exogen = train_diff[, c(2,18:ncol(train_diff))]
)

lastday <- train %>% tail(1)
pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)

var.pred <- predict(var.model, n.ahead = nrow(test_diff), dumvar = test_diff[, c(2,18:ncol(test_diff))], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
  }
}
pred$date[-1] <- test$date

truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)

for (i in 2:ncol(truth)) {
  mae_loop <- MAE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  mae[(i-1),count] <- mae_loop
}

colnames(mae)[count] <- modname

}



# NO SEASONALITY
count <- count+1
modname <- paste0("season", "None")

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = lag_order,
                       type = "both",
                       season = NULL,
                       exogen = train_diff[, c(2,18:ncol(train_diff))]
)
pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)
var.pred <- predict(var.model, n.ahead = nrow(test_diff), dumvar = test_diff[, c(2,18:ncol(test_diff))], ci = 0.95)
for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
  }
}
pred$date[-1] <- test$date
truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)
for (i in 2:ncol(truth)) {
  mae_loop <- MAE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  mae[(i-1),count] <- mae_loop
}
colnames(mae)[count] <- modname





# mae
# write.csv(mae, str_c(outputname, ".csv"))


colMeans(mae) %>% sort()
apply(mae, 2, sd) %>% sort()
