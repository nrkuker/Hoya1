# FORECAST EVALUATION

# do everything up to running model from VAR_NK_2.R

# SET UP
seasonal_options <- c(5, 20, 30, 62, 90, 125, 180, 250, 365, NULL)
count <- 0 
outputname <- "rmse_VAR_by_seasonality_financialexogdiff"
rmse <- matrix(nrow = 15, ncol = 10) %>% as.data.frame()
rownames(rmse) <- colnames(test)[3:17]


# LOOP THRU SEASONALITY
for (i in seasonal_options) {

SEASONALITY <- i
count <- count+1
modname <- paste0("season", SEASONALITY)

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "both",
                       season = SEASONALITY,
                       exogen = train_diff[, c(2,18:22)]
)

lastday <- train %>% tail(1)
pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)

var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:22)], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
  }
}
pred$date[-1] <- test$date

truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)

for (i in 2:ncol(truth)) {
  rmse_loop <- RMSE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  rmse[(i-1),count] <- rmse_loop
}

colnames(rmse)[count] <- modname

}



# NO SEASONALITY
count <- count+1
modname <- paste0("season", "None")

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "const",
                       season = NULL,
                       exogen = train_diff[, c(2,18:22)]
)
pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)
var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:22)], ci = 0.95)
for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
  }
}
pred$date[-1] <- test$date
truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)
for (i in 2:ncol(truth)) {
  rmse_loop <- RMSE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  rmse[(i-1),count] <- rmse_loop
}
colnames(rmse)[count] <- modname




# rmse
# write.csv(rmse, str_c(outputname, ".csv"))



