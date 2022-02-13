# FORECAST EVALUATION

# do everything up to running model from VAR_NK_2.R

seasonal_options <- c(5, 20, 30, 62, 90, 125, 180, 250, 365, NULL)
count <- 0 
outputname <- "rmse_VAR_by_seasonality_financialexogscaled_plusUSDavgprice"

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
lower <- pred
upper <- pred


var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:22)], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
    lower[(j+1),i] <- var.pred$fcst[[i]][j,2] + pred[(j),i]
    upper[(j+1),i] <- var.pred$fcst[[i]][j,3] + pred[(j),i]
  }
  # print(pred[,i])
}
pred$date[-1] <- test$date

truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)


resid <- matrix(nrow = nrow(truth)-1, ncol = ncol(truth)-1) %>% as.data.frame()
colnames(resid) <- colnames(truth)[-1]

# rmse <- matrix(nrow = 15, ncol = 10) %>% as.data.frame()
# rownames(rmse) <- colnames(truth)[-1]

for (i in 2:ncol(truth)) {
  rmse_loop <- RMSE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  # print(paste0("RMSE for ", var, ": ", rmse_loop))
  
  rmse[(i-1),count] <- rmse_loop
}

colnames(rmse)[count] <- modname

}

# rmse
write.csv(rmse, str_c(outputname, ".csv"))



