# FORECAST EVALUATION

# do everything up to running model from VAR_NK_3.R

# SET UP
seasonal_options <- c(5, 20, 30, 62, 90, 125, 180, 250, 365, NULL)
count <- 0 
outputname <- "mape_VAR_by_seasonality_financial&demand"
mape <- matrix(nrow = 15, ncol = 10) %>% as.data.frame()
rownames(mape) <- colnames(truth)[-1]


# LOOP THRU SEASONALITY
for (i in seasonal_options) {

SEASONALITY <- i
count <- count+1
modname <- paste0("season", i)

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "both",
                       season = i,
                       exogen = train_diff[, c(2,18:24)]
)

lastday <- train %>% tail(1)
pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)

var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:24)], ci = 0.95)

for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
  }
}
pred$date[-1] <- test$date

truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)

for (i in 2:ncol(truth)) {
  mape_loop <- MAPE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  mape[(i-1),count] <- mape_loop
}

colnames(mape)[count] <- modname

}



# NO SEASONALITY
count <- count+1
modname <- paste0("season", "None")

var.model <- vars::VAR(train_diff[, colnames(train_diff) %in% crudes_to_predict],
                       p = 1,
                       type = "const",
                       season = NULL,
                       exogen = train_diff[, c(2,18:24)]
)
pred <- data.frame(lastday) %>%
  dplyr::select(date, m_number_dated_brent:eagleford_45)
var.pred <- predict(var.model, n.ahead = 7, dumvar = test_diff[, c(2,18:24)], ci = 0.95)
for (i in crudes_to_predict) {
  for (j in 1:nrow(var.pred$fcst[[i]])) {
    pred[(j+1),i] <- var.pred$fcst[[i]][j,1] + pred[(j),i]
  }
}
pred$date[-1] <- test$date
truth <- rbind(lastday, test) %>% 
  dplyr::select(date, m_number_dated_brent:eagleford_45)
for (i in 2:ncol(truth)) {
  mape_loop <- MAPE(pred[-1,i], truth[-1,i])
  var <- colnames(pred)[i]
  mape[(i-1),count] <- mape_loop
}
colnames(mape)[count] <- modname





# mape
write.csv(mape, str_c(outputname, ".csv"))



