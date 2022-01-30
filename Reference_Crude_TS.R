# PREDICT REFERENCE

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
str(data)
colnames(data)

colnames(data)[!(colnames(data) %like% "m_number")]

djia <- import("Capstone/data/djia.csv", skip = 15, colClasses = c("POSIXct", "numeric"))
str(djia)
djia$date[1:10]
djia$date <- force_tz(djia$date, "UTC")
djia$date[1:10]



data2 <- data %>% 
  dplyr::select(date, m_number_dated_brent, dollar_euros)

# # brent <- ts(data$m_number_dated_brent, frequency = 30)
# # plot(brent)
# 
# # start_date <- data$date[1]
# # end_date <- data$date[nrow(data)]
# # dates <- seq(start_date, end_date, by = "day")
# 
# # left_join(dates, data[,c("date", "m_number_dated_brent")], by = "date")








# WITH DJIA ####
data2_djia <- left_join(data2, djia, by = "date") %>% 
  rename(djia = value)
sum(is.na(data2_djia))
visdat::vis_miss(data2_djia)

data2_djia %<>% filter(!is.na(djia))

# brent <- ts(data2_djia[,-1], frequency = 30)
# plot(brent)




refdj_ts <- ts(data2_djia[,-1], frequency = 1)
plot(refdj_ts)
sum(is.na(refdj_ts))

#####
# ts.d <- diffM(refdj_ts, d = 1)
# var.a <- vars::VAR(ts.d,
#                    lag.max = 10, #highest lag order for lag length selection according to the choosen ic
#                    ic = "AIC", #information criterion
#                    type = "none") #type of deterministic regressors to include
# summary(var.a)
# 
# 
# 
# autoplot(refdj_ts, facets = TRUE) +
#   xlab("") + ylab("") +
#   ggtitle("")
# 
# # plot the ACF
# ggAcf(refdj_ts)
# 
# # plot the ACF
# ggPacf(refdj_ts)
# 
# brent_freq30 <- refdj_ts[,"m_number_dated_brent"] %>% ts(., frequency = 30)
# brent_stl <- stl(brent_freq30, s.window = "periodic")
# autoplot(brent_stl)
# 
# djia_freq30 <- refdj_ts[,"djia"] %>% ts(., frequency = 30)
# djia_stl <- stl(djia_freq30, s.window = "periodic")
# autoplot(djia_stl)
#####

exog_reg <- as.matrix(refdj_ts[,-1])
arimax1 <- auto.arima(refdj_ts[,"m_number_dated_brent"],
                      xreg = refdj_ts[,"dollar_euros"],
                      trace = T, seasonal = F, stepwise = F, approximation = F)
summary(arimax1)
# Best model: Regression with ARIMA(0,1,0) errors
# Series: refdj_ts[, "m_number_dated_brent"] 
# Regression with ARIMA(0,1,0) errors 
# 
# Coefficients:
#   xreg
# 9.8838
# s.e.  6.7281
# 
# sigma^2 = 1.19:  log likelihood = -1273.42
# AIC=2550.85   AICc=2550.86   BIC=2560.33
# 
# Training set error measures:
#   ME    RMSE       MAE        MPE     MAPE      MASE         ACF1
# Training set 0.03224663 1.08948 0.8254562 0.03035572 1.561583 0.9977365 -0.003488188


refdj_ts2 <- ts(data2_djia[,-1], frequency = 365)
arimax2 <- auto.arima(refdj_ts2[,"m_number_dated_brent"],
                      xreg = refdj_ts2[,"dollar_euros"],
                      trace = T, seasonal = T, stepwise = T, approximation = F)
summary(arimax2)
# Best model: Regression with ARIMA(0,1,0)(0,1,0)[365] errors 
# Series: refdj_ts2[, "m_number_dated_brent"] 
# Regression with ARIMA(0,1,0)(0,1,0)[365] errors 
# 
# Coefficients:
#   xreg
# 7.7947
# s.e.  8.8152
# 
# sigma^2 = 2.172:  log likelihood = -872.06
# AIC=1748.13   AICc=1748.15   BIC=1756.48
# 
# Training set error measures:
#   ME     RMSE       MAE        MPE     MAPE       MASE       ACF1
# Training set 0.01503668 1.109567 0.6590076 0.01985414 1.056049 0.03781169 0.02267338


autoplot(arimax1)
checkresiduals(arimax1) 
test(resid(arimax1))    # from package itsmr

autoplot(arimax2)
checkresiduals(arimax2) # wtf even just no
test(resid(arimax2))    # from package itsmr

arimax3 <- auto.arima(refdj_ts[,"m_number_dated_brent"],
                      xreg = refdj_ts[,"dollar_euros"],
                      trace = T, seasonal = T, stepwise = F, approximation = F)
summary(arimax3)



################################################################################

# create functions
CheckNormal <- function(model) {
  hist(model$residuals, breaks = 30)
  shaptest <- shapiro.test(model$residuals)
  print(shaptest)
  if (shaptest$p.value <= 0.05) {
    print("H0 rejected: the residuals are NOT distributed normally")
  } else {
    print("H0 failed to reject: the residuals ARE distributed normally")
  }
}
CheckSced <- function(model){
  plot(model$fitted.values, model$residuals)
  abline(h = 0, col = "red")
  BP <- bptest(model)
  print(BP)
  if (BP$p.value <= 0.05) {
    print("H0 rejected: Error variance spreads INCONSTANTLY/generating patterns (Heteroscedasticity)")
  } else {
    print("H0 failed to reject: Error variance spreads CONSTANTLY (Homoscedasticity)")
  }
}
metrics <- function(y_pred, y_true){
  mse <- MSE(y_pred, y_true)
  rmse <- RMSE(y_pred, y_true)
  mae <- MAE(y_pred, y_true)
  print(paste0("MSE: ", round(mse, 6)))
  print(paste0("RMSE: ", round(rmse, 6)))
  print(paste0("MAE: ", round(mae, 6)))
  corPredAct <- cor(y_pred, y_true)
  print(paste0("Correlation: ", round(corPredAct, 6)))
  print(paste0("R^2 between y_pred & y_true: ", round(corPredAct^2, 6)))
}



# WITH LAGGED PREDICTOR

data2_djia %<>% 
  cbind(brent_lag1 = lag(data2_djia$m_number_dated_brent, 1))

ols <- lm(m_number_dated_brent ~ brent_lag1 + dollar_euros + djia, data = data2_djia)
summary(ols)

CheckNormal(ols)  # resid not normal
CheckSced(ols)    # constant variance

qqnorm(ols$residuals); qqline(ols$residuals)

dwt(ols) # null = uncorrelated; fail to reject
vif(ols) # collinearity in lag and DJIA

# OUTLIERS
cooks.distance(ols) %>% sort(decreasing = T) %>% head(10)
hatvalues(ols) %>% sort(decreasing = T) %>% head(10)

plot(ols)
  # resid vs fitted: 758, 766, 767
  # normal QQ: same
  # resid vs leverage: 764 highest lev, 767 & 780 other "outliers"




# WITH DIFFERENCED DATA - NOPE

data3 <- data2_djia %>% 
  mutate(brent_diff = c(NA_real_, diff(data2_djia$m_number_dated_brent, 1)))


ols <- lm(m_number_dated_brent ~ brent_diff + dollar_euros + djia, data = data3)
summary(ols)

CheckNormal(ols)  # resid not normal
CheckSced(ols)    # constant variance

qqnorm(ols$residuals); qqline(ols$residuals)

dwt(ols) # null = uncorrelated; fail to reject
vif(ols) # collinearity in lag and DJIA

# OUTLIERS
cooks.distance(ols) %>% sort(decreasing = T) %>% head(10)
hatvalues(ols) %>% sort(decreasing = T) %>% head(10)

plot(ols)
# resid vs fitted: 758, 766, 767
# normal QQ: same
# resid vs leverage: 764 highest lev, 767 & 780 other "outliers"






################################################################################

# DATA PARTITION ####
set.seed(123)
index <- createDataPartition(data3$m_number_dated_brent, p = 0.8, 
                                  list = FALSE)
train <- data3[index[,1], ]
test  <- data3[-index[,1], ]



train[,-1] %>% PerformanceAnalytics::chart.Correlation()

ols1 <- lm(m_number_dated_brent ~ brent_lag1 + dollar_euros + djia, data = train)
summary(ols1)

CheckNormal(ols1)  
CheckSced(ols1)   
qqnorm(ols1$residuals); qqline(ols1$residuals)
dwt(ols1) # null = uncorrelated
vif(ols1)
metrics(y_pred = ols1$fitted.values, y_true = train$m_number_dated_brent[-1])
plot(ols1$fitted.values, train$m_number_dated_brent[-1])


# no dollar_euros
ols2 <- lm(m_number_dated_brent ~ brent_lag1 + djia, data = train)
summary(ols2)

CheckNormal(ols2)  
CheckSced(ols2)   
qqnorm(ols2$residuals); qqline(ols2$residuals)
dwt(ols2) # null = uncorrelated
vif(ols2)
metrics(y_pred = ols2$fitted.values, y_true = train$m_number_dated_brent[-1])


# no djia
ols3 <- lm(m_number_dated_brent ~ brent_lag1 + dollar_euros, data = train)
summary(ols3)

CheckNormal(ols3)  
CheckSced(ols3)   
qqnorm(ols3$residuals); qqline(ols3$residuals)
dwt(ols3) # null = uncorrelated
vif(ols3)
metrics(y_pred = ols3$fitted.values, y_true = train$m_number_dated_brent[-1])


# just lagged
ols4 <- lm(m_number_dated_brent ~ brent_lag1, data = train)
summary(ols4)

CheckNormal(ols4)  
CheckSced(ols4)   
qqnorm(ols4$residuals); qqline(ols4$residuals)
dwt(ols4) # null = uncorrelated
vif(ols4)
metrics(y_pred = ols4$fitted.values, y_true = train$m_number_dated_brent[-1])










# WITH FINANCIAL DATA
financial <- import("data/Crude3.csv", na.strings = "#N/A") %>% clean_names()

financial$date %<>% lubridate::as_date(format = "%m/%d/%Y")
financial$usd_fx_index %<>% as.numeric()

data2 <- data[,c(1, 80, 87:101)]
join_data <- left_join(data2, financial, by = "date")

data2_djia <- join_data[,-(4:17)]

data2_djia %<>% 
  cbind(brent_lag1 = lag(data2_djia$m_number_dated_brent, 1))

ols <- lm(m_number_dated_brent ~ #dollar_euros +   
            usd_fx_index + emerging_market_etf + brent_lag1
          , data = data2_djia)
summary(ols)
# attempts: 
# all
# -S&P: FX not signif
# -avg price: only lag & FX, var looks ok but fails test, uncorrel
# -dollar euros: best so far


CheckNormal(ols)  # 
CheckSced(ols)    # 

# qqnorm(ols$residuals); qqline(ols$residuals)

dwt(ols) # null = uncorrelated; 
vif(ols) # 

# OUTLIERS
cooks.distance(ols) %>% sort(decreasing = T) %>% head(10)
hatvalues(ols) %>% sort(decreasing = T) %>% head(10)

plot(ols)


# STEPWISE SELECTION

data2_djia_noNA <- data2_djia[complete.cases(data2_djia)==T,]

model_start <- glm(m_number_dated_brent ~ brent_lag1, 
                     data = data2_djia_noNA) 
model_end   <- glm(m_number_dated_brent ~ brent_lag1 + dollar_euros + avg_price_all +  
                   s_p_close + usd_fx_index + dow_dji_close + emerging_market_etf,
                     data = data2_djia_noNA)

# Backward Approach, trace=TRUE, information is printed during the running of step 
back <- stats::step(model_end, direction = "backward", trace = F)
summary(back)

# Forward Approach
forward <- stats::step(model_start, scope = list(lower = model_start, upper = model_end),
                               direction = "forward", trace = T)
summary(forward)

# Stepwise Approach
both <- stats::step(model_start, scope = list(lower = model_start, upper = model_end),
                            direction = "both", trace = F)
summary(both)










################################################################################

# PCA ###


# scale the data
data_scaled <- data[,sapply(data, function(x) is.numeric(x)) == T] %>% scale() %>% as.data.frame()

# handle
sapply(data_scaled, function(x) sum(is.na(x))) %>% sort(decreasing = T) %>% head()
data_scaled %<>% dplyr::select(-m_number_vgo_1_6pct_nwe_barge)

# correlation matrix of standardized data
c <- cor(data_scaled)
# correlation plot
corrplot(c, method = "color",
         # addCoef.col = T, number.digits = 2,
         type = "full",
         diag = T,
         # addgrid.col = "darkgrey",
         mar = c(0,0,2,0),
         title = "Variable Correlation Plot",
         order  = "FPC",
         # addrect = 6,
         tl.col = "black",
         tl.cex = 0.5,
         tl.srt = 45
)
# everything in this dataset moves together

# perform PCA
pca <- prcomp(data_scaled, 
              center = TRUE,
              scale. = TRUE)
summary(pca)

# screeplot
fviz_eig(pca, 
         choice = "variance", 
         addlabels = T) + 
  labs(title = "Scree Plot of Variance Explained by PCA Dimension") +
  theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))

fviz_eig(pca, 
         choice = "eigenvalue",
         addlabels = T) + 
  labs(title = "Scree Plot of Eigenvalues by PCA Dimension") +
  theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))

# extract results at variable level
var <- get_pca_var(pca)

# display as a correlation plot
var$cor[,1:2] %>% as.data.frame() %>% arrange(-Dim.1) %>% 
  as.matrix() %>% 
  corrplot(is.corr = F, tl.col = "black",
           cl.align.text = "l",
           mar = c(0,0,2,0),
           tl.cex = 0.5,
           title = "Correlations Between Variables & Dimensions")


# Contributions of variables to all PCs kept
fviz_contrib(pca, choice = "var", axes = 1:7)
# dashed line is expected value if contributions were uniform

# Contributions of variables to individual PCs
fviz_contrib(pca, choice = "var", axes = 1)
fviz_contrib(pca, choice = "var", axes = 2)


# Correlation Plots
fviz_pca_var(pca, col.var = "cos2",
             labelsize = 3,
             # select.var = list(cos2 = 0.3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoids text overlapping
) + labs(title = "Variable Correlation - Dims 1 & 2",
         subtitle = "colored by 'quality of representation'")

fviz_pca_var(pca, col.var = "contrib",
             labelsize = 3,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
) + labs(title = "Variable Correlation - Dims 1 & 2",
         subtitle = "colored by variable contribution")

fviz_pca_var(pca, col.var = "cos2",
             axes = c(1,2),
             labelsize = 3,
             # select.var = list(cos2 = 0.3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


















################################################################################

# TABLING FOR NOW ###

# tried <- data.frame(matrix(nrow = 10, ncol = 10))
# colnames(tried) <- c("call", "normality_null", "sced_null", "dwt_null", "vif", "mse", "rmse", "mae", "corr", "r2")
# 
# x <- vector(mode = "character", length = 10)
# tried[1,1] <- paste0(ols1$call[1], "(", ols1$call[2], ", data = ", ols1$call[3], ")")


