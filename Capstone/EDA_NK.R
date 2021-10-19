# Capstone EDA

pacman::p_load(pacman, rio, tidyverse)

data <- import("Capstone/data/Historical Crude Price Data.xlsx", 
               sheet = "Essar_Data_Consolidated")
str(data)

ggplot(data, aes(Date, `M#DATED BRENT`)) + geom_line()

t0 <- data$`M#DATED BRENT`
t1 <- c(NA_real_, diff(t0, lag = 1))

data.frame(cbind(Date = data$Date, diff = t1)) %>% 
  ggplot(aes(Date, diff)) + geom_line()
