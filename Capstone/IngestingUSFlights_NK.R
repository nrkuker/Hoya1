# INGESTING FLIGHT DATA
# Yearly operations data for domestic airports 

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect,
  stargazer, data.table,
  forecast, scales, tseries
)



data <- import("Capstone/data/WEB-Report-43146.xlsx",
               range = "A8:L2600") %>% clean_names()
str(data)

airports <- import("Capstone/data/airport-data_large.xlsx") %>% clean_names()
str(airports)

airports_small <- airports %>% 
  select(loc_id, region, state_id, state_name, name)
data_small <- data %>% 
  select(facility, calendar_year, total_operations)

data2 <- left_join(airports_small, data_small, by = c("loc_id" = "facility"))
data2 %>% head()

ggplot(data2, aes(calendar_year, total_operations, group = loc_id)) + geom_line()

data2 %>%
  group_by(calendar_year) %>%
  summarise(ops = sum(total_operations)) %>%
  plot()

table(airports$loc_id)
table(data$facility)

write.csv(data2, file = "Capstone/data/US_airport_operations_2015-2019.csv", row.names = F)
