# Weather EDA Pre-processing

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, tidyquant, lubridate,
  stargazer, data.table, gridExtra,
  forecast, scales, tseries, tibbletime,
  lmtest, itsmr, here, fpp2,
  vars, MTS, car, caret,
  MLmetrics, imputeTS,
  psych,        # EDA
  visdat,       # missingness
  corrplot,     # correlation plot
  FactoMineR,   # EDA, PCA, MFA
  factoextra    # extract and visualize PCA/MFA
)


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

table(weather$name)

weather %<>% 
  mutate(region = case_when(
    name == "Australia" ~ "AUS",
    name %like% "United States" ~ "NA",
    name == "South America- Ecuador" ~ "SA",
    name %in% c("Africa-Angola", "Africa-Nigera") ~ "AFR",
    name %in% c("Europe, Paris, Île-de-France, France", "Africa, Nocera Umbra, Umbria, Italia") ~ "EUR",
    name %in% c("Middle East-Saudi Arabia", "Middle East-Iran/Iraq") ~ "ME",
    TRUE ~ NA_character_
  ))

table(weather$conditions)

weather %<>% 
  mutate(weathersit = case_when(
    conditions == "Clear" ~ "Clear",
    conditions %in% c("Overcast", "Partially cloudy") ~ "Cloudy",
    conditions %in% c("Rain", "Rain, Overcast", "Rain, Partially cloudy") ~ "Rain",
    conditions %in% c("Snow", "Snow, Overcast", "Snow, Partially cloudy") ~ "Snow",
    TRUE ~ NA_character_
  ))






weather %>% 
  ggplot(aes(region)) + geom_bar()


ggplot(weather, aes(region, maximum_temperature)) + geom_boxplot()
ggplot(weather, aes(region, minimum_temperature)) + geom_boxplot()

# plot difference in max & min daily temp
ggplot(weather, aes(region, maximum_temperature-minimum_temperature)) + geom_boxplot()

ggplot(weather, aes(region, temperature)) + geom_boxplot()
ggplot(weather, aes(region, wind_chill)) + geom_boxplot()  # no wind chills reported for Africa
ggplot(weather, aes(region, heat_index)) + geom_boxplot()
ggplot(weather, aes(region, precipitation)) + geom_boxplot()
ggplot(weather, aes(region, snow)) + geom_boxplot()
ggplot(weather, aes(region, snow_depth)) + geom_boxplot()
ggplot(weather, aes(region, wind_speed)) + geom_boxplot()
ggplot(weather, aes(region, wind_direction)) + geom_boxplot()
ggplot(weather, aes(region, wind_gust)) + geom_boxplot()
ggplot(weather, aes(region, visibility)) + geom_boxplot()
ggplot(weather, aes(region, cloud_cover)) + geom_boxplot()
ggplot(weather, aes(region, relative_humidity)) + geom_boxplot()
ggplot(weather, aes(region, conditions)) + geom_jitter(alpha = 0.25)



ggplot(weather, aes(minimum_temperature, maximum_temperature)) + geom_point(aes(color = region), alpha = 0.1)
ggplot(weather, aes(minimum_temperature, maximum_temperature)) + geom_point(aes(color = region), alpha = 0.5) + facet_wrap(~region)
ggplot(weather, aes(temperature, relative_humidity)) + geom_point(aes(color = region), alpha = 0.5) + facet_wrap(~region)
ggplot(weather, aes(temperature, heat_index)) + geom_point(aes(color = region), alpha = 0.5) + facet_wrap(~region)
ggplot(weather, aes(temperature, precipitation)) + geom_point(aes(color = region), alpha = 0.5) + facet_wrap(~region)


weather %>% 
  filter(region == "EUR") %>% 
  ggplot(aes(date_time, temperature, color = weathersit)) + geom_point() #+ facet_wrap(~weathersit)

