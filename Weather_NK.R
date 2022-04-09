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

weather3 <- import("data/North Sea Weather Data - Bergen Norway.csv") %>% clean_names()
weather3 %<>% 
  dplyr::select(name, datetime, tempmax, tempmin, temp, precipprob, feelslike, 
                precip, snow, snowdepth, windspeed, winddir, windgust, visibility,
                cloudcover, humidity, conditions)
weather3 %<>% mutate(datetime = lubridate::as_date(datetime, format = "%m/%d/%y"))
weather3 %<>% mutate(across(tempmax:temp, ~ (.x * 1.8)+32))
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
  ))

table(weather$name)
table(weather$region)
table(weather$name, weather$region)

table(weather$conditions)

weather %<>% 
  mutate(weathersit = case_when(
    conditions %in% c("Clear") ~ "Clear",
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



ggplot(weather, aes(date_time, minimum_temperature, color = name)) + geom_line() + facet_wrap(~region)

weather %>% 
  filter(name %in% c("bergen, norway", "South St W, Norwood Young America, MN 55368, United States")) %>% 
  ggplot(aes(date_time, minimum_temperature, color = name)) + geom_line() + facet_wrap(~name)


w <- weather %>% 
  dplyr::select(date_time, name, minimum_temperature) %>% 
  pivot_wider(names_from = name, names_prefix = "mintemp_", values_from = minimum_temperature) %>% clean_names() 

weather$minimum_temperature %>% scale()
attr(scale(weather$minimum_temperature),"scaled:center")

w[,-1] %>% scale(
  # center = attr(scale(weather$minimum_temperature),"scaled:center"),
  # scale = attr(scale(weather$minimum_temperature),"scaled:scale")
) %>% summary()

ww <- weather %>% 
  dplyr::select(date_time, name, minimum_temperature) 
# ww$minimum_temperature %<>% scale()
ggplot(ww, aes(date_time, minimum_temperature, color = name)) + geom_line() + facet_wrap(~name)



ww %<>% 
  pivot_wider(names_from = name, names_prefix = "mintemp_", values_from = minimum_temperature) %>% clean_names() 

psych::describeBy(minimum_temperature ~ name, data = weather)

join_data[,25] %<>% scale(
  center = attr(scale(weather$minimum_temperature),"scaled:center"),
  scale = attr(scale(weather$minimum_temperature),"scaled:scale")
)

wMin <- weather %>% 
  group_by(date_time) %>% 
  summarize(min = min(minimum_temperature))

weather %>% 
  filter(date_time == as.Date("2016-01-01")) %>% 
  dplyr::select(name, minimum_temperature)

wMin %>% mutate(index = str_c(date_time, min))
weather %>% 
  mutate(index = str_c(date_time, minimum_temperature)) %>% 
  dplyr::select(index, name)
  

wMinName <- left_join(
  wMin %>% mutate(index = str_c(date_time, min)),
  weather %>% 
    mutate(index = str_c(date_time, minimum_temperature)) %>% 
    dplyr::select(index, name),
  by = "index"
) %>% 
  dplyr::select(-index)

wMinName %>% 
  filter(between(date_time, lower = as.Date("2015-10-20"), upper = as.Date("2019-05-01"))) %>% 
  group_by(name) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)) %>% 
  arrange(-count)

wMinName %>% 
  filter(between(date_time, lower = as.Date("2015-10-20"), upper = as.Date("2019-05-01"))) %>% 
  ggplot(aes(date_time, min, color = name)) + geom_point() +
  labs(title = "Lowest Minimum Daily Temperature among Locations in our Dataset",
       x = "Date", y = "Temperature (deg F)", color = "Location")# + 
  # scale_color_manual(labels = c("Italy", "Australia", "Paris", "Iran", "Arkansas",
  #                               "Ecuador", "Minnesota"),
  #                    values = ) +
  scale_color_discrete(labels = c("Norway", "Minnesota")) 


wMinName




  weather %>% 
  filter(region == "EUR" | region == "NA") %>% 
  ggplot(aes(date_time, temperature, color = weathersit)) + geom_point() + facet_grid(weathersit~region)




df <- weather %>%
  filter(name %in% c("Europe, Paris, Île-de-France, France", "United States-Houston")) %>%
  dplyr::select(name, date_time, conditions, weathersit)

table(df$conditions)
table(df$weathersit)

df %>% pivot_wider(names_from = name, values_from = c(conditions, weathersit)) %>% 
  rename(condition_EUR = `conditions_Europe, Paris, Île-de-France, France`,
         conditions_USA = `conditions_United States-Houston`,
         weathersit_EUR = `weathersit_Europe, Paris, Île-de-France, France`,
         weathersit_USA = `weathersit_United States-Houston`)

################################################################################

# TIME SERIES FOR PREDICTION
wMin <- weather %>% 
  group_by(date_time) %>% 
  summarize(min = min(minimum_temperature))

wMinName <- left_join(
  wMin %>% mutate(index = str_c(date_time, min)),
  weather %>% 
    mutate(index = str_c(date_time, minimum_temperature)) %>% 
    dplyr::select(index, name),
  by = "index"
) %>% 
  filter(between(date_time, lower = as.Date("2015-10-20"), upper = as.Date("2019-07-01"))) %>% 
  dplyr::select(-index)

wMinName

weather_ts <- ts(wMinName$min, frequency = 30)
plot(weather_ts)

weather_ts_add <- decompose(weather_ts, "additive")
weather_ts_mult <- decompose(weather_ts, "multiplicative")

plot(decompose(ts(wMinName$min, frequency = 30), "additive"))
plot(decompose(ts(wMinName$min, frequency = 62), "additive"))
plot(decompose(ts(wMinName$min, frequency = 90), "additive"))
plot(decompose(ts(wMinName$min, frequency = 250), "additive"))
plot(decompose(ts(wMinName$min, frequency = 365), "additive"))


