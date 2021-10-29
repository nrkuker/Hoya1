# INGESTING & MODIFYING WVS

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect,
  stargazer, data.table,
  forecast, scales, tseries
)



wave6 <- import("Capstone/data/WV6_Data_R_v20201117.rdata")
vars <- colnames(wave6)



vars6_of_interest <- c("V1", "V2", "V2A", "V23", "V45", "V55", "V60", "V61", "V68",
                      "V81", "V95", "V99", "V141", "V183", "V184", "V185", "V212",
                      "V240","V241", "V242", "V248"
                      )
# V23=satisfaction
# V45 = menJobs
# V55 = choiceFreedom
# V60/61 = countryAims1/2
# V68 = futureMoreTech
# V95 = political
# V125s?
# V141 = democracy
# V183/4/5 = worryWar / Terror / CivilWar
# V212 = worldCitizen

table(wave6$V1)

wave6small <- wave6 %>% 
  dplyr::select(all_of(vars6_of_interest))

summary(wave6small[,-c(1:3)])
visdat::vis_miss(wave6small)
wave6small <-  wave6small[complete.cases(wave6small) == T,]
summary(wave6small[,-c(1:3)])
wave6small$V2 %>% table()


wave7 <- import("Capstone/data/WVS_Cross-National_Wave_7_R_v2_0.rds")
colnames(wave7)

vars7_of_interest <- c("A_YEAR", "B_COUNTRY", )

wave7 %>% 
  dplyr::select(all_of(vars_of_interest))
