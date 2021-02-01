library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(data.table)
library(janitor)
library(stringr)

# function below from https://cran.r-project.org/web/packages/ISOweek/index.html
ISOweekday <- function(date) {
  date <- as.Date(date)
  return(as.integer((as.integer(format(date, "%w"))+6) %% 7 + 1))
}

weekday0 <- function(date) {
  return(ISOweekday(date) - 1L)
}

thursday0 <- function(date) {
  date <- as.Date(date)
  return(date - weekday0(date) + 3)
}

year0 <- function(date) {
  date <- as.Date(date)
  return(as.integer(format(date, "%Y")))
}

ISOweek2date <- function(weekdate) {
  kPattern <- "^([0-9]{4})-W([0-9]{2})-([0-9]{1})$"
  # not used kPattern <- "^([0-9]{4})-W([0][1-9]|[1-4][0-9]|[5][0-3])-([1-7]{1})$"
  # instead check ranges separately
  stopifnot(all(is.na(weekdate) | stringr::str_detect(weekdate, kPattern)))
  wd_ywd <- stringr::str_match(weekdate, kPattern)
  # take care of all NA input because this will break the split into 4 columns
  if (all(is.na(weekdate))) {
    return(rep(as.Date(NA_character_), length.out = length(weekdate)))
  }
  stopifnot(ncol(wd_ywd) == 4)
  year <- wd_ywd[, 2]
  week <- as.integer(wd_ywd[, 3])
  weekday <- as.integer(wd_ywd[, 4])
  stopifnot(all(is.na(week) | (1 <= week & week <= 53)))
  stopifnot(all(is.na(weekday) | (1 <= weekday & weekday <= 7)))
  # first week of the year includes always the 4th of January,
  # take care of NA dates
  january04 <- as.Date(ifelse(is.na(year), NA, paste(year, "01", "04", sep="-")))
  # first thursday of the year
  first_thursday <- thursday0(january04)
  # advance by week-1 thursdays
  nearest_thursday <- first_thursday + 7 * (week - 1)
  # correct for weekday
  return(nearest_thursday - 4 + weekday)
}



#https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv

min_max <- function(x, na.rm = FALSE){
(x - min(x, na.rm))/(max(x, na.rm) - min(x, na.rm))
}

be <- fread("COVID19BE_CASES_AGESEX.csv") %>%
  clean_names

be <- be %>%
  filter(province == "Luxembourg") %>%
  filter(date >= ymd("2020-02-24")) %>%
  mutate(week = isoweek(date)) %>%
  mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
  mutate(week = paste0(year(date), "-W", week, "-1")) %>%
  mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
  mutate(week = ISOweek2date(week)) %>%  
  group_by(week) %>%  
  summarise(cases = sum(cases)) %>%
  mutate(region = "Lux-Belge") %>%
  ungroup() %>%  
  mutate(cases = cases/285000*1000) #for 100k


#vieilles données
#https://www.data.gouv.fr/fr/datasets/r/72050bc8-9959-4bb1-88a0-684ff8db5fb5

fr_alt <- fread("chiffres-cles.csv") %>%
  filter(maille_code == "DEP-57") %>%
  select(date, maille_nom, cas_confirmes) %>%
  filter(!is.na(cas_confirmes)) %>%  
  mutate(week = isoweek(date)) %>%
  mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
  mutate(week = paste0(year(date), "-W", week, "-1")) %>%
  mutate(week = ISOweek2date(week)) %>%
  select(week, nb_pos = cas_confirmes)

fr_old <- fread("donnees-tests-covid19-labo-hebdomadaire-2020-05-27-19h00.csv")%>%
  filter(dep == 57) %>%
  select(week, nb_test, nb_pos) 

# données à partir de mai

# https://www.data.gouv.fr/fr/datasets/r/dd3ac13c-e87f-4b33-8897-07baff4e1783

fr_new <- fread("sp-pos-heb-dep-2021-01-28-19h20.csv") %>%
  filter(dep == 57) %>%
  select(week, nb_pos = P, nb_test = T)

fr <- bind_rows(fr_old,
                fr_new) %>%
  group_by(week) %>%
  summarise_all(.funs = sum) %>%
  mutate(cases = min_max(nb_pos),
         region = "Moselle") %>%
  mutate(week = str_replace_all(week, "S", "W")) %>%  
  mutate(week = paste0(week, "-1")) %>%
  mutate(week = ISOweek2date(week)) %>%
  ungroup() %>%
  bind_rows(fr_alt) %>%  
  group_by(week) %>%
  summarise(cases = sum(nb_pos)) %>%  
  ungroup() %>%  
  mutate(cases = cases/1043522*1000) %>%   #for 100k
  mutate(region = "Moselle") 


de_all <- fread("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv") %>%
   filter(Landkreis %in% c("LK Bitburg-Prüm", "LK Trier-Saarburg", "SK Trier", "LK Merzig-Wadern"))
 
 
de <- de_all %>%
  rename(date = Refdatum) %>%
  mutate(date = str_sub(date, 1, 10)) %>%  
  mutate(date = ymd(date)) %>%  
  filter(date >= ymd("2020-02-24")) %>% 
  mutate(week = isoweek(date)) %>%
  mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
  mutate(week = paste0(year(date), "-W", week, "-1")) %>%
  mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
  mutate(week = ISOweek2date(week)) %>%  
  group_by(week) %>%  
  summarise(cases = sum(AnzahlFall)) %>%
  ungroup() %>%  
  mutate(cases = cases/(99058+149398+111528+103243)*1000) %>%  
  mutate(region = "Rheinland-Pfalz") 
 
 
path_data <- "../covid_story/datapublic-covid19.csv"
 
lu <- fread(path_data) %>%
  clean_names() %>%
  mutate(date = dmy(date)) %>%
  filter(date >= ymd("2020-02-24")) %>% 
  mutate(across(where(is.character), as.numeric)) %>%
  rename_all(~str_remove(., "x1_")) %>%  
  mutate(week = isoweek(date)) %>%
  mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
  mutate(week = paste0(year(date), "-W", week, "-1")) %>%
  mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
  mutate(week = ISOweek2date(week)) %>%  
  group_by(week) %>%
  summarise(cases = sum(nb_de_positifs, na.rm = TRUE)) %>%
  ungroup() %>%  
  mutate(cases = cases/626108*1000) %>%  
  mutate(region = "GD Luxembourg")

bind_rows(lu, be, fr, de) %>% 
  ggplot() +
  geom_line(aes(y = cases, x = week, colour = region))

#usethis::use_data(covid_data, overwrite = TRUE)


mobility <- fread("2020_LU_Region_Mobility_Report.csv")

lu_mob <- mobility %>%
  mutate(week = isoweek(date)) %>%
  mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
  mutate(week = paste0(year(date), "-W", week, "-1")) %>%
  mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
  mutate(week = ISOweek2date(week)) %>%  
  group_by(week) %>%
  summarise(stay_home = mean(residential_percent_change_from_baseline)) %>%
  ungroup() %>%  
  fill(stay_home, .direction = "down") %>%
  mutate(cases = min_max(stay_home)) %>%
  mutate(region = "mobility") 


dataset <- bind_rows(lu, lu_mob, be, fr, de) %>%
  select(week, cases, region)

ggplot(dataset) +
  geom_line(aes(y = cases, x = week, colour = region))

library(modeltime)
library(timetk)
library(recipes)
library(tidymodels)

dataset <- dataset %>%
  pivot_wider(names_from = region, values_from = cases) %>%
  clean_names %>%
  filter(!is.na(lux_belge)) %>%
  as.data.frame

splits <- dataset %>%
  time_series_split(date_var = week, assess = "1 month", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(week, gd_luxembourg, .interactive = FALSE)

model_fit_prophet_boost <- prophet_boost(seasonality_daily = FALSE,
                           seasonality_weekly = TRUE,
                           seasonality_yearly = FALSE) %>%  
  set_engine("prophet_xgboost")


model_fit_arima_boost <- arima_boost() %>%
  set_engine("arima_xgboost")

recipe_spec <- recipe(gd_luxembourg ~ ., training(splits)) %>%
  #step_timeseries_signature(week) %>%
  #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
  #        contains("second"), contains("xts")) %>%   
  step_fourier(week, period = 365, K = 3)

recipe_spec %>% prep() %>% juice() 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_fit_prophet_boost) %>%
  add_recipe(recipe_spec) 

fitted_prophet_boost <- fit(workflow_fit_prophet_boost, training(splits))

workflow_fit_arima_boost <- workflow() %>%
  add_model(model_fit_arima_boost) %>%
  add_recipe(recipe_spec) 

fitted_arima_boost <- fit(workflow_fit_arima_boost, training(splits))

model_table <- modeltime_table(
  fitted_prophet_boost,
  fitted_arima_boost
)

calibrated_wf <- modeltime_calibrate(model_table,
                                     new_data = testing(splits))

calibrated_wf %>%
  modeltime_forecast(actual_data = dataset, new_data = testing(splits)) %>%
  plot_modeltime_forecast()
