library(targets)
library(tarchetypes)
#library(covidGrandeRegion)
#library(dplyr)
##library(lubridate)
##library(tidyr)
#library(ggplot2)
##library(data.table)
##library(janitor)
##library(stringr)
#library(modeltime)
#library(timetk)
#library(tidymodels)

source("functions/functions.R")
options(clustermq.scheduler = "multicore")

tar_option_set(
  packages = c(
    "covidGrandeRegion",
    "dplyr",
    "data.table",
    "lubridate",
    "ggplot2",
    "modeltime",
    "timetk",
    "tidymodels"
  )
)

list(
  tar_target(
    raw_data,
    get_greater_region_data(),
    format = "fst"
  ),
  tar_target(
    raw_weekly_data,
    get_greater_region_data(daily = FALSE),
    format = "fst"
  ),
  tar_target(
    population_data,
    get_population_data(),
    format = "fst"
  ),
  tar_target(
    normalized_weekly_data,
    normalize_weekly_data(raw_weekly_data, population_data),
    format = "fst"
  ),
  tar_target(
    mobility_raw,
    fread("data/2020_LU_Region_Mobility_Report_2021_02_18.csv"),
    format = "fst"
  ),
  tar_target(
    mobility,
    prep_mobility(mobility_raw),
    format = "fst"
  ),
  tar_target(
    epid_curves,
    plot_epidem_curve(normalized_weekly_data),
    format = "qs"
  ),
  tar_render(
    paper,
    "paper/paper.Rmd"
  )
)


#
##usethis::use_data(covid_data, overwrite = TRUE)
#
#
#mobility <- fread("2020_LU_Region_Mobility_Report.csv")
#
#lu_mob <- mobility %>%
#  mutate(week = isoweek(date)) %>%
#  mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
#  mutate(week = paste0(year(date), "-W", week, "-1")) %>%
#  mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
#  mutate(week = ISOweek2date(week)) %>%  
#  group_by(week) %>%
#  summarise(stay_home = mean(residential_percent_change_from_baseline)) %>%
#  ungroup() %>%  
#  fill(stay_home, .direction = "down") %>%
#  mutate(cases = min_max(stay_home)) %>%
#  mutate(region = "mobility") 
#
#
#dataset <- bind_rows(lu, lu_mob, be, fr, de) %>%
#  select(week, cases, region)
#
#ggplot(dataset) +
#  geom_line(aes(y = cases, x = week, colour = region))
#dataset <- dataset %>%
#  pivot_wider(names_from = region, values_from = cases) %>%
#  clean_names %>%
#  filter(!is.na(lux_belge)) %>%
#  as.data.frame
#
#splits <- dataset %>%
#  time_series_split(date_var = week, assess = "1 month", cumulative = TRUE)
#
#splits %>%
#  tk_time_series_cv_plan() %>%
#  plot_time_series_cv_plan(week, gd_luxembourg, .interactive = FALSE)
#
#model_fit_prophet_boost <- prophet_boost(seasonality_daily = FALSE,
#                           seasonality_weekly = TRUE,
#                           seasonality_yearly = FALSE) %>%  
#  set_engine("prophet_xgboost")
#
#
#model_fit_arima_boost <- arima_boost() %>%
#  set_engine("arima_xgboost")
#
#recipe_spec <- recipe(gd_luxembourg ~ ., training(splits)) %>%
#  #step_timeseries_signature(week) %>%
#  #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
#  #        contains("second"), contains("xts")) %>%   
#  step_fourier(week, period = 365, K = 3)
#
#recipe_spec %>% prep() %>% juice() 
#
#workflow_fit_prophet_boost <- workflow() %>%
#  add_model(model_fit_prophet_boost) %>%
#  add_recipe(recipe_spec) 
#
#fitted_prophet_boost <- fit(workflow_fit_prophet_boost, training(splits))
#
#workflow_fit_arima_boost <- workflow() %>%
#  add_model(model_fit_arima_boost) %>%
#  add_recipe(recipe_spec) 
#
#fitted_arima_boost <- fit(workflow_fit_arima_boost, training(splits))
#
#model_table <- modeltime_table(
#  fitted_prophet_boost,
#  fitted_arima_boost
#)
#
#calibrated_wf <- modeltime_calibrate(model_table,
#                                     new_data = testing(splits))
#
#calibrated_wf %>%
#  modeltime_forecast(actual_data = dataset, new_data = testing(splits)) %>%
#  plot_modeltime_forecast()
