
# prepares google mobility data

prep_mobility <- function(mobility){
  mobility %>%
    mutate(week = isoweek(date)) %>%
    mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
    mutate(week = paste0(year(date), "-W", week, "-1")) %>%
    mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
    mutate(week = covidGrandeRegion:::ISOweek2date(week)) %>%  
    group_by(week) %>%
    summarise(stay_home = mean(residential_percent_change_from_baseline)) %>%
    ungroup() %>%  
    fill(stay_home, .direction = "down") %>%
    mutate(region = "mobility") 
}

plot_epidem_curve <- function(covid_data){
    ggplot(covid_data) +
    geom_line(aes(y = cases, x = week, colour = country)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(title = "Weekly COVID-19 cases in the countries of the Greater Region",
         subtitle = "Positive cases per 100'000 inhabitants")
}

normalize_weekly_data <- function(covid_data, population){
  
  covid_data_country <- covid_data %>%
    filter(!is.na(cases)) %>%  
    group_by(week, country) %>%
    summarise(cases = sum(cases))

  population_country <- population %>%
    group_by(country) %>%
    summarise(population = sum(population))

  covid_data_country  %>%
    right_join(population_country) %>%
    mutate(cases = cases/population*100000)
}

get_population_data <- function(){
  covidGrandeRegion::population
}

prep_data_for_model <- function(covid_data, mobility){
  covid_data %>%
    filter(!is.na(cases)) %>%
    group_by(week, country) %>%
    summarise(cases = sum(cases)) %>%  
    pivot_wider(names_from = country, values_from = cases) %>%
    left_join(select(mobility, -region))
}

view_cv_plan <- function(splits){
  splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(week, Luxembourg, .interactive = FALSE)
}

set_up_prophet_boost_model <- function(){
  prophet_boost(seasonality_daily = FALSE,
                seasonality_weekly = FALSE,
                seasonality_yearly = FALSE) %>%
    set_engine("prophet_xgboost")
}

set_up_arima_boost_model <- function(...){
  arima_boost(...) %>%
    set_engine("arima_xgboost")
}

set_up_recipe_spec <- function(formula, splits){
  recipe(formula, training(splits)) %>%
    step_lag(all_numeric(), lag = seq(1, 4), default = 0) %>%  
    #step_timeseries_signature(week) %>%
    #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
    #        contains("second"), contains("xts")) %>%
    step_fourier(week, period = 365, K = 3)
}

view_prepped_training_data <- function(recipe_spec){
  recipe_spec %>%
    prep() %>%
    juice()
}

setup_workflow <- function(model, recipe_spec){
  workflow() %>%
    add_model(model) %>%
    add_recipe(recipe_spec) 
}

view_forecast <- function(calibrated_wf, actual_dataset, splits){
  calibrated_wf %>%
    modeltime_forecast(actual_data = actual_dataset, new_data = testing(splits)) %>%
    plot_modeltime_forecast(.interactive = FALSE)
}


