
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


get_normalized_monthly_data <- function(raw_weekly_data, population){

  raw_weekly_data %>%
    group_by(year = year(week), month = month(week), sub_region) %>%
    mutate(month = as.character(month)) %>%  
    summarise(cases = sum(cases)) %>%
    mutate(month = ifelse(nchar(month) == 1, paste0("0", month), month)) %>%  
    mutate(year_month = paste0(year, "-", month)) %>%  
    left_join(select(population, -year)) %>%
    mutate(cases_per_100k = cases/population * 100000)

}

predict_wrapper2 <- function(model, newdata){
  workflows:::predict.workflow(object = model, new_data = newdata)
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

get_grande_region_map <- function(){
  covidGrandeRegion::grande_region_map
}

plot_epidem_map <- function(normalized_monthly_data, grande_region_map){

  grande_region_map <- geojson_sf(grande_region_map)

  data_to_plot <- normalized_monthly_data %>%
    left_join(grande_region_map, by = c("sub_region" = "NAME_2"))

  first_wave <- ggplot() +
    geom_sf(data = grande_region_map, colour = "black", alpha = 0) +
    geom_sf(data = filter(data_to_plot,
                        year_month %in% paste0("2020-0", seq(2, 8))), aes(geometry = geometry, fill = cases_per_100k)) +
    scale_fill_continuous_sequential(palette = "Heat", name = "Cases per 100k inhabitants") +
    theme_void() +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(barheight = 0.5, barwidth =  10)) +
    facet_wrap(~year_month) +
    labs(title = "Monthly cases in the Greater Region",
         subtitle = "Positive cases per 100'000 inhabitants, from February 2020 to August 2020")


  second_wave <- ggplot() +
    geom_sf(data = grande_region_map, colour = "black", alpha = 0) +
    geom_sf(data = filter(data_to_plot,
                          !(year_month %in% paste0("2020-0", seq(2, 8)))), aes(geometry = geometry, fill = cases_per_100k)) +
    scale_fill_continuous_sequential(palette = "Heat", name = "Cases per 100k inhabitants") +
    theme_void() +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(barheight = 0.5, barwidth =  10)) +
    facet_wrap(~year_month) +
    labs(title = "Monthly cases in the Greater Region, second wave",
         subtitle = "Positive cases per 100'000 inhabitants, from September 2020 to February 2021")

  list("map_first_wave" = first_wave,
       "map_second_wave" = second_wave)
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

setup_prophet_boost_model <- function(){
  prophet_boost(seasonality_daily = FALSE,
                seasonality_weekly = FALSE,
                seasonality_yearly = FALSE) %>%
    set_engine("prophet_xgboost")
}

setup_arima_boost_model <- function(...){
  arima_boost(...) %>%
    set_engine("arima_xgboost")
}

setup_recipe_spec <- function(formula, splits){
  recipe(formula, training(splits)) %>%
    step_lag(all_numeric(), lag = seq(1, 4), default = 0) #%>%  
    #step_mutate(week = as.numeric(week))
    #step_timeseries_signature(week) %>%
    #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
    #        contains("second"), contains("xts")) %>%
    #step_fourier(week, period = 365, K = 3)
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
    plot_modeltime_forecast(.interactive = FALSE) +
    guides(col = guide_legend(nrow = 2))
}


