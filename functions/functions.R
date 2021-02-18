
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
