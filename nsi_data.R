library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(data.table)
library(janitor)
library(stringr)


#https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv

min_max <- function(x){
(x - min(x))/(max(x) - min(x))
}

be <- fread("COVID19BE_CASES_AGESEX.csv") %>%
  clean_names

be <- be %>%
  filter(province == "Luxembourg") %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(region = "Lux-Belge") %>%
  mutate(cases = min_max(cases))


# france
#vieilles données
#https://www.data.gouv.fr/fr/datasets/r/72050bc8-9959-4bb1-88a0-684ff8db5fb5

fr_old <- fread("donnees-tests-covid19-labo-hebdomadaire-2020-05-27-19h00.csv") %>%
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
         region = "Moselle")

# allemagne

de <- fread("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv") %>%
  filter(Bundesland == "Rheinland-Pfalz")


de <- de %>%
  rename(date = Refdatum) %>%  
  group_by(date) %>%
  summarise(cases = sum(AnzahlFall)) %>%
  mutate(date = as.Date(date, "%Y/%m/%d")) %>%  
  mutate(region = "Rheinland-Pfalz") %>%  
  mutate(cases = min_max(cases))

bind_rows(be, de) %>% 
  ggplot() +
  geom_line(aes(y = cases, x = date, colour = region))


## old


ecdc <- fread("covid_ecdc.csv")


lux_neighbours <- ecdc %>%
  filter(region_name %in% c("Region Wallonne", "Grand Est", "Luxembourg", "Rheinland-Pfalz")) %>%
  rename(rate = rate_14_day_per_100k) %>%
  mutate(year_week = paste0(year_week, "-1")) %>%
  mutate(year_week = ISOweek2date(year_week))

lux_neighbours %>%
  ggplot() +
  geom_line(aes(x = year_week, y = rate, colour = region_name, group = region_name))


lux_neighbours %>%
  select(nuts_code, year_week, rate) %>%
  pivot_wider(names_from = nuts_code, values_from = rate) %>% View 

