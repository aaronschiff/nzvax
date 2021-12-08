# Chart of vaccination rate by ethnicity over time for delta outbreak

library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(lubridate)

dat_vax <- read_excel(path = here("data/20211205_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx"), 
                      sheet = "Vaccination Query") |> 
  clean_names() |> 
  mutate(week_ending_date = as_date(week_ending_date)) |> 
  mutate(number_people_partially_vaccinated = as.integer(number_people_partially_vaccinated), 
         number_people_fully_vaccinated = as.integer(number_people_fully_vaccinated)) |> 
  print()

dat_pop <- read_excel(path = here("data/20211205_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx"), 
                      sheet = "HSU Table") |> 
  clean_names() |> 
  mutate(number_people_hsu = as.integer(number_people_hsu))

dat_vax_rate_by_week_ethnic_group <- 
  dat_vax |> 
  filter(week_ending_date >= ymd("2021-02-14")) |> 
  group_by(week_ending_date, ethnic_group) |> 
  summarise(number_people_partially_vaccinated = sum(number_people_partially_vaccinated), 
            number_people_fully_vaccinated = sum(number_people_fully_vaccinated)) |> 
  ungroup() |> 
  complete(week_ending_date, ethnic_group, 
           fill = list(number_people_partially_vaccinated = 0L, 
                       number_people_fully_vaccinated = 0L)) |> 
  arrange(ethnic_group, week_ending_date) |> 
  group_by(ethnic_group) |> 
  mutate(total_people_partially_vaccinated = cumsum(number_people_partially_vaccinated), 
         total_people_fully_vaccinated = cumsum(number_people_fully_vaccinated)) |> 
  left_join(y = dat_pop |> 
              filter(age_group != "< 12") |> 
              group_by(ethnic_group) |> 
              summarise(number_people_hsu = sum(number_people_hsu)) |> 
              ungroup(), 
            by = "ethnic_group") |> 
  mutate(partially_vax_rate = total_people_partially_vaccinated / number_people_hsu, 
         fully_vax_rate = total_people_fully_vaccinated / number_people_hsu) 

chart_vax_rate_by_week_ethnic_group <- dat_vax_rate_by_week_ethnic_group |> 
  select(week_ending_date, ethnic_group, partially_vax_rate, fully_vax_rate) |> 
  pivot_longer(cols = c(partially_vax_rate, fully_vax_rate), 
               names_to = "measure", 
               values_to = "value") |> 
  ggplot(mapping = aes(x = week_ending_date, 
                        y = value, 
                        colour = ethnic_group)) + 
  geom_line() + 
  facet_wrap(facets = vars(measure), ncol = 1)
