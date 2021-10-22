# Comparison of Stats NZ and HSU population by DHB

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(glue)
library(janitor)
library(readxl)

latest_date <- "19_10_2021"               # Date of most recent week's data

# *****************************************************************************


# *****************************************************************************
# Load data ----

# HSU population by DHB area
pop_hsu <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                      sheet = "HSU Population") |>
  clean_names() |> 
  select(-x6, -x7, -notes) |> 
  mutate(population = as.integer(population)) |> 
  rename(pop_hsu = population) |> 
  group_by(dhb_of_residence) |> 
  summarise(pop_hsu = sum(pop_hsu)) |> 
  ungroup() |> 
  filter(dhb_of_residence != "Overseas / Unknown", 
         dhb_of_residence != "Various") 

# Stats NZ population estimates June 2021
pop_snz <- read_excel(path = here("data/statsnz-population-by-dhb.xlsx"), 
                      skip = 4) |> 
  clean_names() |> 
  filter(!is.na(x1996)) |> 
  select(-x2) |> 
  rename(dhb = year_at_30_june) |> 
  pivot_longer(cols = -dhb, names_to = "year", values_to = "population") |> 
  mutate(year = str_remove(string = year, pattern = "x")) |> 
  mutate(year = as.integer(year), 
         population = as.integer(population)) |> 
  filter(year == 2021) |> 
  filter(dhb != "Total New Zealand by DHB area/DHB constituency", 
         dhb != "Area outside district Health Board", 
         dhb != "Otago constituency", 
         dhb != "Southland constituency") |> 
  rename(pop_snz = population) |> 
  mutate(dhb_grouped = case_when(
    dhb == "Waitemata" ~ "Auckland Metro", 
    dhb == "Auckland" ~ "Auckland Metro", 
    dhb == "Counties Manukau" ~ "Auckland Metro", 
    dhb == "Capital and Coast" ~ "Capital & Coast and Hutt Valley", 
    dhb == "Hutt Valley" ~ "Capital & Coast and Hutt Valley", 
    dhb == "Hawke's Bay" ~ "Hawkes Bay", 
    TRUE ~ dhb
  )) |> 
  group_by(dhb_grouped) |> 
  summarise(pop_snz = sum(pop_snz)) |> 
  ungroup()

# Combined population data
dat_pop <- full_join(
  x = pop_hsu, 
  y = pop_snz, 
  by = c("dhb_of_residence" = "dhb_grouped")
) |> 
  mutate(diff = pop_hsu - pop_snz) |> 
  rename(dhb = dhb_of_residence) |> 
  mutate(dhb = factor(x = dhb, 
                      levels = c("Northland", 
                                 "Auckland Metro", 
                                 "Waikato", 
                                 "Bay of Plenty", 
                                 "Taranaki", 
                                 "Lakes", 
                                 "Tairawhiti", 
                                 "Whanganui", 
                                 "MidCentral", 
                                 "Hawkes Bay", 
                                 "Capital & Coast and Hutt Valley", 
                                 "Wairarapa", 
                                 "Nelson Marlborough", 
                                 "West Coast", 
                                 "Canterbury", 
                                 "South Canterbury", 
                                 "Southern"), 
                      ordered = TRUE)) |> 
  arrange(dhb) |> 
  print()

# *****************************************************************************