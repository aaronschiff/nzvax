# Vaccination rates in North Island holiday spots

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(sf)
library(janitor)
library(glue)
library(readxl)

latest_date <- "20211103"
latest_date_nice <- "2 November 2021"

# *****************************************************************************


# *****************************************************************************
# Load data ----

# SA2 vaccination data
dat_vax_sa2 <- read_csv(file = here(glue("data/uptake_sa2_dhb_{latest_date}.csv"))) |> 
  clean_names() |> 
  # Exclude unknown SA2
  filter(sa2_code != "Unknown") |> 
  # Replace >950 and <50 with numeric values
  mutate(dose1_uptake = ifelse(dose1_uptake == ">950", "950", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">950", "950", dose2_uptake), 
         dose1_uptake = ifelse(dose1_uptake == "<50", "50", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == "<50", "50", dose2_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake), 
         dose2_uptake = as.integer(dose2_uptake), 
         pop_cnt = as.integer(pop_cnt)) 

# SA2 full population (Stats NZ)
dat_pop_sa2 <- read_csv(file = here("data/Subnational population estimates/TABLECODE7979_Data_25c70cb9-4a80-43df-bc55-cf1c79f65282.csv")) |> 
  clean_names() |> 
  select(area, pop = value) |> 
  mutate(area = str_replace(string = area, pattern = "city", replacement = "City"), 
         area = str_replace(string = area, pattern = "district", replacement = "District"))

# SA2 shapes
dat_sa2_shapes <- read_sf(dsn = here("data/statsnzstatistical-area-2-2020-clipped-generalised-SHP/statistical-area-2-2020-clipped-generalised.shp")) |> 
  clean_names()

# Holiday spots
dat_spots <- read_csv(file = here("data/holiday_spots.csv"), col_types = "cc") |> 
  distinct()

# *****************************************************************************


# *****************************************************************************
# Manipulate data ----

# Estimated vaccination rates for holiday spots
vax_rates_spots <- dat_spots |> 
  left_join(y = dat_vax_sa2, by = c("sa2" = "sa2_code")) |> 
  left_join(y = dat_pop_sa2, by = c("sa2_name" = "area")) |> 
  mutate(first_doses = pop_cnt * dose1_uptake / 1000, 
         second_doses = pop_cnt * dose2_uptake / 1000) |> 
  filter(spot == "Raglan") |> 
  print()
  group_by(spot) |> 
  summarise(pop_eligible = sum(pop_cnt), 
            pop_total = sum(pop), 
            first_doses = sum(first_doses), 
            second_doses = sum(second_doses)) |> 
  ungroup() |> 
  mutate(first_dose_rate = first_doses / pop_total, 
         second_dose_rate = second_doses / pop_total) |> 
  mutate(unvaxed = pop_total - first_doses) |> 
  print()

# *****************************************************************************