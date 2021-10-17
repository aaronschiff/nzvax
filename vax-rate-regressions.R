# Regression analysis of determinants of SA2-level vaccination rates

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(glue)
library(lmtest)
library(sandwich)

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

dat <- read_csv(file = here("data/00_SA2_Access_Vax_rates_12th_October_2021.csv")) |> 
  clean_names() |> 
  mutate(total_d1_uptake = ifelse(total_d1_uptake == "<50", "50", total_d1_uptake), 
         total_d1_uptake = ifelse(total_d1_uptake == ">950", "950", total_d1_uptake), 
         total_d2_uptake = ifelse(total_d2_uptake == "<50", "50", total_d2_uptake), 
         total_d2_uptake = ifelse(total_d2_uptake == ">950", "950", total_d2_uptake)) |> 
  mutate(total_d1_uptake = as.integer(total_d1_uptake), 
         total_d2_uptake = as.integer(total_d2_uptake), 
         total_pop_count = as.integer(total_pop_count), 
         maori_pop_count = as.integer(maori_pop_count), 
         pacific_pop_count = as.integer(pacific_pop_count)) |> 
  filter(!is.na(total_pop_count)) |> 
  mutate(maori_pop_count = replace_na(maori_pop_count, 0L), 
         pacific_pop_count = replace_na(pacific_pop_count, 0L)) |> 
  mutate(maori_pop_pct = maori_pop_count / total_pop_count, 
         pacific_pop_pct = pacific_pop_count / total_pop_count)

# *****************************************************************************


# *****************************************************************************
# Model ----

m <- lm(total_d1_uptake / 1000 ~ 
          time_to_nearest_vaccination_services_minutes + 
          clinic_to_population_ratio_per_1000 + 
          maori_pop_pct * median_age, 
        data = dat)

# *****************************************************************************