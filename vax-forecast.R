# Attempted forecasts of vaccination uptake

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(glue)
library(lubridate)

latest_date <- "211011"

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Vaccinations
dat_vax <- read_excel(path = here(glue("data/{latest_date}_-_covid_vaccine_equity_-_rate_ratios_and_uptake_over_time.xlsx")), 
                      sheet = "Vaccination Query") |> 
  clean_names() |> 
  mutate(week_ending_date = as_date(week_ending_date), 
         dose_number = as.integer(dose_number), 
         number_doses_administered = as.integer(number_doses_administered))

# HSU population
dat_pop <- read_excel(path = here(glue("data/{latest_date}_-_covid_vaccine_equity_-_rate_ratios_and_uptake_over_time.xlsx")), 
                      sheet = "HSU Table") |> 
  clean_names() |> 
  mutate(number_people_hsu = as.integer(number_people_hsu))

# *****************************************************************************


# *****************************************************************************
# Prepare forecasting data ----

# Combined data for forecasting
dat_combined <- dat_vax |> 
  # Filter out unknown groups and complete for all combinations
  filter(age_group != "Unknown", 
         dhb_of_residence != "Overseas and undefined", 
         dhb_of_residence != "Unknown") |> 
  complete(week_ending_date, dose_number, ethnic_group, age_group, dhb_of_residence, 
           fill = list(number_doses_administered = 0L)) |> 
  # Join population
  left_join(dat_pop, by = c("ethnic_group", "age_group", "dhb_of_residence")) |> 
  # Combine Auckland DHBs
  mutate(dhb_grouped = case_when(
    dhb_of_residence == "Auckland" ~ "Auckland Metro", 
    dhb_of_residence == "Counties Manukau" ~ "Auckland Metro", 
    dhb_of_residence == "Waitemata" ~ "Auckland Metro", 
    TRUE ~ dhb_of_residence
  )) |> 
  # Summarise by age group and DHB
  group_by(dhb_grouped, age_group, dose_number, week_ending_date) |> 
  summarise(number_doses_administered = sum(number_doses_administered), 
            number_people_hsu = sum(number_people_hsu)) |> 
  ungroup() |> 
  # Calculate cumulative uptake
  arrange(dhb_grouped, age_group, dose_number, week_ending_date) |> 
  group_by(dhb_grouped, age_group, dose_number) |> 
  mutate(cumu_doses = cumsum(number_doses_administered)) |> 
  ungroup() |> 
  mutate(unvax = number_people_hsu - cumu_doses) |> 
  mutate(uptake_pct = number_doses_administered / dplyr::lag(unvax)) |> 
  filter(number_people_hsu > 50)

# Models of uptake. Fit a time trend to the most recent 8 weeks
# of the uptake_pct variable (i.e. doses administered as a proportion of
# the unvaccinated at the end of the previous week). Separate model for
# each dhb x ethnic grouop x age group combination, for every 
# combination with population of at least 50 people.
uptake_models <- dat_combined |> 
  group_by(dhb_grouped, age_group, dose_number) |> 
  slice_max(order_by = week_ending_date, 
            n = 8) |> 
  mutate(t = row_number()) |> 
  nest() |> 
  ungroup() |> 
  rowwise() |> 
  mutate(m = list(lm(formula = uptake_pct ~ t, data = data))) |> 
  mutate(ms = list(summary(m))) |> 
  mutate(sigma = ms$sigma) 

# Predictions from uptake models for the next 8 weeks
uptake_predictions <- uptake_models |> 
  mutate(f = list(tibble(p = predict(object = m, 
                                     newdata = tibble(t = 9:20), 
                                     interval = "prediction", 
                                     type = "response", 
                                     level = 0.8), 
                         t = 9:20))) |> 
  ungroup() |> 
  select(dhb_grouped, age_group, dose_number, sigma, f) |> 
  unnest(cols = f) %>%
  do.call(data.frame, .) |> 
  as_tibble() |> 
  rename(uptake_pct_fit = p.1, 
         uptake_pct_lwr = p.2, 
         uptake_pct_upr = p.3) |> 
  # mutate(uptake_pct_fit = exp(uptake_pct_fit) + (sigma^2) / 2, 
  #        uptake_pct_lwr = exp(uptake_pct_lwr) + (sigma^2) / 2, 
  #        uptake_pct_upr = exp(uptake_pct_upr) + (sigma^2) / 2) |> 
  mutate(week_ending_date = max(dat_combined$week_ending_date) + dweeks(t - 8))

# Chart to check the uptake predictions
uptake_predictions_chart_dat <- bind_rows(
  dat_combined |> 
    group_by(dhb_grouped, age_group, dose_number) |> 
    slice_max(order_by = week_ending_date, 
              n = 16) |> 
    ungroup(), 
  uptake_predictions
) |> 
  mutate(category = ifelse(is.na(t), "actual", "predicted")) |> 
  mutate(uptake_pct_chart = ifelse(!is.na(uptake_pct), uptake_pct, uptake_pct_fit))

uptake_predictions_chart <- uptake_predictions_chart_dat |> 
  filter(dose_number == 2L) |> 
  ggplot(mapping = aes(x = week_ending_date, 
                       y = uptake_pct_chart, 
                       group = category, 
                       fill = category, 
                       colour = category)) + 
  geom_line() + 
  facet_grid(rows = vars(age_group), 
             cols = vars(dhb_grouped))

uptake_predictions_chart

# Doses administered predictions
doses_predictions <- dat_combined |> 
  group_by(dhb_of_residence, ethnic_group, age_group, dose_number) |> 
  slice_max(order_by = week_ending_date, 
            n = 1) |> 
  ungroup() |> 
  mutate(t = 8L) |> 
  bind_rows(uptake_predictions) |> 
  arrange(dhb_of_residence, ethnic_group, age_group, dose_number, t) |> 
  mutate(doses_fit = NA_integer_, 
         doses_lwr = NA_integer_, 
         doses_upr = NA_integer_, 
         cumu_doses_fit = cumu_doses, 
         cumu_doses_lwr = cumu_doses, 
         cumu_doses_upr = cumu_doses, 
         unvax_fit = unvax, 
         unvax_lwr = unvax, 
         unvax_upr = unvax) |> 
  fill(number_people_hsu, .direction = "down")

for (i in 1:nrow(doses_predictions)) {
  if(is.na(doses_predictions[i, "number_doses_administered"])) {
    # Mean 'fit' prediction
    doses_predictions[i, "doses_fit"] <- as.integer(doses_predictions[i - 1, "unvax_fit"] * doses_predictions[i, "uptake_pct_fit"])
    doses_predictions[i, "cumu_doses_fit"] <- doses_predictions[i - 1, "cumu_doses_fit"] + doses_predictions[i, "doses_fit"]
    doses_predictions[i, "unvax_fit"] <- doses_predictions[i, "number_people_hsu"] - doses_predictions[i, "cumu_doses_fit"]
    
    # Lower prediction
    doses_predictions[i, "doses_lwr"] <- as.integer(doses_predictions[i - 1, "unvax_lwr"] * doses_predictions[i, "uptake_pct_lwr"])
    doses_predictions[i, "cumu_doses_lwr"] <- doses_predictions[i - 1, "cumu_doses_lwr"] + doses_predictions[i, "doses_lwr"]
    doses_predictions[i, "unvax_lwr"] <- doses_predictions[i, "number_people_hsu"] - doses_predictions[i, "cumu_doses_lwr"]
    
    # Upper prediction
    doses_predictions[i, "doses_upr"] <- as.integer(doses_predictions[i - 1, "unvax_upr"] * doses_predictions[i, "uptake_pct_upr"])
    doses_predictions[i, "cumu_doses_upr"] <- doses_predictions[i - 1, "cumu_doses_upr"] + doses_predictions[i, "doses_upr"]
    doses_predictions[i, "unvax_upr"] <- doses_predictions[i, "number_people_hsu"] - doses_predictions[i, "cumu_doses_upr"]
  }
}
