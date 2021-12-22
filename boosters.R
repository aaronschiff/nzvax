# Looking at booster shots

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(glue)
library(janitor)
library(scales)
library(readxl)
library(systemfonts)
library(ragg)
library(lubridate)

latest_date <- "21_12_2021"

latest_date_nice <- "21 December 2021"

boost_interval <- dmonths(6)

# *****************************************************************************


# *****************************************************************************
# Custom font setup ----

register_font(
  name = "Fira Sans Custom", 
  plain = system_fonts() |> filter(family == "Fira Sans", style == "Regular") |> pull(path), 
  bold = system_fonts() |> filter(family == "Fira Sans", style == "ExtraBold") |> pull(path), 
  italic = system_fonts() |> filter(family == "Fira Sans", style == "Italic") |> pull(path), 
  bolditalic = system_fonts() |> filter(family == "Fira Sans", style == "ExtraBold Italic") |> pull(path), 
  features = font_feature(ligatures = c("discretionary", 
                                        "standard", 
                                        "contextual"), 
                          numbers = c("lining", "proportional"))
)

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Latest vaccination data from MoH
dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "Date") |>
  clean_names() |> 
  select(-x6, -note) |> 
  mutate(date = as_date(date))

# Boosted vs eligible for boost
dat_boost <- dat |> 
  mutate(boost_eligible_date = as_date(date + boost_interval)) |> 
  mutate(cumu_second_doses = cumsum(second_doses), 
         cumu_third_primary_doses = cumsum(third_primary_doses)) |> 
  mutate(boost_eligible = cumu_second_doses - cumu_third_primary_doses) |> 
  filter(boost_eligible_date >= ymd("2021-11-21")) |> 
  select(boost_eligible_date, boost_eligible) |> 
  left_join(y = dat |> 
              select(date, boosters) |> 
              mutate(cumu_boosters = cumsum(boosters)) |> 
              select(date, cumu_boosters), 
            by = c("boost_eligible_date" = "date")) |> 
  mutate(unboosted = boost_eligible - cumu_boosters) |> 
  select(-boost_eligible) |> 
  pivot_longer(cols = -boost_eligible_date, 
               names_to = "measure", values_to = "value") |> 
  mutate(measure = factor(x = measure, 
                          levels = c("cumu_boosters", 
                                     "unboosted"), 
                          labels = c("Boosted", 
                                     "Eligible but unboosted"), 
                          ordered = TRUE))

# *****************************************************************************


# *****************************************************************************
# Visualise ----

chart_boost <- dat_boost |>
  filter(measure != "boost_eligible") |> 
  ggplot(mapping = aes(x = boost_eligible_date, 
                       y = value, 
                       fill = fct_rev(measure))) + 
  geom_col()

# *****************************************************************************

