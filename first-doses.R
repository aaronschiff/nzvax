# Visualisation of people who received their first doses in the past week

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)
library(ggstar)

latest_date <- "26_10_2021"               # Date of most recent week's data
prev_date <- "19_10_2021"                 # Date of previous week's data
latest_date_nice <- "26 October 2021"     # For chart title

# *****************************************************************************


# ***************************************************************************** 
# Load data ----

# Raw data
dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) |> 
  mutate(week = "current")

dat_prev <- read_excel(path = here(glue("data/covid_vaccinations_{prev_date}.xlsx")), 
                       sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) |> 
  mutate(week = "previous")

# Combine data and manipulate 
dat_clean <- bind_rows(dat, dat_prev) |> 
  filter(age_group != "Various") |> 
  # Create custom age groups
  mutate(age_group_2 = case_when(
    age_group == "12-15" ~ "12-29", 
    age_group == "16-19" ~ "12-29", 
    age_group == "20-24" ~ "12-29", 
    age_group == "25-29" ~ "12-29", 
    age_group == "30-34" ~ "30-59", 
    age_group == "35-39" ~ "30-59", 
    age_group == "40-44" ~ "30-59", 
    age_group == "45-49" ~ "30-59", 
    age_group == "50-54" ~ "30-59", 
    age_group == "55-59" ~ "30-59", 
    age_group == "60-64" ~ "60+", 
    age_group == "65-69" ~ "60+", 
    age_group == "70-74" ~ "60+", 
    age_group == "75-79" ~ "60+", 
    age_group == "80-84" ~ "60+", 
    age_group == "85-89" ~ "60+",
    age_group == "90+" ~ "60+"
  )) |>
  # Summarise by week, dhb, ethnicity, age group
  group_by(week, age_group_2) |> 
  summarise(first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  pivot_wider(names_from = week, values_from = first_dose_administered) |> 
  mutate(first_doses_this_week = current - previous) |> 
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+", 
                                         "All"), 
                              labels = c("12-29 years", 
                                         "30-59 years", 
                                         "60+ years", 
                                         "All"), 
                              ordered = TRUE)) 

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Convert counts to individual people and create chart data
dat_chart <- dat_clean |> 
  select(-current, -previous) |> 
  uncount(weights = first_doses_this_week) |> 
  group_by(age_group_2) |> 
  mutate(group_i = row_number()) |> 
  mutate(y = as.integer(100L - ceiling(group_i / 200))) |> 
  group_by(age_group_2, y) |> 
  mutate(x = row_number()) |> 
  ungroup() 

chart <- dat_chart |> 
  ggplot(mapping = aes(x = x, y = y)) + 
  geom_point(size = 0.25, 
             shape = 21, 
             fill = "yellow", 
             stroke = 0) + 
  facet_grid(rows = vars(age_group_2), scales = "free_y", space = "free_y") + 
  scale_x_continuous(expand = expansion(0.01, 0)) + 
  scale_y_continuous(expand = expansion(0.01, 0)) + 
  ggtitle(glue("People in NZ who received their first dose of a COVID-19 vaccine\nin the week to {latest_date_nice}. One dot = one person.")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.title = element_text(colour = "white", size = rel(1)), 
        plot.subtitle = element_text(colour = "white"), 
        strip.text.y = element_text(angle = 0, colour = "white"), 
        strip.background = element_rect(fill = "black", colour = "black"), 
        plot.background = element_rect(fill = "black", colour = "black"))

ggsave(filename = here(glue("outputs/first_doses_stars_{latest_date}.png")), 
       plot = chart, 
       width = 1400, 
       height = 2600, 
       units = "px", 
       device = "png", 
       bg = "black")
# *****************************************************************************
