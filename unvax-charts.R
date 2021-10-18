# Experimenting with charting unvaccinated people

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)

latest_date <- "12_10_2021"               # Date of most recent week's data

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

# Vaccination data by DHB, age, ethnicity
dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) 

# *****************************************************************************


# *****************************************************************************
# Manipulate data ----

# Unvaccinated by alternative age groups and dhb
dat_unvax <- dat |> 
  # Remove unknown ethnicity and age group
  filter(ethnic_group != "Unknown", 
         ethnic_group != "Various", 
         age_group != "Various") |> 
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
  # Create category factors for ordering
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("12-29 years", 
                                         "30-59 years", 
                                         "60+ years"), 
                              ordered = TRUE)) |> 
  group_by(ethnic_group, age_group_2) |>
  summarise(first_dose_administered = sum(first_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(unvax = pmax(population - first_dose_administered, 0L))


dat_unvax_i <- dat_unvax |> 
  select(-first_dose_administered, -population) |> 
  uncount(weights = unvax) |> 
  arrange(ethnic_group, age_group_2) |> 
  group_by(ethnic_group) |> 
  mutate(group_i = row_number()) |> 
  mutate(row = 500L - as.integer(floor(group_i / 500))) |> 
  group_by(ethnic_group, row) |> 
  mutate(col = row_number()) |> 
  ungroup()

# *****************************************************************************


# *****************************************************************************
# Visualise ---- 

chart_unvax_i <- dat_unvax_i |> 
  ggplot(mapping = aes(x = col, 
                       y = row, 
                       fill = age_group_2)) + 
  geom_point(shape = 21, 
             stroke = 0, 
             size = 0.2) + 
  facet_grid(rows = vars(ethnic_group), 
             scale = "free_y", 
             space = "free") + 
  scale_fill_brewer(palette = "Set2") + 
  guides(fill = guide_legend(override.aes = list(size = 2))) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        strip.text = element_text(colour = "black"), 
        panel.spacing = unit(2, "pt"))

ggsave(filename = here(glue("outputs/unvax_i_{latest_date}.png")), 
       plot = chart_unvax_i, 
       device = "png", 
       width = 2400, 
       height = 2400, 
       units = "px", 
       bg = "white")
# *****************************************************************************