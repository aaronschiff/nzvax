# Charts of numbers of unvaccinated Māori people

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(glue)
library(janitor)
library(scales)
library(readxl)

latest_date <- "09_11_2021"

latest_date_nice <- "9 November 2021"

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Latest vaccination data from MoH
dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) 
# *****************************************************************************


# *****************************************************************************
# Process data ----

# Manipulate vaccination data for Māori
dat_m <- dat |> 
  # Filter Māori and exclude unknown DHB
  filter(ethnic_group == "Maori", 
         dhb_of_residence != "Overseas / Unknown") |> 
  # Assign custom age groups
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
  # Summarise population and first doses by DHB and age group
  group_by(dhb_of_residence, age_group_2) |> 
  summarise(population = sum(population), 
            first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  # Calculate unvaccinated number in each group
  mutate(unvax = pmax(population - first_dose_administered, 0)) |> 
  # Apply factors for ordering
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("12-29 years old", 
                                         "30-59 years old", 
                                         "60+ years old"), 
                              ordered = TRUE)) |> 
  mutate(dhb_of_residence = factor(x = dhb_of_residence, 
                                   levels = c("Northland", 
                                              "Auckland Metro", 
                                              "Auckland", 
                                              "Waitemata", 
                                              "Counties Manukau", 
                                              "Waikato", 
                                              "Bay of Plenty", 
                                              "Taranaki", 
                                              "Lakes", 
                                              "Tairawhiti", 
                                              "Whanganui", 
                                              "MidCentral", 
                                              "Hawkes Bay", 
                                              "Capital & Coast and Hutt Valley", 
                                              "Capital and Coast", 
                                              "Hutt Valley", 
                                              "Wairarapa", 
                                              "Nelson Marlborough", 
                                              "West Coast", 
                                              "Canterbury", 
                                              "South Canterbury", 
                                              "Southern"), 
                                   ordered = TRUE)) |> 
  arrange(dhb_of_residence, age_group_2)

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Bar chart of numbers of unvaccinated people
chart_m_bar <- dat_m |> 
  ggplot(mapping = aes(x = dhb_of_residence, 
                       y = unvax, 
                       fill = age_group_2, 
                       colour = age_group_2, 
                       label = comma(x = unvax, accuracy = 1))) + 
  geom_hline(yintercept = 0, size = 0.25, colour = grey(0.5)) +
  geom_col(size = 0) + 
  geom_text(nudge_y = 800, 
            family = "Fira Sans", 
            size = 3) + 
  facet_wrap(facets = vars(age_group_2), ncol = 1) + 
  scale_y_continuous(labels = comma_format(accuracy = 1), 
                     limits = c(0, 12000), 
                     breaks = seq(0, 24000, 2000)) + 
  scale_x_discrete(position = "top") + 
  scale_colour_manual(values = c("12-29 years old" = "#6929c4", 
                                 "30-59 years old" = "#1192e8", 
                                 "60+ years old" = "#005d5d"), 
                      guide = "none", 
                      aesthetics = c("colour", "fill")) + 
  xlab("") + 
  ylab("") + 
  ggtitle(label = glue("Number of unvaccinated Māori people as at {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.25, colour = grey(0.9)), 
        axis.text.x.top = element_text(angle = 45, hjust = 0, face = "bold"), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        panel.spacing.y = unit(16, "pt"), 
        plot.margin = margin(4, 16, 4, 4, "pt"), 
        strip.text = element_text(face = "bold"))

ggsave(filename = here(glue("outputs/unvax_maori_{latest_date}.png")), 
       plot = chart_m_bar, 
       width = 2800, 
       height = 2000, 
       units = "px", 
       device = "png", 
       bg = "white")

# *****************************************************************************


# *****************************************************************************
# Scratch ----

# Other charts; unused at this stage

mu <- dat |> 
  filter(dhb_of_residence != "Overseas / Unknown", 
         dhb_of_residence != "Various") |> 
  mutate(ethnic_group_2 = ifelse(ethnic_group == "Maori", "Māori", "Non-Māori")) |> 
  group_by(dhb_of_residence, ethnic_group_2) |> 
  summarise(population = sum(population), 
            first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  mutate(unvax = population - first_dose_administered) |> 
  group_by(dhb_of_residence) |> 
  mutate(population_pct = population / sum(population), 
         unvax_pct = unvax / sum(unvax)) |> 
  ungroup() |> 
  select(dhb_of_residence, 
         ethnic_group_2, 
         population_pct, 
         unvax_pct) |> 
  pivot_longer(cols = c(population_pct, unvax_pct), 
               names_to = "measure", 
               values_to = "value") |> 
  mutate(measure = factor(x = measure, 
                          levels = c("population_pct", 
                                     "unvax_pct"), 
                          labels = c("All people aged 12+", 
                                     "Unvaccinated people aged 12+"), 
                          ordered = TRUE))

chart_mu <- mu |> 
  ggplot(mapping = aes(y = fct_rev(measure), 
                       x = value, 
                       label = percent(x = value, 
                                       accuracy = 0.1), 
                       fill = fct_rev(ethnic_group_2))) + 
  geom_col(size = 0) + 
  geom_text(colour = "white", 
            position = position_stack(vjust = 0.5)) + 
  facet_wrap(facets = vars(dhb_of_residence), ncol = 3) + 
  scale_fill_manual(values = c("Māori" = "firebrick", 
                               "Non-Māori" = grey(0.7)), 
                    name = NULL) + 
  theme(legend.position = "top") + 
  xlab("") + 
  ylab("") + 
  scale_x_continuous(labels = percent_format(accuracy = 1))

chart_mu


mu2 <- dat |> 
  mutate(ethnic_group_2 = ifelse(ethnic_group == "Maori", "Māori", "Non-Māori")) |>
  filter(age_group != "Various") |> 
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
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("12-29 years", 
                                         "30-59 years", 
                                         "60+ years"), 
                              ordered = TRUE))  |> 
  mutate(dhb_of_residence_2 = case_when(
    dhb_of_residence == "Auckland Metro" ~ "Auckland", 
    TRUE ~ "Outside Auckland"
  )) |> 
  group_by(dhb_of_residence_2, age_group_2, ethnic_group_2) |> 
  summarise(population = sum(population), 
            first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  mutate(unvax = population - first_dose_administered) 

chart_mu2 <- mu2 |> 
  ggplot(mapping = aes(x = age_group_2, 
                       y = unvax, 
                       label = comma(unvax, accuracy = 1), 
                       fill = fct_rev(ethnic_group_2), 
                       colour = fct_rev(ethnic_group_2))) + 
  geom_col() + 
  geom_text(position = position_stack(), 
            mapping = aes(y = unvax + 3000)) + 
  facet_wrap(facets = vars(dhb_of_residence_2), ncol = 2) + 
  scale_y_continuous(labels = comma_format(accuracy = 1), 
                     breaks = seq(0, 250000, 25000)) + 
  scale_fill_manual(values = c("Māori" = "firebrick", 
                               "Non-Māori" = grey(0.7)), 
                    name = NULL, 
                    aesthetics = c("colour", "fill")) + 
  ylab("") + 
  xlab("") + 
  ggtitle("Unvaccinated people") + 
  theme_minimal() + 
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank())

chart_mu2

# *****************************************************************************

