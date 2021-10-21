# NZ COVID vaccination rates visualised by 'community'

# TODO: Suppress cells in changes charts for small population groups

# Feedback to consider: 
# - Use a single colour scale. Avoid colours that might appear brown. 
#     [Done, changed light blue to pink and dark red/brown to bright red]

# - Include a title or description to frame the intent of the charts. 
#     [Done]

# - Include vaccination % numbers in the boxes? 
#     [No they are not accurate enough given denominator issues]

# - Show as vulnerable % rather than vaccinated %? 
#     [Good idea but don't want to fundamentally change the chart at this stage]

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)
library(colorspace)

latest_date <- "19_10_2021"               # Date of most recent week's data
prev_date <- "12_10_2021"                 # Date of previous week's data
latest_date_nice <- "19 October 2021"     # For chart title

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
dat_combined <- bind_rows(dat, dat_prev) |> 
  # Remove unknown categories
  filter(ethnic_group != "Unknown", 
         ethnic_group != "Various", 
         dhb_of_residence != "Overseas / Unknown", 
         dhb_of_residence != "Various", 
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
  # Summarise by week, dhb, ethnicity, age group
  group_by(week, dhb_of_residence, ethnic_group, age_group_2) |> 
  summarise(second_dose_administered = sum(second_dose_administered), 
            first_dose_administered = sum(first_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(fully_vax_rate = second_dose_administered / population, 
         first_dose_rate = first_dose_administered / population) |> 
  # Create category factors for ordering
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Maori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "European or Other"), 
                               labels = c("Māori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "Pākehā or other"), 
                               ordered = TRUE)) |> 
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("12-29 years", 
                                         "30-59 years", 
                                         "60+ years"), 
                              ordered = TRUE)) |> 
  mutate(ethnicity_age = fct_cross(age_group_2, ethnic_group, sep = " ")) |> 
  mutate(dhb_of_residence = factor(x = dhb_of_residence, 
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
                                   ordered = TRUE)) 

# *****************************************************************************


# *****************************************************************************
# Current week vaccination rate charts ----

# Create chart data
dat_chart_vax_rate <- dat_combined |> 
  filter(week == "current") |> 
  # Categorise vax rates
  mutate(fully_vax_category = case_when(
    fully_vax_rate > 0.9 ~ "Greater than 90% fully vaccinated", 
    (fully_vax_rate > 0.8) & (fully_vax_rate <= 0.9) ~ "80% to 90% fully vaccinated", 
    (fully_vax_rate >= 0.7) & (fully_vax_rate <= 0.8) ~ "70% to 80% fully vaccinated", 
    TRUE ~ "Less than 70% fully vaccinated"
  )) |> 
  mutate(first_dose_category = case_when(
    first_dose_rate > 0.9 ~ "Greater than 90% first doses", 
    (first_dose_rate > 0.8) & (first_dose_rate <= 0.9) ~ "80% to 90% first doses", 
    (first_dose_rate >= 0.7) & (first_dose_rate <= 0.8) ~ "70% to 80% first doses", 
    TRUE ~ "Less than 70% first doses"
  )) |> 
  # Create factors for ordering
  mutate(fully_vax_category = factor(x = fully_vax_category, 
                                     levels = c("Greater than 90% fully vaccinated", 
                                                "80% to 90% fully vaccinated", 
                                                "70% to 80% fully vaccinated", 
                                                "Less than 70% fully vaccinated"), 
                                     ordered = TRUE)) |> 
  mutate(first_dose_category = factor(x = first_dose_category, 
                                      levels = c("Greater than 90% first doses", 
                                                 "80% to 90% first doses", 
                                                 "70% to 80% first doses", 
                                                 "Less than 70% first doses"), 
                                      ordered = TRUE)) |> 
  arrange(dhb_of_residence, ethnicity_age)

# Current fully vaccinated rate chart
chart_fully_vax_rate <- ggplot(dat_chart_vax_rate) + 
  geom_tile(mapping = aes(y = fct_rev(ethnicity_age), 
                          x = dhb_of_residence, 
                          fill = fully_vax_category), 
            colour = grey(0.97),  size = 3) + 
  geom_hline(yintercept = 3.5, colour = "black") + 
  geom_hline(yintercept = 6.5, colour = "black") + 
  geom_hline(yintercept = 9.5, colour = "black") + 
  scale_fill_manual(values = c("Greater than 90% fully vaccinated" = lighten(col = "red", amount = 0.9),
                               "80% to 90% fully vaccinated" = lighten(col = "red", amount = 0.5),
                               "70% to 80% fully vaccinated" = lighten(col = "red", amount = 0.1),
                               "Less than 70% fully vaccinated" = darken(col = "red", amount = 0.25)),
                    name = NULL) +
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(ncol = 1, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\ngithub.com/aaronschiff/nzvax", 
           hjust = 0, 
           family = "Fira Sans", 
           size = 2.5) + 
  coord_cartesian(clip = "off") + 
  ggtitle(label = "NZ population groups that are not yet protected by COVID-19 vaccination", 
          subtitle = glue("Fully vaccinated (two doses) to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.17), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 24, 0, "pt")))

ggsave(filename = here(glue("outputs/fully_vax_communities_{latest_date}.png")), 
       plot = chart_fully_vax_rate, 
       device = "png", 
       width = 2400, 
       height = 2000, 
       units = "px", 
       bg = "white")

# Current first doses rate chart
chart_first_doses_rate <- ggplot(dat_chart_vax_rate) + 
  geom_tile(mapping = aes(y = fct_rev(ethnicity_age), 
                          x = dhb_of_residence, 
                          fill = first_dose_category), 
            colour = grey(0.97),  size = 3) + 
  geom_hline(yintercept = 3.5, colour = "black") + 
  geom_hline(yintercept = 6.5, colour = "black") + 
  geom_hline(yintercept = 9.5, colour = "black") + 
  scale_fill_manual(values = c("Greater than 90% first doses" = lighten(col = "red", amount = 0.9),
                               "80% to 90% first doses" = lighten(col = "red", amount = 0.5),
                               "70% to 80% first doses" = lighten(col = "red", amount = 0.1),
                               "Less than 70% first doses" = darken(col = "red", amount = 0.25)),
                    name = NULL) +
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(ncol = 1, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\ngithub.com/aaronschiff/nzvax", 
           hjust = 0, 
           family = "Fira Sans", 
           size = 2) + 
  coord_cartesian(clip = "off") + 
  ggtitle(label = "NZ population groups that are not yet protected by COVID-19 vaccination", 
          subtitle = glue("First doses to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.17), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 24, 0, "pt")))

ggsave(filename = here(glue("outputs/first_doses_communities_{latest_date}.png")), 
       plot = chart_first_doses_rate, 
       device = "png", 
       width = 2400, 
       height = 2000, 
       units = "px", 
       bg = "white")

# *****************************************************************************


# *****************************************************************************
# Change in vaccination rate charts ----

dat_chart_vax_rate_change <- dat_combined |> 
  select(-second_dose_administered, 
         -first_dose_administered, 
         -population) |> 
  # Calculate weekly change in vax rates
  pivot_wider(names_from = week, values_from = c("fully_vax_rate", "first_dose_rate")) |> 
  mutate(fully_vax_rate_change = fully_vax_rate_current - fully_vax_rate_previous, 
         first_dose_rate_change = first_dose_rate_current - first_dose_rate_previous) |> 
  mutate(fully_vax_rate_change_category = case_when(
    (fully_vax_rate_change < 0.02) ~ "Less than 2% increase", 
    (fully_vax_rate_change >= 0.02) & (fully_vax_rate_change < 0.04) ~ "2% to 4% increase", 
    (fully_vax_rate_change >= 0.04) & (fully_vax_rate_change < 0.06) ~ "4% to 6% increase", 
    (fully_vax_rate_change >= 0.06) & (fully_vax_rate_change < 0.08) ~ "6% to 8% increase", 
    (fully_vax_rate_change >= 0.08) & (fully_vax_rate_change < 0.1) ~ "8% to 10% increase", 
    (fully_vax_rate_change >= 0.1) ~ "Greater than 10% increase"
  )) |> 
  mutate(first_dose_rate_change_category = case_when(
    (first_dose_rate_change < 0.02) ~ "Less than 2% increase", 
    (first_dose_rate_change >= 0.02) & (first_dose_rate_change < 0.04) ~ "2% to 4% increase", 
    (first_dose_rate_change >= 0.04) & (first_dose_rate_change < 0.06) ~ "4% to 6% increase", 
    (first_dose_rate_change >= 0.06) & (first_dose_rate_change < 0.08) ~ "6% to 8% increase", 
    (first_dose_rate_change >= 0.08) & (first_dose_rate_change < 0.1) ~ "8% to 10% increase", 
    (first_dose_rate_change >= 0.1) ~ "Greater than 10% increase"
  )) |> 
  # Create factors for ordering
  mutate(fully_vax_rate_change_category = factor(x= fully_vax_rate_change_category, 
                                                 levels = c("Less than 2% increase", 
                                                            "2% to 4% increase", 
                                                            "4% to 6% increase", 
                                                            "6% to 8% increase", 
                                                            "8% to 10% increase", 
                                                            "Greater than 10% increase"), 
                                                 ordered = TRUE)) |> 
  mutate(first_dose_rate_change_category = factor(x= first_dose_rate_change_category, 
                                                  levels = c("Less than 2% increase", 
                                                             "2% to 4% increase", 
                                                             "4% to 6% increase", 
                                                             "6% to 8% increase", 
                                                             "8% to 10% increase", 
                                                             "Greater than 10% increase"),
                                                  ordered = TRUE))

# Change in fully vaccinated rate chart
chart_fully_vax_change <- ggplot(dat_chart_vax_rate_change, 
                                 mapping = aes(y = fct_rev(ethnicity_age), 
                                               x = dhb_of_residence)) + 
  geom_tile(mapping = aes(fill = fully_vax_rate_change_category), 
            colour = grey(0.97), size = 3) + 
  geom_text(mapping = aes(angle = 90 * (fully_vax_rate_change / 0.25)), 
            colour = "white", label = "—", 
            family = "Fira Sans", 
            size = 4) + 
  geom_hline(yintercept = 3.5, colour = "black") + 
  geom_hline(yintercept = 6.5, colour = "black") + 
  geom_hline(yintercept = 9.5, colour = "black") + 
  scale_fill_viridis_d(drop = FALSE, name = NULL) + 
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(ncol = 2, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\ngithub.com/aaronschiff/nzvax", 
           hjust = 0, 
           family = "Fira Sans", 
           size = 2) + 
  coord_cartesian(clip = "off") + 
  ggtitle(glue("Change in the fully vaccinated rate in the week to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.2), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 24, 0, "pt")))

ggsave(filename = here(glue("outputs/fully_vax_change_communities_{latest_date}.png")), 
       plot = chart_fully_vax_change, 
       device = "png", 
       width = 2400, 
       height = 2000, 
       units = "px", 
       bg = "white")

# Change in first doses rate chart
chart_first_doses_change <- ggplot(dat_chart_vax_rate_change, 
                                   mapping = aes(y = fct_rev(ethnicity_age), 
                                                 x = dhb_of_residence)) + 
  geom_tile(mapping = aes(fill = first_dose_rate_change_category), 
            colour = grey(0.97), size = 3) + 
  geom_text(mapping = aes(angle = 90 * (first_dose_rate_change / 0.25)), 
            colour = "white", label = "—", 
            family = "Fira Sans", 
            size = 4) + 
  geom_hline(yintercept = 3.5, colour = "black") + 
  geom_hline(yintercept = 6.5, colour = "black") + 
  geom_hline(yintercept = 9.5, colour = "black") + 
  scale_fill_viridis_d(drop = FALSE, name = NULL) + 
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(ncol = 2, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\ngithub.com/aaronschiff/nzvax", 
           hjust = 0, 
           family = "Fira Sans", 
           size = 2) + 
  coord_cartesian(clip = "off") + 
  ggtitle(glue("Change in the first doses rate in the week to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.2), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 24, 0, "pt")))

ggsave(filename = here(glue("outputs/first_doses_change_communities_{latest_date}.png")), 
       plot = chart_first_doses_change, 
       device = "png", 
       width = 2400, 
       height = 2000, 
       units = "px", 
       bg = "white")

# *****************************************************************************

