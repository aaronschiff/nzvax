# NZ COVID vaccination rates visualised by 'community'

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)
library(colorspace)
library(scales)
library(systemfonts)
library(ragg)

latest_date <- "15_02_2022"               # Date of most recent week's data
prev_date <- "08_02_2022"                 # Date of previous week's data 
latest_date_nice <- "15 February 2022"     # For chart title

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

# Raw data
dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) |> 
  mutate(week = "current") |> 
  rename(first_dose_administered = at_least_partially_vaccinated, 
         second_dose_administered = fully_vaccinated)

dat_511 <- read_excel(path = here(glue("data/covid_vaccinations_5_11_yo_{latest_date}.xlsx"))) |> 
  clean_names() |> 
  rename(first_dose_administered = at_least_partially_vaccinated) |> 
  pivot_wider(names_from = ethnicity, values_from = c(first_dose_administered, 
                                                      population), 
              names_sep = ".") |> 
  mutate(population.non_Maori_non_Pacific = 
           population.All - population.Maori - population.Pacific) |> 
  mutate(first_dose_administered.non_Maori_non_Pacific = 
           first_dose_administered.All - first_dose_administered.Maori - first_dose_administered.Pacific) |> 
  select(-first_dose_administered.All, 
         -population.All) |> 
  pivot_longer(cols = -dhb_of_residence) |> 
  separate(col = name, into = c("measure", "ethnic_group"), sep = "\\.") |> 
  pivot_wider(names_from = measure, values_from = value) |> 
  mutate(ethnic_group = case_when(
    ethnic_group == "Pacific" ~ "Pacific Peoples", 
    ethnic_group == "non_Maori_non_Pacific" ~ "non-Maori non-Pacific", 
    TRUE ~ ethnic_group
  )) |> 
  mutate(age_group = "5-11", 
         week = "current")

dat_prev <- read_excel(path = here(glue("data/covid_vaccinations_{prev_date}.xlsx")),
                       sheet = "DHBofResidence by ethnicity") |>
  clean_names() |>
  select(-x10, -notes) |>
  mutate(week = "previous") |>
  rename(first_dose_administered = at_least_partially_vaccinated,
         second_dose_administered = fully_vaccinated)

# Combine data and manipulate 
dat_clean <- bind_rows(dat, dat_511) |> 
  # bind_rows(dat, dat_prev) |> 
  # Create custom age groups
  mutate(age_group_2 = case_when(
    age_group == "5-11" ~ "5-11", 
    age_group == "12-17" ~ "12-29", 
    age_group == "18-24" ~ "12-29", 
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
    age_group == "90+" ~ "60+",
    age_group == "Various" ~ "Various"
  )) |>
  # Summarise by week, dhb, ethnicity, age group
  group_by(week, dhb_of_residence, ethnic_group, age_group_2) |> 
  summarise(second_dose_administered = sum(second_dose_administered), 
            first_dose_administered = sum(first_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(second_dose_administered = replace_na(second_dose_administered, 
                                               replace = 0))

# Add total for each DHB and manipulate for charting
dat_combined <- bind_rows(
  # Clean data
  dat_clean, 
  # Add totals for each DHB
  dat_clean |> 
    group_by(week, dhb_of_residence) |> 
    summarise(second_dose_administered = sum(second_dose_administered), 
              first_dose_administered = sum(first_dose_administered), 
              population = sum(population)) |> 
    mutate(ethnic_group = "All", age_group_2 = "All") |> 
    ungroup()
) |> 
  # Remove unknown categories
  filter(ethnic_group != "Unknown", 
         ethnic_group != "Various", 
         dhb_of_residence != "Overseas / Unknown", 
         dhb_of_residence != "Various", 
         age_group_2 != "Various") |> 
  mutate(fully_vax_rate = second_dose_administered / population, 
         first_dose_rate = first_dose_administered / population) |> 
  # Create category factors for ordering
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Maori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "non-Maori non-Pacific", 
                                          "European or Other", 
                                          "All"), 
                               labels = c("Māori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "non-Maori non-Pacific", 
                                          "Pākehā or other", 
                                          "All"), 
                               ordered = TRUE)) |> 
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("5-11", 
                                         "12-29", 
                                         "30-59", 
                                         "60+", 
                                         "All"), 
                              labels = c("5-11 years", 
                                         "12-29 years", 
                                         "30-59 years", 
                                         "60+ years", 
                                         "All"), 
                              ordered = TRUE)) |> 
  mutate(ethnicity_age = fct_cross(age_group_2, ethnic_group, sep = " ")) |> 
  mutate(ethnicity_age = fct_relabel(.f = ethnicity_age, 
                                     .fun = function(x) {
                                       ifelse(x == "All All", "All people aged 5+", x)
                                     })) |> 
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
  arrange(week, dhb_of_residence, ethnicity_age)

# *****************************************************************************


# *****************************************************************************
# Current week vaccination rate charts ----

# Count how many first doses in age 12+ this week
new_first_doses <- dat |> 
  group_by(ethnic_group, age_group) |> 
  summarise(first_doses_this_week = sum(first_dose_administered)) |> 
  ungroup() |> 
  left_join(y = dat_prev |> 
              group_by(ethnic_group, age_group) |> 
              summarise(first_doses_last_week = sum(first_dose_administered)) |> 
              ungroup(), 
            by = c("ethnic_group", "age_group")) |> 
  mutate(new_doses = first_doses_this_week - first_doses_last_week) 

# Create chart data
dat_chart_vax_rate <- dat_combined |> 
  filter(week == "current") |> 
  # Categorise vax rates
  mutate(fully_vax_category = case_when(
    fully_vax_rate > 0.9 ~ "Greater than 90%", 
    (fully_vax_rate > 0.8) & (fully_vax_rate <= 0.9) ~ "80% to 90%", 
    (fully_vax_rate >= 0.7) & (fully_vax_rate <= 0.8) ~ "70% to 80%", 
    TRUE ~ "Less than 70%"
  )) |> 
  mutate(first_dose_category = case_when(
    first_dose_rate > 0.9 ~ "Greater than 90%", 
    (first_dose_rate > 0.8) & (first_dose_rate <= 0.9) ~ "80% to 90%", 
    (first_dose_rate >= 0.7) & (first_dose_rate <= 0.8) ~ "70% to 80%", 
    TRUE ~ "Less than 70%"
  )) |> 
  # Create factors for ordering
  mutate(fully_vax_category = factor(x = fully_vax_category, 
                                     levels = c("Greater than 90%", 
                                                "80% to 90%", 
                                                "70% to 80%", 
                                                "Less than 70%"), 
                                     ordered = TRUE)) |> 
  mutate(first_dose_category = factor(x = first_dose_category, 
                                      levels = c("Greater than 90%", 
                                                 "80% to 90%", 
                                                 "70% to 80%", 
                                                 "Less than 70%"), 
                                      ordered = TRUE)) |> 
  arrange(dhb_of_residence, ethnicity_age)

# Current fully vaccinated rate chart
chart_fully_vax_rate <- ggplot(dat_chart_vax_rate, 
                               mapping = aes(y = fct_rev(ethnicity_age), 
                                             x = dhb_of_residence)) + 
  geom_tile(mapping = aes(fill = fully_vax_category), 
            colour = grey(0.97),  size = 3) + 
  geom_text(mapping = aes(label = round(100 * fully_vax_rate), 
                          colour = ifelse(fully_vax_rate > 0.9, "90+", "under-90")), 
            data = dat_chart_vax_rate |> filter(age_group_2 == "5-11 years"), 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2.5) + 
  geom_text(mapping = aes(label = round(100 * fully_vax_rate), 
                          colour = ifelse(fully_vax_rate > 0.9, "90+", "under-90")), 
            data = dat_chart_vax_rate |> filter(age_group_2 == "All"), 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2.5) + 
  geom_hline(yintercept = c(1.5, 4.5, 5.5, 8.5, 12.5), colour = "black") + 
  scale_fill_manual(values = c("Greater than 90%" = lighten(col = "red", amount = 0.9),
                               "80% to 90%" = lighten(col = "red", amount = 0.5),
                               "70% to 80%" = lighten(col = "red", amount = 0.1),
                               "Less than 70%" = darken(col = "red", amount = 0.25)),
                    name = NULL) +
  scale_colour_manual(values = c("90+" = grey(0.75),
                                 "under-90" = "white"), 
                      guide = "none") + 
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(nrow = 1, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/", 
           hjust = 0, 
           family = "Fira Sans Custom", 
           size = 2.5) + 
  coord_cartesian(clip = "off") + 
  ggtitle(label = "NZ population groups that are not yet protected by COVID-19 vaccination", 
          subtitle = glue("People who have received two doses to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.15), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 8, 0, "pt")))

ggsave(filename = here(glue("outputs/fully_vax_communities_{latest_date}.png")), 
       plot = chart_fully_vax_rate, 
       device = agg_png, 
       width = 2800, 
       height = 2250, 
       units = "px", 
       bg = "white")

# Current first doses rate chart
chart_first_doses_rate <- ggplot(dat_chart_vax_rate, 
                                 mapping = aes(y = fct_rev(ethnicity_age), 
                                               x = dhb_of_residence)) + 
  geom_tile(mapping = aes(fill = first_dose_category), 
            colour = grey(0.97),  size = 3) + 
  geom_text(mapping = aes(label = round(100 * first_dose_rate), 
                          colour = ifelse(first_dose_rate > 0.9, "90+", "under-90")), 
            data = dat_chart_vax_rate |> filter(age_group_2 == "5-11 years"), 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2.5) + 
  geom_text(mapping = aes(label = round(100 * first_dose_rate), 
                          colour = ifelse(first_dose_rate > 0.9, "90+", "under-90")), 
            data = dat_chart_vax_rate |> filter(age_group_2 == "All"), 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2.5) + 
  geom_hline(yintercept = c(1.5, 4.5, 5.5, 8.5, 12.5), colour = "black") + 
  scale_fill_manual(values = c("Greater than 90%" = lighten(col = "red", amount = 0.9),
                               "80% to 90%" = lighten(col = "red", amount = 0.5),
                               "70% to 80%" = lighten(col = "red", amount = 0.1),
                               "Less than 70%" = darken(col = "red", amount = 0.25)),
                    name = NULL) +
  scale_colour_manual(values = c("90+" = grey(0.75),
                                 "under-90" = "white"), 
                      guide = "none") + 
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(nrow = 1, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/", 
           hjust = 0, 
           family = "Fira Sans Custom", 
           size = 2.5) + 
  coord_cartesian(clip = "off") + 
  ggtitle(label = "NZ population groups that are not yet protected by COVID-19 vaccination", 
          subtitle = glue("People who have received at least one dose to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.15), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 8, 0, "pt")))

ggsave(filename = here(glue("outputs/first_doses_communities_{latest_date}.png")), 
       plot = chart_first_doses_rate, 
       device = agg_png, 
       width = 2800, 
       height = 2250, 
       units = "px", 
       bg = "white")

# *****************************************************************************


# *****************************************************************************
# Change in vaccination rate charts ----
# Not updated any more

# dat_chart_vax_rate_change <- dat_combined |> 
#   select(-second_dose_administered, 
#          -first_dose_administered) |> 
#   # Calculate weekly change in vax rates
#   pivot_wider(names_from = week, values_from = c("fully_vax_rate", "first_dose_rate", "population")) |> 
#   mutate(fully_vax_rate_change = fully_vax_rate_current - fully_vax_rate_previous, 
#          first_dose_rate_change = first_dose_rate_current - first_dose_rate_previous) |> 
#   mutate(fully_vax_rate_change_category = case_when(
#     population_current < 100 ~ "Suppressed (fewer than 100 people)", 
#     (fully_vax_rate_change < 0.02) ~ "Less than 2% increase", 
#     (fully_vax_rate_change >= 0.02) & (fully_vax_rate_change < 0.04) ~ "2% to 4% increase", 
#     (fully_vax_rate_change >= 0.04) & (fully_vax_rate_change < 0.06) ~ "4% to 6% increase", 
#     (fully_vax_rate_change >= 0.06) & (fully_vax_rate_change < 0.08) ~ "6% to 8% increase", 
#     (fully_vax_rate_change >= 0.08) & (fully_vax_rate_change < 0.1) ~ "8% to 10% increase", 
#     (fully_vax_rate_change >= 0.1) ~ "Greater than 10% increase"
#   )) |> 
#   mutate(first_dose_rate_change_category = case_when(
#     population_current < 100 ~ "Suppressed (fewer than 100 people)", 
#     (first_dose_rate_change < 0.02) ~ "Less than 2% increase", 
#     (first_dose_rate_change >= 0.02) & (first_dose_rate_change < 0.04) ~ "2% to 4% increase", 
#     (first_dose_rate_change >= 0.04) & (first_dose_rate_change < 0.06) ~ "4% to 6% increase", 
#     (first_dose_rate_change >= 0.06) & (first_dose_rate_change < 0.08) ~ "6% to 8% increase", 
#     (first_dose_rate_change >= 0.08) & (first_dose_rate_change < 0.1) ~ "8% to 10% increase", 
#     (first_dose_rate_change >= 0.1) ~ "Greater than 10% increase"
#   )) |> 
#   # Create factors for ordering
#   mutate(fully_vax_rate_change_category = factor(x = fully_vax_rate_change_category, 
#                                                  levels = c("Less than 2% increase", 
#                                                             "2% to 4% increase", 
#                                                             "4% to 6% increase", 
#                                                             "6% to 8% increase", 
#                                                             "8% to 10% increase", 
#                                                             "Greater than 10% increase", 
#                                                             "Suppressed (fewer than 100 people)"), 
#                                                  ordered = TRUE)) |> 
#   mutate(first_dose_rate_change_category = factor(x = first_dose_rate_change_category, 
#                                                   levels = c("Less than 2% increase", 
#                                                              "2% to 4% increase", 
#                                                              "4% to 6% increase", 
#                                                              "6% to 8% increase", 
#                                                              "8% to 10% increase", 
#                                                              "Greater than 10% increase", 
#                                                              "Suppressed (fewer than 100 people)"),
#                                                   ordered = TRUE))

# Change in fully vaccinated rate chart
# chart_fully_vax_change <- ggplot(dat_chart_vax_rate_change, 
#                                  mapping = aes(y = fct_rev(ethnicity_age), 
#                                                x = dhb_of_residence)) + 
#   geom_tile(mapping = aes(fill = fully_vax_rate_change_category), 
#             colour = grey(0.97), size = 3) + 
#   geom_text(mapping = aes(angle = 90 * (fully_vax_rate_change / 0.25)), 
#             colour = "white", label = "—", 
#             family = "Fira Sans Custom", 
#             size = 4, 
#             data = dat_chart_vax_rate_change |> 
#               filter(population_current > 99)) + 
#   geom_hline(yintercept = c(1.5, 4.5, 7.5, 10.5), colour = "black") + 
#   scale_fill_manual(drop = FALSE, name = NULL,
#                     values = c("Less than 2% increase" = "#440154FF",
#                                "2% to 4% increase" = "#414487FF",
#                                "4% to 6% increase" = "#2A788EFF",
#                                "6% to 8% increase" = "#22A884FF",
#                                "8% to 10% increase" = "#7AD151FF",
#                                "Greater than 10% increase" = "#FDE725FF",
#                                "Suppressed (fewer than 100 people)" = grey(0.8))) +
#   scale_x_discrete(position = "top") + 
#   guides(fill = guide_legend(ncol = 3, 
#                              override.aes = list(size = 0.5))) + 
#   xlab("") + 
#   ylab("") + 
#   annotate(geom = "text", 
#            x = 0.4, 
#            y = -0.5, 
#            label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/", 
#            hjust = 0, 
#            family = "Fira Sans Custom", 
#            size = 2) + 
#   coord_cartesian(clip = "off") + 
#   ggtitle(glue("Change in the fully vaccinated rate in the week to {latest_date_nice}")) + 
#   theme_minimal(base_family = "Fira Sans Custom") + 
#   theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
#         legend.justification = c(0, 0), 
#         legend.position = c(-0.015, 1.16), 
#         panel.grid = element_blank(), 
#         plot.margin = margin(8, 32, 16, 8, "pt"), 
#         plot.title = element_text(size = rel(1.1), 
#                                   margin = margin(0, 0, 42, 0, "pt")))
# 
# ggsave(filename = here(glue("outputs/fully_vax_change_communities_{latest_date}.png")), 
#        plot = chart_fully_vax_change, 
#        device = agg_png, 
#        width = 2800, 
#        height = 2150, 
#        units = "px", 
#        bg = "white")

# Change in first doses rate chart
# chart_first_doses_change <- ggplot(dat_chart_vax_rate_change, 
#                                    mapping = aes(y = fct_rev(ethnicity_age), 
#                                                  x = dhb_of_residence)) + 
#   geom_tile(mapping = aes(fill = first_dose_rate_change_category), 
#             colour = grey(0.97), size = 3) + 
#   geom_text(mapping = aes(angle = 90 * (first_dose_rate_change / 0.25)), 
#             colour = "white", label = "—", 
#             family = "Fira Sans Custom", 
#             size = 4, 
#             data = dat_chart_vax_rate_change |> 
#               filter(population_current > 99)) + 
#   geom_hline(yintercept = c(1.5, 4.5, 7.5, 10.5), colour = "black") + 
#   scale_fill_manual(drop = FALSE, name = NULL,
#                     values = c("Less than 2% increase" = "#440154FF",
#                                "2% to 4% increase" = "#414487FF",
#                                "4% to 6% increase" = "#2A788EFF",
#                                "6% to 8% increase" = "#22A884FF",
#                                "8% to 10% increase" = "#7AD151FF",
#                                "Greater than 10% increase" = "#FDE725FF",
#                                "Suppressed (fewer than 100 people)" = grey(0.8))) +
#   scale_x_discrete(position = "top") + 
#   guides(fill = guide_legend(ncol = 3, 
#                              override.aes = list(size = 0.5))) + 
#   xlab("") + 
#   ylab("") + 
#   annotate(geom = "text", 
#            x = 0.4, 
#            y = -0.5, 
#            label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/", 
#            hjust = 0, 
#            family = "Fira Sans Custom", 
#            size = 2) + 
#   coord_cartesian(clip = "off") + 
#   ggtitle(glue("Change in the first doses rate in the week to {latest_date_nice}")) + 
#   theme_minimal(base_family = "Fira Sans Custom") + 
#   theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
#         legend.justification = c(0, 0), 
#         legend.position = c(-0.015, 1.16), 
#         panel.grid = element_blank(), 
#         plot.margin = margin(8, 32, 16, 8, "pt"), 
#         plot.title = element_text(size = rel(1.1), 
#                                   margin = margin(0, 0, 42, 0, "pt")))
# 
# ggsave(filename = here(glue("outputs/first_doses_change_communities_{latest_date}.png")), 
#        plot = chart_first_doses_change, 
#        device = agg_png, 
#        width = 2800, 
#        height = 2150, 
#        units = "px", 
#        bg = "white")

# *****************************************************************************


# *****************************************************************************
# Age-standardised vax rates ----
# Age 12+ only, until MoH makes 5-11yo ethnic groups consistent with 12+

# Standard age distributions (all ethnic groups combined)
dist_pop_by_age <- dat |> 
  group_by(age_group) |> 
  summarise(population = sum(population)) |> 
  ungroup() |> 
  mutate(population_pct = population / sum(population)) 

dist_pop_by_age_dhb <- dat |> 
  group_by(dhb_of_residence, age_group) |> 
  summarise(population = sum(population)) |> 
  ungroup() |> 
  mutate(population_pct = population / sum(population)) 

# Vaccination rates by ethnic group 
vax_rates_by_ethnic_group_age <- dat |> 
  filter(ethnic_group != "Unknown", 
         ethnic_group != "Various") |> 
  group_by(ethnic_group, age_group) |> 
  summarise(first_dose_administered = sum(first_dose_administered), 
            second_dose_administered = sum(second_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(first_dose_pct = first_dose_administered / population, 
         second_dose_pct = second_dose_administered / population) 

vax_rates_by_ethnic_group_age_dhb <- dat |> 
  filter(ethnic_group != "Unknown", 
         ethnic_group != "Various") |> 
  group_by(ethnic_group, dhb_of_residence, age_group) |> 
  summarise(first_dose_administered = sum(first_dose_administered), 
            second_dose_administered = sum(second_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(first_dose_pct = first_dose_administered / population, 
         second_dose_pct = second_dose_administered / population) 

# Unadjusted and adjusted vax rates by ethnicity
vax_rates_by_ethnic_group_std <- bind_rows(
  # Unadjusted rates
  vax_rates_by_ethnic_group_age |> 
    group_by(ethnic_group) |> 
    summarise(first_dose_administered = sum(first_dose_administered), 
              second_dose_administered = sum(second_dose_administered), 
              population = sum(population)) |> 
    ungroup() |> 
    mutate(first_dose_pct = first_dose_administered / population, 
           second_dose_pct = second_dose_administered / population) |> 
    select(ethnic_group, first_dose_pct, second_dose_pct) |> 
    mutate(type = "unadjusted"), 
  
  # Age-standardised rates
  vax_rates_by_ethnic_group_age |> 
    select(ethnic_group, age_group, first_dose_pct, second_dose_pct) |> 
    left_join(y = dist_pop_by_age, by = "age_group") |> 
    mutate(first_dose_pct_weighted = first_dose_pct * population_pct, 
           second_dose_pct_weighted = second_dose_pct * population_pct) |> 
    group_by(ethnic_group) |> 
    summarise(first_dose_pct = sum(first_dose_pct_weighted), 
              second_dose_pct = sum(second_dose_pct_weighted)) |> 
    ungroup() |> 
    mutate(type = "age-standardised"), 
  
  # Age- and DHB-standardised rates
  vax_rates_by_ethnic_group_age_dhb |> 
    select(ethnic_group, age_group, dhb_of_residence, first_dose_pct, second_dose_pct) |> 
    left_join(y = dist_pop_by_age_dhb, by = c("age_group", "dhb_of_residence")) |> 
    mutate(first_dose_pct_weighted = first_dose_pct * population_pct, 
           second_dose_pct_weighted = second_dose_pct * population_pct) |> 
    group_by(ethnic_group) |> 
    summarise(first_dose_pct = sum(first_dose_pct_weighted), 
              second_dose_pct = sum(second_dose_pct_weighted)) |> 
    ungroup() |> 
    mutate(type = "age-and-dhb-standardised")
) |> 
  pivot_longer(cols = c(first_dose_pct, second_dose_pct), 
               values_to = "value", 
               names_to = "dose") |> 
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Maori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "European or Other", 
                                          "All"), 
                               labels = c("Māori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "Pākehā or other", 
                                          "All"), 
                               ordered = TRUE)) |> 
  mutate(type = factor(x = type, 
                       levels = c("unadjusted", 
                                  "age-standardised", 
                                  "age-and-dhb-standardised"), 
                       labels = c("Unadjusted", 
                                  "Age standardised", 
                                  "Age and DHB standardised"), 
                       ordered = TRUE)) |> 
  mutate(dose = factor(x = dose, 
                       levels = c("first_dose_pct", 
                                  "second_dose_pct"), 
                       labels = c("First dose", 
                                  "Second dose"), 
                       ordered = TRUE))

# Chart of rates by ethnic group
chart_vax_rates_by_ethnic_group_std <- 
  vax_rates_by_ethnic_group_std |> 
  ggplot(mapping = aes(x = ethnic_group, 
                       y = value, 
                       label = percent(x = value, accuracy = 1), 
                       colour = type,
                       fill = type)) + 
  geom_col(position = "dodge", size = 0) + 
  geom_text(position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            family = "Fira Sans Custom", 
            size = 2.5) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_colour_manual(values = c("Unadjusted" = "#003f5c",
                                 "Age standardised" = "#bc5090",
                                 "Age and DHB standardised" = "#ffa600"),
                      name = NULL,
                      aesthetics = c("colour", "fill")) +
  facet_wrap(facets = vars(dose), nrow = 1) + 
  ylab("") + 
  xlab("") + 
  ggtitle(label = "COVID-19 vaccination rates by ethnic group (age 12+ only)", 
          subtitle = glue("As at {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.25), 
        axis.ticks.x = element_blank(), 
        legend.position = "top", 
        legend.direction = "horizontal", 
        panel.spacing.x = unit(36, "pt"), 
        strip.text = element_text(face = "bold", hjust = 0), 
        plot.margin = margin(4, 4, 4, 4, "pt"), 
        axis.title = element_blank(), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 8, 0, "pt")))

ggsave(filename = glue(here("outputs/vax_rates_by_ethnicity_standardised_{latest_date}.png")), 
       plot = chart_vax_rates_by_ethnic_group_std, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

# *****************************************************************************

