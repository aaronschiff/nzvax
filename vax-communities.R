# Vaccination rates visualised by 'community'

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)

latest_date <- "28_09_2021"

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Raw data
dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes)

# Chart data
dat_chart <- dat |> 
  # Remove unknown categories
  filter(gender != "Unknown/Other", 
         ethnic_group != "Unknown", 
         ethnic_group != "Various", 
         dhb_of_residence != "Overseas / Unknown") |> 
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
  # Summarise by dhb, ethnicity, age group
  group_by(dhb_of_residence, ethnic_group, age_group_2) |> 
  summarise(second_dose_administered = sum(second_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(fully_vax_rate = second_dose_administered / population) |> 
  # Categorise vax rate
  mutate(vax_category = case_when(
    fully_vax_rate > 0.9 ~ "Greater than 90% fully vaccinated", 
    (fully_vax_rate >= 0.8) & (fully_vax_rate <= 0.9) ~ "80% to 90% fully vaccinated", 
    TRUE ~ "Less than 80% fully vaccinated"
  )) |> 
  # Create factors for ordering
  mutate(vax_category = factor(x = vax_category, 
                               levels = c("Greater than 90% fully vaccinated", 
                                          "80% to 90% fully vaccinated", 
                                          "Less than 80% fully vaccinated"), 
                               ordered = TRUE)) |> 
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Maori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "European or Other"), 
                               labels = c("Māori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "Pākehā or Other"), 
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
                                   ordered = TRUE)) |> 
  arrange(dhb_of_residence, ethnicity_age)

# *****************************************************************************


# *****************************************************************************
# Visualise ----

chart <- ggplot(dat_chart) + 
  geom_tile(mapping = aes(y = fct_rev(ethnicity_age), 
                          x = dhb_of_residence, 
                          fill = vax_category), 
            colour = grey(0.95),  size = 3) + 
  geom_hline(yintercept = 3.5, colour = "black") + 
  geom_hline(yintercept = 6.5, colour = "black") + 
  geom_hline(yintercept = 9.5, colour = "black") + 
  scale_fill_manual(values = c("Greater than 90% fully vaccinated" = grey(0.05), 
                               "80% to 90% fully vaccinated" = "pink", 
                               "Less than 80% fully vaccinated" = "firebrick"), 
                    name = NULL) + 
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(ncol = 1, 
                             override.aes = list(size = 0.5))) + 
  xlab("") + 
  ylab("") + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(0, 1.2), 
        plot.margin = margin(24, 32, 8, 8, "pt"))

ggsave(filename = here(glue("outputs/vax_communities_{latest_date}.png")), 
       plot = chart, 
       device = "png", 
       width = 2400, 
       height = 1800, 
       units = "px", 
       bg = "white")

# *****************************************************************************
