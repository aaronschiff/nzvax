# Charts of numbers of unvaccinated people

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

latest_date <- "25_01_2022"

latest_date_nice <- "25 January 2022"

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
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) |> 
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

# Public hospitals
# dat_hosp <- read_csv(file = here("data/nz_public_hospitals.csv"), 
#                      col_types = "cccci") |> 
#   clean_names() |> 
#   mutate(dhb_name = str_remove(string = dhb_name, 
#                                pattern = "District Health Board")) |> 
#   mutate(dhb_name = str_trim(dhb_name)) |> 
#   mutate(dhb_name = ifelse(dhb_name == "Hawke's Bay", 
#                            "Hawkes Bay", 
#                            dhb_name))

# *****************************************************************************


# *****************************************************************************
# Process data ----

# Manipulate vaccination data for Māori
dat_m <- bind_rows(dat, dat_511) |> 
  # Filter Māori and exclude unknown DHB
  filter(ethnic_group == "Maori", 
         dhb_of_residence != "Overseas / Unknown") |> 
  # Assign custom age groups
  mutate(age_group_2 = case_when(
    age_group == "5-11" ~ "5-11", 
    age_group == "12-18" ~ "12-29", 
    age_group == "19-24" ~ "12-29", 
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
  # Summarise population and first doses by DHB and age group
  group_by(dhb_of_residence, age_group_2) |> 
  summarise(population = sum(population), 
            first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  # Calculate unvaccinated number in each group
  mutate(unvax = pmax(population - first_dose_administered, 0)) |> 
  # Apply factors for ordering
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("5-11", 
                                         "12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("5-11 years old", 
                                         "12-29 years old", 
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

# Manipulate vaccination data for age 60+
dat_60plus <- dat |> 
  # Exclude unknowns
  filter(ethnic_group != "Unknown",
         ethnic_group != "Various", 
         dhb_of_residence != "Overseas / Unknown") |> 
  # Assign custom age groups
  mutate(age_group_2 = case_when(
    age_group == "5-11" ~ "5-11", 
    age_group == "12-18" ~ "12-29", 
    age_group == "19-24" ~ "12-29", 
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
  filter(age_group_2 == "60+") |> 
  # Summarise population and first doses by DHB and ethnic group
  group_by(dhb_of_residence, ethnic_group) |> 
  summarise(population = sum(population), 
            first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  # Calculate unvaccinated number in each group
  mutate(unvax = pmax(population - first_dose_administered, 0)) |> 
  # Apply factors for ordering
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
  arrange(dhb_of_residence, ethnic_group)

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Bar chart of numbers of unvaccinated Māori people
chart_m_bar <- dat_m |> 
  ggplot(mapping = aes(x = dhb_of_residence, 
                       y = unvax, 
                       fill = age_group_2, 
                       colour = age_group_2, 
                       label = comma(x = unvax, accuracy = 1))) + 
  geom_hline(yintercept = 0, size = 0.25, colour = grey(0.5)) +
  geom_col(size = 0) + 
  geom_text(nudge_y = 800, 
            family = "Fira Sans Custom", 
            size = 3) + 
  facet_wrap(facets = vars(age_group_2), ncol = 1) + 
  scale_y_continuous(labels = comma_format(accuracy = 1), 
                     limits = c(0, 14000), 
                     breaks = seq(0, 14000, 2000)) + 
  scale_x_discrete(position = "top") + 
  scale_colour_manual(values = c("5-11 years old" = grey(0.5), 
                                 "12-29 years old" = "#6929c4", 
                                 "30-59 years old" = "#1192e8", 
                                 "60+ years old" = "#005d5d"), 
                      guide = "none", 
                      aesthetics = c("colour", "fill")) + 
  xlab("") + 
  ylab("") + 
  ggtitle(label = glue("Number of unvaccinated Māori people as at {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
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
       height = 2400, 
       units = "px", 
       device = agg_png, 
       bg = "white")

# Bar chart of numbers of unvaccinated people aged 60+
chart_60plus_bar <- dat_60plus |> 
  ggplot(mapping = aes(y = unvax, 
                       x = dhb_of_residence, 
                       fill = ethnic_group, 
                       colour = ethnic_group, 
                       label = comma(x = unvax, accuracy = 1))) + 
  geom_hline(yintercept = 0, size = 0.25, colour = grey(0.5)) +
  geom_col(size = 0) + 
  geom_text(family = "Fira Sans Custom", 
            size = 3, 
            nudge_y = 500) + 
  facet_wrap(facets = vars(ethnic_group), ncol = 1) + 
  scale_x_discrete(position = "top") + 
  scale_y_continuous(labels = comma_format(accuracy = 1),
                     limits = c(0, 4800),
                     breaks = seq(0, 4000, 1000), 
                     expand = expansion(0, 0)) +
  scale_colour_manual(values = c("Māori" = "#6929c4", 
                                 "Pacific Peoples" = "#1192e8", 
                                 "Asian" = "#005d5d", 
                                 "Pākehā or other" = "#9f1853"),
                      guide = "none",
                      aesthetics = c("colour", "fill")) +
  xlab("") + 
  ylab("") + 
  ggtitle(label = glue("Number of unvaccinated people aged 60+ as at {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = 0.25, colour = grey(0.9)), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x.top = element_text(angle = 45, hjust = 0, face = "bold"), 
        panel.spacing.y = unit(16, "pt"), 
        plot.margin = margin(4, 16, 8, 4, "pt"), 
        strip.text = element_text(face = "bold"))

ggsave(filename = here(glue("outputs/unvax_60plus_{latest_date}.png")), 
       plot = chart_60plus_bar, 
       width = 2800, 
       height = 2000, 
       units = "px", 
       device = agg_png, 
       bg = "white")

# *****************************************************************************
