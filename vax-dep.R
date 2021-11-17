# Vaccination rates vs deprivation index


# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)
library(scales)
library(ggrepel)

latest_date_tla <- "05_10_2021"  # Date of most recent week's TLA data
latest_date_sa2 <- "20211006"    # Date of most recent week's SA2 data
latest_date_dhb <- "09_11_2021"  # Date of most recent week's DHB data

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Vaccination data by TLA
dat_vax_tla <- read_csv(file = here(glue("data/vaccination_tla_{latest_date_tla}.csv"))) |> 
  clean_names() |> 
  select(-percent_had_first_dose, -percent_fully_vaccinated) |> 
  mutate(first_dose_rate = first_dose_administered / population, 
         fully_vax_rate = second_dose_administered / population)

# Vaccination data by SA2
dat_vax_sa2 <- read_csv(file = here(glue("data/uptake_sa2_dhb_{latest_date_sa2}.csv"))) |> 
  mutate(dose1_cnt = as.integer(dose1_cnt), 
         dose2_cnt = as.integer(dose2_cnt), 
         pop_cnt = as.integer(pop_cnt)) |> 
  mutate(dose1_uptake = ifelse(dose1_uptake == ">950", "950", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">950", "950", dose2_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake), 
         dose2_uptake = as.integer(dose2_uptake))

# Vaccination data by DHB
dat_vax_dhb <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date_dhb}.xlsx")), 
                          sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) |> 
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
    age_group == "90+" ~ "60+",
    age_group == "Various" ~ "Various"
  )) |>
  # Summarise by dhb, age group
  group_by(dhb_of_residence, age_group_2) |> 
  summarise(second_dose_administered = sum(second_dose_administered), 
            first_dose_administered = sum(first_dose_administered), 
            population = sum(population)) |> 
  ungroup() |> 
  mutate(unvaxed = population - first_dose_administered) |> 
  mutate(unvaxed_rate = unvaxed / population, 
         first_dose_rate = first_dose_administered / population)

# Deprivation index data
dat_dep <- read_excel(path = here("data/otago823836.xlsx")) |> 
  clean_names()

# *****************************************************************************


# *****************************************************************************
# Compare deprivation and vaccination by TLA ----

# Deprivation vs vaccination data
dat_dep_vax_tla <- 
  # TLA-level population-weighted average deprivation score
  dat_dep |> 
  group_by(ta_2018_name) |> 
  summarise(wt_avg_nz_dep2018_score = weighted.mean(x = nz_dep2018_score, 
                                                    w = ur_popn_sa1_2018, 
                                                    na.rm = TRUE)) |> 
  ungroup() |> 
  filter(ta_2018_name != "Area Outside Territorial Authority", 
         ta_2018_name != "Chatham Islands Territory") |> 
  # Join to vaccination data
  left_join(y = dat_vax_tla, by = c("ta_2018_name" = "tla")) |> 
  arrange(desc(population)) |> 
  # Create shortened TLA labels
  mutate(ta_name_short = str_remove(string = ta_2018_name, pattern = "City")) |> 
  mutate(ta_name_short = str_remove(string = ta_name_short, pattern = "District")) |> 
  mutate(ta_name_short = str_trim(string = ta_name_short))

chart_dat_dep_vax_tla <- dat_dep_vax_tla |> 
  ggplot(mapping = aes(x = wt_avg_nz_dep2018_score, 
                       y = first_dose_rate, 
                       label = ta_name_short)) + 
  geom_point(shape = 21, colour = "white", fill = "cornflowerblue", 
             size = 2) + 
  geom_text_repel(size = 2, 
                  family = "Fira Sans", 
                  colour = grey(0.25),
                  min.segment.length = 0.1, 
                  max.overlaps = 20, 
                  segment.colour = grey(0.75), 
                  segment.size = 0.25) + 
  scale_y_continuous(limits = c(0.55, 0.95), 
                     breaks = seq(0.5, 1, .05), 
                     labels = percent_format(accuracy = 1), 
                     expand = expansion(0, 0)) + 
  scale_x_continuous(limits = c(900, 1200), 
                     breaks = seq(900, 1200, 50), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  xlab("Population-weighted average deprivation score") + 
  ylab("First dose\nvaccination rate") + 
  theme(panel.grid.minor = element_blank(), 
        plot.margin = margin(8, 16, 8, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  face = "bold", 
                                  margin = margin(0, 0, 8, 0, "pt")), 
        axis.title.y = element_text(angle = 0, 
                                    margin = margin(0, 8, 0, 0, "pt"), 
                                    hjust = 0, 
                                    face = "bold"), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(8, 0, 0, 0, "pt")))

ggsave(filename = here("outputs/vax_vs_dep_tla.png"), 
       plot = chart_dat_dep_vax_tla, 
       device = "png", 
       width = 2000, 
       height = 2000, 
       units = "px", 
       bg = "white")

# *****************************************************************************


# *****************************************************************************
# Compare deprivation and vaccination by SA2 ----

# Deprivation vs vaccination data
dat_dep_vax_sa2 <- 
  # SA2-level population-weighted average deprivation score
  dat_dep |> 
  group_by(sa22018_code, sa22018_name, ta_2018_name) |> 
  summarise(wt_avg_nz_dep2018_score = weighted.mean(x = nz_dep2018_score, 
                                                    w = ur_popn_sa1_2018, 
                                                    na.rm = TRUE)) |> 
  ungroup() |> 
  filter(ta_2018_name != "Area Outside Territorial Authority", 
         ta_2018_name != "Chatham Islands Territory") |> 
  # Join to vaccination data
  left_join(y = dat_vax_sa2, by = c("sa22018_code" = "sa2_code"))

chart_dat_dep_vax_sa2 <- dat_dep_vax_sa2 |> 
  ggplot(mapping = aes(x = wt_avg_nz_dep2018_score, 
                       y = dose1_uptake / 1000)) + 
  geom_point(colour = "cornflowerblue", size = 0.5) + 
  scale_y_continuous(limits = c(0.3, 1),
                     breaks = seq(0.3, 1, .05),
                     labels = percent_format(accuracy = 1),
                     expand = expansion(0, 0)) +
  scale_x_continuous(limits = c(800, 1400),
                     breaks = seq(800, 1400, 100),
                     labels = comma_format(accuracy = 1)) +
  theme_minimal(base_family = "Fira Sans") + 
  xlab("Population-weighted average deprivation score") + 
  ylab("First dose\nvaccination rate") + 
  theme(panel.grid.minor = element_blank(), 
        plot.margin = margin(8, 16, 8, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  face = "bold", 
                                  margin = margin(0, 0, 8, 0, "pt")), 
        axis.title.y = element_text(angle = 0, 
                                    margin = margin(0, 8, 0, 0, "pt"), 
                                    hjust = 0, 
                                    face = "bold"), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(8, 0, 0, 0, "pt")))

ggsave(filename = here("outputs/vax_vs_dep_sa2.png"), 
       plot = chart_dat_dep_vax_sa2, 
       device = "png", 
       width = 2000, 
       height = 2000, 
       units = "px", 
       bg = "white")

# *****************************************************************************


# *****************************************************************************
# Compare deprivation and vaccination by DHB and age ----

dat_dep_vax_dhb_age <- 
  # DHB-level population-weighted average deprivation score
  dat_dep |> 
  group_by(dhb_2018_name) |> 
  summarise(wt_avg_nz_dep2018_score = weighted.mean(x = nz_dep2018_score, 
                                                    w = ur_popn_sa1_2018, 
                                                    na.rm = TRUE)) |> 
  ungroup() |> 
  filter(dhb_2018_name != "Area Outside District Health Board") |> 
  mutate(dhb_2018_name = ifelse(dhb_2018_name == "Hawke's Bay", 
                                "Hawkes Bay", 
                                dhb_2018_name)) |> 
  # Join to vaccination data
  left_join(y = dat_vax_dhb, by = c("dhb_2018_name" = "dhb_of_residence")) |> 
  left_join(y = dat_vax_dhb |> 
              group_by(dhb_of_residence) |> 
              summarise(population = sum(population), 
                        first_dose_administered = sum(first_dose_administered), 
                        unvaxed = sum(unvaxed)) |> 
              ungroup() |> 
              mutate(dhb_unvaxed_rate = unvaxed / population, 
                     dhb_first_dose_rate = first_dose_administered / population) |> 
              select(dhb_of_residence, dhb_unvaxed_rate, dhb_first_dose_rate), 
            by = c("dhb_2018_name" = "dhb_of_residence")) |> 
  mutate(excess_unvaxed_rate = unvaxed_rate - dhb_unvaxed_rate, 
         excess_first_dose_rate = first_dose_rate - dhb_first_dose_rate) |> 
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("12-29 years old", 
                                         "30-59 years old", 
                                         "60+ years old"), 
                              ordered = TRUE))
  
chart_dat_dep_vax_dhb_age <- dat_dep_vax_dhb_age |> 
  ggplot(mapping = aes(x = wt_avg_nz_dep2018_score, 
                       y = excess_first_dose_rate)) + 
  geom_point(size = 1) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5) + 
  scale_size_area() + 
  facet_wrap(facets = vars(age_group_2), nrow = 1) + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_x_continuous(labels = comma_format(accuracy = 1)) + 
  xlab("Weighted average deprivation score for DHB (higher = more deprived)") + 
  ylab("Difference between\nfirst dose rate\nfor age group in DHB\nand overall\nfirst dose rate for DHB") + 
  theme_minimal(base_family = "Fira Sans", 
                base_size = 13) + 
  theme(panel.grid.minor = element_blank(), 
        panel.spacing.x = unit(24, "pt"), 
        panel.grid = element_line(size = 0.25), 
        strip.text = element_text(face = "bold", 
                                  margin = margin(8, 0, 12, 0, "pt")), 
        axis.title.y = element_text(angle = 0, 
                                    margin = margin(0, 12, 0, 0, "pt"), 
                                    hjust = 0), 
        axis.title.x = element_text(margin = margin(12, 0, 0, 0, "pt")))

ggsave(filename = here("outputs/vax-dep-age-by-dhb.png"), 
       plot = chart_dat_dep_vax_dhb_age, 
       device = "png", 
       width = 2800, 
       height = 1600, 
       units = "px", 
       bg = "white")
# *****************************************************************************
