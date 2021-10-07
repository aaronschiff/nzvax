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
