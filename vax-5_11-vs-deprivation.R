# Kid vaccinations vs deprivation for SA2 areas

library(tidyverse)
library(glue)
library(janitor)
library(here)
library(readxl)
library(scales)
library(ragg)
library(systemfonts)

latest_date <- "22_02_2022"

register_font(
  name = "National 2 Custom", 
  plain = system_fonts() |> filter(family == "National 2", style == "Regular") |> pull(path), 
  bold = system_fonts() |> filter(family == "National 2", style == "Extrabold") |> pull(path), 
  italic = system_fonts() |> filter(family == "National 2", style == "Regular Italic") |> pull(path), 
  bolditalic = system_fonts() |> filter(family == "National 2", style == "Extrabold Italic") |> pull(path), 
  features = font_feature(ligatures = c("discretionary", 
                                        "standard", 
                                        "contextual"), 
                          numbers = c("lining", "proportional"))
)

# Load raw vaccination data and manipulate
dat_sa2 <-  read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                       sheet = "SA2 All Ethnicities") |> 
  clean_names() |> 
  filter(age == "5-11") |> 
  select(-notes) |> 
  rename(dose1_uptake = partially_vaccinated_uptake, 
         dose2_uptake = fully_vaccinated_uptake, 
         pop_cnt = population) |> 
  # Exclude unknown SA2
  filter(sa2_code_2018 != "Unknown") |> 
  # Clean up population
  mutate(pop_cnt = str_remove(string = pop_cnt, pattern = ",")) |> 
  # Replace >950 and <50 with numeric values
  mutate(dose1_uptake = str_remove(string = dose1_uptake, pattern = "%"), 
         dose2_uptake = str_remove(string = dose2_uptake, pattern = "%"), 
         booster_uptake = str_remove(string = booster_uptake, pattern = "%")) |> 
  mutate(dose1_uptake = ifelse(dose1_uptake == ">95", "95", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">95", "95", dose2_uptake), 
         booster_uptake = ifelse(booster_uptake == ">95", "95", booster_uptake), 
         dose1_uptake = ifelse(dose1_uptake == "<5", "5", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == "<5", "5", dose2_uptake), 
         booster_uptake = ifelse(booster_uptake == "<5", "5", booster_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake), 
         dose2_uptake = as.integer(dose2_uptake), 
         booster_uptake = as.integer(booster_uptake)) |> 
  # Assign uptake categories
  mutate(fully_vax_category = case_when(
    dose2_uptake > 90 ~ "Greater than 90%", 
    (dose2_uptake > 80) & (dose2_uptake <= 90) ~ "80% to 90%", 
    (dose2_uptake >= 70) & (dose2_uptake <= 80) ~ "70% to 80%", 
    TRUE ~ "Less than 70%"
  )) |> 
  mutate(first_dose_category = case_when(
    dose1_uptake > 90 ~ "Greater than 90%", 
    (dose1_uptake > 80) & (dose1_uptake <= 90) ~ "80% to 90%", 
    (dose1_uptake >= 70) & (dose1_uptake <= 80) ~ "70% to 80%", 
    TRUE ~ "Less than 70%"
  )) |> 
  mutate(booster_category = case_when(
    booster_uptake > 90 ~ "Greater than 90%", 
    (booster_uptake > 80) & (booster_uptake <= 90) ~ "80% to 90%", 
    (booster_uptake >= 70) & (booster_uptake <= 80) ~ "70% to 80%", 
    TRUE ~ "Less than 70%"
  ))

dat_dep_sa2 <- read_excel(path = here("data/otago823836.xlsx")) |> 
  clean_names() |> 
  group_by(sa22018_code) |> 
  summarise(mean_nz_dep2018 = weighted.mean(x = nz_dep2018_score, 
                                            w = ur_popn_sa1_2018, 
                                            na.rm = TRUE)) |> 
  ungroup()

dat_vax_vs_dep <- dat_sa2 |> 
  left_join(y = dat_dep_sa2, 
            by = c("sa2_code_2018" = "sa22018_code")) |> 
  mutate(pop_cnt = as.integer(pop_cnt)) |> 
  filter(!is.na(mean_nz_dep2018))

chart <- dat_vax_vs_dep |> 
  arrange(desc(pop_cnt)) |> 
  ggplot(mapping = aes(x = mean_nz_dep2018, 
                       y = dose1_uptake, 
                       weight = pop_cnt)) + 
  geom_point(mapping = aes(size = pop_cnt), 
             shape = 21, 
             fill = rgb(red = 31/255, 
                        green = 144/255, 
                        blue = 255/255, 
                        alpha = 0.5), 
             colour = grey(1), 
             stroke = 0.2) + 
  geom_smooth(se = FALSE, method = "loess", colour = "firebrick") + 
  scale_size(range = c(0.1, 6), 
             limits = c(0, 1000), 
             name = "Population aged 5-11", 
             breaks = c(50, 100, 250, 500, 1000), 
             labels = comma_format(accuracy = 1)) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) + 
  scale_x_continuous(limits = c(800, 1400), 
                     breaks = seq(800, 1400, 100), 
                     labels = comma_format(accuracy = 1)) + 
  xlab("Deprivation score (higher = more deprived)") + 
  ylab("Percent\nvaccinated\n(first doses)") + 
  labs(title = "Vaccination rate for 5-11 year olds vs deprivation score by SA2 area", 
       caption = "Chart by Aaron Schiff with data from Ministry of Health and University of Otago") + 
  theme_minimal(base_family = "National 2 Custom") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.25), 
        axis.title.y = element_text(angle = 0, 
                                    hjust = 0, 
                                    face = "bold", 
                                    margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(8, 0, 8, 0, "pt")), 
        plot.title = element_text(margin = margin(0, 0, 12, 0, "pt")), 
        legend.position = c(0.85, 0.82), 
        plot.caption = element_text(face = "italic"))

ggsave(filename = here(glue("outputs/latest/vax_5_11_vs_deprivation_{latest_date}.png")), 
       plot = chart, 
       device = agg_png, 
       width = 2800, 
       height = 2000, 
       units = "px", 
       bg = "white")
