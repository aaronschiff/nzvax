# Looking at booster shots for population groups

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
library(lubridate)
library(colorspace)
library(shadowtext)

latest_date <- "08_02_2022"

latest_date_nice <- "8 February 2022"

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

# Latest booster data from MoH
dat_boost <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                        sheet = "Boosters") |>
  clean_names() |> 
  select(-x5, -x6, -x7) |> 
  mutate(ethnic_group = ifelse(ethnic_group == "Other", 
                               "European/Other", 
                               ethnic_group))

# HSU population data
dat_pop <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                      sheet = "HSU Population") |>
  clean_names() |> 
  select(-x6, -x7, -notes) |> 
  mutate(dhb_of_residence = ifelse(dhb_of_residence == "Hawkes Bay", 
                                   "Hawke's Bay", 
                                   dhb_of_residence))

# Combine boost and 18+ population 
dat_boosted_pop <- dat_boost |> 
  left_join(y = dat_pop |> 
              filter(age_group != "0-4", 
                     age_group != "12-17", 
                     age_group != "12+", 
                     age_group != "5-11") |> 
              group_by(dhb_of_residence, ethnic_group) |> 
              summarise(population = sum(population)) |> 
              ungroup() |> 
              mutate(ethnic_group = case_when(
                ethnic_group == "European or Other" ~ "European/Other", 
                ethnic_group == "Various" ~ "Unknown", 
                TRUE ~ ethnic_group
              )) |> 
              mutate(dhb_of_residence = case_when(
                dhb_of_residence == "Overseas / Unknown" ~ "Overseas/Unknown", 
                TRUE ~ dhb_of_residence
              )), 
            by = c("dhb_of_residence", "ethnic_group"))

# Add total for each DHB and manipulate for charting
dat_combined <- bind_rows(
  # Clean data
  dat_boosted_pop, 
  # Add totals for each DHB
  dat_boosted_pop |> 
    group_by(dhb_of_residence) |> 
    summarise(eligible_for_booster = sum(eligible_for_booster), 
              booster_received = sum(booster_received), 
              population = sum(population)) |> 
    mutate(ethnic_group = "All") |> 
    ungroup() 
) |> 
  # Remove unknown categories
  filter(ethnic_group != "Unknown", 
         ethnic_group != "Various", 
         dhb_of_residence != "Overseas/Unknown", 
         dhb_of_residence != "Unknown", 
         dhb_of_residence != "Various") |> 
  mutate(booster_rate = booster_received / population) |> 
  # Create category factors for ordering
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Maori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "European/Other", 
                                          "All"), 
                               labels = c("Māori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "Pākehā or other", 
                                          "All"), 
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
                                              "Hawke's Bay", 
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
  mutate(booster_rate_rounded = round(100 * booster_rate)) |> 
  mutate(boost_rate_category = case_when(
    booster_rate_rounded < 20 ~ "Less than 20%", 
    (booster_rate_rounded >= 20) & (booster_rate_rounded <= 30) ~ "20% to 30%", 
    (booster_rate_rounded > 30) & (booster_rate_rounded <= 40) ~ "30% to 40%", 
    (booster_rate_rounded > 40) & (booster_rate_rounded <= 50) ~ "40% to 50%", 
    (booster_rate_rounded > 50) & (booster_rate_rounded <= 60) ~ "50% to 60%", 
    (booster_rate_rounded > 60) & (booster_rate_rounded <= 70) ~ "60% to 70%", 
    (booster_rate_rounded > 70) & (booster_rate_rounded <= 80) ~ "70% to 80%", 
    (booster_rate_rounded > 80) & (booster_rate_rounded <= 90) ~ "80% to 90%", 
    (booster_rate_rounded > 90) ~ "Greater than 90%"
  )) |> 
  mutate(boost_rate_category = factor(x = boost_rate_category, 
                                      levels = c("Less than 20%", 
                                                 "20% to 30%", 
                                                 "30% to 40%", 
                                                 "40% to 50%", 
                                                 "50% to 60%", 
                                                 "60% to 70%", 
                                                 "70% to 80%", 
                                                 "80% to 90%", 
                                                 "Greater than 90%"), 
                                      ordered = TRUE)) |> 
  arrange(dhb_of_residence, ethnic_group)

# *****************************************************************************


# *****************************************************************************
# Visualise ----

chart_boost_rate <- ggplot(dat_combined, 
                           mapping = aes(y = fct_rev(ethnic_group), 
                                         x = dhb_of_residence)) + 
  geom_tile(mapping = aes(fill = boost_rate_category), 
            colour = grey(0.97),  size = 3) + 
  geom_shadowtext(mapping = aes(label = round(100 * booster_rate)), 
                  colour = "white", 
                  bg.colour = "black", 
                  bg.r = 0.05, 
                  family = "Fira Sans Custom", 
                  fontface = "bold", 
                  size = 2.5) + 
  scale_x_discrete(position = "top") + 
  xlab("") + 
  ylab("") + 
  scale_fill_brewer(palette = "RdYlBu", 
                    drop = FALSE, 
                    guide = "none") + 
  scale_colour_manual(values = c("80+" = grey(0.75),
                                 "under-80" = "white"), 
                      guide = "none") + 
  annotate(geom = "text", 
           x = 0.4, 
           y = -0.5, 
           label = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/", 
           hjust = 0, 
           family = "Fira Sans Custom", 
           size = 2.5) + 
  coord_cartesian(clip = "off") + 
  ggtitle(label = "COVID-19 boosters for NZ population groups", 
          subtitle = glue("Proportion of people aged 18+ who have received a booster dose to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0), 
        legend.justification = c(0, 0), 
        legend.position = c(-0.015, 1.35), 
        panel.grid = element_blank(), 
        plot.margin = margin(8, 32, 16, 8, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 0, 0, "pt")))

ggsave(filename = here(glue("outputs/boost_communities_{latest_date}.png")), 
       plot = chart_boost_rate, 
       device = agg_png, 
       width = 2800, 
       height = 1250, 
       units = "px", 
       bg = "white")

# *****************************************************************************

