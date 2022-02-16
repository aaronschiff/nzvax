# Charts of vaccine uptake by SA2 areas

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(glue)
library(ggbeeswarm)
library(colorspace)
library(scales)
library(systemfonts)
library(ragg)
library(readxl)

latest_date <- "15_02_2022"
latest_date_nice <- "15 February 2022"

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

# Load raw vaccination data and manipulate
dat_sa2 <-  read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                       sheet = "SA2 All Ethnicities") |> 
  clean_names() |> 
  select(-x12, -notes) |> 
  rename(dose1_uptake = partially_vaccinated_uptake, 
         dose2_uptake = fully_vaccinated_uptake, 
         pop_cnt = population) |> 
  # Exclude unknown SA2
  filter(sa2_code_2018 != "Unknown") |> 
  # Clean up population
  mutate(pop_cnt = str_remove(string = pop_cnt, pattern = ",")) |> 
  # Replace >950 and <50 with numeric values
  mutate(dose1_uptake = str_remove(string = dose1_uptake, pattern = "%"), 
         dose2_uptake = str_remove(string = dose1_uptake, pattern = "%"), 
         booster_uptake = str_remove(string = booster_uptake, pattern = "%")) |> 
  mutate(dose1_uptake = ifelse(dose1_uptake == ">95", "95", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">95", "95", dose2_uptake), 
         booster_uptake = ifelse(booster_uptake == ">95", "95", booster_uptake)) |> 
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

# SA2-TA-Region concordance
dat_areas <- read_csv(file = here("data/statsnzgeographic-areas-file-2018-CSV/geographic-areas-file-2018.csv"), 
                      col_types = cols(.default = "character")) |> 
  clean_names() |> 
  select(sa22018_code, sa22018_name, 
         ta2018_code, ta2018_name, 
         regc2018_code, regc2018_name) |> 
  distinct()

# *****************************************************************************


# *****************************************************************************
# Combine data for chart ----

dat_chart <- dat_sa2 |> 
  left_join(y = dat_areas, by = c("sa2_code_2018" = "sa22018_code")) |> 
  filter(regc2018_name != "Area Outside Region") |> 
  filter(pop_cnt != "masked") |> 
  mutate(pop_cnt = as.integer(pop_cnt)) |> 
  filter(pop_cnt > 49) |> 
  mutate(regc2018_name = str_remove(string = regc2018_name, 
                                    pattern = " Region")) |> 
  mutate(regc2018_name = factor(x = regc2018_name, 
                                levels = c("Northland", 
                                           "Auckland", 
                                           "Waikato", 
                                           "Bay of Plenty", 
                                           "Gisborne", 
                                           "Hawke's Bay", 
                                           "Taranaki", 
                                           "Manawatu-Wanganui", 
                                           "Wellington", 
                                           "Tasman", 
                                           "Nelson", 
                                           "Marlborough", 
                                           "West Coast", 
                                           "Canterbury", 
                                           "Otago", 
                                           "Southland"),
                                labels = c("Northland", 
                                           "Auckland", 
                                           "Waikato", 
                                           "Bay of Plenty", 
                                           "Gisborne", 
                                           "Hawke's Bay", 
                                           "Taranaki", 
                                           "Manawatū-Whanganui", 
                                           "Wellington", 
                                           "Tasman", 
                                           "Nelson", 
                                           "Marlborough", 
                                           "West Coast", 
                                           "Canterbury", 
                                           "Otago", 
                                           "Southland"),
                                ordered = TRUE))

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# First doses
chart_first_doses <- dat_chart |> 
  ggplot(mapping = aes(x = dose1_uptake / 100, 
                       y = 0, 
                       fill = first_dose_category)) + 
  geom_vline(xintercept = c(0.7, 0.8, 0.9), 
             colour = grey(0.5), 
             size = 0.25) + 
  geom_beeswarm(priority = "random", 
                groupOnX = FALSE, 
                size = 0.8, 
                shape = 21, 
                colour = grey(0.75), 
                stroke = 0.2) + 
  geom_text(x = 0.705,  
            y = 1.9,
            label = "70%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +   
  geom_text(x = 0.805,  
            y = 1.9,
            label = "80%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  geom_text(x = 0.905,  
            y = 1.9,
            label = "90%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  # Used geom_text because annotate seems to interfere with geom_beeswarm
  facet_wrap(facets = vars(regc2018_name),
             ncol = 4) + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     expand = expansion(0, 0), 
                     limits = c(0.7, 0.99)) + 
  scale_fill_manual(values = c("Greater than 90%" = lighten(col = "red", amount = 0.9),
                               "80% to 90%" = lighten(col = "red", amount = 0.5),
                               "70% to 80%" = lighten(col = "red", amount = 0.1),
                               "Less than 70%" = darken(col = "red", amount = 0.25)),
                    name = NULL) + 
  guides(fill = guide_legend(override.aes = list(size = 2, 
                                                 stroke = 0.35), 
                             reverse = TRUE)) + 
  xlab("") + 
  ylab("") + 
  labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/") + 
  ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
          subtitle = glue("People who have received at least one dose to {latest_date_nice} (percent of age 12+ population)")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_line(size = 0.25), 
        panel.spacing.y = unit(16, "pt"), 
        panel.spacing.x = unit(16, "pt"), 
        legend.position = "top", 
        legend.box.margin = margin(0, 0, 0, 0, "pt"), 
        legend.margin = margin(0, 0, 0, 0, "pt"), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = -0.04), 
        plot.margin = margin(4, 4, 4, 4, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 8, 0, "pt")), 
        plot.tag.position = c(0.99, 0.985), 
        plot.tag = element_text(hjust = 1, size = rel(0.6)))

ggsave(filename = here(glue("outputs/first_doses_SA2_{latest_date}.png")), 
       plot = chart_first_doses, 
       device = agg_png, 
       width = 2600, 
       height = 1800, 
       units = "px", 
       bg = "white")

# Second doses
chart_fully_vax <- dat_chart |> 
  ggplot(mapping = aes(x = dose2_uptake / 100, 
                       y = 0, 
                       fill = fully_vax_category)) + 
  geom_vline(xintercept = c(0.7, 0.8, 0.9), 
             colour = grey(0.5), 
             size = 0.25) + 
  geom_beeswarm(priority = "random", 
                groupOnX = FALSE, 
                size = 0.8, 
                shape = 21, 
                colour = grey(0.75), 
                stroke = 0.2) + 
  geom_text(x = 0.705,  
            y = 1.8,
            label = "70%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +   
  geom_text(x = 0.805,  
            y = 1.8,
            label = "80%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  geom_text(x = 0.905,  
            y = 1.8,
            label = "90%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  # Used geom_text because annotate seems to interfere with geom_beeswarm
  facet_wrap(facets = vars(regc2018_name),
             ncol = 4) + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     expand = expansion(0, 0), 
                     limits = c(0.7, 0.99)) + 
  scale_fill_manual(values = c("Greater than 90%" = lighten(col = "red", amount = 0.9),
                               "80% to 90%" = lighten(col = "red", amount = 0.5),
                               "70% to 80%" = lighten(col = "red", amount = 0.1),
                               "Less than 70%" = darken(col = "red", amount = 0.25)),
                    name = NULL) + 
  guides(fill = guide_legend(override.aes = list(size = 2, 
                                                 stroke = 0.35), 
                             reverse = TRUE)) + 
  xlab("") + 
  ylab("") + 
  labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/") + 
  ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
          subtitle = glue("People who have received two doses to {latest_date_nice} (percent of age 12+ population)")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_line(size = 0.25), 
        panel.spacing.y = unit(16, "pt"), 
        panel.spacing.x = unit(16, "pt"), 
        legend.position = "top", 
        legend.box.margin = margin(0, 0, 0, 0, "pt"), 
        legend.margin = margin(0, 0, 0, 0, "pt"), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = -0.04), 
        plot.margin = margin(4, 4, 4, 4, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 8, 0, "pt")), 
        plot.tag.position = c(0.99, 0.985), 
        plot.tag = element_text(hjust = 1, size = rel(0.6)))

ggsave(filename = here(glue("outputs/fully_vax_SA2_{latest_date}.png")), 
       plot = chart_fully_vax, 
       device = agg_png, 
       width = 2600, 
       height = 1800, 
       units = "px", 
       bg = "white")

# Boosters
chart_booster <- dat_chart |> 
  ggplot(mapping = aes(x = booster_uptake / 100, 
                       y = 0, 
                       fill = booster_category)) + 
  geom_vline(xintercept = c(0.7, 0.8, 0.9), 
             colour = grey(0.5), 
             size = 0.25) + 
  geom_beeswarm(priority = "random", 
                groupOnX = FALSE, 
                size = 0.8, 
                shape = 21, 
                colour = grey(0.75), 
                stroke = 0.2) + 
  geom_text(x = 0.705,  
            y = 0.15,
            label = "70%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +   
  geom_text(x = 0.805,  
            y = 0.15,
            label = "80%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  geom_text(x = 0.905,  
            y = 0.15,
            label = "90%",
            hjust = 0, 
            family = "Fira Sans Custom", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  # Used geom_text because annotate seems to interfere with geom_beeswarm
  facet_wrap(facets = vars(regc2018_name),
             ncol = 4) + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     expand = expansion(0, 0), 
                     limits = c(0.2, 0.99)) + 
  scale_fill_manual(values = c("Greater than 90%" = lighten(col = "red", amount = 0.9),
                               "80% to 90%" = lighten(col = "red", amount = 0.5),
                               "70% to 80%" = lighten(col = "red", amount = 0.1),
                               "Less than 70%" = darken(col = "red", amount = 0.25)),
                    name = NULL) + 
  guides(fill = guide_legend(override.aes = list(size = 2, 
                                                 stroke = 0.35), 
                             reverse = TRUE)) + 
  xlab("") + 
  ylab("") + 
  labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/") + 
  ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
          subtitle = glue("People who have received a booster dose to {latest_date_nice} (percent of population eligible to receive a booster)")) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_line(size = 0.25), 
        panel.spacing.y = unit(16, "pt"), 
        panel.spacing.x = unit(16, "pt"), 
        legend.position = "top", 
        legend.box.margin = margin(0, 0, 0, 0, "pt"), 
        legend.margin = margin(0, 0, 0, 0, "pt"), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = -0.04), 
        plot.margin = margin(4, 4, 4, 4, "pt"), 
        plot.title = element_text(size = rel(1.1), 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.9), 
                                     face = "bold", 
                                     margin = margin(0, 0, 8, 0, "pt")), 
        plot.tag.position = c(0.99, 0.985), 
        plot.tag = element_text(hjust = 1, size = rel(0.6)))

ggsave(filename = here(glue("outputs/booster_SA2_{latest_date}.png")), 
       plot = chart_booster, 
       device = agg_png, 
       width = 2600, 
       height = 1800, 
       units = "px", 
       bg = "white")

# Mega beeswarm for all SA2 together
# No longer updated
# chart_megaswarm_first_doses <- dat_chart |> 
#   ggplot(mapping = aes(x = dose1_uptake / 1000, 
#                        y = 0, 
#                        fill = first_dose_category)) + 
#   geom_vline(xintercept = c(0.7, 0.8, 0.9), 
#              colour = grey(0.5), 
#              size = 0.25) + 
#   geom_beeswarm(priority = "random", 
#                 groupOnX = FALSE, 
#                 size =  1, 
#                 shape = 21, 
#                 colour = grey(0.75), 
#                 stroke = 0.2) + 
#   geom_text(x = 0.705,  
#             y = 4.5,
#             label = "70%",
#             hjust = 0, 
#             family = "Fira Sans Custom", 
#             fontface = "bold", 
#             size = 3, 
#             colour = grey(0.5)) +   
#   geom_text(x = 0.805,  
#             y = 4.5,
#             label = "80%",
#             hjust = 0, 
#             family = "Fira Sans Custom", 
#             fontface = "bold", 
#             size = 3, 
#             colour = grey(0.5)) +
#   geom_text(x = 0.905,  
#             y = 4.5,
#             label = "90%",
#             hjust = 0, 
#             family = "Fira Sans Custom", 
#             fontface = "bold", 
#             size = 3, 
#             colour = grey(0.5)) +
#   scale_x_continuous(breaks = seq(0, 1, 0.1), 
#                      expand = expansion(0, 0), 
#                      limits = c(0.6, 0.99)) + 
#   scale_fill_manual(values = c("Greater than 90% first doses" = lighten(col = "red", amount = 0.9),
#                                "80% to 90% first doses" = lighten(col = "red", amount = 0.5),
#                                "70% to 80% first doses" = lighten(col = "red", amount = 0.1),
#                                "Less than 70% first doses" = darken(col = "red", amount = 0.25)),
#                     name = NULL) + 
#   guides(fill = guide_legend(override.aes = list(size = 2, 
#                                                  stroke = 0.35), 
#                              reverse = TRUE)) + 
#   xlab("") + 
#   ylab("") + 
#   labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/") + 
#   ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
#           subtitle = glue("First doses to {latest_date_nice} (percent of age 12+ population)")) + 
#   theme_minimal(base_family = "Fira Sans Custom") + 
#   theme(panel.grid.major.y = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.grid.major.x = element_line(size = 0.25), 
#         legend.position = "top", 
#         legend.box.margin = margin(0, 0, 0, 0, "pt"), 
#         legend.margin = margin(0, 0, 0, 0, "pt"), 
#         axis.text.y = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         plot.margin = margin(4, 4, 4, 4, "pt"), 
#         plot.title = element_text(size = rel(1.1), 
#                                   margin = margin(0, 0, 4, 0, "pt")), 
#         plot.subtitle = element_text(size = rel(0.9), 
#                                      face = "bold", 
#                                      margin = margin(0, 0, 8, 0, "pt")), 
#         plot.tag.position = c(0.99, 0.985), 
#         plot.tag = element_text(hjust = 1, size = rel(0.6)))
# 
# ggsave(filename = here(glue("outputs/first_doses_SA2_megaswarm_{latest_date}.png")), 
#        plot = chart_megaswarm_first_doses, 
#        device = agg_png, 
#        width = 2600, 
#        height = 1600, 
#        units = "px", 
#        bg = "white")

# chart_megaswarm_fully_vax <- dat_chart |> 
#   ggplot(mapping = aes(x = dose2_uptake / 1000, 
#                        y = 0, 
#                        fill = fully_vax_category)) + 
#   geom_vline(xintercept = c(0.7, 0.8, 0.9), 
#              colour = grey(0.5), 
#              size = 0.25) + 
#   geom_beeswarm(priority = "random", 
#                 groupOnX = FALSE, 
#                 size = 1, 
#                 shape = 21, 
#                 colour = grey(0.75), 
#                 stroke = 0.2) + 
#   geom_text(x = 0.705,  
#             y = 2.5,
#             label = "70%",
#             hjust = 0, 
#             family = "Fira Sans Custom", 
#             fontface = "bold", 
#             size = 3, 
#             colour = grey(0.5)) +   
#   geom_text(x = 0.805,  
#             y = 2.5,
#             label = "80%",
#             hjust = 0, 
#             family = "Fira Sans Custom", 
#             fontface = "bold", 
#             size = 3, 
#             colour = grey(0.5)) +
#   geom_text(x = 0.905,  
#             y = 2.5,
#             label = "90%",
#             hjust = 0, 
#             family = "Fira Sans Custom", 
#             fontface = "bold", 
#             size = 3, 
#             colour = grey(0.5)) +
#   # Used geom_text because annotate seems to interfere with geom_beeswarm
#   scale_x_continuous(breaks = seq(0, 1, 0.1), 
#                      expand = expansion(0, 0), 
#                      limits = c(0.5, 0.99)) + 
#   scale_fill_manual(values = c("Greater than 90% fully vaccinated" = lighten(col = "red", amount = 0.9),
#                                "80% to 90% fully vaccinated" = lighten(col = "red", amount = 0.5),
#                                "70% to 80% fully vaccinated" = lighten(col = "red", amount = 0.1),
#                                "Less than 70% fully vaccinated" = darken(col = "red", amount = 0.25)),
#                     name = NULL) + 
#   guides(fill = guide_legend(override.aes = list(size = 2, 
#                                                  stroke = 0.35), 
#                              reverse = TRUE)) + 
#   xlab("") + 
#   ylab("") + 
#   labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\nCC-BY 4.0. schiff.nz/covid/nz-vax/") + 
#   ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
#           subtitle = glue("Fully vaccinated (two doses) to {latest_date_nice} (percent of age 12+ population)")) + 
#   theme_minimal(base_family = "Fira Sans Custom") + 
#   theme(panel.grid.major.y = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.grid.major.x = element_line(size = 0.25), 
#         legend.position = "top", 
#         legend.box.margin = margin(0, 0, 0, 0, "pt"), 
#         legend.margin = margin(0, 0, 0, 0, "pt"), 
#         axis.text.y = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         plot.margin = margin(4, 4, 4, 4, "pt"), 
#         plot.title = element_text(size = rel(1.1), 
#                                   margin = margin(0, 0, 4, 0, "pt")), 
#         plot.subtitle = element_text(size = rel(0.9), 
#                                      face = "bold", 
#                                      margin = margin(0, 0, 8, 0, "pt")), 
#         plot.tag.position = c(0.99, 0.985), 
#         plot.tag = element_text(hjust = 1, size = rel(0.6)))
# 
# ggsave(filename = here(glue("outputs/fully_vax_SA2_megaswarm_{latest_date}.png")), 
#        plot = chart_megaswarm_fully_vax, 
#        device = agg_png, 
#        width = 2600, 
#        height = 1600, 
#        units = "px", 
#        bg = "white")


# *****************************************************************************

