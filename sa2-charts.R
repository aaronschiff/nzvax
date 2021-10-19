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

latest_date <- "20211013"
latest_date_nice <- "13 October 2021"

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Load raw vaccination data and manipulate
dat_sa2 <- read_csv(file = here(glue("data/uptake_sa2_dhb_{latest_date}.csv"))) |> 
  clean_names() |> 
  # Exclude unknown SA2
  filter(sa2_code != "Unknown") |> 
  # Replace >950 and <50 with numeric values
  mutate(dose1_uptake = ifelse(dose1_uptake == ">950", "950", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">950", "950", dose2_uptake), 
         dose1_uptake = ifelse(dose1_uptake == "<50", "50", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == "<50", "50", dose2_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake), 
         dose2_uptake = as.integer(dose2_uptake)) |> 
  # Assign uptake categories
  mutate(fully_vax_category = case_when(
    dose2_uptake > 900 ~ "Greater than 90% fully vaccinated", 
    (dose2_uptake > 800) & (dose2_uptake <= 900) ~ "80% to 90% fully vaccinated", 
    (dose2_uptake >= 700) & (dose2_uptake <= 800) ~ "70% to 80% fully vaccinated", 
    TRUE ~ "Less than 70% fully vaccinated"
  )) |> 
  mutate(first_dose_category = case_when(
    dose1_uptake > 900 ~ "Greater than 90% first doses", 
    (dose1_uptake > 800) & (dose1_uptake <= 900) ~ "80% to 90% first doses", 
    (dose1_uptake >= 700) & (dose1_uptake <= 800) ~ "70% to 80% first doses", 
    TRUE ~ "Less than 70% first doses"
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
  left_join(y = dat_areas, by = c("sa2_code" = "sa22018_code")) |> 
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
  ggplot(mapping = aes(x = dose1_uptake / 1000, 
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
            y = 0.2,
            label = "70%",
            hjust = 0, 
            family = "Fira Sans", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +   
  geom_text(x = 0.805,  
            y = 0.2,
            label = "80%",
            hjust = 0, 
            family = "Fira Sans", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  geom_text(x = 0.905,  
            y = 0.2,
            label = "90%",
            hjust = 0, 
            family = "Fira Sans", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  # Used geom_text because annotate seems to interfere with geom_beeswarm
  facet_wrap(facets = vars(regc2018_name),
             ncol = 4) + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     expand = expansion(0, 0), 
                     limits = c(0.3, 0.99)) + 
  scale_fill_manual(values = c("Greater than 90% first doses" = lighten(col = "red", amount = 0.9),
                               "80% to 90% first doses" = lighten(col = "red", amount = 0.5),
                               "70% to 80% first doses" = lighten(col = "red", amount = 0.1),
                               "Less than 70% first doses" = darken(col = "red", amount = 0.25)),
                    name = NULL) + 
  guides(fill = guide_legend(override.aes = list(size = 2, 
                                                 stroke = 0.35), 
                             reverse = TRUE)) + 
  xlab("") + 
  ylab("") + 
  labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\ngithub.com/aaronschiff/nzvax") + 
  ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
          subtitle = glue("First doses to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
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
        strip.text = element_text(face = "bold", hjust = -0.05), 
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
       device = "png", 
       width = 2600, 
       height = 1800, 
       units = "px", 
       bg = "white")

chart_fully_vax <- dat_chart |> 
  ggplot(mapping = aes(x = dose2_uptake / 1000, 
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
            y = 0.18,
            label = "70%",
            hjust = 0, 
            family = "Fira Sans", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +   
  geom_text(x = 0.805,  
            y = 0.18,
            label = "80%",
            hjust = 0, 
            family = "Fira Sans", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  geom_text(x = 0.905,  
            y = 0.18,
            label = "90%",
            hjust = 0, 
            family = "Fira Sans", 
            fontface = "bold", 
            size = 2, 
            colour = grey(0.5)) +
  # Used geom_text because annotate seems to interfere with geom_beeswarm
  facet_wrap(facets = vars(regc2018_name),
             ncol = 4) + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     expand = expansion(0, 0), 
                     limits = c(0, 0.99)) + 
  scale_fill_manual(values = c("Greater than 90% fully vaccinated" = lighten(col = "red", amount = 0.9),
                               "80% to 90% fully vaccinated" = lighten(col = "red", amount = 0.5),
                               "70% to 80% fully vaccinated" = lighten(col = "red", amount = 0.1),
                               "Less than 70% fully vaccinated" = darken(col = "red", amount = 0.25)),
                    name = NULL) + 
  guides(fill = guide_legend(override.aes = list(size = 2, 
                                                 stroke = 0.35), 
                             reverse = TRUE)) + 
  xlab("") + 
  ylab("") + 
  labs(tag = "Chart by Aaron Schiff using data from the NZ Ministry of Health\ngithub.com/aaronschiff/nzvax") + 
  ggtitle(label = "NZ COVID-19 vaccination rates for each suburb (SA2 area)", 
          subtitle = glue("Fully vaccinated (two doses) to {latest_date_nice}")) + 
  theme_minimal(base_family = "Fira Sans") + 
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
        strip.text = element_text(face = "bold", hjust = -0.05), 
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
       device = "png", 
       width = 2600, 
       height = 1800, 
       units = "px", 
       bg = "white")


# *****************************************************************************

