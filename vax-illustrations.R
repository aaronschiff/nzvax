# Illustrate vaccination rates across ethnic groups

# *****************************************************************************
# Setup ---- 

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(scales)
library(glue)

latest_date <- "21_09_2021"

# *****************************************************************************


# *****************************************************************************
# Load data ----

dat <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                  sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes)

dat_pop <- read_excel(path = here("data/covid_vaccinations_21_09_2021.xlsx"), 
                      sheet = "HSU Population") |> 
  clean_names() |> 
  select(-x6, -x7, -notes)

dat_pop_5yr <- read_excel(path = here("data/2020-21 Population Projections.xlsx"), 
                          sheet = "DHB20POPPROJS_FINYEAR", 
                          skip = 1) |> 
  clean_names()

# *****************************************************************************


# *****************************************************************************
# Illustrate 90% overall vax rate ----

# Combine population into two groups
dat_90 <- dat |> 
  mutate(ethnic_group_2 = case_when(
    ethnic_group == "Asian" ~ "Pākehā, Asian & all others", 
    ethnic_group == "European or Other" ~ "Pākehā, Asian & all others", 
    ethnic_group == "Maori" ~ "Māori & Pacific", 
    ethnic_group == "Pacific Peoples" ~ "Māori & Pacific", 
    ethnic_group == "Unknown" ~ "Pākehā, Asian & all others", 
    ethnic_group == "Various" ~ "Pākehā, Asian & all others"
  )) |> 
  group_by(ethnic_group_2) |> 
  summarise(population = sum(population)) |> 
  ungroup() 

# Calculate combinations of vax rates that give 90% overall
c90 <- tibble(Ve = c(0.9, 1.0)) |> 
  mutate(Vmp = 0.9 + (0.9 - Ve) * (3353767/855290)) |> 
  bind_rows(
    tibble(Ve = c(0.9, 1.0), 
           Vmp = c(1.0, 1.0))
  ) |> 
  arrange(Vmp, Ve) 

# Create chart
chart_c90 <- c90 |> 
  ggplot(mapping = aes(x = Ve, y = Vmp)) + 
  geom_polygon(fill = "cornflowerblue") + 
  annotate(geom = "text", 
           x = 0.96, y = 0.85, 
           label = "90% or more vaccinated overall", 
           colour = "white", 
           size = 3.5, 
           fontface = "bold") + 
  scale_x_continuous(limits = c(0.9, 1),
                     breaks = seq(0.9, 1, 0.01),
                     labels = percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0.5, 1),
                     breaks = seq(0.5, 1.0, 0.05),
                     labels = percent_format(accuracy = 1)) +
  xlab("Vaccination rate for Pākehā, Asian & all other ethnicities") + 
  ylab("Vaccination rate\nfor Māori & Pacific") + 
  theme_minimal(base_size = 10) + 
  theme(panel.grid.minor = element_blank(), 
        axis.title.y = element_text(angle = 0, 
                                    face = "bold", 
                                    margin = margin(0, 6, 0, 0, "pt")), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(8, 0, 0, 0, "pt")))

ggsave(plot = chart_c90, 
       filename = here("outputs/chart_c90.png"), 
       device = "png", 
       width = 16, 
       height = 12, 
       units = "cm", 
       dpi = 300, 
       bg = "white")

# *****************************************************************************


# *****************************************************************************
# Illustrate impact of different eligiblity ----

# If 12+ population is eligible
dat_pop_elig_12plus <- dat_pop |>
  mutate(elig = ifelse(age_group == "0-11", "no", "yes")) |> 
  group_by(ethnic_group, elig) |> 
  summarise(population = sum(population)) |> 
  ungroup() |>
  complete(ethnic_group, elig, fill = list(population = 0)) |> 
  group_by(ethnic_group) |> 
  mutate(pct = population / sum(population)) |> 
  ungroup() |> 
  filter(elig == "yes", 
         ethnic_group != "Unknown", 
         ethnic_group != "Various") |> 
  crossing(vax = seq(0.7, 1, 0.01)) |> 
  mutate(coverage = pct * vax) |> 
  mutate(uncovered = 1 - coverage) |> 
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("European or Other", 
                                          "Asian", 
                                          "Pacific Peoples", 
                                          "Maori"), 
                               labels = c("Pākehā or other", 
                                          "Asian", 
                                          "Pacific Peoples", 
                                          "Māori"), 
                               ordered = TRUE)) |> 
  mutate(elig = "12plus")

# If 5+ population is eligible
dat_pop_elig_5plus <- dat_pop_5yr |>
  mutate(elig = ifelse(age_group == "00-04", "no", "yes")) |> 
  group_by(ethnicity, elig) |> 
  summarise(population = sum(pop2020_2021)) |> 
  ungroup() |>
  complete(ethnicity, elig, fill = list(population = 0)) |> 
  group_by(ethnicity) |> 
  mutate(pct = population / sum(population)) |> 
  ungroup() |> 
  filter(elig == "yes") |> 
  crossing(vax = seq(0.7, 1, 0.01)) |> 
  mutate(coverage = pct * vax) |> 
  mutate(uncovered = 1 - coverage) |> 
  mutate(ethnicity = factor(x = ethnicity, 
                               levels = c("Other", 
                                          "Asian", 
                                          "Pacific", 
                                          "Maori"), 
                               labels = c("Pākehā or other", 
                                          "Asian", 
                                          "Pacific Peoples", 
                                          "Māori"), 
                               ordered = TRUE)) |> 
  mutate(elig = "5plus") |> 
  rename(ethnic_group = ethnicity)

# Combined 12+ and 5+ 
dat_pop_elig <- bind_rows(
  dat_pop_elig_5plus, 
  dat_pop_elig_12plus
) |> 
  mutate(elig = factor(x = elig, 
                       levels = c("5plus", "12plus"), 
                       labels = c("Age 5+ eligible", 
                                  "Age 12+ eligible"), 
                       ordered = TRUE))

# Create chart
chart_dat_pop_elig <- dat_pop_elig |> 
  ggplot(mapping = aes(x = vax, y = coverage, 
                       colour = ethnic_group, 
                       group = ethnic_group)) + 
  geom_vline(xintercept = 0.9, colour = grey(0.5), size = 0.25) + 
  geom_line(size = 0.5) + 
  scale_x_continuous(breaks = seq(0.7, 1, 0.05), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0.4, 1, 0.05), 
                     limits = c(0.5, 1.0), 
                     labels = percent_format(accuracy = 1)) + 
  scale_colour_brewer(palette = "Set2", name = NULL) +
  facet_wrap(facets = vars(elig), nrow = 1) + 
  xlab("Proportion of eligible population vaccinated") + 
  ylab("Proportion of\ntotal population\nvaccinated") + 
  theme_minimal(base_size = 10) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.25), 
        axis.title.y = element_text(angle = 0, 
                                    face = "bold", 
                                    hjust = 0, 
                                    margin = margin(0, 6, 0, 0, "pt")), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(8, 0, 0, 0, "pt")), 
        panel.spacing.x = unit(12, "pt"), 
        strip.text = element_text(face = "bold"))

ggsave(plot = chart_dat_pop_elig, 
       filename = here("outputs/chart_dat_pop_elig.png"), 
       device = "png", 
       width = 18, 
       height = 12, 
       units = "cm", 
       dpi = 300, 
       bg = "white")

# *****************************************************************************

