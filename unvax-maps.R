# Maps of absolute numbers of unvaccinated people for a specific area


# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(glue)
library(sf)
library(ggrepel)
library(ggthemes)

latest_date <- "20211013"               # Date of most recent week's data

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Vaccination uptake by SA2
dat_vax_sa2 <- read_csv(file = here(glue("data/uptake_sa2_dhb_{latest_date}.csv"))) |> 
  mutate(dose1_cnt = as.integer(dose1_cnt), 
         dose2_cnt = as.integer(dose2_cnt), 
         pop_cnt = as.integer(pop_cnt)) |> 
  mutate(dose1_uptake = ifelse(dose1_uptake == ">950", "950", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">950", "950", dose2_uptake), 
         dose1_uptake = ifelse(dose1_uptake == "<50", "50", dose1_uptake),
         dose2_uptake = ifelse(dose2_uptake == "<50", "50", dose2_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake), 
         dose2_uptake = as.integer(dose2_uptake)) |> 
  mutate(unvax_pop = pop_cnt - dose1_cnt) |> 
  mutate(unvax_pop = ifelse(unvax_pop < 0, 0, unvax_pop))

# SA2 shapes
dat_sa2 <- read_sf(dsn = here("data/statsnzstatistical-area-2-2020-clipped-generalised-SHP/statistical-area-2-2020-clipped-generalised.shp")) |> 
  clean_names()

# SA2-TA-LB-REGC concordance
dat_areas <- read_csv(file = here("data/statsnzgeographic-areas-file-2020-CSV/geographic-areas-file-2020.csv"), 
                      col_types = cols(.default = "character")) |> 
  clean_names() |> 
  select(sa22020_code, sa22020_name, 
         cb2020_code, cb2020_name, 
         regc2020_code, regc2020_name, 
         ta2020_code, ta2020_name) |> 
  distinct()

# *****************************************************************************


# *****************************************************************************
# Unvaccinated map for area ----

# SA2 shapes
dat_unvax_map_sa2 <- dat_areas |> 
  filter(ta2020_name == "Kaipara District") |> 
  left_join(y = dat_sa2, by = c("sa22020_code" = "sa22020_v1")) |> 
  st_as_sf()

# SA2 centroids and vax data
dat_unvax_map_centroids <- bind_cols(
  # Vax data
  dat_areas |> 
    filter(ta2020_name == "Kaipara District") |> 
    left_join(y = dat_vax_sa2, by = c("sa22020_code" = "sa2_code")), 
  
  # Centroids
  dat_areas |> 
    filter(ta2020_name == "Kaipara District") |> 
    left_join(y = dat_sa2, by = c("sa22020_code" = "sa22020_v1")) |> 
    st_as_sf() |> 
    st_centroid() |> 
    st_coordinates() |> 
    as_tibble()
)

unvax_map <- ggplot() + 
  geom_sf(data = dat_unvax_map, fill = grey(0.85), colour = grey(0.7)) + 
  geom_point(data = dat_unvax_map_centroids, 
             mapping = aes(x = X, y = Y), 
             colour = "red") + 
  geom_text_repel(data = dat_map_centroids, 
                  mapping = aes(x = X, y = Y, label = unvax_pop), 
                  colour = "red", 
                  fontface = "bold", 
                  size = 5) + 
  theme_map()

# *****************************************************************************


# *****************************************************************************
# Heat map of vaccination rate for an area ----

coastline <- st_union(dat_sa2)

dat_vax_map_sa2 <- dat_areas |> 
  filter(ta2020_name == "Kaipara District") |> 
  left_join(y = dat_vax_sa2, by = c("sa22020_code" = "sa2_code")) |> 
  left_join(y = dat_sa2, by = c("sa22020_code" = "sa22020_v1")) |> 
  st_as_sf() |> 
  filter(!is.na(dose1_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake)) |> 
  mutate(dose1_uptake_category = case_when(
    dose1_uptake > 900 ~ "More than 90% first doses", 
    (dose1_uptake > 800) & (dose1_uptake <= 900) ~ "80% to 90% first doses", 
    (dose1_uptake > 700) & (dose1_uptake <= 800) ~ "70% to 80% first doses", 
    (dose1_uptake >= 600) & (dose1_uptake <= 700) ~ "60% to 70% first doses", 
    (dose1_uptake < 600) ~ "Less than 60% first doses", 
  )) |> 
  mutate(dose1_uptake_category = factor(x = dose1_uptake_category, 
                                        levels = c("More than 90% first doses", 
                                                   "80% to 90% first doses", 
                                                   "70% to 80% first doses", 
                                                   "60% to 70% first doses", 
                                                   "Less than 60% first doses"), 
                                        ordered = TRUE))

map_coastline <- st_crop(x = coastline, y = dat_vax_map_sa2)

map_vax_sa2 <- dat_vax_map_sa2 |> 
  ggplot() + 
  geom_sf(data = map_coastline, 
          size = 0, 
          fill = grey(0.85)) + 
  geom_sf(mapping = aes(fill = dose1_uptake_category), 
          colour = grey(0.1), 
          size = 0.25) + 
  scale_fill_brewer(drop = FALSE, na.value = grey(0.75), 
                    palette = "RdYlBu", 
                    direction = -1, 
                    name = NULL) + 
  ggtitle("Proportion of Kaipara eligible population (age 12+)\nwho have received their first dose as at 6 October 2021") + 
  theme_map(base_family = "Fira Sans") + 
  theme(plot.title = element_text(face = "bold"), 
        legend.text = element_text(size = rel(1)), 
        plot.margin = margin(0, 8, 0, 8, "pt"))

ggsave(filename = here("maps/kaipara-first-doses.png"), 
       plot = map_vax_sa2, 
       device = "png", 
       width = 2000, 
       height = 2000, 
       units = "px", 
       bg = "white")

dat_unvax_map_sa2 <- dat_areas |> 
  filter(ta2020_name == "Kaipara District") |> 
  left_join(y = dat_vax_sa2, by = c("sa22020_code" = "sa2_code")) |> 
  left_join(y = dat_sa2, by = c("sa22020_code" = "sa22020_v1")) |> 
  st_as_sf() |> 
  filter(!is.na(dose1_uptake)) |> 
  mutate(dose1_uptake = as.integer(dose1_uptake)) |> 
  mutate(unvax = 1000 - dose1_uptake) |> 
  mutate(unvax_category = case_when(
    unvax < 100 ~ "Less than 10% unvaccinated", 
    (unvax >= 100) & (unvax < 200) ~ "10% to 20% unvaccinated", 
    (unvax >= 200) & (unvax < 300) ~ "20% to 30% unvaccinated", 
    (unvax >= 300) & (unvax < 400) ~ "30% to 40% unvaccinated", 
    (unvax >= 400) & (unvax < 500) ~ "40% to 50% unvaccinated"
  )) |> 
  mutate(unvax_category = factor(x = unvax_category, 
                                        levels = c("Less than 10% unvaccinated", 
                                                   "10% to 20% unvaccinated", 
                                                   "20% to 30% unvaccinated", 
                                                   "30% to 40% unvaccinated", 
                                                   "40% to 50% unvaccinated"), 
                                        ordered = TRUE))

map_coastline <- st_crop(x = coastline, y = dat_unvax_map_sa2)

map_unvax_sa2 <- dat_unvax_map_sa2 |> 
  ggplot() + 
  geom_sf(data = map_coastline, 
          size = 0, 
          fill = grey(0.85)) + 
  geom_sf(mapping = aes(fill = unvax_category), 
          colour = grey(0.1), 
          size = 0.25) + 
  scale_fill_brewer(drop = FALSE, na.value = grey(0.75), 
                    palette = "RdYlBu", 
                    direction = -1, 
                    name = NULL) + 
  ggtitle("Proportion of Kaipara eligible population (age 12+)\nwho have not received at least their first dose as at 6 October 2021") + 
  theme_map(base_family = "Fira Sans") + 
  theme(plot.title = element_text(face = "bold"), 
        legend.text = element_text(size = rel(1)), 
        plot.margin = margin(0, 8, 0, 8, "pt"))

ggsave(filename = here(glue("maps/kaipara-unvaccinated-{latest_date}.png")), 
       plot = map_unvax_sa2, 
       device = "png", 
       width = 2000, 
       height = 2000, 
       units = "px", 
       bg = "white")

# *****************************************************************************