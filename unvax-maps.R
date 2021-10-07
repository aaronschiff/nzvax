# Maps of absolute numbers of unvaccinated people for a specific area


# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(glue)
library(sf)
library(ggrepel)

latest_date <- "20211006"               # Date of most recent week's data

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Vaccination uptake by SA2
dat_vax_sa2 <- read_csv(file = here(glue("data/uptake_sa2_dhb_{latest_date_sa2}.csv"))) |> 
  mutate(dose1_cnt = as.integer(dose1_cnt), 
         dose2_cnt = as.integer(dose2_cnt), 
         pop_cnt = as.integer(pop_cnt)) |> 
  mutate(dose1_uptake = ifelse(dose1_uptake == ">950", "950", dose1_uptake), 
         dose2_uptake = ifelse(dose2_uptake == ">950", "950", dose2_uptake)) |> 
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
dat_map_sa2 <- dat_areas |> 
  filter(ta2020_name == "Kaipara District") |> 
  left_join(y = dat_sa2, by = c("sa22020_code" = "sa22020_v1")) |> 
  st_as_sf()

# SA2 centroids and vax data
dat_map_centroids <- bind_cols(
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

map <- ggplot() + 
  geom_sf(data = dat_map, fill = grey(0.85), colour = grey(0.7)) + 
  geom_point(data = dat_map_centroids, 
             mapping = aes(x = X, y = Y), 
             colour = "red") + 
  geom_text_repel(data = dat_map_centroids, 
                  mapping = aes(x = X, y = Y, label = unvax_pop), 
                  colour = "red", 
                  fontface = "bold", 
                  size = 5) + 
  theme_map()

map
# *****************************************************************************