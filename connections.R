# Attempted visualisation of ACN connections

# Plot is unintelligible. Need to aggregate by local board area.
# Need to use thickness of lines to show thickness of connections.
# Show impact of schools closed, certain ANZSIC work at home. 



# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(fst)
library(janitor)
library(dtplyr)
library(sf)

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Full ACN data
dat_acn <- read_fst(path = here("data/symmetrical_network.fst")) |> 
  as_tibble() |> 
  clean_names() |>
  mutate(from_sa2_node = as.character(from_sa2_node), 
         to_sa2_node = as.character(to_sa2_node))

# Unique ACN connections between SA2
dat_conn <- dat_acn |> 
  lazy_dt() |> 
  select(from_sa2_node, to_sa2_node, all_connections) |> 
  filter(from_sa2_node != to_sa2_node) |> 
  mutate(pair = paste(pmax(from_sa2_node, to_sa2_node), 
                      pmin(from_sa2_node, to_sa2_node), 
                      sep = "_")) |> 
  group_by(pair) |> 
  slice(1) |> 
  ungroup() |> 
  select(-pair) |> 
  as_tibble() 

# SA2 shapes (2018 SA2)
dat_sa2 <- read_sf(dsn = here("data/statistical-area-2-2018-clipped-generalised/statistical-area-2-2018-clipped-generalised.shp")) |> 
  clean_names()

# SA2 centroids (2018 SA2)
dat_sa2_c <- read_sf(dsn = here("data/statistical-area-2-2018-centroid-inside/statistical-area-2-2018-centroid-inside.shp")) |> 
  clean_names()

# SA2-TA-LB-REGC concordance (2020 geographic areas file but use 2018 SA2s)
dat_areas <- read_csv(file = here("data/statsnzgeographic-areas-file-2020-CSV/geographic-areas-file-2020.csv"), 
                      col_types = cols(.default = "character")) |> 
  clean_names() |> 
  select(sa22018_code, sa22018_name, 
         regc2020_code, regc2020_name) |> 
  distinct()

# *****************************************************************************


akl <- dat_areas |> filter(regc2020_name == "Auckland Region") 

akl_n <- dat_acn |> filter(from_sa2_node %in% akl$sa22018_code, 
                           to_sa2_node %in% akl$sa22018_code) |> 
  left_join(y = dat_sa2_c |> 
              st_drop_geometry() |> 
              select(sa22018_v1, easting, northing) |> 
              rename(from_x = easting, from_y = northing), 
            by = c("from_sa2_node" = "sa22018_v1")) |> 
  left_join(y = dat_sa2_c |> 
              st_drop_geometry() |> 
              select(sa22018_v1, easting, northing) |> 
              rename(to_x = easting, to_y = northing), 
            by = c("to_sa2_node" = "sa22018_v1"))


chart <- akl_n |> 
  ggplot() + 
  geom_segment(mapping = aes(x = from_x, xend = to_x, 
                             y = from_y, yend = to_y),
               size = 0.1, 
               colour = grey(level = 0, alpha = 0.005)) + 
  theme_minimal()

ggsave(filename = here("outputs/akl.png"), 
       plot = chart, 
       device = "png", 
       width = 2000, 
       height = 2000, 
       units = "px", 
       bg = "white")
