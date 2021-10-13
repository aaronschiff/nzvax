# Simulate contagion

library(tidyverse)
library(here)
library(fst)
library(janitor)
library(sf)

dat_conn <- read_fst(path = here("data/symmetrical_network.fst")) |> 
  clean_names() |> 
  as_tibble() |> 
  mutate(from_sa2_node = as.character(from_sa2_node), 
         to_sa2_node = as.character(to_sa2_node)) |> 
  select(from_sa2_node, to_sa2_node, all_connections) |> 
  group_by(from_sa2_node) |> 
  mutate(prob = all_connections / sum(all_connections)) |> 
  ungroup() 

# SA2 shapes (2018 SA2)
dat_sa2 <- read_sf(dsn = here("data/statistical-area-2-2018-clipped-generalised/statistical-area-2-2018-clipped-generalised.shp")) |> 
  clean_names()

# SA2 centroids (2018 SA2)
dat_sa2_c <- read_sf(dsn = here("data/statistical-area-2-2018-centroid-inside/statistical-area-2-2018-centroid-inside.shp")) |> 
  clean_names()

z <- dat_conn |> 
  select(from_sa2_node) |> 
  distinct() |> 
  mutate(infected = ifelse(from_sa2_node == "133000", 1L, 0L)) |> 
  left_join(y = dat_conn, by = "from_sa2_node") |> 
  print()

