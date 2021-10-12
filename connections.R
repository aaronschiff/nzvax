# Attempted visualisation of ACN connections

# Plot is unintelligible. Need to aggregate by local board area.
# Need to use thickness of lines to show thickness of connections.
# Show impact of schools closed, certain ANZSIC work at home. 



# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(dtplyr)
library(sf)
library(ggthemes)

# *****************************************************************************


# *****************************************************************************
# Load data ----

# ACN connections
dat_acn <- read_csv(file = here("data/asymmetrical_network.csv"), 
                    col_types = "iccciiiiiiiiiiiiiiiiiiiiiiiiiiccc") |> 
  select(-...1) |> 
  clean_names()

# SA2 shapes (2018 SA2)
dat_sa2 <- read_sf(dsn = here("data/statistical-area-2-2018-clipped-generalised/statistical-area-2-2018-clipped-generalised.shp")) |> 
  clean_names()

# SA2 centroids (2018 SA2)
dat_sa2_c <- read_sf(dsn = here("data/statistical-area-2-2018-centroid-inside/statistical-area-2-2018-centroid-inside.shp")) |> 
  clean_names()

dat_sa2_c_xy <- dat_sa2_c |> 
  st_drop_geometry() |> 
  select(sa22018_v1, sa22018_1, x = easting, y = northing)

# SA2-TA-LB-REGC concordance (2018)
dat_areas <- read_csv(file = here("data/statsnzgeographic-areas-file-2018-CSV/geographic-areas-file-2018.csv"), 
                      col_types = cols(.default = "character")) |> 
  clean_names() |> 
  select(sa22018_code, sa22018_name, 
         regc2018_code, regc2018_name, 
         ta2018_code, ta2018_name, 
         cb2018_code, cb2018_name) |> 
  distinct()

# *****************************************************************************


# *****************************************************************************
# Visualise SA2 network within an Auckland local board area

# Local board area to visualise
vis_cb_name <- "Henderson-Massey Local Board Area"

# Selected SA2 in local board area
vis_cb <- dat_sa2 |> 
  left_join(dat_areas, 
            by = c("sa22018_v1" = "sa22018_code")) |> 
  filter(cb2018_name == vis_cb_name) 

# Selected SA2 centroids
vis_cb_c <- dat_sa2_c_xy |> 
  left_join(dat_areas, 
            by = c("sa22018_v1" = "sa22018_code")) |> 
  filter(cb2018_name == vis_cb_name) |> 
  filter(sa22018_1 != "Inlet Waitemata Harbour")

# Auckland region shape within local board area
vis_akl <- dat_sa2 |> 
  left_join(dat_areas, 
            by = c("sa22018_v1" = "sa22018_code")) |> 
  filter(regc2018_name == "Auckland Region") |> 
  st_union() |> 
  st_crop(y = vis_cb)

# Connections for selected local board
vis_conn_lb <- 
  # All "from" SA2 in the local board area
  dat_areas |> 
  filter(cb2018_name == vis_cb_name) |> 
  rename(from_sa2_node = sa22018_code) |> 
  left_join(y = dat_acn, 
            by = "from_sa2_node") |> 
  rename(from_cb = cb2018_name) |> 
  # All "to" SA2 also in the local board area
  left_join(y = dat_areas |> 
              select(to_sa2_node = sa22018_code, 
                     to_cb = cb2018_name), 
            by = "to_sa2_node") |> 
  filter(to_cb == vis_cb_name) |> 
  # To and from coordinates
  left_join(y = dat_sa2_c_xy |> 
              select(sa22018_v1, x, y) |> 
              rename(from_x = x, from_y = y), 
            by = c("from_sa2_node" = "sa22018_v1")) |> 
  left_join(y = dat_sa2_c_xy |> 
              select(sa22018_v1, x, y) |> 
              rename(to_x = x, to_y = y), 
            by = c("to_sa2_node" = "sa22018_v1")) |> 
  arrange(all_connections)

# Map
vis <- ggplot() + 
  # Background Auckland region shape
  geom_sf(data = vis_akl, 
          size = 0, 
          fill = grey(0.1)) + 
  # Selected SA2 shapes
  # geom_sf(data = vis_cb, 
  #         size = 0.2,
  #         colour = grey(0.25), 
  #         fill = grey(0.1)) + 
  # Connections
  geom_segment(data = vis_conn_lb, 
               mapping = aes(x = from_x, xend = to_x, 
                             y = from_y, yend = to_y, 
                             alpha = all_connections), 
               colour = "white", 
               size = 0.25) + 
  # SA2 centroid points
  geom_point(data = vis_cb_c, 
             mapping = aes(x = x, y = y), 
             size = 0.25, 
             colour = "white") + 
  scale_alpha_continuous(range = c(0, 1.0), 
                         limits = c(0, 17000), 
                         guide = "none") + 
  # scale_colour_gradient(low = grey(0.15), high = grey(1.0), 
  #                       guide = "none", 
  #                       limits = c(0, 17000)) + 
  scale_x_continuous(expand = expansion(0, 0)) + 
  scale_y_continuous(expand = expansion(0, 0)) + 
  ggtitle("Schools open") + 
  theme_map() +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"), 
        plot.background = element_rect(fill = rgb(0, 0, 0, maxColorValue = 255), 
                                       size = 0), 
        plot.title = element_text(colour = "white", 
                                  face = "bold", 
                                  margin = margin(8, 0, 0, 52, "pt")))

ggsave(filename = here("outputs/connections_test.png"), 
       plot = vis, 
       device = "png", 
       width = 1200, 
       height = 2000, 
       units = "px", 
       bg = "black")

# Map no schools
vis2 <- ggplot() + 
  # Background Auckland region shape
  geom_sf(data = vis_akl, 
          size = 0, 
          fill = grey(0.1)) + 
  # Selected SA2 shapes
  # geom_sf(data = vis_cb, 
  #         size = 0.2,
  #         colour = grey(0.25), 
  #         fill = grey(0.1)) + 
  # Connections
  geom_segment(data = vis_conn_lb, 
               mapping = aes(x = from_x, xend = to_x, 
                             y = from_y, yend = to_y, 
                             alpha = all_connections - all_schools), 
               colour = "white", 
               size = 0.25) + 
  # SA2 centroid points
  geom_point(data = vis_cb_c, 
             mapping = aes(x = x, y = y), 
             size = 0.25, 
             colour = "white") + 
  scale_alpha_continuous(range = c(0, 1.0), 
                         limits = c(0, 17000), 
                         guide = "none") + 
  # scale_colour_gradient(low = grey(0.15), high = grey(1.0), 
  #                       guide = "none", 
  #                       limits = c(0, 17000)) + 
  scale_x_continuous(expand = expansion(0, 0)) + 
  scale_y_continuous(expand = expansion(0, 0)) + 
  ggtitle("Schools closed") + 
  theme_map() +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"), 
        plot.background = element_rect(fill = rgb(0, 0, 0, maxColorValue = 255), 
                                       size = 0), 
        plot.title = element_text(colour = "white", 
                                  face = "bold", 
                                  margin = margin(8, 0, 0, 52, "pt")))

ggsave(filename = here("outputs/connections_test2.png"), 
       plot = vis2, 
       device = "png", 
       width = 1200, 
       height = 2000, 
       units = "px", 
       bg = "black")



# *****************************************************************************
