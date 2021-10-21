# Attempted visualisation of connections in the Aotearoa Co-incidence Network
# data published by Te Pūnaha Matatini. 
# Data source: https://gitlab.com/tpm-public-projects/aotearoa-connection-network


# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(dtplyr)
library(sf)
library(ggthemes)
library(scales)
library(glue)

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

# X-Y coordinates of SA2 centroids
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

# School locations
dat_schools <- read_csv(file = here("data/school_directory.csv"), 
                        skip = 15) |> 
  clean_names() |> 
  filter(!is.na(school_name), 
         !is.na(latitude), !is.na(longitude)) |> 
  select(school_number, school_name, school_type,
         total_school_roll, decile, 
         town_city, suburb, postal_address_city, 
         latitude, longitude) |> 
  mutate(school_type_grouped = ifelse(school_type %in% c("Secondary (Year 11-15)", 
                                                         "Secondary (Year 7-15)", 
                                                         "Secondary (Year 9-15)"), 
                                      "secondary", "other")) |> 
  st_as_sf(coords = c("longitude", "latitude")) |>
  st_set_crs(value = 4326) |> 
  st_transform(crs = 2193) 

# *****************************************************************************


# *****************************************************************************
# Visualise SA2 network within an Auckland local board area ----
# Show how connections differ with schools open / closed

# Function to visualise connections for a local board area
vis_for_cb <- function(vis_cb_name, 
                       output_width, 
                       output_height, 
                       connections_scale_breaks, 
                       legend_position, 
                       legend_justification) {
  # SA2 in selected local board area
  vis_sa2 <- dat_sa2 |> 
    left_join(dat_areas, 
              by = c("sa22018_v1" = "sa22018_code")) |> 
    filter(cb2018_name == vis_cb_name) |> 
    filter(!str_detect(string = sa22018_1, pattern = "Inlet"), 
           !str_detect(string = sa22018_1, pattern = "Oceanic"))
  
  # Selected SA2 centroids coordinates
  vis_sa2_c_xy <- dat_sa2_c_xy |> 
    left_join(dat_areas, 
              by = c("sa22018_v1" = "sa22018_code")) |> 
    filter(cb2018_name == vis_cb_name) |> 
    filter(!str_detect(string = sa22018_1, pattern = "Inlet"), 
           !str_detect(string = sa22018_1, pattern = "Oceanic"))
  
  # Auckland region shape within local board area (for background)
  vis_akl <- dat_sa2 |> 
    left_join(dat_areas, 
              by = c("sa22018_v1" = "sa22018_code")) |> 
    filter(regc2018_name == "Auckland Region") |> 
    st_union() |> 
    st_crop(y = vis_sa2)
  
  # ACN connections for selected local board area - only connections where
  # the 'from' and 'to' SA2 are both in the local board area are included 
  # and where the 'from' and 'to' SA2 are not the same
  vis_conn <- 
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
    # Tidy up
    select(from_sa2_node, to_sa2_node, 
           from_x, from_y, to_x, to_y, 
           all_schools, starts_with("schools_"), 
           all_work_sectors, 
           all_connections) |> 
    filter(from_sa2_node != to_sa2_node)
  
  cat("Range of connections: ", range(vis_conn$all_connections))
  
  vis_conn_max <- 1000 * ceiling(max(vis_conn$all_connections) / 1000)
  
  # Visualise all connections
  vis_all <- ggplot() + 
    # Background Auckland region shape
    geom_sf(data = vis_akl, 
            size = 0, 
            fill = grey(0.15)) + 
    # Selected SA2 shapes
    geom_sf(data = vis_sa2,
            size = 0.2,
            colour = grey(0.3),
            fill = grey(0.15)) +
    # Connections between SA2
    geom_segment(data = vis_conn |> 
                   filter(all_connections > 99) |> 
                   arrange(all_connections), 
                 mapping = aes(x = from_x, xend = to_x, 
                               y = from_y, yend = to_y, 
                               colour = all_connections, 
                               size = all_connections, 
                               alpha = all_connections)) + 
    # SA2 centroid points
    geom_point(data = vis_sa2_c_xy, 
               mapping = aes(x = x, y = y), 
               size = 1, 
               colour = grey(0.75)) + 
    scale_colour_gradient(low = "firebrick",
                          high = "yellow",
                          limits = c(0, vis_conn_max), 
                          breaks = connections_scale_breaks, 
                          labels = comma_format(accuracy = 1), 
                          name = "Number of connections") +
    scale_size_continuous(limits = c(0, vis_conn_max), 
                          range = c(0, 1.5), 
                          breaks = connections_scale_breaks,
                          labels = comma_format(accuracy = 1), 
                          name = "Number of connections") + 
    scale_alpha_continuous(limits = c(0, vis_conn_max), 
                           range = c(0.4, 0.9), 
                           breaks = connections_scale_breaks,
                           labels = comma_format(accuracy = 1), 
                           name = "Number of connections") + 
    scale_x_continuous(expand = expansion(0, 0)) + 
    scale_y_continuous(expand = expansion(0, 0)) + 
    guides(colour = guide_legend(), 
           size = guide_legend(), 
           alpha = guide_legend()) + 
    ggtitle("All workplace and school connections", 
            subtitle = vis_cb_name) + 
    theme_map(base_family = "Fira Sans", 
              base_size = 11) +
    theme(plot.margin = margin(2, 2, 2, 2, "pt"), 
          plot.background = element_rect(fill = rgb(0, 0, 0, maxColorValue = 255), 
                                         size = 0), 
          plot.title = element_text(colour = "white", 
                                    margin = margin(8, 0, 4, 0, "pt")), 
          plot.subtitle = element_text(colour = "white", 
                                       face = "bold"), 
          legend.background = element_rect(fill = grey(0.15)), 
          legend.box.background = element_rect(fill = grey(0.15), 
                                               size = 1), 
          legend.key = element_rect(fill = grey(0.15)), 
          legend.title = element_text(colour = "white", face = "bold"), 
          legend.text = element_text(colour = "white"), 
          legend.position = legend_position, 
          legend.justification = legend_justification)
  
  ggsave(filename = here(glue("outputs/connections/connections_all_{vis_cb_name}.png")), 
         plot = vis_all, 
         device = "png", 
         width = output_width, 
         height = output_height,
         units = "px", 
         bg = "black")
  
  # Visualise only workplace connections
  vis_workplace <- ggplot() + 
    # Background Auckland region shape
    geom_sf(data = vis_akl, 
            size = 0, 
            fill = grey(0.15)) + 
    # Selected SA2 shapes
    geom_sf(data = vis_sa2,
            size = 0.2,
            colour = grey(0.3),
            fill = grey(0.15)) +
    # Connections between SA2
    geom_segment(data = vis_conn |> 
                   filter(all_work_sectors > 99) |> 
                   arrange(all_work_sectors), 
                 mapping = aes(x = from_x, xend = to_x, 
                               y = from_y, yend = to_y, 
                               colour = all_work_sectors, 
                               size = all_work_sectors, 
                               alpha = all_work_sectors)) + 
    # SA2 centroid points
    geom_point(data = vis_sa2_c_xy, 
               mapping = aes(x = x, y = y), 
               size = 1, 
               colour = grey(0.75)) + 
    scale_colour_gradient(low = "firebrick",
                          high = "yellow",
                          limits = c(0, vis_conn_max), 
                          breaks = connections_scale_breaks, 
                          labels = comma_format(accuracy = 1), 
                          name = "Number of connections") +
    scale_size_continuous(limits = c(0, vis_conn_max), 
                          range = c(0, 1.5), 
                          breaks = connections_scale_breaks,
                          labels = comma_format(accuracy = 1), 
                          name = "Number of connections") + 
    scale_alpha_continuous(limits = c(0, vis_conn_max), 
                           range = c(0.4, 0.9), 
                           breaks = connections_scale_breaks,
                           labels = comma_format(accuracy = 1), 
                           name = "Number of connections") + 
    scale_x_continuous(expand = expansion(0, 0)) + 
    scale_y_continuous(expand = expansion(0, 0)) + 
    guides(colour = guide_legend(), 
           size = guide_legend(), 
           alpha = guide_legend()) + 
    ggtitle("Only workplace connections", 
            subtitle = vis_cb_name) + 
    theme_map(base_family = "Fira Sans", 
              base_size = 11) +
    theme(plot.margin = margin(2, 2, 2, 2, "pt"), 
          plot.background = element_rect(fill = rgb(0, 0, 0, maxColorValue = 255), 
                                         size = 0), 
          plot.title = element_text(colour = "white", 
                                    margin = margin(8, 0, 4, 0, "pt")), 
          plot.subtitle = element_text(colour = "white", 
                                       face = "bold"), 
          legend.background = element_rect(fill = grey(0.15)), 
          legend.box.background = element_rect(fill = grey(0.15), 
                                               size = 1), 
          legend.key = element_rect(fill = grey(0.15)), 
          legend.title = element_text(colour = "white", face = "bold"), 
          legend.text = element_text(colour = "white"), 
          legend.position = legend_position, 
          legend.justification = legend_justification)
  
  ggsave(filename = here(glue("outputs/connections/connections_workplace_{vis_cb_name}.png")), 
         plot = vis_workplace, 
         device = "png", 
         width = output_width, 
         height = output_height, 
         units = "px", 
         bg = "black")
}

vis_for_cb(vis_cb_name = "Manurewa Local Board Area", 
           connections_scale_breaks = c(1000, 5000, 10000, 15000), 
           legend_position = c(1, 0),
           legend_justification = c(1, 0), 
           output_width = 1860,
           output_height = 1800)

vis_for_cb(vis_cb_name = "Henderson-Massey Local Board Area", 
           connections_scale_breaks = c(1000, 5000, 10000, 15000), 
           legend_position = c(0, 0), 
           legend_justification = c(0, 0), 
           output_width = 1350,
           output_height = 2400)

vis_for_cb(vis_cb_name = "Albert-Eden Local Board Area", 
           connections_scale_breaks = c(1000, 5000, 10000, 15000, 20000, 25000), 
           legend_position = c(1, 1),
           legend_justification = c(1, 1), 
           output_width = 2400,
           output_height = 1680)

vis_for_cb(vis_cb_name = "Devonport-Takapuna Local Board Area", 
           connections_scale_breaks = c(1000, 5000, 10000, 15000, 20000, 25000), 
           legend_position = c(0, 0), 
           legend_justification = c(0, 0), 
           output_width = 1600,
           output_height = 2300)


# *****************************************************************************

