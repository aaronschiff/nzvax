m <- dat |> 
  # Filter Māori and exclude unknown DHB
  filter(ethnic_group == "Maori", 
         dhb_of_residence != "Overseas / Unknown") |> 
  # Assign custom age groups
  mutate(age_group_2 = case_when(
    age_group == "12-15" ~ "12-29", 
    age_group == "16-19" ~ "12-29", 
    age_group == "20-24" ~ "12-29", 
    age_group == "25-29" ~ "12-29", 
    age_group == "30-34" ~ "30-59", 
    age_group == "35-39" ~ "30-59", 
    age_group == "40-44" ~ "30-59", 
    age_group == "45-49" ~ "30-59", 
    age_group == "50-54" ~ "30-59", 
    age_group == "55-59" ~ "30-59", 
    age_group == "60-64" ~ "60+", 
    age_group == "65-69" ~ "60+", 
    age_group == "70-74" ~ "60+", 
    age_group == "75-79" ~ "60+", 
    age_group == "80-84" ~ "60+", 
    age_group == "85-89" ~ "60+",
    age_group == "90+" ~ "60+"
  )) |> 
  # Summarise population and first doses by DHB and age group
  group_by(dhb_of_residence, age_group_2) |> 
  summarise(population = sum(population), 
            first_dose_administered = sum(first_dose_administered)) |> 
  ungroup() |> 
  # Calculate unvaccinated number in each group
  mutate(unvax = pmax(population - first_dose_administered, 0)) |>
  select(-population, -first_dose_administered) |> 
  # Expand data into individual people
  uncount(weights = unvax) |> 
  # DHB and age group factors for ordering
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("12-29 years", 
                                         "30-59 years", 
                                         "60+ years"), 
                              ordered = TRUE)) |> 
  mutate(dhb_of_residence = factor(x = dhb_of_residence, 
                                   levels = c("Northland", 
                                              "Auckland Metro", 
                                              "Waikato", 
                                              "Bay of Plenty", 
                                              "Taranaki", 
                                              "Lakes", 
                                              "Tairawhiti", 
                                              "Whanganui", 
                                              "MidCentral", 
                                              "Hawkes Bay", 
                                              "Capital & Coast and Hutt Valley", 
                                              "Wairarapa", 
                                              "Nelson Marlborough", 
                                              "West Coast", 
                                              "Canterbury", 
                                              "South Canterbury", 
                                              "Southern"), 
                                   ordered = TRUE)) |>
  arrange(dhb_of_residence, age_group_2) |> 
  # Assign coordinates for chart
  group_by(dhb_of_residence) |> 
  mutate(dhb_i = row_number()) |> 
  ungroup() |> 
  mutate(x = as.integer(floor(dhb_i / 500))) |> 
  group_by(dhb_of_residence, x) |> 
  mutate(y = row_number()) |> 
  ungroup()

chart_m <- m |> ggplot() + 
  geom_point(mapping = aes(x = x, y = y, fill = age_group_2), 
             shape = 21,
             stroke = 0,
             size = 0.4) + 
  facet_grid(cols = vars(dhb_of_residence), 
             scales = "free_x", 
             space = "free_x") + 
  scale_fill_brewer(palette = "Set2", name = NULL) + 
  guides(fill = guide_legend(override.aes = list(size = 2))) + 
  theme_minimal() + 
  theme(strip.text.x = element_text(angle = 90, hjust = 0), 
        panel.grid = element_blank(), 
        plot.margin = margin(4, 4, 4, 4, "pt"), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank())

ggsave(filename = here("outputs/m.png"), 
       plot = chart_m, 
       device = "png", 
       width = 3600, 
       height = 2400, 
       units = "px", 
       bg = "white")