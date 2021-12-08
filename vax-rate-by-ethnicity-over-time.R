# Chart of vaccination rate by ethnicity over time for delta outbreak

library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(lubridate)
library(scales)
library(lemon)

dat_vax <- read_excel(path = here("data/20211205_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx"), 
                      sheet = "Vaccination Query") |> 
  clean_names() |> 
  mutate(week_ending_date = as_date(week_ending_date)) |> 
  mutate(number_people_partially_vaccinated = as.integer(number_people_partially_vaccinated), 
         number_people_fully_vaccinated = as.integer(number_people_fully_vaccinated)) |> 
  print()

dat_pop <- read_excel(path = here("data/20211205_-_cvip_equity_-_rate_ratios_and_uptake_over_time.xlsx"), 
                      sheet = "HSU Table") |> 
  clean_names() |> 
  mutate(number_people_hsu = as.integer(number_people_hsu))

dat_vax_rate_by_week_ethnic_group <- 
  dat_vax |> 
  filter(week_ending_date >= ymd("2021-02-14")) |> 
  group_by(week_ending_date, ethnic_group) |> 
  summarise(number_people_partially_vaccinated = sum(number_people_partially_vaccinated), 
            number_people_fully_vaccinated = sum(number_people_fully_vaccinated)) |> 
  ungroup() |> 
  complete(week_ending_date, ethnic_group, 
           fill = list(number_people_partially_vaccinated = 0L, 
                       number_people_fully_vaccinated = 0L)) |> 
  arrange(ethnic_group, week_ending_date) |> 
  group_by(ethnic_group) |> 
  mutate(total_people_partially_vaccinated = cumsum(number_people_partially_vaccinated), 
         total_people_fully_vaccinated = cumsum(number_people_fully_vaccinated)) |> 
  left_join(y = dat_pop |> 
              filter(age_group != "< 12") |> 
              group_by(ethnic_group) |> 
              summarise(number_people_hsu = sum(number_people_hsu)) |> 
              ungroup(), 
            by = "ethnic_group") |> 
  mutate(partially_vax_rate = total_people_partially_vaccinated / number_people_hsu, 
         fully_vax_rate = total_people_fully_vaccinated / number_people_hsu) |> 
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Māori", 
                                          "Pacific", 
                                          "Non-Māori Non-Pacific"), 
                               ordered = TRUE)) |> 
  select(week_ending_date, ethnic_group, partially_vax_rate, fully_vax_rate) |> 
  pivot_longer(cols = c(partially_vax_rate, fully_vax_rate), 
               names_to = "measure", 
               values_to = "value") |> 
  mutate(measure = factor(x = measure, 
                          levels = c("partially_vax_rate", 
                                     "fully_vax_rate"), 
                          labels = c("First dose rate", 
                                     "Second dose rate"), 
                          ordered = TRUE))

chart_vax_rate_by_week_ethnic_group <- dat_vax_rate_by_week_ethnic_group |> 
  ggplot(mapping = aes(x = week_ending_date, 
                       y = value, 
                       fill = ethnic_group, 
                       colour = ethnic_group)) + 
  geom_vline(xintercept = c(ymd("2021-08-18"), 
                            ymd("2021-09-22"), 
                            ymd("2021-12-03")), 
             linetype = "dashed", 
             size = 0.25) + 
  geom_point(size = 0.75) + 
  geom_line(size = 0.5) + 
  annotate(geom = "text", 
           size = 2.5, 
           family = "Fira Sans", 
           fontface = "bold", 
           x = ymd("2021-08-20"), 
           y = 1, 
           label = "AL4", 
           hjust = 0) + 
  annotate(geom = "text", 
           size = 2.5, 
           family = "Fira Sans", 
           fontface = "bold", 
           x = ymd("2021-09-24"), 
           y = 1, 
           label = "Auckland AL3", 
           hjust = 0) + 
  annotate(geom = "text", 
           size = 2.5, 
           family = "Fira Sans", 
           fontface = "bold", 
           x = ymd("2021-12-05"), 
           y = 1, 
           label = "CPF", 
           hjust = 0) + 
  facet_rep_wrap(facets = vars(measure), 
                 ncol = 1, 
                 repeat.tick.labels = TRUE) + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_x_date(breaks = seq(from = min(dat_vax_rate_by_week_ethnic_group$week_ending_date), 
                            to = max(dat_vax_rate_by_week_ethnic_group$week_ending_date), 
                            by = "14 days"), 
               limits = c(min(dat_vax_rate_by_week_ethnic_group$week_ending_date) - ddays(7), 
                          max(dat_vax_rate_by_week_ethnic_group$week_ending_date) + ddays(10)), 
               expand = expansion(0, 0), 
               labels = date_format("%d\n%b")) + 
  scale_colour_manual(values = c("Māori" = "cornflowerblue", 
                                 "Pacific" = "firebrick", 
                                 "Non-Māori Non-Pacific" = "black"), 
                      name = NULL, 
                      aesthetics = c("colour", "fill")) + 
  xlab("Week ending") + 
  ylab("Proportion of\neligible population\n(age 12+)") + 
  theme_minimal(base_family = "Fira Sans", 
                base_size = 8) + 
  theme(panel.grid.minor.y = element_blank(), 
        panel.grid.major = element_line(size = 0.25, colour = grey(0.85)), 
        panel.grid.minor.x = element_line(size = 0.25, colour = grey(0.85)), 
        axis.title.y = element_text(angle = 0, hjust = 0, 
                                    margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        strip.text = element_text(size = rel(1.1), 
                                  face = "bold"), 
        panel.spacing.y = unit(24, "pt"), 
        legend.position = "top", 
        legend.box.margin = margin(0, 0, 0, 0), 
        legend.margin = margin(0, 0, 0, 0)) 

ggsave(filename = here("outputs/vax_rate_by_week_ethnic_group.png"), 
       plot = chart_vax_rate_by_week_ethnic_group, 
       width = 2400, 
       height = 1800, 
       units = "px", 
       device = "png", 
       bg = "white")

