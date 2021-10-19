# Comparison of HSU and Stats NZ population and implications for vaccination rates

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(glue)
library(scales)

latest_date <- "12_10_2021"               # Date of most recent week's data

# *****************************************************************************


# *****************************************************************************
# Load data ----

# HSU population
dat_pop_hsu <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                          sheet = "HSU Population") |>
  clean_names() |> 
  select(-x6, -x7, -notes)

# Stats NZ 2020 population estimates for DHB areas
dat_pop_statsnz <- read_excel(path = here("data/2020-21 Population Projections.xlsx"), 
                              sheet = "DHB20POPPROJS_FINYEAR", 
                              skip = 1) |> 
  clean_names() |> 
  select(-combine_field, -age_group_10) |> 
  rename(pop = pop2020_2021)

# Stats NZ ERP for 2018 with breakdown for every age
dat_pop_erp <- read_excel(path = here("data/ERP-2018-by-every-age.xlsx"), 
                          sheet = "NZ.Stat export", 
                          skip = 3) |> 
  clean_names() |> 
  filter(!is.na(total_people_ethnic_group)) |> 
  rename(sex = x2, 
         age = ethnic_group) |> 
  select(-x3) |> 
  fill(age, .direction = "down") |> 
  pivot_longer(cols = c(-age, -sex), 
               names_to = "ethnic_group", 
               values_to = "pop")

# Vaccination
dat_vax <- read_excel(path = here(glue("data/covid_vaccinations_{latest_date}.xlsx")), 
                      sheet = "DHBofResidence by ethnicity") |>
  clean_names() |> 
  select(-x10, -notes) 

# *****************************************************************************


# *****************************************************************************
# Manipulate data for comparisons ----

# Stats NZ population uses 10 year age groups. Apply the 10 and 11 year old
# proportions from the 2018 ERP data to calculate an estimated population 
# aged under 12. 

# Proportion of population aged 10 to 14 who are 10 and 11 years old by 
# ethnicity and gender
pop_10_11_by_ethnicity_gender <- 
  # Population aged 10 and 11 by ethnicity and gender
  dat_pop_erp |> 
  filter(age %in% c("10 Years", "11 Years"), 
         ethnic_group != "total_people_ethnic_group", 
         ethnic_group != "middle_eastern_latin_american_african") |> 
  group_by(sex, ethnic_group) |> 
  summarise(pop_10_11 = sum(pop)) |> 
  ungroup() |> 
  # Population aged 10 to 14 by ethnicity and gender
  left_join(y = dat_pop_erp |> 
              filter(age %in% c("10 Years", "11 Years", "12 Years", "13 Years", "14 Years"), 
                     ethnic_group != "total_people_ethnic_group", 
                     ethnic_group != "middle_eastern_latin_american_african") |> 
              group_by(sex, ethnic_group) |> 
              summarise(pop_10_14 = sum(pop)) |> 
              ungroup(), 
            by = c("sex", "ethnic_group")) |> 
  mutate(pct_10_11 = pop_10_11 / pop_10_14) |> 
  # Make ethnic group labels same as in other Stats NZ dataset
  mutate(ethnic_group = case_when(
    ethnic_group == "asian" ~ "Asian", 
    ethnic_group == "maori" ~ "Maori", 
    ethnic_group == "pacific" ~ "Pacific", 
    ethnic_group == "european_or_other_including_new_zealander" ~ "Other"
  ))

# Apply these proportions to Stats NZ populations by DHB areas to split the
# 10-14 age group into 10/11 and 12/13/14
pop_statsnz_split <- bind_rows(
  # Age group 10-14 split
  dat_pop_statsnz |> 
    filter(age_group == "10-14") |> 
    left_join(y = pop_10_11_by_ethnicity_gender |> 
                select(-pop_10_11, -pop_10_14), 
              by = c("ethnicity" = "ethnic_group", 
                     "sex" = "sex")) |> 
    mutate(pop_10_11 = round(pop * pct_10_11)) |> 
    mutate(pop_12_13_14 = pop - pop_10_11) |> 
    select(-age_group, -pop, -pct_10_11) |> 
    pivot_longer(cols = c(pop_10_11, pop_12_13_14), 
                 names_to = "age_group", 
                 values_to = "pop") |> 
    mutate(age_group = case_when(
      age_group == "pop_10_11" ~ "10-11", 
      age_group == "pop_12_13_14" ~ "12-14"
    )), 
  # All other age groups
  dat_pop_statsnz |> filter(age_group != "10-14")
) |> 
  rename(ethnic_group = ethnicity) |> 
  arrange(dhb_name, sex, ethnic_group, age_group) 

# Summarise populations by ethnicity and broader age groups for comparison
pop_comp <- bind_rows(
  # Stats NZ population 
  pop_statsnz_split |> 
    mutate(ethnic_group = case_when(
      ethnic_group == "Pacific" ~ "Pacific Peoples", 
      ethnic_group == "Other" ~ "European or Other", 
      TRUE ~ ethnic_group
    )) |> 
    mutate(age_group_2 = case_when(
      age_group == "00-04" ~ "Under 12", 
      age_group == "05-09" ~ "Under 12", 
      age_group == "10-11" ~ "Under 12", 
      age_group == "12-14" ~ "12-29", 
      age_group == "15-19" ~ "12-29", 
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
      age_group == "90+" ~ "60+", 
    )) |> 
    group_by(ethnic_group, age_group_2) |> 
    summarise(pop = sum(pop)) |> 
    mutate(measure = "statsnz") |> 
    ungroup(), 
  
  # HSU population 
  dat_pop_hsu |> 
    filter(ethnic_group != "Unknown", 
           ethnic_group != "Various") |> 
    mutate(age_group_2 = case_when(
      age_group == "0-11" ~ "Under 12",
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
    group_by(ethnic_group, age_group_2) |> 
    summarise(pop = sum(population)) |> 
    mutate(measure = "hsu") |> 
    ungroup()
) |> 
  # Factor levels for ordering
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Maori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "European or Other"), 
                               labels = c("Māori", 
                                          "Pacific Peoples", 
                                          "Asian", 
                                          "Pākehā or other"), 
                               ordered = TRUE)) |> 
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("Under 12", 
                                         "12-29", 
                                         "30-59", 
                                         "60+"), 
                              labels = c("Under 12 years", 
                                         "12-29 years", 
                                         "30-59 years", 
                                         "60+ years"), 
                              ordered = TRUE)) |> 
  arrange(ethnic_group, age_group_2) 


pop_comp |> pivot_wider(values_from = pop, names_from = measure) |> 
  mutate(diff = hsu - statsnz) |> 
  mutate(diff_pct = diff / hsu)

# *****************************************************************************
