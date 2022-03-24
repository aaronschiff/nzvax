# Automated update of vaccination charts

# *****************************************************************************
# Setup ---- 

library(tidyverse)
library(devtools)
library(gert)
library(httr)
library(rvest)
library(glue)
library(janitor)
library(here)

latest_date <- "22_03_2022"                # Date of current week's data
prev_date <- "15_03_2022"                  # Date of previous week's data 
latest_date_nice <- "22 March 2022"         # For chart title

# *****************************************************************************


# *****************************************************************************
# Update data ----

# Download latest age 12+ vaccination data
cat("* Updating data\n")
dl_vax_data <- download.file(url = glue("https://www.health.govt.nz/system/files/documents/pages/covid_vaccinations_{latest_date}.xlsx"), 
                             destfile = here(glue("data/covid_vaccinations_{latest_date}.xlsx")))

if (dl_vax_data != 0L) {
  stop("* Download vaccination data spreadsheet failed")
}

# Scrape age 5-11 tables (no longer required as data is in the main spreadsheet)
vax_page <- read_html("https://www.health.govt.nz/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data")

vax_page_tables <- html_table(vax_page)

vax_511_all <- vax_page_tables[[7]] |> 
  clean_names() |> 
  mutate(at_least_partially_vacc = str_remove(string = at_least_partially_vacc, 
                                              pattern = "\\,")) |> 
  mutate(fully_vacc = str_remove(string = fully_vacc, 
                                 pattern = "\\,")) |>
  mutate(population = str_remove(string = population, 
                                 pattern = "\\,")) |> 
  mutate(at_least_partially_vacc = as.integer(at_least_partially_vacc), 
         fully_vacc = as.integer(fully_vacc), 
         population = as.integer(population)) |> 
  select(-at_least_partially_vacc_percent, -fully_vacc_percent) |>  
  mutate(ethnicity = "All")

vax_511_maori <- vax_page_tables[[8]] |> 
  clean_names() |> 
  mutate(at_least_partially_vacc = str_remove(string = at_least_partially_vacc, 
                                              pattern = "\\,")) |> 
  mutate(fully_vacc = str_remove(string = fully_vacc, 
                                 pattern = "\\,")) |>
  mutate(population = str_remove(string = population, 
                                 pattern = "\\,")) |> 
  mutate(at_least_partially_vacc = as.integer(at_least_partially_vacc), 
         fully_vacc = as.integer(fully_vacc), 
         population = as.integer(population)) |> 
  select(-at_least_partially_vacc_percent, -fully_vacc_percent) |> 
  mutate(ethnicity = "Maori")

vax_511_pacific <- vax_page_tables[[9]] |> 
  clean_names() |> 
  mutate(at_least_partially_vacc = str_remove(string = at_least_partially_vacc, 
                                              pattern = "\\,")) |> 
  mutate(fully_vacc = str_remove(string = fully_vacc, 
                                 pattern = "\\,")) |>
  mutate(population = str_remove(string = population, 
                                 pattern = "\\,")) |> 
  mutate(at_least_partially_vacc = as.integer(at_least_partially_vacc), 
         fully_vacc = as.integer(fully_vacc), 
         population = as.integer(population)) |> 
  select(-at_least_partially_vacc_percent, -fully_vacc_percent) |> 
  mutate(ethnicity = "Pacific")

vax_511 <- bind_rows(
  vax_511_all, 
  vax_511_maori, 
  vax_511_pacific
)

write_csv(x = vax_511, 
          file = here(glue("data/covid_vaccinations_5_11_yo_{latest_date}.csv")))

# *****************************************************************************


# *****************************************************************************
# Update charts ----

# Move previous week's files from 'latest' to 'archive
prev_output_files <- list.files(path = here("outputs/latest"), 
                                full.names = TRUE)

file.copy(from = prev_output_files, 
          to = here("outputs/archive"), 
          overwrite = TRUE)

file.remove(prev_output_files)

# Update vaccination rates by communities
cat("* Creating charts of vax rates by community\n")
source("vax-communities.R")

# Update SA2 charts
cat("* Creating SA2 charts\n")
source("sa2-charts.R")

# Update unvaccinated charts
cat("* Creating unvaccinated charts\n")
source("unvaccinated.R")

# Update boosters charts
cat("* Creating boosters charts\n")
source("boosters.R")

# Check all charts are ok before proceeding
confirm <- readline(prompt = "Please check all charts in outputs/latest. Ok to proceed [y/n]? ")
if (str_to_lower(confirm) != "y") {
  stop("* Charts not ok, stopping")
}

# *****************************************************************************


# *****************************************************************************
# Update github and website ----

# Update github for this project
cat("* Updating github repository\n")
git_add(files = ".")
git_commit_all(message = glue("Updated vaccination charts to {latest_date_nice}"))

# Copy latest charts to website
cat("* Updating website\n")
old_website_files <- list.files(path = "/Users/aaron/Library/Mobile Documents/com~apple~CloudDocs/Work - iCloud/Website/website-current/content/covid/nz-vax/images", 
                                full.names = TRUE)
file.remove(old_website_files)
latest_output_files <- list.files(path = here("outputs/latest"), 
                                  full.names = TRUE)
file.copy(from = latest_output_files, 
          to = "/Users/aaron/Library/Mobile Documents/com~apple~CloudDocs/Work - iCloud/Website/website-current/content/covid/nz-vax/images", 
          overwrite = TRUE)
latest_website_files <- list.files(path = "/Users/aaron/Library/Mobile Documents/com~apple~CloudDocs/Work - iCloud/Website/website-current/content/covid/nz-vax/images", 
                                   full.names = TRUE)
latest_output_newnames <- str_remove(string = latest_website_files, 
                                     pattern = glue("_{latest_date}"))
file.rename(from = latest_website_files, 
            to = latest_output_newnames)

cat("- Done! Please commit and push website repo now.")

# *****************************************************************************


