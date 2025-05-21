 

# set-up ------------------------------------------------------------------

# Load Libraries
library(readxl)
library(here)
library(haven)
library(janitor)
library(tidyverse)


# 01-data-load ------------------------------------------------------------

cliar_clusters <- read.csv(
                  here(
                    "data",
                    "raw",
                    "cliar_aggregates_timeseries_19902023.csv"
                    )
                  ) |> 
                  clean_names()

wdi_outcomes <- read.csv(
              here(
                "data",
                "cleaned",
                "wdi_outcomes.csv"
              )
            ) 


# 02-data-wrangling -------------------------------------------------------

# Clean dataframe pre merging
clusters_cleaned <- cliar_clusters |> 
                    select(-(13:16)) |> # Select only relevant clusters
                    rename(
                      pfm_inst = public_finance_inst,
                      hrms_inst = public_hr_inst,
                      integrity_inst = corruption_lack_of,
                      env_inst = climate_change_inst
                    )


# Merge clusters with indicators
panel_merged <- clusters_cleaned |> 
                left_join(
                 wdi_outcomes |> 
                  select(-country_name), # Avoid duplication
                by = c("country_code", "year")
                )
  

# 03-data-quality-overview ------------------------------------------------
# Check for sumplicates
panel_merged |> 
  count(country_code, year) |>  
  filter(n > 1) 

year_coverage <- panel_merged |> 
                    select(year, ends_with("_inst"), gdppc, govdebt) |>
                    group_by(year) |>
                    summarise(across(everything(), ~ sum(!is.na(.x))))

# Filter countries that have *every* year and *no* missing values 
# in any of the following variables
vars <- c("pfm_isnt", "digital_inst", "hrms_inst", "justice_inst",
          "political_inst", "social_inst", "integrity_inst",
          "transparency_inst", "env_inst", "gdppc", "govdebt")

balanced_countries <- panel_merged |>
                    filter(year > 2009) |> # keep 2010 and onwards
                    group_by(country_code) |>
                    summarise(across(all_of(vars), ~ all(!is.na(.x))), .groups = "drop") |>
                    filter(if_all(all_of(vars), identity)) |> # keep only those with no NAs
                    pull(country_code)

balanced_panel <- panel_merged |>
  filter(country_code %in% balanced_countries) # Empty


# 05-export-panel-for-analysis --------------------------------------------

write_csv(
  panel_merged,
  here(
    "data",
    "cleaned",
    "clusters_outcomes_panel.csv"
  )
)


