

# set-up ------------------------------------------------------------------

# Load Libraries
library(readxl)
library(here)
library(haven)
library(janitor)
library(WDI)
library(tidyverse)


# 0-extract-wdi-indicators ------------------------------------------------

wdi_outcomes <- WDI(
  country = "all",
  indicator = c("NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2021 international $)
                "GC.DOD.TOTL.GD.ZS" # Central government debt, total (% of GDP)
  ),
  start = 1990,
  end = 2023,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

# Last access recorded on 05-20-2025
# Source: https://databank.worldbank.org/source/world-development-indicators


# 1-clean-df --------------------------------------------------------------

wdi_cleaned <- wdi_outcomes |> 
  rename(
    country_name = country,
    country_code = iso3c,
    gdppc = `NY.GDP.PCAP.PP.KD`,
    govdebt = `GC.DOD.TOTL.GD.ZS`
  ) |> 
  select(-c(
    iso2c,
    status,
    lastupdated,
    capital,
    longitude,
    latitude   
  ))
  


# 2-export-csv ------------------------------------------------------------

write_csv(
  wdi_cleaned,
    here(
      "data",
      "cleaned",
      "wdi_outcomes.csv"
  )
)

