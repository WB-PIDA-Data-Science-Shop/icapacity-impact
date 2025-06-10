

# set-up ------------------------------------------------------------------

# Load Libraries
library(readxl)
library(here)
library(haven)
library(janitor)
library(tidyverse)
library(writexl) 



# 01-data-load ------------------------------------------------------------

panel_corr <- read.csv(
  here(
    "data",
    "cleaned",
    "clusters_outcomes_panel.csv")
  ) |> 
  filter(region != "North America", region != "")


# 02-averages-calculations ----------------------------------------------------------
# Calculate regional average per year at cluster level
regional_cluster <- panel_corr |> 
  drop_na(pfm_inst, justice_inst) # Balancing the panel

balanced_panel_avg <- regional_cluster |> 
  group_by(region, year) %>% 
  summarise(
    across(
      .cols = ends_with("_inst"),   # all *_inst cluster indicators
      .fns  = ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

regional_clusters_avgs <- regional_cluster |>  
  group_by(region, year) %>% 
  summarise(
    across(
      c(ends_with("_inst"), gdppc, govdebt, cgdppc),   # cluster indicators + macro vars
      ~ mean(.x, na.rm = TRUE),
      .names = "regional_{.col}"  
    ),
    .groups = "drop"
  ) ### This data is for the bubble 




# 03-export_data -----------------------------------------------------------

write_csv(
  regional_clusters_avgs,
  here(
    "data",
    "cleaned",
    "regional_clusters_avgs.csv",
    na = ""
  )
)

write_xlsx(
  regional_clusters_avgs,
  here("data", "cleaned", "regional_clusters_avgs.xlsx")   # destination file
  # writexl leaves NA cells blank by default
)
 
