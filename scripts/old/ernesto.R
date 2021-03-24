
### China stats for Ernesto - 1/8/2020

library(tidyverse)

dat <- read_csv(here::here("data", "GFW", "EDIT_FV_effort_by_eez_and_region_2018.csv"))

china <- dat %>%
  dplyr::filter(flag == "CHN")

china_totals_no_hs <- china %>%
  dplyr::filter(!is.na(eez_iso3)) %>%
  summarize(n_vessels = n_distinct(ssvid, na.rm = T),
            n_vessel_classes = n_distinct(vessel_class, na.rm = T),
            n_eez_hs_codes = n_distinct(eez_hs_code, na.rm = T),
            n_eez_iso3s = n_distinct(eez_iso3, na.rm = T),
            tot_fishing_hours = sum(fishing_hours_year_eez_fao, na.rm = T),
            tot_fishing_days = sum(fishing_days_year_eez_fao, na.rm = T))