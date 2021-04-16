
SummaryUI <- function(region_dat,
                    input_selected_eez){
    
    # Distant water fishing totals
    total_stats_eez <- region_dat$connect %>%
      st_drop_geometry() %>%
      dplyr::filter(eez_ter_iso3 == input_selected_eez) %>% 
      group_by(eez_ter_iso3, eez_ter_name, flag_iso3) %>%
      summarize(vessels = unique(n_vessels),
                capacity = unique(tot_engine_power),
                tonnage = unique(tot_tonnage),
                fishing_hours = unique(fishing_hours),
                fishing_KWh = unique(fishing_KWh),
                bad_subs = unique(bad_subs)) %>%
      ungroup() %>%
      group_by(eez_ter_iso3, eez_ter_name) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                tonnage = sum(tonnage, na.rm = T),
                fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                bad_subs = sum(bad_subs, na.rm = T)) %>%
      ungroup()
    
  ### Combine into country profile/summary of DW fishing
  info_out <- paste0(
    "<h4>AIS-observed distant water fishing in the EEZ of ", unique(total_stats_eez$eez_ter_name), " (2018)</h3>",
    "<h5>Totals</h5>",
    "<b>Different DW vessels: </b>", format(round(total_stats_eez$vessels, 0), big.mark = ","),
    "<br>",
    "<b>Total DW vessel capacity (kW): </b>", format(round(total_stats_eez$capacity, 0), big.mark = ","),
    "<br>",
    "<b>Total DW vessel tonnage (gt): </b>", format(round(total_stats_eez$tonnage, 0), big.mark = ","),
    "<br>",
    "<b>Total DW fishing effort (hours): </b>", format(round(total_stats_eez$fishing_hours, 0), big.mark = ","),
    "<br>",
    "<b>Total DW fishing effort (kW hours): </b>", format(round(total_stats_eez$fishing_KWh, 0), big.mark = ","),
    "<br>",
    "<b>Estimated DW subsidies to EEZ (2018 $US): </b>", "$", format(round(total_stats_eez$bad_subs, 0), big.mark = ","),
    "<hr>") %>%
    lapply(htmltools::HTML)

}

SummaryDT <- function(region_dat,
                      input_selected_eez){
  
  # Distant water fishing summary
  flag_stats_eez <- region_dat$connect %>%
    st_drop_geometry() %>%
    dplyr::filter(eez_ter_iso3 == input_selected_eez) %>% 
    group_by(eez_ter_iso3, eez_ter_name, flag_iso3, admin) %>%
    summarize(`Number of DW vessels` = round(unique(n_vessels), 0),
              `Total DW vessel capacity (kW)` = round(unique(tot_engine_power), 0),
              `Total DW vessel tonnage (gt)` = round(unique(tot_tonnage), 0),
              `Total DW fishing effort (hours)` = round(unique(fishing_hours), 0),
              `Total DW fishing effort (kW hours)` = round(unique(fishing_KWh), 0),
              `Estimated DW subsidies to EEZ (2018 $US)` = round(unique(bad_subs), 0)) %>%
    ungroup() %>%
    arrange(admin) %>%
    dplyr::select(-eez_ter_iso3, -eez_ter_name, -flag_iso3) %>%
    rename(`Flag state` = admin)
  
  # Convert format
  DT::datatable(flag_stats_eez, options = list(orderClasses = TRUE, dom = 'tip', pageLength = 5), rownames = F)
  
}

#### -----------------
DownloadData <- function(region_dat,
                         input_selected_eez){
  
  # Distant water fishing summary
  flag_stats_eez <- region_dat$connect %>%
    st_drop_geometry() %>%
    dplyr::filter(eez_ter_iso3 == input_selected_eez) %>% 
    group_by(eez_ter_iso3, eez_ter_name, flag_iso3, admin) %>%
    summarize(`Number of DW vessels` = round(unique(n_vessels), 0),
              `Total DW vessel capacity (kW)` = round(unique(tot_engine_power), 0),
              `Total DW vessel tonnage (gt)` = round(unique(tot_tonnage), 0),
              `Total DW fishing effort (hours)` = round(unique(fishing_hours), 0),
              `Total DW fishing effort (kW hours)` = round(unique(fishing_KWh), 0),
              `Estimated DW subsidies to EEZ (2018 $US)` = round(unique(bad_subs), 0)) %>%
    ungroup() %>%
    arrange(admin) %>%
    dplyr::select(-eez_ter_iso3, -eez_ter_name, -flag_iso3) %>%
    rename(`Flag state` = admin)
  
}