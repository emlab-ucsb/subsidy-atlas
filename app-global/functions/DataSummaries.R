
DataSummaryDT <- function(dat,
                          type = "eez"){
  
  if(type == "eez"){
    
    summary_stats <- dat %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(geoname, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`EEZ` = geoname,
                    `Number of DW vessels` = n_vessels,
                    `Total DW vessel capacity (kW)` = round(tot_engine_power, 0),
                    `Total DW vessel tonnage (gt)` = round(tot_tonnage, 0),
                    `Total DW fishing effort (hours)` = round(fishing_hours, 0),
                    `Total DW fishing effort (kW hours)` = round(fishing_KWh, 0),
                    `Estimated DW subsidies to EEZ (2018 $US)` = round(bad_subs, 0)) %>%
      dplyr::select(-geoname, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
  }else if(type == "flag_eezs_only"){
    
    summary_stats <- dat %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(flag_iso3, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`Flag State` = flag_iso3,
                    `Number of DW vessels` = n_vessels,
                    `Total DW vessel capacity (kW)` = round(tot_engine_power, 0),
                    `Total DW vessel tonnage (gt)` = round(tot_tonnage, 0),
                    `Total DW fishing effort (hours)` = round(fishing_hours, 0),
                    `Total DW fishing effort (kW hours)` = round(fishing_KWh, 0),
                    `Estimated DW subsidies to EEZs (2018 $US)` = round(bad_subs, 0)) %>%
      dplyr::select(-flag_iso3, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
  }else if(type == "hs_area"){
    
    summary_stats <- dat %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(fao_region, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`FAO Area` = paste0("FAO area ", fao_region),
                    `Number of DW vessels` = n_vessels,
                    `Total DW vessel capacity (kW)` = round(tot_engine_power, 0),
                    `Total DW vessel tonnage (gt)` = round(tot_tonnage, 0),
                    `Total DW fishing effort (hours)` = round(fishing_hours, 0),
                    `Total DW fishing effort (kW hours)` = round(fishing_KWh, 0),
                    `Estimated DW subsidies to area (2018 $US)` = round(bad_subs, 0)) %>%
      dplyr::select(-fao_region, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
    
  }else if(type == "flag_hs_only"){
    
    summary_stats <- dat %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(flag_iso3, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`Flag State` = flag_iso3,
                    `Number of DW vessels` = n_vessels,
                    `Total DW vessel capacity (kW)` = round(tot_engine_power, 0),
                    `Total DW vessel tonnage (gt)` = round(tot_tonnage, 0),
                    `Total DW fishing effort (hours)` = round(fishing_hours, 0),
                    `Total DW fishing effort (kW hours)` = round(fishing_KWh, 0),
                    `Estimated DW subsidies to high seas (2018 $US)` = round(bad_subs, 0)) %>%
      dplyr::select(-flag_iso3, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
  }

  # Convert format to DT
  DT::datatable(summary_stats, 
                options = list(orderClasses = TRUE, dom = 'tip', pageLength = 10), 
                rownames = F)
  
}