
DataSummaryDT <- function(dat,
                          type = "eez"){
  
  if(type == "eez"){
    
    summary_stats <- dat %>%
      distinct(geoname, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs, mark) %>%
      arrange(desc(bad_subs)) %>%
      # dplyr::select(geoname, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs, mark) %>%
      dplyr::mutate(`EEZ` = geoname,
                    `Number of DW vessels` = format(n_vessels, big.mark = ","),
                    `Total DW vessel capacity (kW)` = format(round(tot_engine_power, 0), big.mark = ","),
                    `Total DW vessel tonnage (gt)` = format(round(tot_tonnage, 0), big.mark = ","),
                    `Total DW fishing effort (hours)` = format(round(fishing_hours, 0), big.mark = ","),
                    `Total DW fishing effort (kWh)` = format(round(fishing_KWh, 0), big.mark = ","),
                    `Estimated DW subsidies to EEZ (2018 $US)` = format(round(bad_subs, 0), big.mark = ","),
                    Notes = case_when(mark == "none" ~ "",
                                      mark == "is_overlapping" ~ "ᵝ",
                                      mark == "is_joint" ~ "ᵞ",
                                      mark == "multi-eez" ~ "¹",
                                      mark == "multi-eez & overlapping" ~ "¹˒²",
                                      mark == "multi-eez & joint" ~ "¹˒³",
                                      mark == "multi-eez & overlapping & joint" ~ "¹˒²˒³",
                                      mark == "overlapping" ~ "²",
                                      mark == "joint" ~ "³",
                                      mark == "overlapping & joint" ~ "²˒³",
                                      mark == "aggregate" ~ "ᵟ")) %>%
      dplyr::select(-geoname, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs, -mark) 
    
  }else if(type == "flag_eezs_only"){
    
    summary_stats <- dat %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(flag_iso3, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`Flag State` = flag_iso3,
                    `Number of DW vessels` = format(n_vessels, big.mark = ","),
                    `Total DW vessel capacity (kW)` = format(round(tot_engine_power, 0), big.mark = ","),
                    `Total DW vessel tonnage (gt)` = format(round(tot_tonnage, 0), big.mark = ","),
                    `Total DW fishing effort (hours)` = format(round(fishing_hours, 0), big.mark = ","),
                    `Total DW fishing effort (kWh)` = format(round(fishing_KWh, 0), big.mark = ","),
                    `Estimated DW subsidies to all EEZs (2018 $US)` = format(round(bad_subs, 0), big.mark = ",")) %>%
      dplyr::select(-flag_iso3, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
  }else if(type == "hs_area"){
    
    fao_region_names <- c("77" = "Eastern Central part of the Pacific Ocean", 
                          "71" = "Western Central part of the Pacific Ocean",
                          "51" = "Western part of the Indian Ocean",
                          "61" = "Northwestern part of the Pacific Ocean",
                          "87" = "Southeastern part of the Pacific Ocean",
                          "41" = "Southwestern part of the Atlantic Ocean",
                          "34" = "Eastern Central part of the Atlantic Ocean",
                          "47" = "Southeastern part of the Atlantic Ocean",
                          "27" = "Northeastern part of the Atlantic Ocean",
                          "81" = "Southwestern part of the Pacific Ocean",
                          "21" = "Northwestern part of the Atlantic Ocean",
                          "57" = "Eastern part of the Indian Ocean",
                          "31" = "Western part of the Atlantic Ocean",
                          "67" = "Northeastern part of the Pacific Ocean",
                          "48" = "Antarctic part of the Atlantic Ocean",
                          "88" = "Antarctic part of the Pacific Ocean",
                          "58" = "Antarctic and Southern parts of the Indian Ocean",
                          "18" = "Arctic Ocean",
                          "37" = "Mediterranean and Black Sea")
    
    fao_region_names_df <- tibble(fao_region = as.integer(names(fao_region_names)),
                                  name = fao_region_names)
    
    summary_stats <- dat %>%
      left_join(fao_region_names_df, by = "fao_region") %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(fao_region, name, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`FAO Area` = paste0(fao_region, " (", name, ")"),
                    `Number of DW vessels` = format(n_vessels, big.mark = ","),
                    `Total DW vessel capacity (kW)` = format(round(tot_engine_power, 0), big.mark = ","),
                    `Total DW vessel tonnage (gt)` = format(round(tot_tonnage, 0), big.mark = ","),
                    `Total DW fishing effort (hours)` = format(round(fishing_hours, 0), big.mark = ","),
                    `Total DW fishing effort (kWh)` = format(round(fishing_KWh, 0), big.mark = ","),
                    `Estimated DW subsidies to area (2018 $US)` = format(round(bad_subs, 0), big.mark = ",")) %>%
      dplyr::select(-fao_region, -name, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
    
  }else if(type == "flag_hs_only"){
    
    summary_stats <- dat %>%
      arrange(desc(bad_subs)) %>%
      dplyr::select(flag_iso3, n_vessels, tot_engine_power, tot_tonnage, fishing_hours, fishing_KWh, bad_subs) %>%
      dplyr::mutate(`Flag State` = flag_iso3,
                    `Number of DW vessels` = format(n_vessels, big.mark = ","),
                    `Total DW vessel capacity (kW)` = format(round(tot_engine_power, 0), big.mark = ","),
                    `Total DW vessel tonnage (gt)` = format(round(tot_tonnage, 0), big.mark = ","),
                    `Total DW fishing effort (hours)` = format(round(fishing_hours, 0), big.mark = ","),
                    `Total DW fishing effort (kWh)` = format(round(fishing_KWh, 0), big.mark = ","),
                    `Estimated DW subsidies to high seas (2018 $US)` = format(round(bad_subs, 0), big.mark = ",")) %>%
      dplyr::select(-flag_iso3, -n_vessels, -tot_engine_power, -tot_tonnage, -fishing_hours, -fishing_KWh, -bad_subs) 
    
  }

  # Convert format to DT
  DT::datatable(summary_stats, 
                options = list(orderClasses = TRUE, dom = 'tip', pageLength = 10), 
                rownames = F)
  
}