
WidgetEEZSelect <- function(region_dat,
                            widget_id){
  
  if(widget_id == "high_seas_eez_select"){
    
    # Get data
    region_eez_data <- region_dat$connect %>%
      st_drop_geometry() %>%
      distinct(fao_region, description) %>%
      arrange(fao_region) %>%
      mutate(zone_name = paste0(fao_region, ": ", description))
    
    # Unique choices
    eezs <- region_eez_data$fao_region
    names(eezs) <- region_eez_data$zone_name
    
    selectizeInput(widget_id,
                   label = NULL,
                   choices = c("Select a FAO area..." = "Select a coastal state...", eezs),
                   selected = "Select a coastal state...",
                   width = "100%")
    
  }else{
    
  # Get data
  region_eez_data <- region_dat$connect %>%
    st_drop_geometry() %>%
    distinct(region, eez_ter_iso3, eez_ter_name) %>%
    arrange(eez_ter_name)
  
  # Unique choices
  eezs <- region_eez_data$eez_ter_iso3
  names(eezs) <- region_eez_data$eez_ter_name
  
  selectizeInput(widget_id,
                 label = NULL,
                 choices = c("Select a coastal state..." = "Select a coastal state...", eezs),
                 selected = "Select a coastal state...",
                 width = "100%")
  }
  
}

WidgetFlagStateSelect <- function(region_dat,
                                  input_selected_eez,
                                  widget_id){
  
  if(widget_id == "high_seas_effort_select_flag_state" | widget_id == "high_seas_subsidies_select_flag_state"){
    
    # Get data
    region_eez_data <- region_dat$connect %>%
      st_drop_geometry() %>%
      mutate(fao_region = as.character(fao_region)) %>%
      dplyr::filter(fao_region == input_selected_eez) %>%
      distinct(region, flag_iso3, admin) %>%
      arrange(flag_iso3)
    
    # Unique choices
    flag_states <- region_eez_data$flag_iso3
    names(flag_states) <- region_eez_data$admin
    
    selectizeInput(widget_id,
                   label = NULL,
                   choices = c("Filter by flag state..." = "Select a flag state...", flag_states),
                   selected = "Select a flag state...",
                   width = "80%")
    
  }else{
    
    # Get data
    region_eez_data <- region_dat$connect %>%
      st_drop_geometry() %>%
      dplyr::filter(eez_ter_iso3 == input_selected_eez) %>%
      distinct(region, flag_iso3, admin) %>%
      arrange(flag_iso3)
    
    # Unique choices
    flag_states <- region_eez_data$flag_iso3
    names(flag_states) <- region_eez_data$admin
    
    selectizeInput(widget_id,
                   label = NULL,
                   choices = c("Filter by flag state..." = "Select a flag state...", flag_states),
                   selected = "Select a flag state...",
                   width = "80%")
    
    
    
  }
}