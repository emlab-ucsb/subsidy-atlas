
VesselOriginsMap <- function(region_dat,
                             land_sf,
                             input_selected_eez,
                             input_fill_variable,
                             input_fill_scale,
                             region_pal_light,
                             map_id){
  
  if(map_id == "high_seas_vessel_origins_map"){
    
    # Connectivity data for the whole region
    region_connectivity_data <- subset(region_dat$connect,
                                       region_dat$connect$flag_iso3 != "UNK")
    
    # Connectivity data for the selected coastal state
    selected_eez_connectivity_data <- region_connectivity_data %>%
      dplyr::filter(fao_region == input_selected_eez) %>%
      arrange(admin)
    
    # EEZ polygons for selected coastal state
    selected_eez <- region_dat$eezs %>%
      mutate(zone = as.character(zone)) %>%
      dplyr::filter(zone == input_selected_eez)
    
    # Country polygons for flag states
    flag_states_for_selected_eez <- land_sf %>%
      dplyr::filter(admin_iso3 %in% selected_eez_connectivity_data$flag_iso3) %>%
      rename(flag_iso3 = admin_iso3) %>% 
      arrange(flag_iso3)
    
    # Connectivity stats by flag state (attached to land polygons)
    flag_state_summary_dat <- selected_eez_connectivity_data %>%
      st_drop_geometry() %>%
      group_by(flag_iso3, fao_region) %>%
      summarize(n_vessels = unique(n_vessels),
                mean_length = unique(mean_length),
                tot_tonnage = unique(tot_tonnage),
                tot_engine_power = unique(tot_engine_power),
                fishing_hours = unique(fishing_hours),
                fishing_KWh = unique(fishing_KWh),
                bad_subs = unique(bad_subs)) %>%
      ungroup()
    
    flag_state_summary <- flag_states_for_selected_eez %>%
      left_join(flag_state_summary_dat, by = "flag_iso3")
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary$admin,
      "<br/>",
      "<b>", "Different vessels: ", "</b>", flag_state_summary$n_vessels,
      "</br>",
      "<b>", "Total vessel capacity (kW): ", "</b>", format(round(flag_state_summary$tot_engine_power, 0), big.mark = ","), 
      "</br>",
      "<b>", "Total vessel tonnage (gt): ", "</b>", format(round(flag_state_summary$tot_tonnage, 0), big.mark = ","), 
      "</br>",
      "<b>", "Total fishing effort in selected area (hours): ", "</b>",  format(round(flag_state_summary$fishing_hours, 2), big.mark = ","), 
      "</br>",
      "<b>", "Total fishing effort in selected area (kW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    ### Interactive color palette ---
    
    # Set fill variable for map
    fill_scale <- switch(input_fill_variable,
                         "n_vessels" = list("n_vessels", region_connectivity_data$n_vessels, flag_state_summary$n_vessels),
                         "tot_engine_power" = list("tot_engine_power", region_connectivity_data$tot_engine_power, flag_state_summary$tot_engine_power),
                         "tot_tonnage" = list("tot_tonnage", region_connectivity_data$tot_tonnage, flag_state_summary$tot_tonnage),
                         "fishing_hours" = list("fishing_hours", region_connectivity_data$fishing_hours, flag_state_summary$fishing_hours),
                         "fishing_KWh" = list("fishing_KWh", region_connectivity_data$fishing_KWh, flag_state_summary$fishing_KWh))
    
    # Make color palette
    domain <- switch(input_fill_scale,
                     "region" = 2,
                     "selected_eez" = 3)
    
    pal <- colorBin("YlOrRd", domain = fill_scale[[domain]], bins = 7)
    
    ### Leaflet map ---
    
    leaflet(map_id, 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topleft' }).addTo(this)}") %>% 
      
      addProviderTiles("CartoDB.PositronNoLabels", group = "basemap") %>% 
      
      addPolygons(data = flag_state_summary,
                  fillColor = ~pal(get(fill_scale[[1]])),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_state_summary$flag_iso3,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      addPolygons(data = selected_eez, 
                  fillColor = region_pal_light$light_col,
                  fillOpacity = 0.8,
                  color= "white",
                  group = "eez",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", selected_eez$title, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
      addPolylines(data = selected_eez_connectivity_data,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod",
                   group = "lines") %>% 
      
      addLegend(pal = pal, 
                values = fill_scale[[domain]], 
                opacity=0.9, 
                title = NULL, 
                position = "bottomleft" ) %>%
      setView(lng= region_dat$map_lng, 
              lat = region_dat$map_lat, 
              zoom = region_dat$map_zoom-1)
    
  }else{
    
  # Connectivity data for the whole region
  region_connectivity_data <- subset(region_dat$connect,
                                     region_dat$connect$flag_iso3 != "UNK")
  
  # Connectivity data for the selected coastal state
  selected_eez_connectivity_data <- region_connectivity_data %>%
    dplyr::filter(eez_ter_iso3 == input_selected_eez) %>%
    arrange(admin)
  
  # EEZ polygons for selected coastal state
  selected_eez <- region_dat$eezs %>%
    dplyr::filter(eez_ter_iso3 == input_selected_eez)
  
  # Country polygons for flag states
  flag_states_for_selected_eez <- land_sf %>%
    dplyr::filter(admin_iso3 %in% selected_eez_connectivity_data$flag_iso3) %>%
    rename(flag_iso3 = admin_iso3) %>% 
    arrange(flag_iso3)
  
  # Connectivity stats by flag state (attached to land polygons)
  flag_state_summary_dat <- selected_eez_connectivity_data %>%
    st_drop_geometry() %>%
    group_by(flag_iso3, eez_ter_iso3) %>%
    summarize(n_vessels = unique(n_vessels),
              mean_length = unique(mean_length),
              tot_tonnage = unique(tot_tonnage),
              tot_engine_power = unique(tot_engine_power),
              fishing_hours = unique(fishing_hours),
              fishing_KWh = unique(fishing_KWh),
              bad_subs = unique(bad_subs)) %>%
    ungroup()
  
  flag_state_summary <- flag_states_for_selected_eez %>%
    left_join(flag_state_summary_dat, by = "flag_iso3")
  
  flag_state_summary_text <- paste0(
    "<b>", "Flag state: ", "</b>", flag_state_summary$admin,
    "<br/>",
    "<b>", "Different vessels: ", "</b>", flag_state_summary$n_vessels,
    "</br>",
    "<b>", "Total vessel capacity (kW): ", "</b>", format(round(flag_state_summary$tot_engine_power, 0), big.mark = ","), 
    "</br>",
    "<b>", "Total vessel tonnage (gt): ", "</b>", format(round(flag_state_summary$tot_tonnage, 0), big.mark = ","), 
    "</br>",
    "<b>", "Total fishing effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_hours, 2), big.mark = ","), 
    "</br>",
    "<b>", "Total fishing effort in selected EEZ (kW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
    lapply(htmltools::HTML)
  
  ### Interactive color palette ---
  
  # Set fill variable for map
  fill_scale <- switch(input_fill_variable,
                       "n_vessels" = list("n_vessels", region_connectivity_data$n_vessels, flag_state_summary$n_vessels),
                       "tot_engine_power" = list("tot_engine_power", region_connectivity_data$tot_engine_power, flag_state_summary$tot_engine_power),
                       "tot_tonnage" = list("tot_tonnage", region_connectivity_data$tot_tonnage, flag_state_summary$tot_tonnage),
                       "fishing_hours" = list("fishing_hours", region_connectivity_data$fishing_hours, flag_state_summary$fishing_hours),
                       "fishing_KWh" = list("fishing_KWh", region_connectivity_data$fishing_KWh, flag_state_summary$fishing_KWh))
  
  # Make color palette
  domain <- switch(input_fill_scale,
                   "region" = 2,
                   "selected_eez" = 3)
  
  pal <- colorBin("YlOrRd", domain = fill_scale[[domain]], bins = 7)
  
  ### Leaflet map ---
  
  leaflet(map_id, 
          options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    
    htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topleft' }).addTo(this)}") %>% 
    
    addProviderTiles("CartoDB.PositronNoLabels", group = "basemap") %>% 
    
    addPolygons(data = flag_state_summary,
                fillColor = ~pal(get(fill_scale[[1]])),
                fillOpacity = 0.8,
                color= "white",
                weight = 0.3,
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             fillOpacity = 1,
                                             bringToFront = TRUE),
                label = flag_state_summary_text,
                layerId = flag_state_summary$flag_iso3,
                labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                         padding = "3px 8px"),
                                            textsize = "13px",
                                            direction = "auto")) %>%
    addPolygons(data = selected_eez, 
                fillColor = ~region_pal_light(region),
                fillOpacity = 0.8,
                color= "white",
                group = "eez",
                weight = 0.3,
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             fillOpacity = 1,
                                             bringToFront = TRUE),
                label = (paste0("<b>", selected_eez$geoname_new, "</b>") %>%
                           lapply(htmltools::HTML)),
                labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                         padding = "3px 8px"),
                                            textsize = "13px",
                                            direction = "auto")) %>%
    
    addPolylines(data = selected_eez_connectivity_data,
                 fillColor = "goldenrod",
                 fillOpacity = 1,
                 weight = 1,
                 color = "darkgoldenrod",
                 group = "lines") %>% 
    
    addLegend(pal = pal, 
              values = fill_scale[[domain]], 
              opacity=0.9, 
              title = NULL, 
              position = "bottomleft" ) %>%
    setView(lng= region_dat$map_lng, 
            lat = region_dat$map_lat, 
            zoom = region_dat$map_zoom-1)
  
  }
}