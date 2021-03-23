
VesselOriginsMap <- function(region_dat,
                             land_sf,
                             input_selected_eez,
                             input_fill_variable,
                             input_fill_scale,
                             region_pal_light,
                             map_id){
  
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
    summarize(n_vessels = sum(n_vessels, na.rm = T),
              mean_length = mean(mean_length, na.rm = T),
              mean_tonnage = mean(mean_tonnage, na.rm = T),
              mean_engine_power = mean(mean_engine_power, na.rm = T),
              fishing_hours = sum(fishing_hours, na.rm = T),
              fishing_KWh = sum(fishing_KWh, na.rm = T),
              bad_subs = sum(bad_subs, na.rm = T)) %>%
    ungroup()
  
  flag_state_summary <- flag_states_for_selected_eez %>%
    left_join(flag_state_summary_dat, by = "flag_iso3")
  
  flag_state_summary_text <- paste0(
    "<b>", "Flag state: ", "</b>", flag_state_summary$admin,
    "<br/>",
    "<b>", "# of DW vessels: ", "</b>", flag_state_summary$n_vessels,
    "</br>",
    "<b>", "Avg. engine capacity (kW): ", "</b>", format(round(flag_state_summary$mean_engine_power, 0), big.mark = ","), 
    "</br>",
    "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_hours, 0), big.mark = ","), 
    "</br>",
    "<b>", "DW effort in selected EEZ (kW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
    lapply(htmltools::HTML)
  
  ### Interactive color palette ---
  
  # Set fill variable for map
  fill_scale <- switch(input_fill_variable,
                       "n_vessels" = list("n_vessels", region_connectivity_data$n_vessels, flag_state_summary$n_vessels),
                       "mean_engine_power" = list("mean_engine_power", region_connectivity_data$mean_engine_power, flag_state_summary$mean_engine_power),
                       "mean_tonnage" = list("mean_tonnage", region_connectivity_data$mean_tonnage, flag_state_summary$mean_tonnage),
                       "mean_length" = list("mean_length", region_connectivity_data$mean_length, flag_state_summary$mean_length),
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
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
    
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