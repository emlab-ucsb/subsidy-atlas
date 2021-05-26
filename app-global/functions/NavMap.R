
NavMap <- function(region_dat,
                   region_pal,
                   map_id,
                   min_zoom = 1){
  
  if(map_id == "high_seas_nav_map"){
    
    # Extract FAO areas that should be selectable
    eezs <- region_dat$eezs
    
    # Map
    leaflet(map_id, 
            options = leafletOptions(minZoom = min_zoom, zoomControl = FALSE, 
                                     attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'bottomleft' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldPhysical") %>% 
      
      addPolygons(data = eezs,
                  fillColor = region_pal$dark_col,
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = eezs$title,
                  layerId = eezs$zone, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = region_dat$map_lng, 
              lat = region_dat$map_lat, 
              zoom = region_dat$map_zoom) %>%
      setMaxBounds(lng1 = region_dat$map_lng - 300, 
                   lat1 = -90, 
                   lng2 = region_dat$map_lng + 300, 
                   lat2 = 90)

  }else{
  
    # Get EEZs for which we have distant water fishing activity
    good_eezs <- unique(region_dat$connect$eez_ter_iso3)
    
    # Extract regional EEZs that should be selectable
    eezs <- region_dat$eezs %>%
      dplyr::filter(pol_type == "200NM" & eez_ter_iso3 %in% good_eezs)
    
    # Extract other regions & non-selectable EEZs
    disputed <- region_dat$eezs %>%
      dplyr::filter(pol_type != "200NM" | !(eez_ter_iso3 %in% good_eezs))
    
    # Map
    leaflet(map_id, 
            options = leafletOptions(minZoom = min_zoom, zoomControl = FALSE, 
                                     attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'bottomleft' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldPhysical") %>% 
      
      addPolygons(data = disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = eezs$geoname_new,
                  layerId = eezs$eez_ter_iso3, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = region_dat$map_lng, 
              lat = region_dat$map_lat, 
              zoom = region_dat$map_zoom) %>%
      setMaxBounds(lng1 = region_dat$map_lng - 270, 
                   lat1 = -90, 
                   lng2 = region_dat$map_lng + 270, 
                   lat2 = 90)
    
  }

}

NavMapHighlight <- function(region_dat,
                            region_pal_light,
                            proxy_map,
                            input_selected_eez,
                            is_hs = F){
  
  if(is_hs){
    
    # Get code for selected EEZ
    selected_eez <- subset(region_dat$eezs, 
                           region_dat$eezs$zone == input_selected_eez)
    
    req(nrow(selected_eez) > 0)
    
    # Remove any previously highlighted polygon
    proxy_map %>% clearGroup("highlighted_eez")
    
    # Check to see if there are any manual zoom/extent corrections that need to be made
    if(input_selected_eez %in% names(manual_region)){
      
      eez_manual_info <- manual_region[[input_selected_eez]]
      
      lon_map <- unname(ifelse(!is.na(eez_manual_info["lon"]), 
                               eez_manual_info["lon"],
                               mean(selected_eez$fao_lon, na.rm = T)))
      
      lat_map <- unname(ifelse(!is.na(eez_manual_info["lat"]), 
                               eez_manual_info["lat"],
                               mean(selected_eez$fao_lat, na.rm = T)))
      
      zoom_map <- unname(ifelse(!is.na(eez_manual_info["zoom"]), 
                                eez_manual_info["zoom"],
                                region_dat$map_zoom))
      
      
    }else{
      
      lon_map <- mean(selected_eez$fao_lon, na.rm = T)
      lat_map <- mean(selected_eez$fao_lat, na.rm = T)
      zoom_map <- region_dat$map_zoom
      
    }
    
    # Add a different colored polygon on top of map
    proxy_map %>% 
      addPolygons(data = selected_eez,
                  fillColor = region_pal_light$light_col,
                  fillOpacity = 1,
                  color = "white",
                  weight = 2,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  group = "highlighted_eez",
                  label = selected_eez$title,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      setView(lng=lon_map, 
              lat=lat_map, 
              zoom=zoom_map)
    
  }else{
  
  # Get code for selected EEZ
  selected_eez <- subset(region_dat$eezs, 
                         region_dat$eezs$eez_ter_iso3 == input_selected_eez)
  
  req(nrow(selected_eez) > 0)
  
  # Remove any previously highlighted polygon
  proxy_map %>% clearGroup("highlighted_eez")
  
  # Check to see if there are any manual zoom/extent corrections that need to be made
  if(input_selected_eez %in% names(manual_eez)){
    
    eez_manual_info <- manual_eez[[input_selected_eez]]
    
    lon_map <- unname(ifelse(!is.na(eez_manual_info["lon"]), 
                      eez_manual_info["lon"],
                      mean(selected_eez$eez_lon, na.rm = T)))
    
    lat_map <- unname(ifelse(!is.na(eez_manual_info["lat"]), 
                      eez_manual_info["lat"],
                      mean(selected_eez$eez_lat, na.rm = T)))
    
    zoom_map <- unname(ifelse(!is.na(eez_manual_info["zoom"]), 
                      eez_manual_info["zoom"],
                      region_dat$map_zoom + 1))
    
    
  }else{
    
    lon_map <- mean(selected_eez$eez_lon, na.rm = T)
    lat_map <- mean(selected_eez$eez_lat, na.rm = T)
    zoom_map <- region_dat$map_zoom + 1
    
  }
  
  # Add a different colored polygon on top of map
  proxy_map %>% 
    addPolygons(data = selected_eez,
                fillColor = ~ region_pal_light(region),
                fillOpacity = 1,
                color = "white",
                weight = 2,
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             fillOpacity = 1,
                                             bringToFront = TRUE),
                group = "highlighted_eez",
                label = selected_eez$geoname_new,
                labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                         padding = "3px 8px"),
                                            textsize = "13px",
                                            direction = "auto")) %>%
    setView(lng = lon_map, 
            lat = lat_map, 
            zoom = zoom_map)
  
  }

}