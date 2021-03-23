
NavMap <- function(region_dat,
                   region_pal,
                   map_id,
                   min_zoom = 1){
  
  if(map_id == "south_asia_nav_map"){
    
    # Extract regional EEZs
    eezs <- region_dat$eezs %>%
      dplyr::filter(pol_type == "200NM")
    
    # Map
    leaflet(map_id, 
            options = leafletOptions(minZoom = min_zoom, zoomControl = FALSE, 
                                     attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldPhysical") %>% 
      
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
      setMaxBounds(lng1 = region_dat$map_lng - 90, 
                   lat1 = -90, 
                   lng2 = region_dat$map_lng + 90, 
                   lat2 = 90)
    
  }else {
    
    # Extract regional EEZs
    eezs <- region_dat$eezs %>%
      dplyr::filter(pol_type == "200NM")
    
    disputed <- region_dat$eezs %>%
      dplyr::filter(pol_type != "200NM")
    
    # Map
    leaflet(map_id, 
            options = leafletOptions(minZoom = min_zoom, zoomControl = FALSE, 
                                     attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%
      
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
      setMaxBounds(lng1 = region_dat$map_lng - 90, 
                   lat1 = -90, 
                   lng2 = region_dat$map_lng + 90, 
                   lat2 = 90)

  }
  
}

NavMapHighlight <- function(region_dat,
                            region_pal_light,
                            proxy_map,
                            input_selected_eez){
  
  # Get code for selected EEZ
  selected_eez <- subset(region_dat$eezs, 
                         region_dat$eezs$eez_ter_iso3 == input_selected_eez)
  
  req(nrow(selected_eez) > 0)
  
  # Remove any previously highlighted polygon
  proxy_map %>% clearGroup("highlighted_eez")
  
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
    setView(lng=mean(selected_eez$eez_lon, na.rm = T), 
            lat=mean(selected_eez$eez_lat, na.rm = T), 
            zoom=region_dat$map_zoom+1)

}