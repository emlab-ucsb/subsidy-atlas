### ----------------------------------------------------------------------
# 
# Distant Water Fishing Atlas - Server Logic
# This app allows users to visualize distant-water fishing effort in the EEZs of ACP countries
#
### ----------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### ------------------ 
  ### Header / Navigation -----------
  ### ------------------
  
  ### Navigation button on the top header
  observeEvent(input$ab_home, {
    updateTabItems(session, "tabs", "selectregion")
  })
  
  ### Return to regional map buttons 
  observeEvent(c(input$east_asia_pacific_return_to_region,
                input$europe_central_asia_return_to_region,
                input$latin_america_caribbean_return_to_region,
                input$middle_east_north_africa_return_to_region,
                input$north_america_return_to_region,
                input$south_asia_return_to_region,
                input$sub_saharan_africa_return_to_region), {
    updateTabItems(session, "tabs", "selectregion")
  })
  
  ### ------------------------------------------
  ### Introduction / Select a region -----------
  ### ------------------------------------------
  
  region_pal <- colorFactor(palette = "Dark2",
                            domain = eez_region_360$region,
                            na.color = "grey")
  
  region_pal_light <- colorFactor(palette = "Set2",
                                  domain = eez_region_360$region,
                                  na.color = "grey"
  )
  
  ## Leaflet output: map of ACP countries aggregated by region -------------
  
  output$regional_map <- renderLeaflet({
    
    ## Get regional dat
    regional_dat <- eez_region_360
    
    # Formatting for semi-transparent title box over map
    intro_overlay_formatting <- tags$style(HTML("
  .leaflet-control.map-title {

    transform: translate(0, 10%);
    position: fixed !important;
    left: 10%;
    right: 10%;
    text-align: center;
    padding: 20px;
    background: rgba(255,255,255,0.6);
    color:black;

    -webkit-box-shadow: 0px 2px 2px 0px rgba(0, 0, 0, 0.5);
    box-shadow: 0px 2px 2px 0px rgba(0, 0, 0, 0.5);
  }
"))
    
    # Load text for title box
    intro_top_overlay_text <- includeHTML("./text/01_intro_overlay.html")
    
    # Combine formatting and text for semi-transparent title box over map
    intro_overlay <- tags$div(
      intro_overlay_formatting, 
      intro_top_overlay_text)
      
    # Leaflet map
    leaflet("regional_map", 
            options = leafletOptions(minZoom = 2, zoomControl = FALSE,
                                     attributionControl = FALSE)) %>%
      
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      # Title box
      addControl(intro_overlay, 
                 position = "topleft", 
                 className = "map-title") %>%
       
      addPolygons(data = regional_dat, 
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = regional_dat$region,
                  group = regional_dat$region) %>%
      setView(0, 20, zoom = 2.5) %>%
      setMaxBounds(lng1 = -270, lat1 = -90, lng2 = 270, lat2 = 90)
      
  })
  
  
  ### Based on where the user clicks on regional map, change tab
  observeEvent(input$regional_map_shape_click, {
    
    tab_navigation <- switch(input$regional_map_shape_click$group,
                             "East Asia & Pacific" = list("east-asia-pacific"),
                             "Europe & Central Asia" = list("europe-central-asia"),
                             "Latin America & Caribbean" = list("latin-america-caribbean"),
                             "Middle East & North Africa" = list("middle-east-north-africa"),
                             "North America" = list("north-america"),
                             "South Asia" = list("south-asia"),
                             "Sub-Saharan Africa" = list("sub-saharan-africa"))
    
    updateTabItems(session, "tabs", tab_navigation[[1]])
    
  })
  
  ###------------------------------------------------------------------
  ### East Asia & Pacific ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$east_asia_pacific_nav_map <- renderLeaflet({
    
    # Extract East Asia & Pacific EEZs
    east_asia_pacific_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "East Asia & Pacific") %>%
      dplyr::filter(pol_type == "200NM")
    
    east_asia_pacific_disputed <- eez_ter_360 %>%
      dplyr::filter(region == "East Asia & Pacific") %>%
      dplyr::filter(pol_type != "200NM")
    
    # # Merge non-contiguous EEZs for the same coastal state (South Africa)
    # africa_eezs_merged <- africa_eezs %>%
    #   group_by(iso_ter, geoname, region) %>%
    #   summarize(geometry = st_union(geometry))
    
    # Also extract disputed areas/joint management areas involving ACP coastal states in Africa
    # africa_disputed_joint <- africa_eez_map %>%
    #   dplyr::filter(pol_type != "200NM" & iso_ter %in% africa_eezs$iso_ter) %>%
    #   mutate(region = "Africa")
    
    # Map
    leaflet('east_asia_pacific_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = east_asia_pacific_disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = east_asia_pacific_disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = east_asia_pacific_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = east_asia_pacific_eezs$geoname_new,
                  layerId = east_asia_pacific_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng= 175, lat = -5, zoom = 1) %>%
      setMaxBounds(lng1 = 90, lat1 = -90, lng2 = 270, lat2 = 90)
    
})
  
  ### Update selectInput: Register user clicks on nav map ---------

  observeEvent(input$east_asia_pacific_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$east_asia_pacific_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "east_asia_pacific_eez_select",
                         choices = east_asia_pacific_eezs,
                         selected = input$east_asia_pacific_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------

  east_asia_pacific_nav_map_proxy <- leafletProxy("east_asia_pacific_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom

  observeEvent(input$east_asia_pacific_eez_select, {
    
    if(input$east_asia_pacific_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      east_asia_pacific_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      east_asia_pacific_nav_map_proxy %>% setView(lng= 175, lat = -5, zoom = 1)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$east_asia_pacific_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()

      # Remove any previously highlighted polygon
      east_asia_pacific_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      east_asia_pacific_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ###------------------------------------------------------------------
  ### Europe & Central Asia ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$europe_central_asia_nav_map <- renderLeaflet({
    
    # Extract Europe & Central Asia EEZs
    europe_central_asia_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "Europe & Central Asia") %>%
      dplyr::filter(pol_type == "200NM")
    
    europe_central_asia_disputed <- eez_ter_360 %>%
      dplyr::filter(region == "Europe & Central Asia") %>%
      dplyr::filter(pol_type != "200NM")

    # Map
    leaflet('europe_central_asia_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = europe_central_asia_disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = europe_central_asia_disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = europe_central_asia_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = europe_central_asia_eezs$geoname_new,
                  layerId = europe_central_asia_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng= 0, lat = 40, zoom = 2) %>%
      setMaxBounds(lng1 = -90, lat1 = -90, lng2 = 90, lat2 = 90)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  
  observeEvent(input$europe_central_asia_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$europe_central_asia_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "europe_central_asia_eez_select",
                         choices = europe_central_asia_eezs,
                         selected = input$europe_central_asia_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  
  europe_central_asia_nav_map_proxy <- leafletProxy("europe_central_asia_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom --------------
  
  observeEvent(input$europe_central_asia_eez_select, {
    
    if(input$europe_central_asia_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      europe_central_asia_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      europe_central_asia_nav_map_proxy %>% setView(lng= 0, lat = 40, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$europe_central_asia_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()
      
      # Remove any previously highlighted polygon
      europe_central_asia_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      europe_central_asia_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ###------------------------------------------------------------------
  ### Latin America & Caribbean ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$latin_america_caribbean_nav_map <- renderLeaflet({
    
    # Extract Latin America & Caribbean EEZs
    latin_america_caribbean_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "Latin America & Caribbean") %>%
      dplyr::filter(pol_type == "200NM")
    
    latin_america_caribbean_disputed <- eez_ter_360 %>%
      dplyr::filter(region == "Latin America & Caribbean") %>%
      dplyr::filter(pol_type != "200NM")
    
    # Map
    leaflet('latin_america_caribbean_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = latin_america_caribbean_disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = latin_america_caribbean_disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = latin_america_caribbean_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = latin_america_caribbean_eezs$geoname_new,
                  layerId = latin_america_caribbean_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = -70, lat = -15, zoom = 2) %>%
      setMaxBounds(lng1 = -160, lat1 = -90, lng2 = 20, lat2 = 90)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  
  observeEvent(input$latin_america_caribbean_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$latin_america_caribbean_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "latin_america_caribbean_eez_select",
                         choices = latin_america_caribbean_eezs,
                         selected = input$latin_america_caribbean_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  
  latin_america_caribbean_nav_map_proxy <- leafletProxy("latin_america_caribbean_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom --------------
  
  observeEvent(input$latin_america_caribbean_eez_select, {
    
    if(input$latin_america_caribbean_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      latin_america_caribbean_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      latin_america_caribbean_nav_map_proxy %>% setView(lng = -70, lat = -15, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$latin_america_caribbean_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()
      
      # Remove any previously highlighted polygon
      latin_america_caribbean_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      latin_america_caribbean_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ###------------------------------------------------------------------
  ### Middle East & North Africa ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$middle_east_north_africa_nav_map <- renderLeaflet({
    
    # Extract Middle East & North Africa EEZs
    middle_east_north_africa_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "Middle East & North Africa") %>%
      dplyr::filter(pol_type == "200NM")
    
    middle_east_north_africa_disputed <- eez_ter_360 %>%
      dplyr::filter(region == "Middle East & North Africa") %>%
      dplyr::filter(pol_type != "200NM")
    
    # Map
    leaflet('middle_east_north_africa_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = middle_east_north_africa_disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = middle_east_north_africa_disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = middle_east_north_africa_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = middle_east_north_africa_eezs$geoname_new,
                  layerId = middle_east_north_africa_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = 30, lat = 0, zoom = 2) %>%
      setMaxBounds(lng1 = -60, lat1 = -90, lng2 = 120, lat2 = 90)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  
  observeEvent(input$middle_east_north_africa_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$middle_east_north_africa_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "middle_east_north_africa_eez_select",
                         choices = middle_east_north_africa_eezs,
                         selected = input$middle_east_north_africa_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  
  middle_east_north_africa_nav_map_proxy <- leafletProxy("middle_east_north_africa_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom --------------
  
  observeEvent(input$middle_east_north_africa_eez_select, {
    
    if(input$middle_east_north_africa_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      middle_east_north_africa_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      middle_east_north_africa_nav_map_proxy %>% setView(lng = 30, lat = 0, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$middle_east_north_africa_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()
      
      # Remove any previously highlighted polygon
      middle_east_north_africa_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      middle_east_north_africa_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ###------------------------------------------------------------------
  ### North America ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$north_america_nav_map <- renderLeaflet({
    
    # Extract North America EEZs
    north_america_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "North America") %>%
      dplyr::filter(pol_type == "200NM")
    
    north_america_disputed <- eez_ter_360 %>%
      dplyr::filter(region == "North America") %>%
      dplyr::filter(pol_type != "200NM")
    
    # Map
    leaflet('north_america_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = north_america_disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = north_america_disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = north_america_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = north_america_eezs$geoname_new,
                  layerId = north_america_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = -90, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 0, lat2 = 90)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  
  observeEvent(input$north_america_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$north_america_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "north_america_eez_select",
                         choices = north_america_eezs,
                         selected = input$north_america_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  
  north_america_nav_map_proxy <- leafletProxy("north_america_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom --------------
  
  observeEvent(input$north_america_eez_select, {
    
    if(input$north_america_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      north_america_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      north_america_nav_map_proxy %>% setView(lng = -90, lat = 20, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$north_america_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()
      
      # Remove any previously highlighted polygon
      north_america_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      north_america_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ###------------------------------------------------------------------
  ### South Asia ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$south_asia_nav_map <- renderLeaflet({
    
    # Extract South Asia EEZs
    south_asia_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "South Asia") %>%
      dplyr::filter(pol_type == "200NM")
    
    # south_asia_disputed <- eez_ter_360 %>%
    #   dplyr::filter(region == "South Asia") %>%
    #   dplyr::filter(pol_type != "200NM")
    
    # Map
    leaflet('south_asia_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      # addPolygons(data = south_asia_disputed, 
      #             fillColor = "grey",
      #             fillOpacity = 0.8,
      #             color= "white",
      #             weight = 0.3,
      #             highlight = highlightOptions(weight = 5,
      #                                          color = "#666",
      #                                          fillOpacity = 1,
      #                                          bringToFront = TRUE),
      #             label = south_asia_disputed$geoname_new,
      #             layerId = NULL, #need this to select input below
      #             labelOptions = labelOptions(style = list("font-weight" = "normal",
      #                                                      padding = "3px 8px"),
      #                                         textsize = "13px",
      #                                         direction = "auto")
      # ) %>%
      
      addPolygons(data = south_asia_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = south_asia_eezs$geoname_new,
                  layerId = south_asia_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = 70, lat = 0, zoom = 3) %>%
      setMaxBounds(lng1 = -20, lat1 = -90, lng2 = 160, lat2 = 90)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  
  observeEvent(input$south_asia_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$south_asia_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "south_asia_eez_select",
                         choices = south_asia_eezs,
                         selected = input$south_asia_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  
  south_asia_nav_map_proxy <- leafletProxy("south_asia_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom --------------
  
  observeEvent(input$south_asia_eez_select, {
    
    if(input$south_asia_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      south_asia_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      south_asia_nav_map_proxy %>% setView(lng = 70, lat = 0, zoom = 3) 
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$south_asia_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()
      
      # Remove any previously highlighted polygon
      south_asia_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      south_asia_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ###------------------------------------------------------------------
  ### Sub-Saharan Africa ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Leaflet output: Navigational map for the region ---------
  
  output$sub_saharan_africa_nav_map <- renderLeaflet({
    
    # Extract Sub-Saharan Africa EEZs
    sub_saharan_africa_eezs <- eez_ter_360 %>%
      dplyr::filter(region == "Sub-Saharan Africa") %>%
      dplyr::filter(pol_type == "200NM")
    
    sub_saharan_africa_disputed <- eez_ter_360 %>%
      dplyr::filter(region == "Sub-Saharan Africa") %>%
      dplyr::filter(pol_type != "200NM")
    
    # Map
    leaflet('sub_saharan_africa_nav_map', 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE, attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = sub_saharan_africa_disputed, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = sub_saharan_africa_disputed$geoname_new,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = sub_saharan_africa_eezs,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = sub_saharan_africa_eezs$geoname_new,
                  layerId = sub_saharan_africa_eezs$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = 10, lat = -20, zoom = 2) %>%
      setMaxBounds(lng1 = -85, lat1 = -90, lng2 = 95, lat2 = 90)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  
  observeEvent(input$sub_saharan_africa_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$sub_saharan_africa_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "sub_saharan_africa_eez_select",
                         choices = sub_saharan_africa_eezs,
                         selected = input$sub_saharan_africa_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  
  sub_saharan_africa_nav_map_proxy <- leafletProxy("sub_saharan_africa_nav_map")
  
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom --------------
  
  observeEvent(input$sub_saharan_africa_eez_select, {
    
    if(input$sub_saharan_africa_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      sub_saharan_africa_nav_map_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      sub_saharan_africa_nav_map_proxy %>% setView(lng = 10, lat = -20, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_ter_360, 
                             eez_ter_360$iso_ter == input$sub_saharan_africa_eez_select)
      
      # selected_eez_centroid <- st_centroid(selected_eez) %>%
      #   st_coordinates()
      
      # Remove any previously highlighted polygon
      sub_saharan_africa_nav_map_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      sub_saharan_africa_nav_map_proxy %>% 
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
                                                direction = "auto")) 
      
      # %>%
      # setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  ### -------------------------------------------------------------------------
  
  
  ### -----
  ### UI output: Links and summary statistics for the selected ACP Africa state
  ### -----
  
  output$africa_country_profile <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")
    
    ### Data wrangling
    # Filter connectivity data
    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$africa_eez_select) %>%
      mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
    
    # Distant water fishing summary
    total_stats_africa <- connectivity_data_filter_africa %>%
      as.data.frame() %>%
      group_by(eez_territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T)) %>%
      arrange(eez_territory_iso3)
    

    # Filter and format Country profile data
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$africa_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>%
      separate_rows(fao_memberships, sep = ",")
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    ### Make HTML sections with links for each type of information
    # Fisheries management agency
    fisheries_mgmt_agency <- ifelse(
      length(unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])) > 0,
      # Create link if we have one
      paste0("<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "' target='_blank'>", ACP_codes_links$fishery_org_eng, "</a>"),
      # Otherwise just paste name of the agency
      paste0(ACP_codes_links$fishery_org_eng)
    )
    
    # FAO country profile
    country_profiles <- paste0(
      # FAO
      "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "' target='_blank'>", "FAO", "</a>", " | ",
      # World Bank
      ifelse(
        length(unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)])) > 0,
        paste0("<a href='", unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)]), "' target='_blank'>", "World Bank", "</a>"),
        "World Bank"
      ), " | ",
      # UN
      ifelse(
        length(unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)])) > 0,
        paste0("<a href='", unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)]), "' target='_blank'>", "United Nations", "</a>"),
        "United Nations"
      )
    ) # close paste
    
    
    # Treaties and Conventions
    treaties_conventions <- paste0(
      "<a href= '",
      unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]),
      "' target='_blank'>",
      unique(ACP_codes_links$territory[!is.na(ACP_codes_links$treaties_conventions)]),
      "</a>",
      collapse = " | "
    )
    
    # Foreign access agreements by EEZ sector
    foreign_access_agreements <- paste0(
      "<a href= '", 
      unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
      "' target='_blank'>", 
      unique(ACP_codes_links$territory[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
      "</a>", 
      collapse = " | ")
    
    # FAO Regional Fisheries Body Memberships
    regional_body_memberships <- paste0(
      "<a href= '", 
      unique(RFMO_links_eez$link[!is.na(RFMO_links_eez$link)]),
      "' target='_blank'>",
      unique(RFMO_links_eez$rfmo_name[!is.na(RFMO_links_eez$link)]),
      "</a>",
      collapse = " | ")
    
    ### Combine into country profile/summary of DW fishing
    EEZ_info <- paste0("<h3 style = 'margin-top: 0px;'>", names(africa_eez_choices[africa_eez_choices == input$africa_eez_select]), "</h3>",
                       
                       "Fisheries management agency:  ", 
                       fisheries_mgmt_agency,
                       "<br>",
                       
                       "Country profile: ",
                       country_profiles,
                       "<br>",
                       
                       "Treaties and conventions: ",
                       treaties_conventions,
                       "<br>",
                       
                       "Foreign access agreements: ",
                       foreign_access_agreements,
                       "<br>",
                       
                       "FAO Regional Fisheries Body Memberships: ",
                       regional_body_memberships,
                       "<br>",
                       
                       "<hr>",
                       "<b>", "AIS-observed distant water fishing in the ", total_stats_africa$eez_nam, " (2018)", "</b>",
                       "<br>",
                       "Vessels: ", format(round(total_stats_africa$vessels, 0), big.mark = ","),
                       "<br>",
                       "Total engine capacity (KW): ", format(round(total_stats_africa$capacity, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (hours): ", format(round(total_stats_africa$fishing_h, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (KWh): ", format(round(total_stats_africa$fishing_KWh, 0), big.mark = ",")) %>%
      lapply(htmltools::HTML)
    
    # Return
    EEZ_info
    
  })
  
  
  ### -----
  ### Leaflet output: Connectivity map for selected ACP Africa state
  ### -----
  
  output$africa_connection_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$africa_eez_select != "Select a coastal state...")
    
    # Connectivity data for the entire region
    connectivity_data_region <- connectivity_data %>% # load this in up above
      dplyr::filter(region == "Africa")

    ### Selected ACP coastal state ---
    
    # Selected Africa ACP coastal state
    selected_eez <- africa_eez_map %>% 
      dplyr::filter(iso_ter == input$africa_eez_select) %>%
      rename(territory_iso3 = iso_ter)
    
    # Connectivity data for the EEZ of the selected ACP coastal state
    connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$africa_eez_select) %>% 
      arrange(flag)%>%
      dplyr::filter(flag != "UNK")
    
    ### Polygons of flag states --- 
    
    # Get polygons of flag states
    flag_states_for_selected_eez <- africa_land_map %>%
      dplyr::filter(admin_iso3 %in% connectivity_data_for_selected_eez$flag) %>%
      rename(flag = admin_iso3) %>% 
      arrange(flag)

    # Get polygons of flag state boundaries merged with EEZ boundaries
    flag_states_combined_for_selected_eez <- africa_land_eez_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    
    # if(all(ifelse(unique(flag_states_for_selected_eez$flag) == unique(flag_states_combined_for_selected_eez$flag), TRUE, FALSE)) == F){
    #   warning("Check flag states")
    #   
    # }
      
    # Connectivity stats with no geometry
    connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
      group_by(eez_territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T))
    st_geometry(connectivity_data_no_geometry) <- NULL
    
    #  Create summary polygons with hover text for flag states
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag")
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary$display_name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    #  Create summary polygons with hover text for flag states merged with EEZ boundaries
    flag_state_summary_combined <- flag_states_combined_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag")
    
    flag_state_summary_combined_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary_combined$display_name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary_combined$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary_combined$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary_combined$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary_combined$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    ### Interactive color palette ---
    
    # Set fill variable for map
    fill_scale <- switch(input$africa_connection_fill,
                         "# of Different Vessels" = list("vessels", connectivity_data_region$vessels, flag_state_summary$vessels),
                         "Total Engine Capacity (KW)" = list("capacity", connectivity_data_region$capacity, flag_state_summary$capacity),
                         "Total Fishing Effort (hours)" = list("fishing_h", connectivity_data_region$fishing_h, flag_state_summary$fishing_h),
                         "Total Fishing Effort (KWh)" = list("fishing_KWh", connectivity_data_region$fishing_KWh, flag_state_summary$fishing_KWh))
    
    # Make color palette
    domain <- switch(input$africa_connection_fill_rescale,
                     "All distant water fishing in the region (default)" = 2,
                     "Selected EEZ only" = 3)
    
    pal <- colorBin("YlOrRd", domain = fill_scale[[domain]], bins = 7)
    
    
    ### Leaflet map ---
    
    leaflet('africa_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
      
      addPolygons(data = flag_state_summary_combined,
                  fillColor = ~pal(get(fill_scale[[1]])),
                  fillOpacity = 0.3,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = FALSE),
                  label = flag_state_summary_combined_text,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
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
                  layerId = flag_state_summary$flag,
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
                  label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
      addPolylines(data = connectivity_data_for_selected_eez,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod",
                   group = "lines") %>% 
      
      addLegend(pal = pal, values = fill_scale[[domain]], 
                opacity=0.9, title = input$africa_connection_fill, position = "bottomleft" ) %>%
      setView(lng= 15, lat = 0, zoom = 2)
    
})
  
  ### -----
  ### Update tab and selectInput: Register clicks on Africa connectivity map and change tab ans flag state input widgets accordingly
  ### -----
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$africa_connection_map_shape_click, {
    
    req(input$africa_connection_map_shape_click$id != input$africa_eez_select)
    
    updateTabItems(session, "africa_tabs", "Distant water fishing effort")
    
    updateSelectizeInput(session, "africa_flag_state_select_effort",
                         selected = input$africa_connection_map_shape_click$id
    )
    
    updateSelectizeInput(session, "africa_flag_state_select_subsidy",
                         selected = input$africa_connection_map_shape_click$id
    )
    
  })
  
  ### -----
  ### Reactive DF: Load 0.1 x 0.1 degree effort/subsidy data for selected ACP Africa state
  ### -----
  
  africa_eez_data <- eventReactive(input$africa_eez_select, {
    
    # Require EEZ selection
    req(input$africa_eez_select != "Select a coastal state...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$africa_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(africa_eez_choices[africa_eez_choices == input$africa_eez_select])) %>%
      arrange(name)
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -----
  ### Update selectInput: Filter possible flag state selections based on selected ACP Africa state
  ### -----
  
  observeEvent(input$africa_eez_select, {
    
    req(input$africa_eez_select != "Select a coastal state...")
    
    flag_state_choices_africa <- unique(africa_eez_data()$flag)
    names(flag_state_choices_africa) <- unique(africa_eez_data()$name)
    
    updateSelectizeInput(session, "africa_flag_state_select_effort",
                         choices = c("Select a flag state...", flag_state_choices_africa))
    
    updateSelectizeInput(session, "africa_flag_state_select_subsidy",
                         choices = c("Select a flag state...", flag_state_choices_africa))
    
  })
  
  ### -----
  ### Reactive object: bounding box coordinates for the EEZ of the selected ACP Africa state
  ### -----
  
  africa_eez_bbox <- eventReactive(input$africa_eez_select, {
    
    eez_map_subsidy <- africa_eez_map %>%
      dplyr::filter(iso_ter == input$africa_eez_select) %>%
      group_by(iso_ter) %>%
      summarize(geometry = st_union(geometry))
    
    st_bbox(eez_map_subsidy)
    
  })
  
  ### -----
  ### UI output: summary stats for effort heat map (selected flag state only) for the selected ACP Africa state
  ### -----
  
  output$africa_effort_summary_all <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
    eez_plot_data <- africa_eez_data() %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T))
    
    effort_summary <- paste0(
      "<b>", "Total DW effort (KWh): ", "</b>", format(round(eez_plot_data$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    effort_summary
    
  })
  
  ### -----
  ### Plot output: effort heat map (all flag states) for the selected ACP Africa state
  ### -----
  
  output$africa_effort_map_all <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
    ### Get totals for data
    eez_totals <- africa_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(africa_eez_bbox()$xmin - 0.5, africa_eez_bbox()$xmax + 0.5)
    y_lim <- c(africa_eez_bbox()$ymin - 0.5, africa_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- eez_totals
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for effort heat map (selected flag state only) for the selected ACP Africa state
  ### -----
  
  output$africa_effort_summary <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    req(input$africa_flag_state_select_effort != "Select a flag state...")
    
    eez_plot_data <- africa_eez_data() %>%
      dplyr::filter(flag == input$africa_flag_state_select_effort) %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T))
    
    effort_summary <- paste0(
      "<b>", "Selected flag state DW effort (KWh): ", "</b>", format(round(eez_plot_data$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    effort_summary
    
  })
  
  ### -----
  ### Plot output: effort heat map (selected flag state only) for the selected ACP Africa state
  ### -----
  
  output$africa_effort_map <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    req(input$africa_flag_state_select_effort != "Select a flag state...")
    
    ### Get limits for map area
    x_lim <- c(africa_eez_bbox()$xmin - 0.5, africa_eez_bbox()$xmax + 0.5)
    y_lim <- c(africa_eez_bbox()$ymin - 0.5, africa_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- africa_eez_data() %>%
      dplyr::filter(flag == input$africa_flag_state_select_effort) %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = africa_eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for subsidy heat map (all flag states) for the selected ACP Africa state
  ### -----
  
  output$africa_subsidy_summary_all <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
    eez_plot_data <- africa_eez_data() %>%
      summarize(subs = sum(subs, na.rm = T))
    
    subsidy_summary <- paste0(
      "<b>", "Estimate of total DW subsidies (US$): ", "</b>", format(round(eez_plot_data$subs, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    subsidy_summary
    
  })
  
  
  ### -----
  ### Plot output: subsidy heat map (all flag states) for the selected ACP Africa state
  ### -----
  
  output$africa_subsidy_map_all <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
    # Get totals for data
    eez_totals <- africa_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Set limits for map area
    x_lim <- c(africa_eez_bbox()$xmin - 0.5, africa_eez_bbox()$xmax + 0.5)
    y_lim <- c(africa_eez_bbox()$ymin - 0.5, africa_eez_bbox()$ymax + 0.5)
    
    # Define plot data  
    eez_plot_data <- eez_totals
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
      scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for subsidy heat map (selected flag state only) for the selected ACP Africa state
  ### -----
  
  output$africa_subsidy_summary <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    req(input$africa_flag_state_select_subsidy != "Select a flag state...")
    
    eez_plot_data <- africa_eez_data() %>%
      dplyr::filter(flag == input$africa_flag_state_select_subsidy) %>%
      summarize(subs = sum(subs, na.rm = T))
    
    subsidy_summary <- paste0(
      "<b>", "Estimate of selected flag state DW subsidies (US$): ", "</b>", format(round(eez_plot_data$subs, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    subsidy_summary
    
  })
  
  
  ### -----
  ### Plot output: subsidy heat map (selected flag state only) for the selected ACP Africa state
  ### -----
  
  output$africa_subsidy_map <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    req(input$africa_flag_state_select_subsidy != "Select a flag state...")
    
    ### Get limits for map area
    x_lim <- c(africa_eez_bbox()$xmin - 0.5, africa_eez_bbox()$xmax + 0.5)
    y_lim <- c(africa_eez_bbox()$ymin - 0.5, africa_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- africa_eez_data() %>%
      dplyr::filter(flag == input$africa_flag_state_select_subsidy) %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
      scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ###--------------------------------------------------------------------------------------------
  ### Caribbean ---------------------------------------------------------------------------------
  ###--------------------------------------------------------------------------------------------
  
  ### -----
  ### Leaflet output: Map of Caribbean ACP EEZs for which we have DW fishing effort
  ### -----
  output$caribbean_map <- renderLeaflet({
    
    # Extract ACP EEZs
    caribbean_eezs <- caribbean_eez_map %>%
      dplyr::filter(region == "Caribbean") %>%
      mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
    
    # Merge non-contiguous EEZs for the same coastal state (South Caribbean)
    caribbean_eezs_merged <- caribbean_eezs %>%
      group_by(iso_ter, geoname, region) %>%
      summarize(geometry = st_union(geometry))
    
    # Also extract disputed areas/joint management areas involving ACP coastal states in Caribbean
    caribbean_disputed_joint <- caribbean_eez_map %>%
      dplyr::filter(pol_type != "200NM" & iso_ter %in% caribbean_eezs$iso_ter) %>%
      mutate(region = "Caribbean")
    
    # Map
    leaflet('caribbean_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = caribbean_disputed_joint, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = caribbean_disputed_joint$geoname,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = caribbean_eezs_merged,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = caribbean_eezs_merged$geoname,
                  layerId = caribbean_eezs_merged$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng= -75, lat = 17, zoom = 3)
    
})
  
  ### -----
  ### Update selectInput: Register user clicks on Caribbean map - change selected value of widget
  ### -----
  
  observeEvent(input$caribbean_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$caribbean_map_shape_click$id))
    
    updateSelectizeInput(session, "caribbean_eez_select",
                         selected = input$caribbean_map_shape_click$id)
    
  })
  
  ### -----
  ### Leaflet proxy: Create proxy for the Caribbean map of ACP countries
  ### -----
  
  caribbean_proxy <- leafletProxy("caribbean_map")
  
  ### -----
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  ### -----

  observeEvent(input$caribbean_eez_select, {
    
    if(input$caribbean_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      caribbean_proxy %>% setView(lng= -75, lat = 17, zoom = 3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(caribbean_eez_map, caribbean_eez_map$iso_ter == input$caribbean_eez_select)
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      caribbean_proxy %>% addPolygons(data = selected_eez,
                                   fillColor = ~region_pal_light(region),
                                   fillOpacity = 1,
                                   color= "white",
                                   weight = 2,
                                   highlight = highlightOptions(weight = 5,
                                                                color = "#666",
                                                                fillOpacity = 1,
                                                                bringToFront = TRUE),
                                   group = "highlighted_eez",
                                   label = selected_eez$geoname,
                                   labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                            padding = "3px 8px"),
                                                               textsize = "13px",
                                                               direction = "auto")) %>%
        setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=4)
    }
    
  }) # close observe event
  
  
  ### -----
  ### UI output: Links and summary statistics for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_country_profile <- renderUI({
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    ### Data wrangling
    # Filter connectivity data
    connectivity_data_filter_caribbean <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$caribbean_eez_select) %>%
      mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
    
    # Distant water fishing summary
    total_stats_caribbean <- connectivity_data_filter_caribbean %>%
      as.data.frame() %>%
      group_by(eez_territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T)) %>%
      arrange(eez_territory_iso3)
    
    # Filter and format Country profile data
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$caribbean_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>%
      separate_rows(fao_memberships, sep = ",")
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    ### Make HTML sections with links for each type of information
    # Fisheries management agency
    fisheries_mgmt_agency <- ifelse(
      length(unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])) > 0,
      # Create link if we have one
      paste0("<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "' target='_blank'>", ACP_codes_links$fishery_org_eng, "</a>"),
      # Otherwise just paste name of the agency
      paste0(ACP_codes_links$fishery_org_eng)
    )
    
    # FAO country profile
    country_profiles <- paste0(
      # FAO
    "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "' target='_blank'>", "FAO", "</a>", " | ",
    # World Bank
    ifelse(
      length(unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)])) > 0,
      paste0("<a href='", unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)]), "' target='_blank'>", "World Bank", "</a>"),
      "World Bank"
      ), " | ",
    # UN
    ifelse(
      length(unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)])) > 0,
      paste0("<a href='", unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)]), "' target='_blank'>", "United Nations", "</a>"),
    "United Nations"
    )
    ) # close paste
    
    
    # Treaties and Conventions
    treaties_conventions <- paste0(
      "<a href= '",
      unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]),
      "' target='_blank'>",
      unique(ACP_codes_links$territory[!is.na(ACP_codes_links$treaties_conventions)]),
      "</a>",
      collapse = " | "
    )
    
    # Foreign access agreements by EEZ sector
    foreign_access_agreements <- paste0(
      "<a href= '", 
      unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
      "' target='_blank'>", 
      unique(ACP_codes_links$territory[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
      "</a>", 
      collapse = " | ")
    
    # FAO Regional Fisheries Body Memberships
    regional_body_memberships <- paste0(
      "<a href= '", 
      unique(RFMO_links_eez$link[!is.na(RFMO_links_eez$link)]),
      "' target='_blank'>",
      unique(RFMO_links_eez$rfmo_name[!is.na(RFMO_links_eez$link)]),
      "</a>",
      collapse = " | ")
    
    ### Combine into country profile/summary of DW fishing
    EEZ_info <- paste0("<h3 style = 'margin-top: 0px;'>", names(caribbean_eez_choices[caribbean_eez_choices == input$caribbean_eez_select]), "</h3>",
                       
                       "Fisheries management agency:  ", 
                       fisheries_mgmt_agency,
                       "<br>",
                       
                       "Country profile: ",
                       country_profiles,
                       "<br>",
                       
                       "Treaties and conventions: ",
                       treaties_conventions,
                       "<br>",
                       
                       "Foreign access agreements: ",
                       foreign_access_agreements,
                       "<br>",
                       
                       "FAO Regional Fisheries Body Memberships: ",
                       regional_body_memberships,
                       "<br>",
                       
                       "<hr>",
                       "<b>", "AIS-observed distant water fishing in the ", total_stats_caribbean$eez_nam, " (2018)", "</b>",
                       "<br>",
                       "Vessels: ", format(round(total_stats_caribbean$vessels, 0), big.mark = ","),
                       "<br>",
                       "Total engine capacity (KW): ", format(round(total_stats_caribbean$capacity, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (hours): ", format(round(total_stats_caribbean$fishing_h, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (KWh): ", format(round(total_stats_caribbean$fishing_KWh, 0), big.mark = ",")) %>%
      lapply(htmltools::HTML)
    
    # Return
    EEZ_info
    
  })
  
  ### -----
  ### Leaflet output: Connectivity map for selected ACP Caribbean state
  ### -----
  
  output$caribbean_connection_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    # Connectivity data for the entire region
    connectivity_data_region <- connectivity_data %>% # load this in up above
      dplyr::filter(region == "Caribbean")
    
    ### Selected ACP coastal state ---
    
    # Selected Caribbean ACP coastal state
    selected_eez <- caribbean_eez_map %>% 
      dplyr::filter(iso_ter == input$caribbean_eez_select) %>%
      rename(territory_iso3 = iso_ter)
    
    # Connectivity data for the EEZ of the selected ACP coastal state
    connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$caribbean_eez_select) %>% 
      arrange(flag)%>%
      dplyr::filter(flag != "UNK")
    
    ### Polygons of flag states --- 
    
    # Get polygons of flag states
    flag_states_for_selected_eez <- caribbean_land_map %>%
      dplyr::filter(admin_iso3 %in% connectivity_data_for_selected_eez$flag) %>%
      rename(flag = admin_iso3) %>% 
      arrange(flag)
    
    # Get polygons of flag state boundaries merged with EEZ boundaries
    flag_states_combined_for_selected_eez <- caribbean_land_eez_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    
    # if(all(ifelse(unique(flag_states_for_selected_eez$flag) == unique(flag_states_combined_for_selected_eez$flag), TRUE, FALSE)) == F){
    #   warning("Check flag states")
    #   
    # }
    
    # Connectivity stats with no geometry
    connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
      group_by(eez_territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T))
    st_geometry(connectivity_data_no_geometry) <- NULL
    
    #  Create summary polygons with hover text for flag states
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag")
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary$display_name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    #  Create summary polygons with hover text for flag states merged with EEZ boundaries
    flag_state_summary_combined <- flag_states_combined_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag")
    
    flag_state_summary_combined_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary_combined$display_name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary_combined$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary_combined$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary_combined$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary_combined$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    ### Interactive color palette ---
    
    # Set fill variable for map
    fill_scale <- switch(input$caribbean_connection_fill,
                         "# of Different Vessels" = list("vessels", connectivity_data_region$vessels, flag_state_summary$vessels),
                         "Total Engine Capacity (KW)" = list("capacity", connectivity_data_region$capacity, flag_state_summary$capacity),
                         "Total Fishing Effort (hours)" = list("fishing_h", connectivity_data_region$fishing_h, flag_state_summary$fishing_h),
                         "Total Fishing Effort (KWh)" = list("fishing_KWh", connectivity_data_region$fishing_KWh, flag_state_summary$fishing_KWh))
    
    # Make color palette
    domain <- switch(input$caribbean_connection_fill_rescale,
                     "All distant water fishing in the region (default)" = 2,
                     "Selected EEZ only" = 3)
    
    pal <- colorBin("YlOrRd", domain = fill_scale[[domain]], bins = 7)
    
    
    ### Leaflet map ---
    
    leaflet('caribbean_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
      
      addPolygons(data = flag_state_summary_combined,
                  fillColor = ~pal(get(fill_scale[[1]])),
                  fillOpacity = 0.3,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = FALSE),
                  label = flag_state_summary_combined_text,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
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
                  layerId = flag_state_summary$flag,
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
                  label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
      addPolylines(data = connectivity_data_for_selected_eez,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod",
                   group = "lines") %>% 
      
      addLegend(pal = pal, values = fill_scale[[domain]], 
                opacity=0.9, title = input$caribbean_connection_fill, position = "bottomleft" ) %>%
      setView(lng= -75, lat = 0, zoom = 2)
    
})
  
  ### -----
  ### Update tab and selectInput: Register clicks on Caribbean connectivity map and change tab ans flag state input widgets accordingly
  ### -----
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$caribbean_connection_map_shape_click, {
    
    req(input$caribbean_connection_map_shape_click$id != input$caribbean_eez_select)
    
    updateTabItems(session, "caribbean_tabs", "Distant water fishing effort")
    
    updateSelectizeInput(session, "caribbean_flag_state_select_effort",
                         selected = input$caribbean_connection_map_shape_click$id
    )
    
    updateSelectizeInput(session, "caribbean_flag_state_select_subsidy",
                         selected = input$caribbean_connection_map_shape_click$id
    )
    
  })
  
  ### -----
  ### Reactive DF: Load 0.1 x 0.1 degree effort/subsidy data for selected ACP Caribbean state
  ### -----
  
  caribbean_eez_data <- eventReactive(input$caribbean_eez_select, {
    
    # Require EEZ selection
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$caribbean_eez_select]
    
    if(length(matching_file) >= 1){
      
      out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnnn")
      
      clean_out <- out %>%
        dplyr::filter(year == 2018) %>%
        mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
        mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
        dplyr::filter(name != names(caribbean_eez_choices[caribbean_eez_choices == input$caribbean_eez_select])) %>%
        arrange(name)
      
      clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
      
      clean_out
      
    }else{
      
      tibble(out = character(0))
    }
    
  })
  
  ### -----
  ### Update selectInput: Filter possible flag state selections based on selected ACP Caribbean state
  ### -----
  
  observeEvent(input$caribbean_eez_select, {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    if(nrow(caribbean_eez_data()) > 0){
      
      flag_state_choices_caribbean <- unique(caribbean_eez_data()$flag)
      names(flag_state_choices_caribbean) <- unique(caribbean_eez_data()$name)
      
      updateSelectizeInput(session, "caribbean_flag_state_select_effort",
                           choices = c("Select a flag state...", flag_state_choices_caribbean))
      
      updateSelectizeInput(session, "caribbean_flag_state_select_subsidy",
                           choices = c("Select a flag state...", flag_state_choices_caribbean))
      
      
    }else{
      
      updateSelectizeInput(session, "caribbean_flag_state_select_effort",
                           choices = c("Select a flag state..."))
      
      updateSelectizeInput(session, "caribbean_flag_state_select_subsidy",
                           choices = c("Select a flag state..."))
      
      
    }
    
  })
  
  ### -----
  ### Reactive object: bounding box coordinates for the EEZ of the selected ACP Caribbean state
  ### -----
  
  caribbean_eez_bbox <- eventReactive(input$caribbean_eez_select, {
    
    eez_map_subsidy <- caribbean_eez_map %>%
      dplyr::filter(iso_ter == input$caribbean_eez_select) %>%
      group_by(iso_ter) %>%
      summarize(geometry = st_union(geometry))
    
    st_bbox(eez_map_subsidy)
    
  })
  
  ### -----
  ### UI output: summary stats for effort heat map (selected flag state only) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_effort_summary_all <- renderUI({
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)

    eez_plot_data <- caribbean_eez_data() %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T))
    
    effort_summary <- paste0(
      "<b>", "Total DW effort (KWh): ", "</b>", format(round(eez_plot_data$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    effort_summary
    
  })
  
  ### -----
  ### Plot output: effort heat map (all flag states) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_effort_map_all <- renderPlot(bg = "#262626", {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    
    browser()
    
    ### Get totals for data
    eez_totals <- caribbean_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(caribbean_eez_bbox()$xmin - 0.5, caribbean_eez_bbox()$xmax + 0.5)
    y_lim <- c(caribbean_eez_bbox()$ymin - 0.5, caribbean_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- eez_totals
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for effort heat map (selected flag state only) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_effort_summary <- renderUI({
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    req(input$caribbean_flag_state_select_effort != "Select a flag state...")

    eez_plot_data <- caribbean_eez_data() %>%
      dplyr::filter(flag == input$caribbean_flag_state_select_effort) %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T))
    
    effort_summary <- paste0(
      "<b>", "Selected flag state DW effort (KWh): ", "</b>", format(round(eez_plot_data$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    effort_summary
    
  })
  
  ### -----
  ### Plot output: effort heat map (selected flag state only) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_effort_map <- renderPlot(bg = "#262626", {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    req(input$caribbean_flag_state_select_effort != "Select a flag state...")
    
    ### Get limits for map area
    x_lim <- c(caribbean_eez_bbox()$xmin - 0.5, caribbean_eez_bbox()$xmax + 0.5)
    y_lim <- c(caribbean_eez_bbox()$ymin - 0.5, caribbean_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- caribbean_eez_data() %>%
      dplyr::filter(flag == input$caribbean_flag_state_select_effort) %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = caribbean_eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for subsidy heat map (all flag states) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_subsidy_summary_all <- renderUI({
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)

    eez_plot_data <- caribbean_eez_data() %>%
      summarize(subs = sum(subs, na.rm = T))
    
    subsidy_summary <- paste0(
      "<b>", "Estimate of total DW subsidies (US$): ", "</b>", format(round(eez_plot_data$subs, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    subsidy_summary
    
  })
  
  
  ### -----
  ### Plot output: subsidy heat map (all flag states) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_subsidy_map_all <- renderPlot(bg = "#262626", {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    
    # Get totals for data
    eez_totals <- caribbean_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Set limits for map area
    x_lim <- c(caribbean_eez_bbox()$xmin - 0.5, caribbean_eez_bbox()$xmax + 0.5)
    y_lim <- c(caribbean_eez_bbox()$ymin - 0.5, caribbean_eez_bbox()$ymax + 0.5)
    
    # Define plot data  
    eez_plot_data <- eez_totals
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
      scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for subsidy heat map (selected flag state only) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_subsidy_summary <- renderUI({
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    req(input$caribbean_flag_state_select_subsidy != "Select a flag state...")
    
    eez_plot_data <- caribbean_eez_data() %>%
      dplyr::filter(flag == input$caribbean_flag_state_select_subsidy) %>%
      summarize(subs = sum(subs, na.rm = T))
    
    subsidy_summary <- paste0(
      "<b>", "Estimate of selected flag state DW subsidies (US$): ", "</b>", format(round(eez_plot_data$subs, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    subsidy_summary
    
  })
  
  
  ### -----
  ### Plot output: subsidy heat map (selected flag state only) for the selected ACP Caribbean state
  ### -----
  
  output$caribbean_subsidy_map <- renderPlot(bg = "#262626", {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    req(input$caribbean_flag_state_select_subsidy != "Select a flag state...")
    
    ### Get limits for map area
    x_lim <- c(caribbean_eez_bbox()$xmin - 0.5, caribbean_eez_bbox()$xmax + 0.5)
    y_lim <- c(caribbean_eez_bbox()$ymin - 0.5, caribbean_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- caribbean_eez_data() %>%
      dplyr::filter(flag == input$caribbean_flag_state_select_subsidy) %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
      scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  
  ###---------------------------------------------------------------------------------------
  ### Pacific ------------------------------------------------------------------------------
  ###---------------------------------------------------------------------------------------
  
  ### -----
  ### Leaflet output: Map of Pacific ACP EEZs for which we have DW fishing effort
  ### -----
  output$pacific_map <- renderLeaflet({
    
    # Extract ACP EEZs
    pacific_eezs <- pacific_eez_map %>%
      dplyr::filter(region == "Pacific") %>%
      mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
    
    # Merge non-contiguous EEZs for the same coastal state (South Pacific)
    pacific_eezs_merged <- pacific_eezs %>%
      group_by(iso_ter, geoname, region) %>%
      summarize(geometry = st_union(geometry))
    
    # Also extract disputed areas/joint management areas involving ACP coastal states in Pacific
    pacific_disputed_joint <- pacific_eez_map %>%
      dplyr::filter(pol_type != "200NM" & iso_ter %in% pacific_eezs$iso_ter) %>%
      mutate(region = "Pacific")
    
    # Map
    leaflet('pacific_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = pacific_disputed_joint, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = pacific_disputed_joint$geoname,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
      addPolygons(data = pacific_eezs_merged,
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = pacific_eezs_merged$geoname,
                  layerId = pacific_eezs_merged$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = 175, lat = 0, zoom = 2)
    
})
  
  ### -----
  ### Update selectInput: Register user clicks on Pacific map - change selected value of widget
  ### -----
  
  observeEvent(input$pacific_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$pacific_map_shape_click$id))
    
    updateSelectizeInput(session, "pacific_eez_select",
                         selected = input$pacific_map_shape_click$id)
    
  })
  
  ### -----
  ### Leaflet proxy: Create proxy for the Pacific map of ACP countries
  ### -----
  
  pacific_proxy <- leafletProxy("pacific_map")
  
  ### -----
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  ### -----
  
  observeEvent(input$pacific_eez_select, {
    
    if(input$pacific_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      pacific_proxy %>% setView(lng = 175, lat = 0, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(pacific_eez_map, pacific_eez_map$iso_ter == input$pacific_eez_select) %>%
        mutate(x_1 = ifelse(x_1 < 0, 360 + x_1, x_1))

        # The coordinates for Fiji are just wrong in the EEZ file for some reason
        selected_eez$x_1[selected_eez$iso_ter == "FJI"] <- 177.956
        # Same for Tuvalu
        selected_eez$x_1[selected_eez$iso_ter == "TUV"] <- 177.54

      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      pacific_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = ~region_pal_light(region),
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 2,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      group = "highlighted_eez",
                                      label = selected_eez$geoname,
                                      labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                               padding = "3px 8px"),
                                                                  textsize = "13px",
                                                                  direction = "auto")) %>%
        setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=3)
    }
    
  }) # close observe event
  
  
  ### -----
  ### UI output: Links and summary statistics for the selected ACP Pacific state
  ### -----
  
  output$pacific_country_profile <- renderUI({
    
    req(input$pacific_eez_select != "Select a coastal state...")
    
    ### Data wrangling
    # Filter connectivity data
    connectivity_data_filter_pacific <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$pacific_eez_select) %>%
      mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
    
    # Distant water fishing summary
    total_stats_pacific <- connectivity_data_filter_pacific %>%
      as.data.frame() %>%
      group_by(eez_territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T)) %>%
      arrange(eez_territory_iso3)
    
    # Filter and format Country profile data
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$pacific_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>%
      separate_rows(fao_memberships, sep = ",")
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    ### Make HTML sections with links for each type of information
    # Fisheries management agency
    fisheries_mgmt_agency <- ifelse(
      length(unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])) > 0,
      # Create link if we have one
      paste0("<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "' target='_blank'>", ACP_codes_links$fishery_org_eng, "</a>"),
      # Otherwise just paste name of the agency
      paste0(ACP_codes_links$fishery_org_eng)
    )
    
    # FAO country profile
    country_profiles <- paste0(
      # FAO
      "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "' target='_blank'>", "FAO", "</a>", " | ",
      # World Bank
      ifelse(
        length(unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)])) > 0,
        paste0("<a href='", unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)]), "' target='_blank'>", "World Bank", "</a>"),
        "World Bank"
      ), " | ",
      # UN
      ifelse(
        length(unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)])) > 0,
        paste0("<a href='", unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)]), "' target='_blank'>", "United Nations", "</a>"),
        "United Nations"
      )
    ) # close paste
    
    
    # Treaties and Conventions
    treaties_conventions <- paste0(
      "<a href= '",
      unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]),
      "' target='_blank'>",
      unique(ACP_codes_links$territory[!is.na(ACP_codes_links$treaties_conventions)]),
      "</a>",
      collapse = " | "
    )
    
    # Foreign access agreements by EEZ sector
    foreign_access_agreements <- paste0(
      "<a href= '", 
      unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
      "' target='_blank'>", 
      unique(ACP_codes_links$territory[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
      "</a>", 
      collapse = " | ")
    
    # FAO Regional Fisheries Body Memberships
    regional_body_memberships <- paste0(
      "<a href= '", 
      unique(RFMO_links_eez$link[!is.na(RFMO_links_eez$link)]),
      "' target='_blank'>",
      unique(RFMO_links_eez$rfmo_name[!is.na(RFMO_links_eez$link)]),
      "</a>",
      collapse = " | ")
    
    ### Combine into country profile/summary of DW fishing
    EEZ_info <- paste0("<h3 style = 'margin-top: 0px;'>", names(pacific_eez_choices[pacific_eez_choices == input$pacific_eez_select]), "</h3>",
                       
                       "Fisheries management agency:  ", 
                       fisheries_mgmt_agency,
                       "<br>",
                       
                       "Country profile: ",
                       country_profiles,
                       "<br>",
                       
                       "Treaties and conventions: ",
                       treaties_conventions,
                       "<br>",
                       
                       "Foreign access agreements: ",
                       foreign_access_agreements,
                       "<br>",
                       
                       "FAO Regional Fisheries Body Memberships: ",
                       regional_body_memberships,
                       "<br>",
                       
                       "<hr>",
                       "<b>", "AIS-observed distant water fishing in the ", total_stats_pacific$eez_nam, " (2018)", "</b>",
                       "<br>",
                       "Vessels: ", format(round(total_stats_pacific$vessels, 0), big.mark = ","),
                       "<br>",
                       "Total engine capacity (KW): ", format(round(total_stats_pacific$capacity, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (hours): ", format(round(total_stats_pacific$fishing_h, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (KWh): ", format(round(total_stats_pacific$fishing_KWh, 0), big.mark = ",")) %>%
      lapply(htmltools::HTML)
    
    # Return
    EEZ_info
    
  })
  
  ### -----
  ### Leaflet output: Connectivity map for selected ACP Pacific state
  ### -----
  
  output$pacific_connection_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$pacific_eez_select != "Select a coastal state...")
    
    # Connectivity data for the entire region
    connectivity_data_region <- connectivity_data %>% # load this in up above
      dplyr::filter(region == "Pacific")
    
    ### Selected ACP coastal state ---
    
    # # Selected Pacific ACP coastal state
    # selected_eez <- pacific_eez_map %>% 
    #   dplyr::filter(iso_ter == input$pacific_eez_select) %>%
    #   rename(territory_iso3 = iso_ter)
    
    # Connectivity data for the EEZ of the selected ACP coastal state
    connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$pacific_eez_select) %>% 
      arrange(flag)%>%
      dplyr::filter(flag != "UNK")
    
    ### Polygons of flag states --- 
    
    # Get polygons of flag states
    flag_states_for_selected_eez <- pacific_land_map %>%
      dplyr::filter(admin_iso3 %in% connectivity_data_for_selected_eez$flag) %>%
      rename(flag = admin_iso3) %>% 
      arrange(flag)
    
    # Get polygons of flag state boundaries merged with EEZ boundaries
    flag_states_combined_for_selected_eez <- pacific_land_eez_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    
    # if(all(ifelse(unique(flag_states_for_selected_eez$flag) == unique(flag_states_combined_for_selected_eez$flag), TRUE, FALSE)) == F){
    #   warning("Check flag states")
    #   
    # }
    
    # Connectivity stats with no geometry
    connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
      group_by(eez_territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T))
    st_geometry(connectivity_data_no_geometry) <- NULL
    
    #  Create summary polygons with hover text for flag states
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag")
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary$display_name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    #  Create summary polygons with hover text for flag states merged with EEZ boundaries
    flag_state_summary_combined <- flag_states_combined_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag")
    
    flag_state_summary_combined_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary_combined$display_name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary_combined$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary_combined$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary_combined$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary_combined$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    ### Interactive color palette ---
    
    # Set fill variable for map
    fill_scale <- switch(input$pacific_connection_fill,
                         "# of Different Vessels" = list("vessels", connectivity_data_region$vessels, flag_state_summary$vessels),
                         "Total Engine Capacity (KW)" = list("capacity", connectivity_data_region$capacity, flag_state_summary$capacity),
                         "Total Fishing Effort (hours)" = list("fishing_h", connectivity_data_region$fishing_h, flag_state_summary$fishing_h),
                         "Total Fishing Effort (KWh)" = list("fishing_KWh", connectivity_data_region$fishing_KWh, flag_state_summary$fishing_KWh))
    
    # Make color palette
    domain <- switch(input$pacific_connection_fill_rescale,
                     "All distant water fishing in the region (default)" = 2,
                     "Selected EEZ only" = 3)
    
    pal <- colorBin("YlOrRd", domain = fill_scale[[domain]], bins = 7)
    
    
    ### Leaflet map ---
    
    leaflet('pacific_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      
      # Basemap
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
      
      # Flag state polygons (merged land and EEZ boundaries)
      addPolygons(data = flag_state_summary_combined,
                  fillColor = ~pal(get(fill_scale[[1]])),
                  fillOpacity = 0.3,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.5,
                                               bringToFront = FALSE),
                  label = flag_state_summary_combined_text,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      # Flag state polygons (land)
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
                  layerId = flag_state_summary$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      # EEZ polygon
      addPolygons(data = (pacific_eez_map %>% dplyr::filter(iso_ter == input$pacific_eez_select)), 
                  fillColor = ~region_pal_light(region),
                  fillOpacity = 0.8,
                  color= "white",
                  group = "eez",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = ~htmlEscape(geoname),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      # Connecting lines
      addPolylines(data = connectivity_data_for_selected_eez,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1.5,
                   color = "darkgoldenrod",
                   group = "lines") %>% 
      
      # Legend
      addLegend(pal = pal, values = fill_scale[[domain]], 
                opacity=0.9, title = input$pacific_connection_fill, position = "bottomleft" ) %>%
      
      # Map extent
      setView(lng = 175, lat = 0, zoom = 2)
    
})
  
  
#   ### -----
#   ### Leaflet output: Connectivity map for selected ACP Pacific state
#   ### -----
#   
#   output$pacific_connection_map <- renderLeaflet({
#     
#     # Require coastal state selection
#     req(input$pacific_eez_select != "Select a coastal state...")
#     
#     # Selected Pacific ACP coastal state
#     selected_eez <- pacific_eez_map %>% 
#       dplyr::filter(iso_ter == input$pacific_eez_select) %>%
#       rename(territory_iso3 = iso_ter)
#     
#     # Connectivity data for the entire region
#     connectivity_data_region <- connectivity_data %>% # load this in up above
#       dplyr::filter(region == "Pacific")
#     
#     # Connectivity data for the EEZ of the selected ACP coastal state
#     connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
#       dplyr::filter(eez_territory_iso3 == input$pacific_eez_select) %>% 
#       arrange(flag)%>%
#       dplyr::filter(flag != "UNK")
#     
#     flag_states_for_selected_eez <- pacific_land_eez_map %>% 
#       dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
#       rename(flag = iso3) %>% 
#       arrange(flag)
#     
#     # Filter out flag states that may not show up on map 
#     if(length(unique(connectivity_data_for_selected_eez$flag)) != length(unique(flag_states_for_selected_eez$flag))) {
#       
#       connectivity_data_for_selected_eez <- connectivity_data_for_selected_eez %>%
#         dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
#     }
#     
#     # Connectivity stats with no geometry
#     connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
#       group_by(eez_territory_iso3, flag) %>%
#       summarize(vessels = sum(vessels, na.rm = T),
#                 capacity = sum(capacity, na.rm = T),
#                 fishing_h = sum(fishing_h, na.rm = T),
#                 fishing_KWh = sum(fishing_KWh, na.rm = T))
#     st_geometry(connectivity_data_no_geometry) <- NULL
#     
#     #  Hover Text
#     flag_state_summary <- flag_states_for_selected_eez %>% 
#       left_join(connectivity_data_no_geometry, by = "flag") %>%
#       mutate(name = countrycode(flag, "iso3c", "country.name"))
#     
#     flag_state_summary_text <- paste0(
#       "<b>", "Flag state: ", "</b>", flag_state_summary$name,
#       "<br/>",
#       "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
#       "</br>",
#       "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
#       "</br>",
#       "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
#       "</br>",
#       "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
#       lapply(htmltools::HTML)
#     
#     # Set fill variable for map
#     fill_scale <- switch(input$pacific_connection_fill,
#                          "# of Different Vessels" = list("vessels", connectivity_data_region$vessels, flag_state_summary$vessels),
#                          "Total Engine Capacity (KW)" = list("capacity", connectivity_data_region$capacity, flag_state_summary$capacity),
#                          "Total Fishing Effort (hours)" = list("fishing_h", connectivity_data_region$fishing_h, flag_state_summary$fishing_h),
#                          "Total Fishing Effort (KWh)" = list("fishing_KWh", connectivity_data_region$fishing_KWh, flag_state_summary$fishing_KWh))
#     
#     # Make color palette
#     domain <- switch(input$pacific_connection_fill_rescale,
#                      "All distant water fishing in the region (default)" = 2,
#                      "Selected EEZ only" = 3)
#     
#     pal <- colorBin("YlOrRd", domain = fill_scale[[domain]], bins = 7)
#     
#     # Leaflet map
#     leaflet('pacific_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
#       htmlwidgets::onRender("function(el, x) {
#                             L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
#       addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
#       
#       addPolygons(data = flag_state_summary,
#                   fillColor = ~pal(get(fill_scale[[1]])),
#                   fillOpacity = 0.8,
#                   color= "white",
#                   weight = 0.3,
#                   highlight = highlightOptions(weight = 5,
#                                                color = "#666",
#                                                fillOpacity = 1,
#                                                bringToFront = TRUE),
#                   label = flag_state_summary_text,
#                   layerId = flag_state_summary$flag,
#                   labelOptions = labelOptions(style = list("font-weight" = "normal",
#                                                            padding = "3px 8px"),
#                                               textsize = "13px",
#                                               direction = "auto")) %>%
#       addPolygons(data = selected_eez, 
#                   fillColor = ~region_pal_light(region),
#                   fillOpacity = 0.8,
#                   color= "white",
#                   group = "eez",
#                   weight = 0.3,
#                   highlight = highlightOptions(weight = 5,
#                                                color = "#666",
#                                                fillOpacity = 1,
#                                                bringToFront = TRUE),
#                   label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
#                              lapply(htmltools::HTML)),
#                   labelOptions = labelOptions(style = list("font-weight" = "normal",
#                                                            padding = "3px 8px"),
#                                               textsize = "13px",
#                                               direction = "auto")) %>%
#       
#       addPolylines(data = connectivity_data_for_selected_eez,
#                    fillColor = "goldenrod",
#                    fillOpacity = 1,
#                    weight = 1,
#                    color = "darkgoldenrod",
#                    group = "lines") %>% 
#       
#       addLegend(pal = pal, values = fill_scale[[domain]], 
#                 opacity=0.9, title = input$pacific_connection_fill, position = "bottomleft" ) %>%
#       setView(lng = 175, lat = 0, zoom = 2)
#     
# })
#   
  ### -----
  ### Update tab and selectInput: Register clicks on Pacific connectivity map and change tab ans flag state input widgets accordingly
  ### -----
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$pacific_connection_map_shape_click, {
    
    req(input$pacific_connection_map_shape_click$id != input$pacific_eez_select)
    
    updateTabItems(session, "pacific_tabs", "Distant water fishing effort")
    
    updateSelectizeInput(session, "pacific_flag_state_select_effort",
                         selected = input$pacific_connection_map_shape_click$id
    )
    
    updateSelectizeInput(session, "pacific_flag_state_select_subsidy",
                         selected = input$pacific_connection_map_shape_click$id
    )
    
  })
  
  ### -----
  ### Reactive DF: Load 0.1 x 0.1 degree effort/subsidy data for selected ACP Pacific state
  ### -----
  
  pacific_eez_data <- eventReactive(input$pacific_eez_select, {
    
    # Require EEZ selection
    req(input$pacific_eez_select != "Select a coastal state...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$pacific_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(pacific_eez_choices[pacific_eez_choices == input$pacific_eez_select])) %>%
      arrange(name) %>%
      mutate(lon_cen = ifelse(lon_cen < 0, 360 + lon_cen, lon_cen)) # correction for 0 to 360 range
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -----
  ### Update selectInput: Filter possible flag state selections based on selected ACP Pacific state
  ### -----
  
  observeEvent(input$pacific_eez_select, {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    
    flag_state_choices_pacific <- unique(pacific_eez_data()$flag)
    names(flag_state_choices_pacific) <- unique(pacific_eez_data()$name)
    
    updateSelectizeInput(session, "pacific_flag_state_select_effort",
                         choices = c("Select a flag state...", flag_state_choices_pacific))
    
    updateSelectizeInput(session, "pacific_flag_state_select_subsidy",
                         choices = c("Select a flag state...", flag_state_choices_pacific))
    
  })
  
  ### -----
  ### Reactive object: bounding box coordinates for the EEZ of the selected ACP Pacific state
  ### -----
  
  pacific_eez_bbox <- eventReactive(input$pacific_eez_select, {
    
    eez_map_subsidy <- pacific_eez_map %>%
      dplyr::filter(iso_ter == input$pacific_eez_select) %>%
      group_by(iso_ter) %>%
      summarize(geometry = st_union(geometry))
    
    st_bbox(eez_map_subsidy)
    
  })
  
  ### -----
  ### UI output: summary stats for effort heat map (selected flag state only) for the selected ACP Pacific state
  ### -----
  
  output$pacific_effort_summary_all <- renderUI({
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    
    eez_plot_data <- pacific_eez_data() %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T))
    
    effort_summary <- paste0(
      "<b>", "Total DW effort (KWh): ", "</b>", format(round(eez_plot_data$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    effort_summary
    
  })
  
  ### -----
  ### Plot output: effort heat map (all flag states) for the selected ACP Pacific state
  ### -----
  
  output$pacific_effort_map_all <- renderPlot(bg = "#262626", {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    
    ### Get totals for data
    eez_totals <- pacific_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
    y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- eez_totals
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for effort heat map (selected flag state only) for the selected ACP Pacific state
  ### -----
  
  output$pacific_effort_summary <- renderUI({
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    req(input$pacific_flag_state_select_effort != "Select a flag state...")
    
    eez_plot_data <- pacific_eez_data() %>%
      dplyr::filter(flag == input$pacific_flag_state_select_effort) %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T))
    
    effort_summary <- paste0(
      "<b>", "Selected flag state DW effort (KWh): ", "</b>", format(round(eez_plot_data$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    effort_summary
    
  })
  
  ### -----
  ### Plot output: effort heat map (selected flag state only) for the selected ACP Pacific state
  ### -----
  
  output$pacific_effort_map <- renderPlot(bg = "#262626", {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    req(input$pacific_flag_state_select_effort != "Select a flag state...")
    
    ### Get limits for map area
    x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
    y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- pacific_eez_data() %>%
      dplyr::filter(flag == input$pacific_flag_state_select_effort) %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = pacific_eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for subsidy heat map (all flag states) for the selected ACP Pacific state
  ### -----
  
  output$pacific_subsidy_summary_all <- renderUI({
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    
    eez_plot_data <- pacific_eez_data() %>%
      summarize(subs = sum(subs, na.rm = T))
    
    subsidy_summary <- paste0(
      "<b>", "Estimate of total DW subsidies (US$): ", "</b>", format(round(eez_plot_data$subs, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    subsidy_summary
    
  })
  
  
  ### -----
  ### Plot output: subsidy heat map (all flag states) for the selected ACP Pacific state
  ### -----
  
  output$pacific_subsidy_map_all <- renderPlot(bg = "#262626", {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    
    # Get totals for data
    eez_totals <- pacific_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Set limits for map area
    x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
    y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
    
    # Define plot data  
    eez_plot_data <- eez_totals
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
      scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  ### -----
  ### UI output: summary stats for subsidy heat map (selected flag state only) for the selected ACP Pacific state
  ### -----
  
  output$pacific_subsidy_summary <- renderUI({
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    req(input$pacific_flag_state_select_subsidy != "Select a flag state...")
    
    eez_plot_data <- pacific_eez_data() %>%
      dplyr::filter(flag == input$pacific_flag_state_select_subsidy) %>%
      summarize(subs = sum(subs, na.rm = T))
    
    subsidy_summary <- paste0(
      "<b>", "Estimate of selected flag state DW subsidies (US$): ", "</b>", format(round(eez_plot_data$subs, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    subsidy_summary
    
  })
  
  
  ### -----
  ### Plot output: subsidy heat map (selected flag state only) for the selected ACP Pacific state
  ### -----
  
  output$pacific_subsidy_map <- renderPlot(bg = "#262626", {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    req(nrow(pacific_eez_data()) > 0)
    req(input$pacific_flag_state_select_subsidy != "Select a flag state...")
    
    ### Get limits for map area
    x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
    y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    eez_plot_data <- pacific_eez_data() %>%
      dplyr::filter(flag == input$pacific_flag_state_select_subsidy) %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
      scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })

  ### ------------------
  ### Info buttons
  ### ------------------
  
  # Pretty much every widget has an info button next to it now - this section controls all of the pop-up boxes cooresponding to those info buttons. 
  
  # Africa Connectivity map: info
  observeEvent(input$africa_distant_water_info, {
    showModal(modalDialog(
      includeHTML("./text/distant_water_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Caribbean Connectivity map: info
  observeEvent(input$caribbean_distant_water_info, {
    showModal(modalDialog(
      includeHTML("./text/distant_water_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Pacific Connectivity map: info
  observeEvent(input$pacific_distant_water_info, {
    showModal(modalDialog(
      includeHTML("./text/distant_water_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Africa Effort map: info
  observeEvent(input$africa_effort_info, {
    showModal(modalDialog(
      includeHTML("./text/effort_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Africa Subsidy map: info
  observeEvent(input$africa_subsidy_info, {
    showModal(modalDialog(
      includeHTML("./text/subsidy_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Caribbean Effort map: info
  observeEvent(input$caribbean_effort_info, {
    showModal(modalDialog(
      includeHTML("./text/effort_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Caribbean Subsidy map: info
  observeEvent(input$caribbean_subsidy_info, {
    showModal(modalDialog(
      includeHTML("./text/subsidy_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Pacific Effort map: info
  observeEvent(input$pacific_effort_info, {
    showModal(modalDialog(
      includeHTML("./text/effort_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  # Pacific Subsidy map: info
  observeEvent(input$pacific_subsidy_info, {
    showModal(modalDialog(
      includeHTML("./text/subsidy_methods.html"),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
}) # /close server


