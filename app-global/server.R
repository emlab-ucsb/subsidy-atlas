### ----------------------------------------------------------------------
# 
# Distant Water Fishing Atlas - Server Logic
# This app allows users to visualize distant-water fishing effort in the EEZs of ACP countries
#
### ----------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### ------------------------------- 
  ### Header / Navigation -----------
  ### -------------------------------
  
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
                input$sub_saharan_africa_return_to_region,
                input$high_seas_return_to_region), {
    updateTabItems(session, "tabs", "selectregion")
  })
  
  # Info modal: DW summary ----------
  observeEvent(c(input$east_asia_pacific_info_summary,
                 input$europe_central_asia_info_summary,
                 input$latin_america_caribbean_info_summary,
                 input$middle_east_north_africa_info_summary,
                 input$north_america_info_summary,
                 input$south_asia_info_summary,
                 input$sub_saharan_africa_info_summary), {
    
    shinyalert(title = "Summary",
               text = paste0("This tab provides a summary of distant water fishing activity in the EEZ(s) cooresponding to the selected coastal. Distant water fishing activity is aggregated by flag state. If nothing is visable, please select a coastal state using the map or widget in the left panel."),
               size = "l",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#0d5ba2",
               timer = 0,
               animation = TRUE)
  }, ignoreInit = T)
  
  observeEvent(c(input$high_seas_info_summary), {
                   
                   shinyalert(title = "Summary",
                              text = paste0("This tab provides a summary of distant water fishing activity on the high seas cooresponding to the selected FAO Major Fishing Area. Distant water fishing activity is aggregated by flag state. If nothing is visable, please select a high seas area using the map or widget in the left panel."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  # Info modal: Vessel origins ----------
  observeEvent(c(input$east_asia_pacific_info_vessel_origins,
                 input$europe_central_asia_info_vessel_origins,
                 input$latin_america_caribbean_info_vessel_origins,
                 input$middle_east_north_africa_info_vessel_origins,
                 input$north_america_info_vessel_origins,
                 input$south_asia_info_vessel_origins,
                 input$sub_saharan_africa_info_vessel_origins), {
                   
                   shinyalert(title = "Vessel Origins",
                              text = paste0("This tab illustrates the flag state origins of distant water vessels fishing in the EEZ(s) of the selected coastal state. If no map is visible, please select a coastal state using the map or widget in the left panel. The fill scale can be changed based on the metric of effort selected below. Hover over each flag state to view more about distant water fishing activity by vessels flagged to that state in the selected EEZ(s)."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  observeEvent(c(input$high_seas_info_vessel_origins), {
                   
                   shinyalert(title = "Vessel Origins",
                              text = paste0("This tab illustrates the flag state origins of distant water vessels fishing on the high seas in the selected FAO Major Fishing Area. If no map is visible, please select a high seas area using the map or widget in the left panel. The fill scale can be changed based on the metric of effort selected below. Hover over each flag state to view more about distant water fishing activity by vessels flagged to that state in the selected high seas area."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  # Info modal: Fishing effort
  observeEvent(c(input$east_asia_pacific_info_effort,
                 input$europe_central_asia_info_effort,
                 input$latin_america_caribbean_info_effort,
                 input$middle_east_north_africa_info_effort,
                 input$north_america_info_effort,
                 input$south_asia_info_effort,
                 input$sub_saharan_africa_info_effort), {
                   
                   shinyalert(title = "Fishing Effort",
                              text = paste0("This tab illustrates satellite derived estimates of distant water fishing effort (in kilowatt-hours, or kWh) in the EEZ(s) of the selected coastal state in 2018. We calculate fishing effort in units of fishing kilowatt-hours (kWh) by weighting the hours spent fishing by the engine power of the vessel. Expressing fishing effort in kWh (as opposed to just hours) gives us a better metric for comparing fishing effort across vessels with different gear types and/or sizes. Fishing effort is aggregated by 0.1 x 0.1 degree latitude/longitude.  If no figure(s) are visible, please select a coastal state using the map or widget in the left panel. The figure on the left shows total distant water fishing effort from all flag states and the figure on the right shows distant water fishing effort for vessels from the selected flag state."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  observeEvent(c(input$high_seas_info_effort), {
                   
                   shinyalert(title = "Fishing Effort",
                              text = paste0("This tab illustrates satellite derived estimates of distant water fishing effort (in kilowatt-hours, or kWh) on the high seas in the selected FAO Major Fishing Area in 2018. We calculate fishing effort in units of fishing kilowatt-hours (kWh) by weighting the hours spent fishing by the engine power of the vessel. Expressing fishing effort in kWh (as opposed to just hours) gives us a better metric for comparing fishing effort across vessels with different gear types and/or sizes. Fishing effort is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a high seas area using the map or widget in the left panel. The figure on the left shows total distant water fishing effort from all flag states and the figure on the right shows distant water fishing effort for vessels from the selected flag state."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  # Info modal: Subsidies
  observeEvent(c(input$east_asia_pacific_info_subsidies,
                 input$europe_central_asia_info_subsidies,
                 input$latin_america_caribbean_info_subsidies,
                 input$middle_east_north_africa_info_subsidies,
                 input$north_america_info_subsidies,
                 input$south_asia_info_subsidies,
                 input$sub_saharan_africa_info_subsidies), {
                   
                   shinyalert(title = "Subsidy Intensity",
                              text = paste0("This tab illustrates the estimated magnitude of capacity-enhancing subsidies (in 2018 USD) supporting distant water fishing in the EEZ(s) of the selected coastal state. Subsidy intensity is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a coastal state using the map or widget in the left panel. The figure on the left shows total subsidy intensity for distant water vessels from all flag states and the figure on the right shows subsidy intensity for distant water vessels from the selected flag state."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  observeEvent(c(input$high_seas_info_subsidies), {
                   
                   shinyalert(title = "Subsidy Intensity",
                              text = paste0("This tab illustrates the estimated magnitude of capacity-enhancing subsidies (in 2018 USD) supporting distant water fishing on the high seas in the selected FAO Major Fishing Area. Subsidy intensity is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a high seas area using the map or widget in the left panel. The figure on the left shows total subsidy intensity for distant water vessels from all flag states and the figure on the right shows subsidy intensity for distant water vessels from the selected flag state."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  ### ------------------------------------------
  ### Introduction / Select a region -----------
  ### ------------------------------------------
  
  region_pal <- colorFactor(palette = "Dark2",
                            domain = eez_region_360$region,
                            na.color = "grey")
  
  region_pal_light <- colorFactor(palette = "Set2",
                                  domain = eez_region_360$region,
                                  na.color = "grey")
  
  region_pal_dark <- colorFactor(palette = c("#0A3E2E", "#8E3E01", "#3B3959", "#6C1240", "#31500F", "#8C6700", "#664811"),
                                 domain = eez_region_360$region,
                                 na.color = "grey")
  
  #connectivity_fill_scale
  
  ## Leaflet output: map of ACP countries aggregated by region -------------
  
  output$regional_map <- renderLeaflet({
    
    ## Get regional dat
    regional_dat <- eez_region_360
    
    # Leaflet map
    leaflet("regional_map", 
            options = leafletOptions(minZoom = 1, zoomControl = FALSE,
                                     attributionControl = FALSE)) %>%
      
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topleft' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldPhysical") %>% 
      
      addPolygons(data = fao_region_360,
                  fillColor = hs_pal$dark_col,
                  fillOpacity = 0.8,
                  color= hs_pal$dark_col,
                  weight = 0,
                  highlight = highlightOptions(fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = fao_region_360$region,
                  group = fao_region_360$region) %>%
      
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
      setView(0, 20, zoom = 2) %>%
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
                             "Sub-Saharan Africa" = list("sub-saharan-africa"),
                             "High Seas" = list("high-seas"))
    
    updateTabItems(session, "tabs", tab_navigation[[1]])
    
  })
  
  ### Action button: Show map text (and show "close" button) ---------------
  observeEvent(input$ab_regional_map_expand_text, {
    
    # show panel
    shinyjs::showElement(id = "regional_map_text_panel")
    shinyjs::showElement(id = "regional_map_hide_text_arrow")
    shinyjs::hideElement(id = "regional_map_expand_text_arrow")
    
  })
  
  ### Action button: Hide global subsidies map disclaimer (and show "expand" button) -------------------
  observeEvent(input$ab_regional_map_hide_text, {
    
    # show panel
    shinyjs::hideElement(id = "regional_map_text_panel")
    shinyjs::hideElement(id = "regional_map_hide_text_arrow")
    shinyjs::showElement(id = "regional_map_expand_text_arrow")
    
  }) 
  
  ###------------------------------------------------------------------
  ### East Asia & Pacific ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Data Container -----------
  east_asia_pacific_rv <- reactiveValues(eezs = eez_ter_360 %>% 
                                           dplyr::filter(region == "East Asia & Pacific"),
                                         map_lng = 170,
                                         map_lat = -5,
                                         map_zoom = 1,
                                         connect = eez_flag_state_connectivity %>%
                                           dplyr::filter(region == "East Asia & Pacific"),
                                         eez_dat = NULL,
                                         eez_bb = NULL,
                                         hs_dat = NULL,
                                         hs_regions = NULL,
                                         links = eez_ter_links %>%
                                           dplyr::filter(region == "East Asia & Pacific"))
  
  ### UI output: Select coastal state widget --------
  output$east_asia_pacific_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = east_asia_pacific_rv,
                    widget_id = "east_asia_pacific_eez_select")

  })
  
  ### UI output: Selected coastal state stats -----------
  output$east_asia_pacific_eez_select_stats <- renderUI({
    
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = east_asia_pacific_rv,
              input_selected_eez = input$east_asia_pacific_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$east_asia_pacific_nav_map <- renderLeaflet({
    
    NavMap(region_dat = east_asia_pacific_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "east_asia_pacific_nav_map",
           min_zoom = 1)

  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$east_asia_pacific_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$east_asia_pacific_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "east_asia_pacific_eez_select",
                         selected = input$east_asia_pacific_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  east_asia_pacific_nav_map_proxy <- leafletProxy("east_asia_pacific_nav_map")
  
  ### Leaflet proxy: Highlight and zoom to EEZ(s) cooresponding to selected state ---------
  observeEvent(input$east_asia_pacific_eez_select, {
    
    if(input$east_asia_pacific_eez_select == "Select a coastal state..." | !(input$east_asia_pacific_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon and reorient
      east_asia_pacific_nav_map_proxy %>% 
        clearGroup("highlighted_eez") %>%
        setView(lng = east_asia_pacific_rv$map_lng, 
                lat = east_asia_pacific_rv$map_lat, 
                zoom = east_asia_pacific_rv$map_zoom)
      
    }else{
      
      NavMapHighlight(region_dat = east_asia_pacific_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = east_asia_pacific_nav_map_proxy,
                      input_selected_eez = input$east_asia_pacific_eez_select)
    }
    
  })
  
  ### UI output: Distant water summary for selected state -------------
  output$east_asia_pacific_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = east_asia_pacific_rv,
              input_selected_eez = input$east_asia_pacific_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$east_asia_pacific_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "east_asia_pacific")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$east_asia_pacific_download_data <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$east_asia_pacific_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = east_asia_pacific_rv,
                          input_selected_eez = input$east_asia_pacific_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$east_asia_pacific_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryDT(region_dat = east_asia_pacific_rv,
              input_selected_eez = input$east_asia_pacific_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$east_asia_pacific_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = east_asia_pacific_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$east_asia_pacific_eez_select,
                     input_fill_variable = input$east_asia_pacific_vessel_origins_fill,
                     input_fill_scale = input$east_asia_pacific_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "east_asia_pacific_vessel_origins_map")

  })
  
  ### UI output: Select flag state widget (effort) --------
  output$east_asia_pacific_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = east_asia_pacific_rv,
                          input_selected_eez = input$east_asia_pacific_eez_select,
                          widget_id = "east_asia_pacific_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$east_asia_pacific_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = east_asia_pacific_rv,
                          input_selected_eez = input$east_asia_pacific_eez_select,
                          widget_id = "east_asia_pacific_subsidies_select_flag_state")
    
  })
  
  ### Load Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$east_asia_pacific_eez_select)
    
    # Save to reactive object
    east_asia_pacific_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- east_asia_pacific_rv$eezs %>%
      st_crop(xmin = 0, ymin = -90, xmax = 360, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$east_asia_pacific_eez_select)
    
    east_asia_pacific_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
        input$east_asia_pacific_effort_high_seas == TRUE | input$east_asia_pacific_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$east_asia_pacific_eez_select])
    
    if(all(fao_regions %in% east_asia_pacific_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      east_asia_pacific_rv$hs_dat <- hs_dat
      east_asia_pacific_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$east_asia_pacific_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
        nrow(east_asia_pacific_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = east_asia_pacific_rv,
                              input_selected_flag_state = input$east_asia_pacific_effort_select_flag_state,
                              input_selected_eez = input$east_asia_pacific_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    east_asia_pacific_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = east_asia_pacific_rv,
                  input_selected_eez = input$east_asia_pacific_eez_select,
                  map_id = "east_asia_pacific_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$east_asia_pacific_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
        nrow(east_asia_pacific_rv$eez_dat) > 0,
        input$east_asia_pacific_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = east_asia_pacific_rv,
                  input_selected_eez = input$east_asia_pacific_eez_select,
                  map_id = "east_asia_pacific_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  east_asia_pacific_effort_map_proxy <- leafletProxy("east_asia_pacific_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$east_asia_pacific_effort_select_flag_state, {
    
    if(input$east_asia_pacific_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      east_asia_pacific_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      east_asia_pacific_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = east_asia_pacific_rv,
                                input_selected_flag_state = input$east_asia_pacific_effort_select_flag_state,
                                input_selected_eez = input$east_asia_pacific_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      east_asia_pacific_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      east_asia_pacific_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      east_asia_pacific_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$east_asia_pacific_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
        nrow(east_asia_pacific_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = east_asia_pacific_rv,
                              input_selected_flag_state = input$east_asia_pacific_subsidies_select_flag_state,
                              input_selected_eez = input$east_asia_pacific_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    east_asia_pacific_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = east_asia_pacific_rv,
                  input_selected_eez = input$east_asia_pacific_eez_select,
                  map_id = "east_asia_pacific_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$east_asia_pacific_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
        nrow(east_asia_pacific_rv$eez_dat) > 0,
        input$east_asia_pacific_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = east_asia_pacific_rv,
                  input_selected_eez = input$east_asia_pacific_eez_select,
                  map_id = "east_asia_pacific_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  east_asia_pacific_subsidies_map_proxy <- leafletProxy("east_asia_pacific_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$east_asia_pacific_subsidies_select_flag_state, {
    
    if(input$east_asia_pacific_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      east_asia_pacific_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      east_asia_pacific_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = east_asia_pacific_rv,
                                input_selected_flag_state = input$east_asia_pacific_subsidies_select_flag_state,
                                input_selected_eez = input$east_asia_pacific_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      east_asia_pacific_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      east_asia_pacific_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      east_asia_pacific_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$east_asia_pacific_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_east_asia_pacific_",input$east_asia_pacific_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
          nrow(east_asia_pacific_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = east_asia_pacific_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$east_asia_pacific_eez_select,
                                     input_selected_flag_state = input$east_asia_pacific_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$east_asia_pacific_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_east_asia_pacific_area_",input$east_asia_pacific_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$east_asia_pacific_eez_select != "Select a coastal state..." & input$east_asia_pacific_eez_select %in% good_eezs_all,
          nrow(east_asia_pacific_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = east_asia_pacific_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$east_asia_pacific_eez_select,
                                     input_selected_flag_state = input$east_asia_pacific_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###------------------------------------------------------------------
  ### Europe & Central Asia ---------------------------------------------
  ###------------------------------------------------------------------
  
  ### Data Container ----------
  europe_central_asia_rv <- reactiveValues(eezs = eez_ter_360 %>%
                                             dplyr::filter(region == "Europe & Central Asia"),
                                           map_lng = 0,
                                           map_lat = 50,
                                           map_zoom = 2,
                                           connect = eez_flag_state_connectivity %>%
                                             dplyr::filter(region == "Europe & Central Asia"),
                                           eez_dat = NULL,
                                           eez_bb = NULL,
                                           hs_dat = NULL,
                                           hs_regions = NULL,
                                           links = eez_ter_links %>%
                                             dplyr::filter(region == "Europe & Central Asia"))
  
  ### UI output: Select coastal state widget --------
  output$europe_central_asia_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = europe_central_asia_rv,
                    widget_id = "europe_central_asia_eez_select")
    
  })
  
  ### UI output: Selected coastal state stats -----------
  output$europe_central_asia_eez_select_stats <- renderUI({
    
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = europe_central_asia_rv,
              input_selected_eez = input$europe_central_asia_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$europe_central_asia_nav_map <- renderLeaflet({
    
    NavMap(region_dat = europe_central_asia_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "europe_central_asia_nav_map",
           min_zoom = 1)
    
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$europe_central_asia_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$europe_central_asia_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "europe_central_asia_eez_select",
                         selected = input$europe_central_asia_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  europe_central_asia_nav_map_proxy <- leafletProxy("europe_central_asia_nav_map")
  
  ### Leaflet proxy: Highlight and zoom to EEZ(s) cooresponding to selected state --------
  observeEvent(input$europe_central_asia_eez_select, {
    
    if(input$europe_central_asia_eez_select == "Select a coastal state..." | !(input$europe_central_asia_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon and reorient
      europe_central_asia_nav_map_proxy %>% 
        clearGroup("highlighted_eez") %>%
        setView(lng = europe_central_asia_rv$map_lng, 
                lat = europe_central_asia_rv$map_lat,
                zoom = europe_central_asia_rv$map_zoom)
      
    }else{
      
      NavMapHighlight(region_dat = europe_central_asia_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = europe_central_asia_nav_map_proxy,
                      input_selected_eez = input$europe_central_asia_eez_select)
      
    }
    
  }) 
  
  ### UI output: Distant water summary for selected state -------------
  output$europe_central_asia_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = europe_central_asia_rv,
              input_selected_eez = input$europe_central_asia_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$europe_central_asia_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "europe_central_asia")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$europe_central_asia_download_data <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$europe_central_asia_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = europe_central_asia_rv,
                          input_selected_eez = input$europe_central_asia_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$europe_central_asia_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryDT(region_dat = europe_central_asia_rv,
              input_selected_eez = input$europe_central_asia_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$europe_central_asia_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = europe_central_asia_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$europe_central_asia_eez_select,
                     input_fill_variable = input$europe_central_asia_vessel_origins_fill,
                     input_fill_scale = input$europe_central_asia_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "europe_central_asia_vessel_origins_map")
    
  })
  
  ### UI output: Select flag state widget (effort) --------
  output$europe_central_asia_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = europe_central_asia_rv,
                          input_selected_eez = input$europe_central_asia_eez_select,
                          widget_id = "europe_central_asia_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$europe_central_asia_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = europe_central_asia_rv,
                          input_selected_eez = input$europe_central_asia_eez_select,
                          widget_id = "europe_central_asia_subsidies_select_flag_state")
    
  })
  
  ### Load Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$europe_central_asia_eez_select)
    
    # Save to reactive object
    europe_central_asia_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- europe_central_asia_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$europe_central_asia_eez_select)
    
    europe_central_asia_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all,
        input$europe_central_asia_effort_high_seas == TRUE | input$europe_central_asia_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$europe_central_asia_eez_select])
    
    if(all(fao_regions %in% europe_central_asia_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      europe_central_asia_rv$hs_dat <- hs_dat
      europe_central_asia_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$europe_central_asia_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all,
        nrow(europe_central_asia_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = europe_central_asia_rv,
                              input_selected_flag_state = input$europe_central_asia_effort_select_flag_state,
                              input_selected_eez = input$europe_central_asia_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    europe_central_asia_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = europe_central_asia_rv,
                  input_selected_eez = input$europe_central_asia_eez_select,
                  map_id = "europe_central_asia_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$europe_central_asia_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all,
        nrow(europe_central_asia_rv$eez_dat) > 0,
        input$europe_central_asia_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = europe_central_asia_rv,
                  input_selected_eez = input$europe_central_asia_eez_select,
                  map_id = "europe_central_asia_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  europe_central_asia_effort_map_proxy <- leafletProxy("europe_central_asia_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$europe_central_asia_effort_select_flag_state, {
    
    if(input$europe_central_asia_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      europe_central_asia_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      europe_central_asia_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = europe_central_asia_rv,
                                input_selected_flag_state = input$europe_central_asia_effort_select_flag_state,
                                input_selected_eez = input$europe_central_asia_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      europe_central_asia_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      europe_central_asia_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      europe_central_asia_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$europe_central_asia_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all,
        nrow(europe_central_asia_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = europe_central_asia_rv,
                              input_selected_flag_state = input$europe_central_asia_subsidies_select_flag_state,
                              input_selected_eez = input$europe_central_asia_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    europe_central_asia_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = europe_central_asia_rv,
                  input_selected_eez = input$europe_central_asia_eez_select,
                  map_id = "europe_central_asia_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$europe_central_asia_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$europe_central_asia_eez_select != "Select a coastal state..." & input$europe_central_asia_eez_select %in% good_eezs_all,
        nrow(europe_central_asia_rv$eez_dat) > 0,
        input$europe_central_asia_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = europe_central_asia_rv,
                  input_selected_eez = input$europe_central_asia_eez_select,
                  map_id = "europe_central_asia_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  europe_central_asia_subsidies_map_proxy <- leafletProxy("europe_central_asia_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$europe_central_asia_subsidies_select_flag_state, {
    
    if(input$europe_central_asia_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      europe_central_asia_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      europe_central_asia_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = europe_central_asia_rv,
                                input_selected_flag_state = input$europe_central_asia_subsidies_select_flag_state,
                                input_selected_eez = input$europe_central_asia_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      europe_central_asia_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      europe_central_asia_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      europe_central_asia_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$europe_central_asia_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_europe_central_asia_",input$europe_central_asia_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$europe_central_asia_eez_select != "Select a coastal state...",
          nrow(europe_central_asia_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = europe_central_asia_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$europe_central_asia_eez_select,
                                     input_selected_flag_state = input$europe_central_asia_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$europe_central_asia_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_europe_central_asia_area_",input$europe_central_asia_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$europe_central_asia_eez_select != "Select a coastal state...",
          nrow(europe_central_asia_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = europe_central_asia_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$europe_central_asia_eez_select,
                                     input_selected_flag_state = input$europe_central_asia_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###------------------------------------------------------------------
  ### Latin America & Caribbean ---------------------------------------
  ###------------------------------------------------------------------
  
  ### Data Container ----------
  latin_america_caribbean_rv <- reactiveValues(eezs = eez_ter_360 %>%
                                             dplyr::filter(region == "Latin America & Caribbean"),
                                             map_lng = -70,
                                             map_lat = -15,
                                             map_zoom = 2,
                                             connect = eez_flag_state_connectivity %>%
                                               dplyr::filter(region == "Latin America & Caribbean"),
                                             eez_dat = NULL,
                                             eez_bb = NULL,
                                             hs_dat = NULL,
                                             hs_regions = NULL,
                                             links = eez_ter_links %>%
                                               dplyr::filter(region == "Latin America & Caribbean"))
  
  ### UI output: Select coastal state widget --------
  output$latin_america_caribbean_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = latin_america_caribbean_rv,
                    widget_id = "latin_america_caribbean_eez_select")
    
  })
  
  ### UI output: Selected coastal state stats -----------
  output$latin_america_caribbean_eez_select_stats <- renderUI({
    
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = latin_america_caribbean_rv,
              input_selected_eez = input$latin_america_caribbean_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$latin_america_caribbean_nav_map <- renderLeaflet({
    
    # Create regional navigation map
    NavMap(region_dat = latin_america_caribbean_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "latin_america_caribbean_nav_map",
           min_zoom = 1)

  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$latin_america_caribbean_nav_map_shape_click, {
    
    # Don't register clicks on the disputed/joint areas
    req(!is.null(input$latin_america_caribbean_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "latin_america_caribbean_eez_select",
                         selected = input$latin_america_caribbean_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  latin_america_caribbean_nav_map_proxy <- leafletProxy("latin_america_caribbean_nav_map")
  
  ### Leaflet proxy: Highlight selected coastal state --------------
  observeEvent(input$latin_america_caribbean_eez_select, {
    
    if(input$latin_america_caribbean_eez_select == "Select a coastal state..." | !(input$latin_america_caribbean_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon and reorient
      latin_america_caribbean_nav_map_proxy %>% 
        clearGroup("highlighted_eez") %>%
        setView(lng = latin_america_caribbean_rv$map_lng, 
                lat = latin_america_caribbean_rv$map_lat, 
                zoom = latin_america_caribbean_rv$map_zoom)
      
    }else{
      
      # Highlight and center EEZ(s) corresponding to selected coastal state
      NavMapHighlight(region_dat = latin_america_caribbean_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = latin_america_caribbean_nav_map_proxy,
                      input_selected_eez = input$latin_america_caribbean_eez_select)
    }
    
  })
  
  ### UI output: Distant water summary for selected state -------------
  output$latin_america_caribbean_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = latin_america_caribbean_rv,
              input_selected_eez = input$latin_america_caribbean_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$latin_america_caribbean_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "latin_america_caribbean")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$latin_america_caribbean_download_data <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$latin_america_caribbean_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = latin_america_caribbean_rv,
                          input_selected_eez = input$latin_america_caribbean_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$latin_america_caribbean_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryDT(region_dat = latin_america_caribbean_rv,
              input_selected_eez = input$latin_america_caribbean_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$latin_america_caribbean_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = latin_america_caribbean_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$latin_america_caribbean_eez_select,
                     input_fill_variable = input$latin_america_caribbean_vessel_origins_fill,
                     input_fill_scale = input$latin_america_caribbean_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "latin_america_caribbean_vessel_origins_map")
    
  })
  
  ### UI output: Select flag state widget (effort) --------
  output$latin_america_caribbean_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = latin_america_caribbean_rv,
                          input_selected_eez = input$latin_america_caribbean_eez_select,
                          widget_id = "latin_america_caribbean_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$latin_america_caribbean_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = latin_america_caribbean_rv,
                          input_selected_eez = input$latin_america_caribbean_eez_select,
                          widget_id = "latin_america_caribbean_subsidies_select_flag_state")
    
  })
  
  ### Load Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$latin_america_caribbean_eez_select)
    
    # Save to reactive object
    latin_america_caribbean_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- latin_america_caribbean_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$latin_america_caribbean_eez_select)
    
    latin_america_caribbean_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
        input$latin_america_caribbean_effort_high_seas == TRUE | input$latin_america_caribbean_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$latin_america_caribbean_eez_select])
    
    if(all(fao_regions %in% latin_america_caribbean_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      latin_america_caribbean_rv$hs_dat <- hs_dat
      latin_america_caribbean_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$latin_america_caribbean_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
        nrow(latin_america_caribbean_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = latin_america_caribbean_rv,
                              input_selected_flag_state = input$latin_america_caribbean_effort_select_flag_state,
                              input_selected_eez = input$latin_america_caribbean_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    latin_america_caribbean_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = latin_america_caribbean_rv,
                  input_selected_eez = input$latin_america_caribbean_eez_select,
                  map_id = "latin_america_caribbean_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$latin_america_caribbean_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
        nrow(latin_america_caribbean_rv$eez_dat) > 0,
        input$latin_america_caribbean_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = latin_america_caribbean_rv,
                  input_selected_eez = input$latin_america_caribbean_eez_select,
                  map_id = "latin_america_caribbean_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  latin_america_caribbean_effort_map_proxy <- leafletProxy("latin_america_caribbean_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$latin_america_caribbean_effort_select_flag_state, {
    
    if(input$latin_america_caribbean_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      latin_america_caribbean_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      latin_america_caribbean_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = latin_america_caribbean_rv,
                                input_selected_flag_state = input$latin_america_caribbean_effort_select_flag_state,
                                input_selected_eez = input$latin_america_caribbean_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      latin_america_caribbean_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      latin_america_caribbean_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      latin_america_caribbean_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$latin_america_caribbean_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
        nrow(latin_america_caribbean_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = latin_america_caribbean_rv,
                              input_selected_flag_state = input$latin_america_caribbean_subsidies_select_flag_state,
                              input_selected_eez = input$latin_america_caribbean_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    latin_america_caribbean_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = latin_america_caribbean_rv,
                  input_selected_eez = input$latin_america_caribbean_eez_select,
                  map_id = "latin_america_caribbean_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$latin_america_caribbean_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
        nrow(latin_america_caribbean_rv$eez_dat) > 0,
        input$latin_america_caribbean_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = latin_america_caribbean_rv,
                  input_selected_eez = input$latin_america_caribbean_eez_select,
                  map_id = "latin_america_caribbean_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  latin_america_caribbean_subsidies_map_proxy <- leafletProxy("latin_america_caribbean_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$latin_america_caribbean_subsidies_select_flag_state, {
    
    if(input$latin_america_caribbean_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      latin_america_caribbean_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      latin_america_caribbean_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = latin_america_caribbean_rv,
                                input_selected_flag_state = input$latin_america_caribbean_subsidies_select_flag_state,
                                input_selected_eez = input$latin_america_caribbean_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      latin_america_caribbean_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      latin_america_caribbean_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      latin_america_caribbean_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$latin_america_caribbean_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_latin_america_caribbean_",input$latin_america_caribbean_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
          nrow(latin_america_caribbean_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = latin_america_caribbean_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$latin_america_caribbean_eez_select,
                                     input_selected_flag_state = input$latin_america_caribbean_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$latin_america_caribbean_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_latin_america_caribbean_area_",input$latin_america_caribbean_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$latin_america_caribbean_eez_select != "Select a coastal state..." & input$latin_america_caribbean_eez_select %in% good_eezs_all,
          nrow(latin_america_caribbean_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = latin_america_caribbean_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$latin_america_caribbean_eez_select,
                                     input_selected_flag_state = input$latin_america_caribbean_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###------------------------------------------------------------------
  ### Middle East & North Africa --------------------------------------
  ###------------------------------------------------------------------
  
  ### Data Container ----------
  middle_east_north_africa_rv <- reactiveValues(eezs = eez_ter_360 %>%
                                                  dplyr::filter(region == "Middle East & North Africa"),
                                                map_lng = 25,
                                                map_lat = 10,
                                                map_zoom = 2,
                                                connect = eez_flag_state_connectivity %>%
                                                  dplyr::filter(region == "Middle East & North Africa"),
                                                eez_dat = NULL,
                                                eez_bb = NULL,
                                                hs_dat = NULL,
                                                hs_regions = NULL,
                                                links = eez_ter_links %>%
                                                  dplyr::filter(region == "Middle East & North Africa"))
  
  ### UI output: Select coastal state widget --------
  output$middle_east_north_africa_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = middle_east_north_africa_rv,
                    widget_id = "middle_east_north_africa_eez_select")
    
  })
  
  ### UI output: Selected coastal state stats -----------
  output$middle_east_north_africa_eez_select_stats <- renderUI({
    
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = middle_east_north_africa_rv,
              input_selected_eez = input$middle_east_north_africa_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$middle_east_north_africa_nav_map <- renderLeaflet({
    
    NavMap(region_dat = middle_east_north_africa_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "middle_east_north_africa_nav_map",
           min_zoom = 1)

  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$middle_east_north_africa_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$middle_east_north_africa_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "middle_east_north_africa_eez_select",
                         selected = input$middle_east_north_africa_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  middle_east_north_africa_nav_map_proxy <- leafletProxy("middle_east_north_africa_nav_map")
  
  ### Leaflet proxy: Highlight and zoom to EEZ(s) cooresponding to selected state ---------
  observeEvent(input$middle_east_north_africa_eez_select, {
    
    if(input$middle_east_north_africa_eez_select == "Select a coastal state..." | !(input$middle_east_north_africa_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon
      middle_east_north_africa_nav_map_proxy %>% 
        clearGroup("highlighted_eez")  %>%
        setView(lng = middle_east_north_africa_rv$map_lng, 
                lat = middle_east_north_africa_rv$map_lat, 
                zoom = middle_east_north_africa_rv$map_zoom)
      
    }else{
      
      # Highlight and center EEZ(s) corresponding to selected coastal state
      NavMapHighlight(region_dat = middle_east_north_africa_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = middle_east_north_africa_nav_map_proxy,
                      input_selected_eez = input$middle_east_north_africa_eez_select)
      
    }
    
  })
  
  ### UI output: Distant water summary for selected state -------------
  output$middle_east_north_africa_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = middle_east_north_africa_rv,
              input_selected_eez = input$middle_east_north_africa_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$middle_east_north_africa_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "middle_east_north_africa")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$middle_east_north_africa_download_data <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$middle_east_north_africa_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = middle_east_north_africa_rv,
                          input_selected_eez = input$middle_east_north_africa_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$middle_east_north_africa_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryDT(region_dat = middle_east_north_africa_rv,
              input_selected_eez = input$middle_east_north_africa_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$middle_east_north_africa_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = middle_east_north_africa_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$middle_east_north_africa_eez_select,
                     input_fill_variable = input$middle_east_north_africa_vessel_origins_fill,
                     input_fill_scale = input$middle_east_north_africa_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "middle_east_north_africa_vessel_origins_map")
    
  })
  
  ### UI output: Select flag state widget (effort) --------
  output$middle_east_north_africa_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = middle_east_north_africa_rv,
                          input_selected_eez = input$middle_east_north_africa_eez_select,
                          widget_id = "middle_east_north_africa_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$middle_east_north_africa_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = middle_east_north_africa_rv,
                          input_selected_eez = input$middle_east_north_africa_eez_select,
                          widget_id = "middle_east_north_africa_subsidies_select_flag_state")
    
  })
  
  ### Load Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$middle_east_north_africa_eez_select)
    
    # Save to reactive object
    middle_east_north_africa_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- middle_east_north_africa_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$middle_east_north_africa_eez_select)
    
    middle_east_north_africa_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
        input$middle_east_north_africa_effort_high_seas == TRUE | input$middle_east_north_africa_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$middle_east_north_africa_eez_select])
    
    if(all(fao_regions %in% middle_east_north_africa_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      middle_east_north_africa_rv$hs_dat <- hs_dat
      middle_east_north_africa_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$middle_east_north_africa_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
        nrow(middle_east_north_africa_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = middle_east_north_africa_rv,
                              input_selected_flag_state = input$middle_east_north_africa_effort_select_flag_state,
                              input_selected_eez = input$middle_east_north_africa_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    middle_east_north_africa_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = middle_east_north_africa_rv,
                  input_selected_eez = input$middle_east_north_africa_eez_select,
                  map_id = "middle_east_north_africa_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$middle_east_north_africa_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
        nrow(middle_east_north_africa_rv$eez_dat) > 0,
        input$middle_east_north_africa_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = middle_east_north_africa_rv,
                  input_selected_eez = input$middle_east_north_africa_eez_select,
                  map_id = "middle_east_north_africa_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  middle_east_north_africa_effort_map_proxy <- leafletProxy("middle_east_north_africa_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$middle_east_north_africa_effort_select_flag_state, {
    
    if(input$middle_east_north_africa_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      middle_east_north_africa_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      middle_east_north_africa_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = middle_east_north_africa_rv,
                                input_selected_flag_state = input$middle_east_north_africa_effort_select_flag_state,
                                input_selected_eez = input$middle_east_north_africa_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      middle_east_north_africa_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      middle_east_north_africa_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      middle_east_north_africa_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$middle_east_north_africa_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
        nrow(middle_east_north_africa_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = middle_east_north_africa_rv,
                              input_selected_flag_state = input$middle_east_north_africa_subsidies_select_flag_state,
                              input_selected_eez = input$middle_east_north_africa_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    middle_east_north_africa_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = middle_east_north_africa_rv,
                  input_selected_eez = input$middle_east_north_africa_eez_select,
                  map_id = "middle_east_north_africa_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$middle_east_north_africa_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
        nrow(middle_east_north_africa_rv$eez_dat) > 0,
        input$middle_east_north_africa_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = middle_east_north_africa_rv,
                  input_selected_eez = input$middle_east_north_africa_eez_select,
                  map_id = "middle_east_north_africa_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  middle_east_north_africa_subsidies_map_proxy <- leafletProxy("middle_east_north_africa_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$middle_east_north_africa_subsidies_select_flag_state, {
    
    if(input$middle_east_north_africa_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      middle_east_north_africa_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      middle_east_north_africa_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = middle_east_north_africa_rv,
                                input_selected_flag_state = input$middle_east_north_africa_subsidies_select_flag_state,
                                input_selected_eez = input$middle_east_north_africa_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      middle_east_north_africa_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      middle_east_north_africa_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      middle_east_north_africa_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$middle_east_north_africa_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_middle_east_north_africa_",input$middle_east_north_africa_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
          nrow(middle_east_north_africa_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = middle_east_north_africa_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$middle_east_north_africa_eez_select,
                                     input_selected_flag_state = input$middle_east_north_africa_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$middle_east_north_africa_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_middle_east_north_africa_area_",input$middle_east_north_africa_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$middle_east_north_africa_eez_select != "Select a coastal state..." & input$middle_east_north_africa_eez_select %in% good_eezs_all,
          nrow(middle_east_north_africa_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = middle_east_north_africa_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$middle_east_north_africa_eez_select,
                                     input_selected_flag_state = input$middle_east_north_africa_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###------------------------------------------------------------
  ### North America ---------------------------------------------
  ###------------------------------------------------------------
  
  ### Data Container ----------
  north_america_rv <- reactiveValues(eezs = eez_ter_360 %>%
                                       dplyr::filter(region == "North America"),
                                     map_lng = -100,
                                     map_lat = 40,
                                     map_zoom = 2,
                                     connect = eez_flag_state_connectivity %>%
                                       dplyr::filter(region == "North America"),
                                     eez_dat = NULL,
                                     eez_bb = NULL,
                                     hs_dat = NULL,
                                     hs_regions = NULL,
                                     links = eez_ter_links %>%
                                       dplyr::filter(region == "North America"))
  
  ### UI output: Select coastal state widget --------
  output$north_america_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = north_america_rv,
                    widget_id = "north_america_eez_select")
    
  })
  
  ### UI output: Selected coastal state stats -----------
  output$north_america_eez_select_stats <- renderUI({
    
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = north_america_rv,
              input_selected_eez = input$north_america_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$north_america_nav_map <- renderLeaflet({
    
    NavMap(region_dat = north_america_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "north_america_nav_map",
           min_zoom = 1)

  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$north_america_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$north_america_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "north_america_eez_select",
                         selected = input$north_america_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  north_america_nav_map_proxy <- leafletProxy("north_america_nav_map")
  
  ### Leaflet proxy: Highlight and zoom to the EEZ(s) cooresponding to the selected state ----------
  observeEvent(input$north_america_eez_select, {
    
    if(input$north_america_eez_select == "Select a coastal state..." | !(input$north_america_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon and reorient
      north_america_nav_map_proxy %>% 
        clearGroup("highlighted_eez") %>%
        setView(lng = north_america_rv$map_lng, 
                lat = north_america_rv$map_lat, 
                zoom = north_america_rv$map_zoom)
      
    }else{
      
      # Highlight and center EEZ(s) corresponding to selected coastal state
      NavMapHighlight(region_dat = north_america_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = north_america_nav_map_proxy,
                      input_selected_eez = input$north_america_eez_select)
      
    }
    
  }) 
  
  ### UI output: Distant water summary for selected state -------------
  output$north_america_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = north_america_rv,
              input_selected_eez = input$north_america_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$north_america_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "north_america")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$north_america_download_data <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$north_america_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = north_america_rv,
                          input_selected_eez = input$north_america_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$north_america_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all) 
    
    # Generate summary
    SummaryDT(region_dat = north_america_rv,
              input_selected_eez = input$north_america_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$north_america_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = north_america_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$north_america_eez_select,
                     input_fill_variable = input$north_america_vessel_origins_fill,
                     input_fill_scale = input$north_america_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "north_america_vessel_origins_map")
    
  })
  
  ### UI output: Select flag state widget (effort) --------
  output$north_america_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = north_america_rv,
                          input_selected_eez = input$north_america_eez_select,
                          widget_id = "north_america_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$north_america_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = north_america_rv,
                          input_selected_eez = input$north_america_eez_select,
                          widget_id = "north_america_subsidies_select_flag_state")
    
  })
  
  ### Load Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$north_america_eez_select)
    
    # Save to reactive object
    north_america_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- north_america_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$north_america_eez_select)
    
    north_america_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
        input$north_america_effort_high_seas == TRUE | input$north_america_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$north_america_eez_select])
    
    if(all(fao_regions %in% north_america_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      north_america_rv$hs_dat <- hs_dat
      north_america_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$north_america_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
        nrow(north_america_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = north_america_rv,
                              input_selected_flag_state = input$north_america_effort_select_flag_state,
                              input_selected_eez = input$north_america_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    north_america_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = north_america_rv,
                  input_selected_eez = input$north_america_eez_select,
                  map_id = "north_america_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$north_america_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
        nrow(north_america_rv$eez_dat) > 0,
        input$north_america_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = north_america_rv,
                  input_selected_eez = input$north_america_eez_select,
                  map_id = "north_america_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  north_america_effort_map_proxy <- leafletProxy("north_america_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$north_america_effort_select_flag_state, {
    
    if(input$north_america_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      north_america_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      north_america_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = north_america_rv,
                                input_selected_flag_state = input$north_america_effort_select_flag_state,
                                input_selected_eez = input$north_america_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      north_america_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      north_america_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      north_america_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$north_america_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
        nrow(north_america_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = north_america_rv,
                              input_selected_flag_state = input$north_america_subsidies_select_flag_state,
                              input_selected_eez = input$north_america_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    north_america_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = north_america_rv,
                  input_selected_eez = input$north_america_eez_select,
                  map_id = "north_america_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$north_america_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
        nrow(north_america_rv$eez_dat) > 0,
        input$north_america_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = north_america_rv,
                  input_selected_eez = input$north_america_eez_select,
                  map_id = "north_america_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  north_america_subsidies_map_proxy <- leafletProxy("north_america_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$north_america_subsidies_select_flag_state, {
    
    if(input$north_america_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      north_america_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      north_america_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = north_america_rv,
                                input_selected_flag_state = input$north_america_subsidies_select_flag_state,
                                input_selected_eez = input$north_america_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      north_america_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      north_america_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      north_america_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$north_america_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_north_america_",input$north_america_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
          nrow(north_america_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = north_america_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$north_america_eez_select,
                                     input_selected_flag_state = input$north_america_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$north_america_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_north_america_area_",input$north_america_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$north_america_eez_select != "Select a coastal state..." & input$north_america_eez_select %in% good_eezs_all,
          nrow(north_america_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = north_america_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$north_america_eez_select,
                                     input_selected_flag_state = input$north_america_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###---------------------------------------------------------
  ### South Asia ---------------------------------------------
  ###---------------------------------------------------------
  
  ### Data Container ----------
  south_asia_rv <- reactiveValues(eezs = eez_ter_360 %>%
                                       dplyr::filter(region == "South Asia"),
                                  map_lng = 80,
                                  map_lat = 15,
                                  map_zoom = 3,
                                  connect = eez_flag_state_connectivity %>%
                                    dplyr::filter(region == "South Asia"),
                                  eez_dat = NULL,
                                  eez_bb = NULL,
                                  hs_dat = NULL,
                                  hs_regions = NULL,
                                  links = eez_ter_links %>%
                                    dplyr::filter(region == "South Asia"))
  
  ### UI output: Select coastal state widget --------
  output$south_asia_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = south_asia_rv,
                    widget_id = "south_asia_eez_select")

  })
  
  ### UI output: Selected coastal state stats -----------
  output$south_asia_eez_select_stats <- renderUI({
    
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = south_asia_rv,
              input_selected_eez = input$south_asia_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$south_asia_nav_map <- renderLeaflet({
    
    NavMap(region_dat = south_asia_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "south_asia_nav_map",
           min_zoom = 1)
 
  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$south_asia_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$south_asia_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "south_asia_eez_select",
                         selected = input$south_asia_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  south_asia_nav_map_proxy <- leafletProxy("south_asia_nav_map")
  
  ### Leaflet proxy: Highlight and zoom to EEZ(s) cooresponding to selected state ----------
  observeEvent(input$south_asia_eez_select, {
    
    if(input$south_asia_eez_select == "Select a coastal state..." | !(input$south_asia_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon and reorient
      south_asia_nav_map_proxy %>% 
        clearGroup("highlighted_eez") %>%  
        setView(lng = south_asia_rv$map_lng, 
                lat = south_asia_rv$map_lat, 
                zoom = south_asia_rv$map_zoom)
      
    }else{
      
      # Highlight and center EEZ(s) corresponding to selected coastal state
      NavMapHighlight(region_dat = south_asia_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = south_asia_nav_map_proxy,
                      input_selected_eez = input$south_asia_eez_select)

    }
    
  })
  
  ### UI output: Distant water summary for selected state -------------
  output$south_asia_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = south_asia_rv,
              input_selected_eez = input$south_asia_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$south_asia_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "south_asia")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$south_asia_download_data <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$south_asia_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = south_asia_rv,
                          input_selected_eez = input$south_asia_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$south_asia_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryDT(region_dat = south_asia_rv,
              input_selected_eez = input$south_asia_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$south_asia_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = south_asia_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$south_asia_eez_select,
                     input_fill_variable = input$south_asia_vessel_origins_fill,
                     input_fill_scale = input$south_asia_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "south_asia_vessel_origins_map")
    
  })
  
  ### UI output: Select flag state widget (effort) --------
  output$south_asia_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = south_asia_rv,
                          input_selected_eez = input$south_asia_eez_select,
                          widget_id = "south_asia_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$south_asia_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = south_asia_rv,
                          input_selected_eez = input$south_asia_eez_select,
                          widget_id = "south_asia_subsidies_select_flag_state")
    
  })
  
  ### Load Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$south_asia_eez_select)
    
    # Save to reactive object
    south_asia_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- south_asia_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$south_asia_eez_select)
    
    south_asia_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
        input$south_asia_effort_high_seas == TRUE | input$south_asia_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$south_asia_eez_select])
    
    if(all(fao_regions %in% south_asia_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      south_asia_rv$hs_dat <- hs_dat
      south_asia_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$south_asia_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
        nrow(south_asia_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = south_asia_rv,
                              input_selected_flag_state = input$south_asia_effort_select_flag_state,
                              input_selected_eez = input$south_asia_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    south_asia_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = south_asia_rv,
                  input_selected_eez = input$south_asia_eez_select,
                  map_id = "south_asia_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$south_asia_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
        nrow(south_asia_rv$eez_dat) > 0,
        input$south_asia_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = south_asia_rv,
                  input_selected_eez = input$south_asia_eez_select,
                  map_id = "south_asia_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  south_asia_effort_map_proxy <- leafletProxy("south_asia_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$south_asia_effort_select_flag_state, {
    
    if(input$south_asia_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      south_asia_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      south_asia_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = south_asia_rv,
                                input_selected_flag_state = input$south_asia_effort_select_flag_state,
                                input_selected_eez = input$south_asia_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      south_asia_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      south_asia_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      south_asia_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$south_asia_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
        nrow(south_asia_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = south_asia_rv,
                              input_selected_flag_state = input$south_asia_subsidies_select_flag_state,
                              input_selected_eez = input$south_asia_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    south_asia_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = south_asia_rv,
                  input_selected_eez = input$south_asia_eez_select,
                  map_id = "south_asia_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$south_asia_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
        nrow(south_asia_rv$eez_dat) > 0,
        input$south_asia_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = south_asia_rv,
                  input_selected_eez = input$south_asia_eez_select,
                  map_id = "south_asia_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  south_asia_subsidies_map_proxy <- leafletProxy("south_asia_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$south_asia_subsidies_select_flag_state, {
    
    if(input$south_asia_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      south_asia_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      south_asia_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = south_asia_rv,
                                input_selected_flag_state = input$south_asia_subsidies_select_flag_state,
                                input_selected_eez = input$south_asia_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      south_asia_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      south_asia_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      south_asia_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$south_asia_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_south_asia_",input$south_asia_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
          nrow(south_asia_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = south_asia_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$south_asia_eez_select,
                                     input_selected_flag_state = input$south_asia_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$south_asia_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_south_asia_area_",input$south_asia_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$south_asia_eez_select != "Select a coastal state..." & input$south_asia_eez_select %in% good_eezs_all,
          nrow(south_asia_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = south_asia_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$south_asia_eez_select,
                                     input_selected_flag_state = input$south_asia_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###-----------------------------------------------------------------
  ### Sub-Saharan Africa ---------------------------------------------
  ###-----------------------------------------------------------------
  
  ### Data Container ----------
  sub_saharan_africa_rv <- reactiveValues(eezs = eez_ter_360 %>%
                                            dplyr::filter(region == "Sub-Saharan Africa"),
                                          map_lng = 25,
                                          map_lat = -15,
                                          map_zoom = 2,
                                          connect = eez_flag_state_connectivity %>%
                                            dplyr::filter(region == "Sub-Saharan Africa"),
                                          eez_dat = NULL,
                                          eez_bb = NULL,
                                          hs_dat = NULL,
                                          hs_regions = NULL,
                                          links = eez_ter_links %>%
                                            dplyr::filter(region == "Sub-Saharan Africa"))
  
  ### UI output: Select coastal state widget --------
  output$sub_saharan_africa_eez_select <- renderUI({
    
    WidgetEEZSelect(region_dat = sub_saharan_africa_rv,
                    widget_id = "sub_saharan_africa_eez_select")
    
  })
  
  ### UI output: Selected coastal state stats -----------
  output$sub_saharan_africa_eez_select_stats <- renderUI({
    
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    StateInfo(region_dat = sub_saharan_africa_rv,
              input_selected_eez = input$sub_saharan_africa_eez_select)
    
  })
  
  ### Leaflet output: Navigational map for the region ---------
  output$sub_saharan_africa_nav_map <- renderLeaflet({
    
    NavMap(region_dat = sub_saharan_africa_rv,
           region_pal = region_pal,
           region_pal_dark = region_pal_dark,
           map_id = "sub_saharan_africa_nav_map",
           min_zoom = 1)

  })
  
  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$sub_saharan_africa_nav_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$sub_saharan_africa_nav_map_shape_click$id))
    
    updateSelectizeInput(session, "sub_saharan_africa_eez_select",
                         selected = input$sub_saharan_africa_nav_map_shape_click$id)
    
  })
  
  ### Leaflet proxy: Create proxy for the nav map -----------
  sub_saharan_africa_nav_map_proxy <- leafletProxy("sub_saharan_africa_nav_map")
  
  ### Leaflet proxy: Highlight and zoom to EEZ(s) cooresponding to selected state ------------
  observeEvent(input$sub_saharan_africa_eez_select, {
    
    if(input$sub_saharan_africa_eez_select == "Select a coastal state..." | !(input$sub_saharan_africa_eez_select %in% good_eezs_all)){
      
      # Remove any previously highlighted polygon and reorient
      sub_saharan_africa_nav_map_proxy %>% 
        clearGroup("highlighted_eez") %>% 
        setView(lng = sub_saharan_africa_rv$map_lng, 
                lat = sub_saharan_africa_rv$map_lat, 
                zoom = sub_saharan_africa_rv$map_zoom)
      
    }else{
      
      # Highlight and center EEZ(s) corresponding to selected coastal state
      NavMapHighlight(region_dat = sub_saharan_africa_rv,
                      region_pal_light = region_pal_light,
                      proxy_map = sub_saharan_africa_nav_map_proxy,
                      input_selected_eez = input$sub_saharan_africa_eez_select)
    }
    
  })
  
  ### UI output: Distant water summary for selected state -------------
  output$sub_saharan_africa_summary_ui <- renderUI({
    
    # Generate summary
    SummaryUI(region_dat = sub_saharan_africa_rv,
              input_selected_eez = input$sub_saharan_africa_eez_select)
    
  })
  
  ### UI output: Distant water summary by flag state for selected state -------------
  output$sub_saharan_africa_summary_ui_flag <- renderUI({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryUIFlag(region_name = "sub_saharan_africa")
    
  })
  
  ### Download buttons: DW fishing data (CSV) -----------------------
  output$sub_saharan_africa_download_data <- downloadHandler(
    
    filename = function() {
      paste0("DWFA_distant_water_fishing_in_EEZ_of_", input$sub_saharan_africa_eez_select, ".csv")
    },
    content = function(file) {
      
      dat <- DownloadData(region_dat = sub_saharan_africa_rv,
                          input_selected_eez = input$sub_saharan_africa_eez_select)
      
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  ### DT output: Distant water summary (by flag) for selected state -------------
  output$sub_saharan_africa_summary_dt <- renderDataTable({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    # Generate summary
    SummaryDT(region_dat = sub_saharan_africa_rv,
              input_selected_eez = input$sub_saharan_africa_eez_select)
    
  })
  
  ### Leaflet output: Connectivity map for selected state -----------
  output$sub_saharan_africa_vessel_origins_map <- renderLeaflet({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    # Make connectivity map
    VesselOriginsMap(region_dat = sub_saharan_africa_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$sub_saharan_africa_eez_select,
                     input_fill_variable = input$sub_saharan_africa_vessel_origins_fill,
                     input_fill_scale = input$sub_saharan_africa_vessel_origins_fill_rescale,
                     region_pal_light = region_pal_light,
                     map_id = "sub_saharan_africa_vessel_origins_map")
  })
  
  ### UI output: Select flag state widget (effort) --------
  output$sub_saharan_africa_effort_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    WidgetFlagStateSelect(region_dat = sub_saharan_africa_rv,
                          input_selected_eez = input$sub_saharan_africa_eez_select,
                          widget_id = "sub_saharan_africa_effort_select_flag_state")
    
  })
  
  ### UI output: Select flag state widget (subsidies) --------
  output$sub_saharan_africa_subsidies_select_flag_state <- renderUI({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)

    WidgetFlagStateSelect(region_dat = sub_saharan_africa_rv,
                          input_selected_eez = input$sub_saharan_africa_eez_select,
                          widget_id = "sub_saharan_africa_subsidies_select_flag_state")
    
  })
  
  ### Load EEZ Data -----------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    # Load data
    eez_dat <- LoadEEZData(input_selected_eez = input$sub_saharan_africa_eez_select)
    
    # Save to reactive object
    sub_saharan_africa_rv$eez_dat <- eez_dat
    
  })
  
  ### Bounding box of selected coastal state ----------------
  observe({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all)
    
    # Crop eez shapefile based on the region 
    eez_region <- sub_saharan_africa_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(eez_ter_iso3 == input$sub_saharan_africa_eez_select)
    
    sub_saharan_africa_rv$eez_bb <- st_bbox(eez_region)
    
  })
  
  ### Load FAO Region Data -------------------------------------------------
  observe({
    
    # Require coastal state selection
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
        input$sub_saharan_africa_effort_high_seas == TRUE | input$sub_saharan_africa_subsidies_high_seas == TRUE)
    
    # Find Corresponding FAO regions 
    fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$sub_saharan_africa_eez_select])
    
    if(all(fao_regions %in% sub_saharan_africa_rv$hs_regions) == F){
      
      # Load data
      hs_dat <- LoadHSData(input_selected_regions = fao_regions)
      
      # Save to reactive object
      sub_saharan_africa_rv$hs_dat <- hs_dat
      sub_saharan_africa_rv$hs_regions <- fao_regions
      
    }
  })
  
  ### Leaflet output: Effort plot  ---------
  output$sub_saharan_africa_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
        nrow(sub_saharan_africa_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = sub_saharan_africa_rv,
                              input_selected_flag_state = input$sub_saharan_africa_effort_select_flag_state,
                              input_selected_eez = input$sub_saharan_africa_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh")
    
    # Save to rv
    sub_saharan_africa_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = sub_saharan_africa_rv,
                  input_selected_eez = input$sub_saharan_africa_eez_select,
                  map_id = "sub_saharan_africa_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$sub_saharan_africa_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
        nrow(sub_saharan_africa_rv$eez_dat) > 0,
        input$sub_saharan_africa_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = sub_saharan_africa_rv,
                  input_selected_eez = input$sub_saharan_africa_eez_select,
                  map_id = "sub_saharan_africa_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  sub_saharan_africa_effort_map_proxy <- leafletProxy("sub_saharan_africa_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$sub_saharan_africa_effort_select_flag_state, {
    
    if(input$sub_saharan_africa_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      sub_saharan_africa_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      sub_saharan_africa_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = sub_saharan_africa_rv,
                                input_selected_flag_state = input$sub_saharan_africa_effort_select_flag_state,
                                input_selected_eez = input$sub_saharan_africa_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh")
      
      # Save to rv
      sub_saharan_africa_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      sub_saharan_africa_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      sub_saharan_africa_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$sub_saharan_africa_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
        nrow(sub_saharan_africa_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = sub_saharan_africa_rv,
                              input_selected_flag_state = input$sub_saharan_africa_subsidies_select_flag_state,
                              input_selected_eez = input$sub_saharan_africa_eez_select,
                              type = "total",
                              plot_variable = "subs")
    
    # Add to rv
    sub_saharan_africa_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = sub_saharan_africa_rv,
                  input_selected_eez = input$sub_saharan_africa_eez_select,
                  map_id = "sub_saharan_africa_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$sub_saharan_africa_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
        nrow(sub_saharan_africa_rv$eez_dat) > 0,
        input$sub_saharan_africa_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = sub_saharan_africa_rv,
                  input_selected_eez = input$sub_saharan_africa_eez_select,
                  map_id = "sub_saharan_africa_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  sub_saharan_africa_subsidies_map_proxy <- leafletProxy("sub_saharan_africa_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$sub_saharan_africa_subsidies_select_flag_state, {
    
    if(input$sub_saharan_africa_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      sub_saharan_africa_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      sub_saharan_africa_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = sub_saharan_africa_rv,
                                input_selected_flag_state = input$sub_saharan_africa_subsidies_select_flag_state,
                                input_selected_eez = input$sub_saharan_africa_eez_select,
                                type = "flag",
                                plot_variable = "subs")
      
      # Add to rv
      sub_saharan_africa_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      sub_saharan_africa_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      sub_saharan_africa_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$sub_saharan_africa_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_sub_saharan_africa_",input$sub_saharan_africa_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
          nrow(sub_saharan_africa_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = sub_saharan_africa_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$sub_saharan_africa_eez_select,
                                     input_selected_flag_state = input$sub_saharan_africa_effort_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$sub_saharan_africa_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_sub_saharan_africa_area_",input$sub_saharan_africa_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$sub_saharan_africa_eez_select != "Select a coastal state..." & input$sub_saharan_africa_eez_select %in% good_eezs_all,
          nrow(sub_saharan_africa_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = sub_saharan_africa_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$sub_saharan_africa_eez_select,
                                     input_selected_flag_state = input$sub_saharan_africa_subsidies_select_flag_state,
                                     eez_sf = eez_ter_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ###--------------------------------------------------------
  ### High Seas ---------------------------------------------
  ###--------------------------------------------------------
  
  ### Data Container -----------
  high_seas_rv <- reactiveValues(eezs = fao_area_360,
                                 map_lng = 0,
                                 map_lat = -5,
                                 map_zoom = 1,
                                 connect = fao_flag_state_connectivity %>%
                                   dplyr::filter(region == "High Seas"),
                                 eez_dat = NULL,
                                 eez_bb = NULL,
                                 hs_dat = NULL,
                                 hs_regions = NULL)

  ### UI output: Select coastal state widget --------
  output$high_seas_eez_select <- renderUI({

    WidgetEEZSelect(region_dat = high_seas_rv,
                    widget_id = "high_seas_eez_select")

  })

  ### UI output: Selected coastal state stats -----------
  output$high_seas_eez_select_stats <- renderUI({

    req(input$high_seas_eez_select != "Select a coastal state...")

    StateInfo(region_dat = high_seas_rv,
              input_selected_eez = input$high_seas_eez_select,
              is_hs = T)

  })

  ### Leaflet output: Navigational map for the region ---------
  output$high_seas_nav_map <- renderLeaflet({

    NavMap(region_dat = high_seas_rv,
           region_pal = hs_pal,
           region_pal_dark = NA,
           map_id = "high_seas_nav_map",
           min_zoom = 1)

  })

  ### Update selectInput: Register user clicks on nav map ---------
  observeEvent(input$high_seas_nav_map_shape_click, {

    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$high_seas_nav_map_shape_click$id))

    updateSelectizeInput(session, "high_seas_eez_select",
                         selected = input$high_seas_nav_map_shape_click$id)

  })

  ### Leaflet proxy: Create proxy for the nav map -----------
  high_seas_nav_map_proxy <- leafletProxy("high_seas_nav_map")

  ### Leaflet proxy: Highlight and zoom to EEZ(s) cooresponding to selected state ---------
  observeEvent(input$high_seas_eez_select, {

    if(input$high_seas_eez_select == "Select a coastal state..."){

      # Remove any previously highlighted polygon and reorient
      high_seas_nav_map_proxy %>%
        clearGroup("highlighted_eez") %>%
        setView(lng = high_seas_rv$map_lng,
                lat = high_seas_rv$map_lat,
                zoom = high_seas_rv$map_zoom)

    }else{

      NavMapHighlight(region_dat = high_seas_rv,
                      region_pal_light = hs_pal,
                      proxy_map = high_seas_nav_map_proxy,
                      input_selected_eez = input$high_seas_eez_select,
                      is_hs = T)
    }

  })

  ### UI output: Distant water summary for selected state -------------
  output$high_seas_summary_ui <- renderUI({

    # Generate summary
    SummaryUI(region_dat = high_seas_rv,
              input_selected_eez = input$high_seas_eez_select,
              is_hs = T)

  })

  ### UI output: Distant water summary by flag state for selected state -------------
  output$high_seas_summary_ui_flag <- renderUI({

    # Require coastal state selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    # Generate summary
    SummaryUIFlag(region_name = "high_seas")

  })

  ### Download buttons: DW fishing data (CSV) -----------------------
  output$high_seas_download_data <- downloadHandler(

    filename = function() {
      paste0("DWFA_distant_water_fishing_in_high_seas_area_", input$high_seas_eez_select, ".csv")
    },
    content = function(file) {

      dat <- DownloadData(region_dat = high_seas_rv,
                          input_selected_eez = input$high_seas_eez_select,
                          is_hs = T)

      write.csv(dat, file, row.names = FALSE)
    }
  )

  ### DT output: Distant water summary (by flag) for selected state -------------
  output$high_seas_summary_dt <- renderDataTable({

    # Require coastal state selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    # Generate summary
    SummaryDT(region_dat = high_seas_rv,
              input_selected_eez = input$high_seas_eez_select,
              is_hs = T)

  })

  ### Leaflet output: Connectivity map for selected state -----------
  output$high_seas_vessel_origins_map <- renderLeaflet({

    # Require coastal state selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    # Make connectivity map
    VesselOriginsMap(region_dat = high_seas_rv,
                     land_sf = land_ter_360,
                     input_selected_eez = input$high_seas_eez_select,
                     input_fill_variable = input$high_seas_vessel_origins_fill,
                     input_fill_scale = input$high_seas_vessel_origins_fill_rescale,
                     region_pal_light = hs_pal,
                     map_id = "high_seas_vessel_origins_map")

  })

  ### UI output: Select flag state widget (effort) --------
  output$high_seas_effort_select_flag_state <- renderUI({

    # Require coastal state selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    WidgetFlagStateSelect(region_dat = high_seas_rv,
                          input_selected_eez = input$high_seas_eez_select,
                          widget_id = "high_seas_effort_select_flag_state")

  })

  ### UI output: Select flag state widget (subsidies) --------
  output$high_seas_subsidies_select_flag_state <- renderUI({

    # Require coastal state selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    WidgetFlagStateSelect(region_dat = high_seas_rv,
                          input_selected_eez = input$high_seas_eez_select,
                          widget_id = "high_seas_subsidies_select_flag_state")

  })

  ### Load Data -----------------------------------------------------
  observe({

    # Require FAO area selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    # Load data
    hs_dat <- LoadHSData(input_selected_regions = input$high_seas_eez_select)

    # Save to reactive object
    high_seas_rv$eez_dat <- hs_dat

  })

  ### Bounding box of selected FAO area ----------------
  observe({

    # Require coastal state selection
    req(input$high_seas_eez_select != "Select a coastal state...")

    # Crop FAO region shapefile based on the region
    eez_region <- high_seas_rv$eezs %>%
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>%
      dplyr::filter(zone == input$high_seas_eez_select)

    high_seas_rv$eez_bb <- st_bbox(eez_region)
    
  })

  # ### Load FAO Region Data -------------------------------------------------
  # observe({
  #   
  #   # Require coastal state selection
  #   req(input$east_asia_pacific_eez_select != "Select a coastal state...",
  #       input$east_asia_pacific_effort_high_seas == TRUE | input$east_asia_pacific_subsidies_high_seas == TRUE)
  #   
  #   # Find Corresponding FAO regions 
  #   fao_regions <- unique(fao_regions_by_eez$fao_region[fao_regions_by_eez$eez_ter_iso3 == input$east_asia_pacific_eez_select])
  #   
  #   if(all(fao_regions %in% east_asia_pacific_rv$hs_regions) == F){
  #     
  #     # Load data
  #     hs_dat <- LoadHSData(input_selected_regions = fao_regions)
  #     
  #     # Save to reactive object
  #     east_asia_pacific_rv$hs_dat <- hs_dat
  #     east_asia_pacific_rv$hs_regions <- fao_regions
  #     
  #   }
  # })
  # 
  
  ### Leaflet output: Effort plot  ---------
  output$high_seas_effort_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$high_seas_eez_select != "Select a coastal state...",
        nrow(high_seas_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = high_seas_rv,
                              input_selected_flag_state = input$high_seas_effort_select_flag_state,
                              input_selected_eez = input$high_seas_eez_select,
                              type = "total",
                              plot_variable = "fishing_KWh",
                              is_hs = T)
    
    # Save to rv
    high_seas_rv$effort_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = high_seas_rv,
                  input_selected_eez = input$high_seas_eez_select,
                  map_id = "high_seas_effort_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r, 
                     colors = raster$pal, 
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft", 
                pal = raster$pal, 
                values = values(raster$r), 
                title = raster$pal_title, 
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Effort plot (selected flag state) ----------------
  output$high_seas_effort_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$high_seas_eez_select != "Select a coastal state...",
        nrow(high_seas_rv$eez_dat) > 0,
        input$high_seas_effort_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = high_seas_rv,
                  input_selected_eez = input$high_seas_eez_select,
                  map_id = "high_seas_effort_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the effort map -----------
  high_seas_effort_map_proxy <- leafletProxy("high_seas_effort_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$high_seas_effort_select_flag_state, {
    
    if(input$high_seas_effort_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      high_seas_effort_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      high_seas_rv$effort_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = high_seas_rv,
                                input_selected_flag_state = input$high_seas_effort_select_flag_state,
                                input_selected_eez = input$high_seas_eez_select,
                                type = "flag",
                                plot_variable = "fishing_KWh",
                                is_hs = T)
      
      # Save to rv
      high_seas_rv$effort_selected_raster <- raster
      
      # Remove any previously added raster images
      high_seas_effort_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      high_seas_effort_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Leaflet output: Subsidies plot  ---------
  output$high_seas_subsidies_map_all <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$high_seas_eez_select != "Select a coastal state...",
        nrow(high_seas_rv$eez_dat) > 0)
    
    # Create raster layer and palette from EEZ data
    raster <- EEZDatRasterize(region_dat = high_seas_rv,
                              input_selected_flag_state = input$high_seas_subsidies_select_flag_state,
                              input_selected_eez = input$high_seas_eez_select,
                              type = "total",
                              plot_variable = "subs",
                              is_hs = T)
    
    # Save to rv
    high_seas_rv$subsidies_all_raster <- raster
    
    # Make leaflet object and add raster image
    EEZLeafletMap(region_dat = high_seas_rv,
                  input_selected_eez = input$high_seas_eez_select,
                  map_id = "high_seas_subsidies_map_all",
                  min_zoom = 1) %>%
      addRasterImage(raster$r,
                     colors = raster$pal,
                     opacity = 0.8) %>%
      addLegend(position = "bottomleft",
                pal = raster$pal,
                values = values(raster$r),
                title = raster$pal_title,
                labFormat = labelFormat(
                  transform = function(x) 10^x)) %>%
      addControl(raster$caption, position = "topright")
    
  })
  
  ### plotOutput: Subsidies plot (selected flag state) ----------------
  output$high_seas_subsidies_map_selected <- renderLeaflet({
    
    # Require coastal state selection & data
    req(input$high_seas_eez_select != "Select a coastal state...",
        nrow(high_seas_rv$eez_dat) > 0,
        input$high_seas_subsidies_select_flag_state != "Select a flag state...")
    
    # Make leaflet object
    EEZLeafletMap(region_dat = high_seas_rv,
                  input_selected_eez = input$high_seas_eez_select,
                  map_id = "high_seas_subsidies_map_selected",
                  min_zoom = 1)
    
  })
  
  ### Leaflet proxy: Create proxy for the subsidies map -----------
  high_seas_subsidies_map_proxy <- leafletProxy("high_seas_subsidies_map_selected")
  
  ### Leaflet proxy: Add data corresponding to selected flag state ---------
  observeEvent(input$high_seas_subsidies_select_flag_state, {
    
    if(input$high_seas_subsidies_select_flag_state == "Select a flag state..."){
      
      # Remove any previously added raster images
      high_seas_subsidies_map_proxy %>% 
        clearGroup("flag_state_dat")
      
      # Clear rv
      high_seas_rv$subsidies_selected_raster <- NULL
      
    }else{
      
      # Create raster layer and palette from EEZ data
      raster <- EEZDatRasterize(region_dat = high_seas_rv,
                                input_selected_flag_state = input$high_seas_subsidies_select_flag_state,
                                input_selected_eez = input$high_seas_eez_select,
                                type = "flag",
                                plot_variable = "subs",
                                is_hs = T)
      
      # Add to rv
      high_seas_rv$subsidies_selected_raster <- raster
      
      # Remove any previously added raster images
      high_seas_subsidies_map_proxy %>% clearGroup("flag_state_dat")
      
      # Add a new raster image
      high_seas_subsidies_map_proxy %>% 
        addRasterImage(raster$r, 
                       colors = raster$pal, 
                       opacity = 0.8,
                       group = "flag_state_dat") %>%
        addControl(raster$caption, position = "topright")
    }
    
  })
  
  ### Download button: Effort maps (PDF) -----------------------
  output$high_seas_db_effort_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_effort_maps_high_seas_",input$high_seas_eez_select,"_EEZ.pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$high_seas_eez_select != "Select a coastal state...",
          nrow(high_seas_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = high_seas_rv,
                                     plot_raster_type = "effort",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$high_seas_eez_select,
                                     input_selected_flag_state = input$high_seas_effort_select_flag_state,
                                     eez_sf = fao_area_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme,
                                     is_hs = T)
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### Download button: Fisheries subsidies maps (PDF) -----------------------
  output$high_seas_db_subsidy_maps <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_subsidy_maps_high_seas_area_",input$high_seas_eez_select,".pdf")
    },
    content = function(file) {
      
      # Require coastal state selection & data
      req(input$high_seas_eez_select != "Select a coastal state...",
          nrow(high_seas_rv$eez_dat) > 0)
      
      # Run wrapper to make and bind plots and legend
      plot <- EEZPlotDownloadWrapper(region_dat = high_seas_rv,
                                     plot_raster_type = "subsidies",
                                     plot_variable = "fishing_KWh",
                                     input_selected_eez = input$high_seas_eez_select,
                                     input_selected_flag_state = input$high_seas_subsidies_select_flag_state,
                                     eez_sf = fao_area_360,
                                     land_sf = land_ter_360,
                                     map_theme = eezmaptheme,
                                     is_hs = T)
      
      # Output
      pdf(file, width = 11, height = 8.5)
      print(plot)
      dev.off()
      
    }
    
  )
  
  ### -------------------------------------------------------------------------
  ### -------------------------------------------------------------------------
  ### -------------------------------------------------------------------------
  
  summary_rv <- reactiveValues(by_eez = NULL,
                               by_flag_eezs_only = NULL,
                               by_hs_area = NULL,
                               by_flag_hs_only = NULL)
  
  ### Info modal: EEZ summary ----------
  observeEvent(input$eez_summary_info, {
                   
                   shinyalert(title = "Data Summary - By EEZ",
                              text = paste0("This table shows total distant water fishing activity in each EEZ. By default, EEZs are sorted based on the total magnitude of capacity enhancing subsidies supporting distant water fishing there. Note: An aggregate entry for the entire EU EEZ area is included alongside individual entries for each EU country. Summing all stats in this table without removing the aggregate entry for the entire EU EEZ area will result in double counting."),
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                 }, ignoreInit = T)
  
  ### DT output: Data summary (by EEZ) -------------
  output$summary_table_by_EEZ <- renderDataTable({
    
    # Read in data
    dat <- read.csv("www/dw_activity_summary_by_eez.csv")
    
    summary_rv$by_eez <- dat
    
    # Create pretty summary table
    DataSummaryDT(dat = dat,
                  type = "eez")
    
  })
  
  ### Download button: Summary by EEZ (CSV) -----------------------
  output$db_data_summary_by_eez <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_summary_by_eez.csv")
    },
    content = function(file) {
      
      file.copy("www/dw_activity_summary_by_eez.csv", file)
      
    }
  )
  
  ### Info modal: Flag state summary (EEZs only) ----------
  observeEvent(input$flag_eez_only_summary_info, {
    
    shinyalert(title = "Data Summary - By Flag State (EEZs Only)",
               text = paste0("This table shows total distant water fishing activity undertaken by each flag state across all EEZs. By default, flag states are sorted based on the total magnitude of capacity enhancing subsidies provided to support distant water fishing in the EEZs of other coastal states. Note: An aggregate entry for the EU is included alongside individual entries for each EU flag state. Summing all stats in this table without removing the aggregate entry for the EU will result in double counting."),
               size = "l",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#0d5ba2",
               timer = 0,
               animation = TRUE)
  }, ignoreInit = T)
  
  ### DT output: Data summary (by flag state - eezs only) -------------
  output$summary_table_by_flag_EEZs_only <- renderDataTable({
    
    # Read in data
    dat <- read.csv("www/dw_activity_summary_by_flag_state.csv")
    
    summary_rv$by_flag_eezs_only <- dat
    
    # Create pretty summary table
    DataSummaryDT(dat = dat,
                  type = "flag_eezs_only")
    
  })
  
  ### Download button: Summary by Flag State - EEZs only (CSV) -----------------------
  output$db_data_summary_by_flag <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_summary_by_flag_state_eezs_only.csv")
    },
    content = function(file) {
      
      file.copy("www/dw_activity_summary_by_flag_state.csv", file)
      
    }
  )
  
  ### Info modal: Flag state summary (EEZs only) ----------
  observeEvent(input$hs_summary_info, {
    
    shinyalert(title = "Data Summary - By High Seas Area",
               text = paste0("This table shows total distant water fishing activity in the high seas area of each FAO Major Fishing Area. By default, high seas regions are sorted based on the total magnitude of capacity-enhancing subsidies supporting distant water fishing there."),
               size = "l",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#0d5ba2",
               timer = 0,
               animation = TRUE)
  }, ignoreInit = T)
  
  ### DT output: Data summary (by hs area) -------------
  output$summary_table_by_hs_area <- renderDataTable({
    
    # Read in data
    dat <- read.csv("www/hs_activity_summary_by_area.csv")
    
    summary_rv$by_hs_area <- dat
    
    # Create pretty summary table
    DataSummaryDT(dat = dat,
                  type = "hs_area")
    
  })
  
  ### Download button: Summary by High Seas Area (CSV) -----------------------
  output$db_data_summary_by_region <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_summary_by_high_seas_area.csv")
    },
    content = function(file) {
      
      file.copy("www/hs_activity_summary_by_area.csv", file)
      
    }
  )
  
  ### Info modal: Flag state summary (HS only) ----------
  observeEvent(input$flag_eez_only_summary_info, {
    
    shinyalert(title = "Data Summary - By Flag State (High Seas Only)",
               text = paste0("This table shows total distant water fishing activity undertaken by each flag state across all high seas areas. By default, flag states are sorted based on the total magnitude of capacity enhancing subsidies provided to support distant water fishing on the high seas. Note: An aggregate entry for the EU is included alongside individual entries for each EU flag state. Summing all stats in this table without removing the aggregate entry for the EU will result in double counting."),
               size = "l",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#0d5ba2",
               timer = 0,
               animation = TRUE)
  }, ignoreInit = T)
  
  ### DT output: Data summary (by flag - hs only) -------------
  output$summary_table_by_flag_hs_only <- renderDataTable({
    
    # Read in data
    dat <- read.csv("www/hs_activity_summary_by_flag_state.csv")
    
    summary_rv$by_flag_hs_only <- dat
    
    # Create pretty summary table
    DataSummaryDT(dat = dat,
                  type = "flag_hs_only")
    
  })
  
  ### Download button: Summary by Flag State - High Seas only (CSV) -----------------------
  output$db_data_summary_by_flag_hs <- downloadHandler(
    
    filename = function(){
      paste0("DWFA_summary_by_flag_state_hs_only.csv")
    },
    content = function(file){
      file.copy("www/hs_activity_summary_by_flag_state.csv", file)
    }
  )

}) # /close server


