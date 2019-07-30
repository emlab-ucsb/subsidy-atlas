### ----------------------------------------------------------------------
# ACP Subsidy Atlas Shiny App 
# This app allows users to visualize distant-water fishing effort in the EEZs of ACP countries
#
### ----------------------------------------------------------------------

### ----------
### Section 1: Initialize app
### ----------

rm(list = ls())
set.seed(123)

##install


### Load packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)

library(tidyverse)
library(sf)
library(png)

### Source UI files ###

# The user interface content for each tab is stored in a separate file - Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)

### Load data ###

# Load csv of ACP EEZ and iso3 codes
ACP_codes <- read_csv("./data/ACP_eez_codes.csv") %>%
  na.omit()

# Load spatial data frame with lines linking countries and EEZs
connectivity_data <- read_sf("./data/eez_results/ACP/eez_mapping_with_lines.shp") %>% 
  rename(eez_territory_iso3 = ez_tr_3) 

### Shapefiles ###

# Simplified EEZ shapefile (with FAO regions for high seas)
eez_map <- read_sf(dsn = "./data/eez_v10_fao_combined_simple", layer="eez_v10_fao_combined_simple") %>%
  dplyr::filter(is.na(zone)) %>%
  st_transform(crs = 4326) 
eez_codes <- unique(eez_map$mrgid)

# Simplified land shapefile 
land_map <- read_sf(dsn = "./data/world_happy_180", layer="world_happy_180") %>%
  st_transform(crs = 4326)

# Combined land/EEZ shapefile 
land_eez_map <- read_sf(dsn = "./data/EEZ_land_union_v2_201410", layer = "EEZ_land_v2_201410")


### Widget choice values that depend on a dataset ------------------------------
# Put this here so we only have to load datasets in one place
country_choices <- unique(ACP_codes$territory_iso3)
names(country_choices) <- unique(ACP_codes$flag)

ACP_choices <- unique(ACP_codes$territory_iso3)  
names(ACP_choices) <- unique(ACP_codes$flag)

africa_eez_choices <- ACP_codes$eez_id[ACP_codes$region == "Africa"]  
names(africa_eez_choices) <- ACP_codes$flag[ACP_codes$region == "Africa"]


caribbean_eez_choices <- ACP_codes$eez_id[ACP_codes$region == "Caribbean"]  
names(caribbean_eez_choices) <- ACP_codes$flag[ACP_codes$region == "Caribbean"]

pacific_eez_choices <- ACP_codes$eez_id[ACP_codes$region == "Pacific"]
names(pacific_eez_choices) <- ACP_codes$flag[ACP_codes$region == "Pacific"]

# ACP_codes_africa <- ACP_codes %>%
#   filter(region == "Africa")  
  
  
# 
# ACP_codes_caribbean <- ACP_codes %>%
#      filter(region == "Caribbean")
# 
# ACP_codes_pacific <- ACP_codes %>%
#      filter(region == "Pacific")
# 
 # Africa_choices <- unique(ACP_codes_africa$territory_iso3)
 # names(Africa_choices) <- unique(ACP_codes_africa)
# 
# Caribbean_choices <- unique(ACP_codes_caribbean$territory_iso3)
# names(Caribbean_choices) <- unique(ACP_codes_caribbean)
# 
# Pacific_choices <- unique(ACP_codes_pacific$territory_iso3)
# names(Pacific_choices) <- unique(ACP_codes_pacific)

### Widget choice values that are text heavy  ------------------------------
# Put this here so it's easier to edit text throughout

### ----------
### Section 2: UI
### ----------

ui <- shinyUI(
  dashboardPage(
    
    # Header bar
    dashboardHeader(title = "Global Atlas of Distant Water Fishing",
                    titleWidth = "350px",
                    
                    # SFG logo
                    tags$li(class = "dropdown",
                            a(href = 'http://sfg.msi.ucsb.edu/',
                              img(
                                src = 'sfg-logo-white.png',
                                title = "The Sustainable Fisheries Group",
                                height = "40px"
                              ), style = "padding-top:10px; padding-bottom:10px;"
                            )
                    ),
                    # emLab logo
                    tags$li(class = "dropdown",
                            a(href = 'http://emlab.msi.ucsb.edu/',
                              img(
                                src = 'emlab-logo-white.png',
                                title = "The Environmental Market Solutions Lab",
                                height = "40px"
                              ), style = "padding-top:10px; padding-bottom:10px;"
                            )
                    )
    ), 
                    
   # Sidebar menu
   dashboardSidebar(width = "250px",
                    sidebarMenu(id = "tabs",
                                
                                # Introduction                     
                                menuItem("Introduction", 
                                         tabName = "introduction", 
                                         icon = NULL,
                                         selected = TRUE),
                                
                                # Regional Map
                                menuItem("Select a region",
                                         tabName = "selectregion",
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Africa
                                menuItem("Africa", 
                                         tabName = "africa", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Caribbean
                                menuItem("Caribbean", 
                                         tabName = "caribbean", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Pacific Islands
                                menuItem("Pacific", 
                                         tabName = "pacific", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                #Leaflet
                                menuItem("Leaflet EEZ",
                                         tabName = 'leaflet_EEZ',
                                         icon = NULL, 
                                         selected = NULL),
                                
                                
                                # EEZ
                                menuItem("EEZ",
                                         tabName = "EEZ",
                                         icon = NULL,
                                         selected = NULL)

                    ) # close sidebarMenu
                    
   ), # close dashboardSidebar
   
   # Main panel
   dashboardBody(
     
     # Custom stylesheet
     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ACP_custom.css")),
     
     # Tabs
     tabItems(
       
       # Introduction
       tabItem(tabName = "introduction",
               introduction()
       ),
       
       # Select a region
       tabItem(tabName = "selectregion",
               selectregion()
       ),
       
       # Africa
       tabItem(tabName = "africa",
               africa(africa_eez_choices)
       ),
       
       # Caribbean
       tabItem(tabName = "caribbean",
               caribbean(caribbean_eez_choices)
       ),
       
       # Pacific Islands
       tabItem(tabName = "pacific",
               pacific(pacific_eez_choices)
       ),
       
       #Leaflet EEZ
       tabItem(tabName = "leaflet_EEZ",
               leaflet_EEZ(ACP_choices)
       ),
       
       #EEZ Connectivity
       tabItem(tabName = "EEZ",
               EEZ(country_choices)
        )
       
     ) # close tabItems
   ) # close dashboardBody
   
  ) # close dashboardPage
) # close shinyUI
       

### ----------
### Section 3: Server
### ----------

server <- shinyServer(function(input, output, session) {
  
  ### Introduction ----------
  
  
  
  
  ### Select a region -----------
  
  ## Leaflet output: map of ACP countries aggregated by region
  output$regional_map <- renderLeaflet({
    
    # Combine shapefile with ACP data
    regional_dat <- land_eez_map %>%
      right_join(ACP_codes %>% na.omit(), by = c("ISO_3digit" = "territory_iso3")) %>%
      group_by(region) %>%
      summarize(geometry = st_union(geometry))
    
    leaflet("regional_map") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      
      addPolygons(data = regional_dat %>% dplyr::filter(region == "Africa"), 
                  fillColor = "slateblue",
                  fillOpacity = 0.5,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = ("Africa"),
                  group = "Africa") %>%
      addPolygons(data = regional_dat %>% dplyr::filter(region == "Caribbean"), 
                  fillColor = "seagreen",
                  fillOpacity = 0.5,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = ("Caribbean"),
                  group = "Caribbean") %>% 
      addPolygons(data = regional_dat %>% dplyr::filter(region == "Pacific"), 
                  fillColor = "coral",
                  fillOpacity = 0.5,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = ("Pacific"),
                  group = "Pacific") %>%
      addLayersControl(
        overlayGroups = c("Africa", "Caribbean", "Pacific"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(0,20, zoom = 2)
      
  })
  
  
  ### Based on where the user clicks on regional map, change tab
  observeEvent(input$regional_map_shape_click, {
    
    tab_navigation <- switch(input$regional_map_shape_click$group,
                             "Africa" = list("africa"),
                             "Caribbean" = list("caribbean"),
                             "Pacific" = list("pacific"))
    
    updateTabItems(session, "tabs", tab_navigation[[1]])
    
  })
  
  ### Africa ----------
  
  ### Map of African EEZs for which we have DW fishing effort
  output$africa_map <- renderLeaflet({
    
    # Filter data
    africa_eezs <- eez_map %>%
      right_join(ACP_codes %>% dplyr::filter(region == "Africa"), by = c("mrgid" = "eez_id"))
    
    # Map
    leaflet('africa_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = africa_eezs, 
                  fillColor = "slateblue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = africa_eezs$geoname,
                  layerId = africa_eezs$mrgid, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(15,-13, zoom = 2)
  
  })
  
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$africa_map_shape_click, {
    
    updateSelectizeInput(session, "africa_eez_select",
                         selected = input$africa_map_shape_click$id
    )
    
  })
  
  ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  africa_proxy <- leafletProxy("africa_map")
  
  observeEvent(input$africa_eez_select, {
    
    if(input$africa_eez_select == "Select an EEZ..."){
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      africa_proxy %>% setView(lng=-75, lat=20, zoom=3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_map, eez_map$mrgid == input$africa_eez_select)
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      africa_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = "darkred",
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 0.3,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      group = "highlighted_eez") %>%
        setView(lng=selected_eez$x_1, lat=selected_eez$y_1, zoom=4)
    }
    
  })
  
  ### Africa Connection Map
  
  output$africa_connection_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$africa_eez_select)
    req(input$africa_eez_select != "Select an EEZ...")
    
    #Data filter
    ACP_codes <- eez_map %>% 
      dplyr::filter(mrgid %in% ACP_codes$eez_id)
    
    ACP_codes_filtered_africa <- ACP_codes %>% 
      dplyr::filter(mrgid == input$africa_eez_select)
    
    
    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$africa_eez_select)
    
    
    country_map_filtered_africa <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_filter_africa$flag)
    
  
    
    #  Hover Text
    flag_subsidy_atlas_text_africa <- paste0(
      "<b>","State: ",  country_map_filtered_africa$cntry_l,
      "<br/>",
      "<b>", "# of Vessels: ", connectivity_data_filter_africa$vessels , #format(round(connectivity_data_filter_africa$vessels, 0), big.mark = ","), # Not matching up
      "</br>",
      "<b>", "Capacity of fishing vessels: ",  format(round(connectivity_data_filter_africa$capacty, 0), big.mark = ","), # not matching up
      "</br>",
      "<b>", "Fishing hours per year in EEZ: ",  format(round(connectivity_data_filter_africa$fshng_h, 0), big.mark = ","), # not matching up
      "</br>",
      "<b>", "Fishing kwhr in EEZ: ", format(round(connectivity_data_filter_africa$fshn_KW, 0), big.mark = ","))  %>% # not matching up
      lapply(htmltools::HTML)
    
    
    
    #Leaflet map
    
    leaflet('africa_connection_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = ACP_codes_filtered_africa, 
                  fillColor = "slateblue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", ACP_codes_filtered_africa$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
      #setView()) %>%
      
      addPolygons(data = country_map_filtered_africa,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_subsidy_atlas_text_africa,
                  #label = (paste0("<b>", country_map_filtered_africa$cntry_l, "</b>") %>%
                             #lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_filter_africa,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(15, -13, zoom = 2)
  
  })
  
  ## Heat maps
  
  output$africa_subsidy_map <- renderPlot({
    
    # Require EEZ selection
    req(input$africa_eez_select)
    req(input$africa_eez_select != "Select an EEZ...")
    

    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$africa_eez_select]

    eez_effort_africa <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")  

    ### Generate plot
    
    # Filter data for selected flag state(s) and aggregate
    eez_effort_aggregated_africa <- eez_effort_africa %>%
      # dplyr::filter(flag == input.???) ### MATT - Add filtering option here
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)

    # Get data quntiles to set fil scale limit appropriately
      intensity_quantile <- quantile(eez_effort_aggregated_africa$subsidy_intensity, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
      scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
      
    # Set map limits appropriately
      x_lim <- c(min(eez_effort_aggregated_africa$lon_cen) - 0.5, max(eez_effort_aggregated_africa$lon_cen) + 0.5)
      y_lim <- c(min(eez_effort_aggregated_africa$lat_cen) - 0.5, max(eez_effort_aggregated_africa$lat_cen) + 0.5)
      
    # Map of subsidy intensity
      ggplot()+
          geom_tile(data = eez_effort_aggregated_africa, aes(x = lon_cen, y = lat_cen, fill = subsidy_intensity))+
          scale_fill_viridis_c(na.value = NA, name = "Subsidy intensity \n(2018 $USD/KWh)", 
                              limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                              labels=scale_labels,
                              breaks=scale_labels,
                              oob=scales::squish)+
          geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
          geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
         labs(x = "", y = "")+
         coord_sf(xlim = x_lim, ylim = y_lim)+
         guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
         scale_x_continuous(expand = c(0,0))+
         scale_y_continuous(expand = c(0,0))+
        theme(text = element_text(color = "white"),
              axis.text = element_text(color = "white"),
              plot.background = element_rect(fill = "#262626"),
              panel.background = element_rect(fill = "#262626"),
              legend.position = "bottom",
              legend.background = element_blank())
    
  
  })
  
  output$africa_effort_map <- renderPlot({
    
    #browser()
    
    #Require EEZ selection
    req(input$africa_eez_select)
    req(input$africa_eez_select != "Select an EEZ...")
    
    
    
    # Data
    #eez_effort_africa <- read_csv(paste0(eez_results_app_dir, input$africa_eez_select, "_", input$names(africa_eez_select), "_EEZ_effort_subs_by_flag.csv"), col_types = "iccnnnnn")  
    #   as.data.frame()
    
    eez_effort_africa <- read_csv(paste0(eez_results_app_dir, "8396_ZAF_EEZ_effort_subs_by_flag.csv"), col_types = "iccnnnnn")  
    
    
    # #Gemerate plot
    # 
    #  ### Make Plots 
    #  # Get quantiles of data for scale limits
    eez_effort_aggregated_africa <- eez_effort_africa %>%
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    #  
    # # # # Quantiles
    intensity_quantile <- quantile(eez_effort_aggregated_africa$subsidy_intensity, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    #  
    # # # # Set map limits for filtering global country map and eez map appropriately
    x_lim <- c(min(eez_effort_aggregated_africa$lon_cen) - 0.5, max(eez_effort_aggregated_africa$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_africa$lat_cen) - 0.5, max(eez_effort_aggregated_africa$lat_cen) + 0.5)
    #  
    
    # ## Map of effort intensity
    
    
    # Labels for the log scale 
    ggplot()+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      geom_tile(data = eez_effort_aggregated_africa, aes(x = lon_cen, y = lat_cen, fill = fishing_KWh))+
      eezmaptheme +
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
    
  })
  
  # observe({
  #   click <- input$africa_connection_map_shape_click
  #   if(is.null(click))
  #     return()
  # })
  
  # observe({
  #   click <- input$africa_connection_map_shape_click
  #   if(is.null(click))
  #     return()
  #   text <- paste0("Fish")
  #   output$Click_text <- renderUI({
  #     text
  #   })
  # })
  
    
  output$africa_summary_text <- renderUI({
    
    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$africa_eez_select) 
    
    country_map_filtered_africa <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_filter_africa$flag)  
      
    
    req(input$africa_connection_map_shape_click)
    
    if (is.null(click)) return()
    text2 <- paste0("<b>","State: ", #country_map_filtered_africa$cntry_l,
           "<br/>",
           "<b>", "# of Vessels: ", #format(round(connectivity_data_filter_africa$vessels, 0), big.mark = ","),
           "</br>",
           "<b>", "Fishing hours per year in EEZ: ######",
           "</br>",
           "<b>", "Fishing kwhr in EEZ: ######")  %>%
      lapply(htmltools::HTML)
  })
  
  
   #close render leaflet
  
  
  ### Caribbean ----------
  
  ### Map of Caribbean EEZs for which we have DW fishing effort
  output$caribbean_map <- renderLeaflet({
    
    # Filter data
    caribbean_eezs <- eez_map %>%
      right_join(ACP_codes %>% dplyr::filter(region == "Caribbean"), by = c("mrgid" = "eez_id"))
    
    # Map
    leaflet('caribbean_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = caribbean_eezs, 
                  fillColor = "seagreen",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = caribbean_eezs$geoname,
                  layerId = caribbean_eezs$mrgid, # need this to link to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(-75,20, zoom = 3)
    
  }) # Close render leaflet
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$caribbean_map_shape_click, {

    updateSelectizeInput(session, "caribbean_eez_select",
                          selected = input$caribbean_map_shape_click$id
    )

  }) # Close observe event
  
  ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  caribbean_proxy <- leafletProxy("caribbean_map")
  
  observeEvent(input$caribbean_eez_select, {
    
    if(input$caribbean_eez_select == "Select an EEZ..."){
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      caribbean_proxy %>% setView(lng=-75, lat=20, zoom=3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_map, eez_map$mrgid == input$caribbean_eez_select)
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      caribbean_proxy %>% addPolygons(data = selected_eez,
                                   fillColor = "darkred",
                                   fillOpacity = 1,
                                   color= "white",
                                   weight = 0.3,
                                   highlight = highlightOptions(weight = 5,
                                                                color = "#666",
                                                                fillOpacity = 1,
                                                                bringToFront = TRUE),
                                   group = "highlighted_eez") %>%
        setView(lng=selected_eez$x_1, lat=selected_eez$y_1, zoom=4)
    }
    
  }) # Close observe event, final bracket for caribbean
  
  
  ##Caribbean Connection Map
  #ACP_codes %>% 
    #select(eez_id)
  
  output$caribbean_connection_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$caribbean_eez_select)
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    #Data filter
    ACP_codes <- eez_map %>% 
      dplyr::filter(mrgid %in% ACP_codes$eez_id)
    
    ACP_codes_filtered_caribbean <- ACP_codes %>% 
      dplyr::filter(mrgid == input$caribbean_eez_select)
    
    
    connectivity_data_filter_caribbean <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$caribbean_eez_select)
    
    country_map_filtered_caribbean <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_filter_caribbean$flag)
    
    #  Hover Text
    flag_subsidy_atlas_text_caribbean <- paste0(
      "<b>","State: ", country_map_filtered_caribbean$cntry_l,
      "<br/>",
      "<b>", "# of Vessels: ", format(round(dw_effort_final_caribbean$number_vessels, 0), big.mark = ","), # Not matching up
      "</br>",
      "<b>", "Fishing hours per year in EEZ: ", format(round(dw_effort_final_caribbean$fishing_hours, 0), big.mark = ","), # not matching up
      "</br>",
      "<b>", "Fishing kwhr in EEZ: ", format(round(dw_effort_final_caribbean$fishing_KWh, 0), big.mark = ","))  %>% # not matching up
      lapply(htmltools::HTML)
    
    #Leaflet map
    
    leaflet('caribbean_connection_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = ACP_codes_filtered_caribbean, 
                  fillColor = "seagreen",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", ACP_codes_filtered_caribbean$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
      #setView()) %>%
      
      addPolygons(data = country_map_filtered_caribbean,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_subsidy_atlas_text_caribbean,
                  # label = (paste0("<b>", country_map_filtered_caribbean$cntry_l, "</b>") %>%
                  #            lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_filter_caribbean,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(-75,20, zoom = 1.75)
  })
  
  output$caribbean_summary_text <- renderUI({
    
    connectivity_data_filter_caribbean <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$caribbean_eez_select)
    
    
    
    country_map_filtered_caribbean <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_filter_caribbean$flag) #%>% 
      
    
    
    req(input$caribbean_connection_map_shape_click)
    
    if (is.null(click)) return()
    text <- paste0("<b>","State: " ,  input$caribbean_connection_map_shape_click %in% country_map_filtered_caribbean$cntry_l,
                    "<br/>",
                    "<b>", "# of Vessels: #####",
                    "</br>",
                    "<b>", "Fishing hours per year in EEZ: ######",
                    "</br>",
                    "<b>", "Fishing kwhr in EEZ: ######")  %>%
      lapply(htmltools::HTML)
  })
  
  
  ### Based on click on regional map, change tab
  # observeEvent(c(input$regional_map_shape_click, 
  #                #condition2
  #                ) {
  #   
  #   filter connectivity dataset <- connectivity_dat %>%
  #       dpply::filter (country = )
  # 
  #   
  # })
  
  ### Leaflet Version of EEZ Connectivity Map
  ACP_codes %>% 
    dplyr::select(eez_id)
  
  output$leaflet_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$ACP_for_profile)
    
    #filter data
    
    # ACP_codes_filtered <- connectivity_data %>% # load this in up above
    #   dplyr::filter(eez_territory_iso3 == input$ACP_for_profile)
    
     ACP_codes <- eez_map %>% 
       dplyr::filter(mrgid %in% ACP_codes$eez_id)
     
     ACP_codes_filtered <- ACP_codes %>% 
       dplyr::filter(ez_hs_c == input$ACP_for_profile)
     
     
     connectivity_data_filter_leaflet <- connectivity_data %>% # load this in up above
       dplyr::filter(eez_territory_iso3 == input$ACP_for_profile)
     
     country_map_filtered <- land_map %>% 
       dplyr::filter(iso3 %in% connectivity_data_filter_leaflet$flag)
     
     
    # connectivity_data_filter <- connectivity_data %>% # load this in up above
    #   dplyr::filter(eez_territory_iso3 == input$EEZ_for_profile)
    
    leaflet('leaflet_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = ACP_codes_filtered, 
                  fillColor = "slateblue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", ACP_codes_filtered$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
                  #setView()) %>%
      
      addPolygons(data = country_map_filtered,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", country_map_filtered$cntry_l, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_filter_leaflet,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(15,-36, zoom = 1.75)

    #setView(20,-2, zoom = 3)
    #addLegend("bottomright", pal = pal_global,
    #           values = log10(tot_subs$value),
    #           labels = round(tot_subs$value, 0),
    #           title = "Est. fisheries subsidies<br>(2018 $USD)",
    #           opacity = 1,
    #           labFormat = labelFormat(prefix = "$",
    #                                   transform = function(x) 10^(x)
    #           )
    #)
    
    
  })
  
  
  ### EEZ Connectivity Maps ---------
  
  output$connectivity_plot <- renderPlot({
    
    #Require EEZ selection
    req(input$EEZ_for_profile)
    
    #Require country selection for flag highlights
    #req(input$flag_for_profile)
    
    #Data
    connectivity_data_filter <- connectivity_data %>% # load this in up above
       dplyr::filter(eez_territory_iso3 == input$EEZ_for_profile)
    
    # ACP_lines <- connectivity_data_filter %>% 
    #     st_coordinates(st_cast(network_data_sf,"MULTILINESTRING")$geometry) %>% 
    #     as_data_frame() 
    
    
    #Data
    
   # iso3 %in% ACP_eez_dat$flag
    
    #iso3 %in% connectivity_data_AGO$flag
    
    
    #map
    ggplot(connectivity_data_filter)+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.1)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.1)+ # world countries (dark grey, white border lines)
      geom_sf(data = land_map %>% dplyr::filter(iso3 %in% connectivity_data_filter$flag), fill = "darkmagenta", alpha = 0.5, color = NA, size = 0.1) + # highlighted flag states (magenta)
      geom_sf(data = eez_map %>% dplyr::filter(ez_hs_c == input$EEZ_for_profile), fill = "slateblue", color = "grey40", size = 0.1) + # highlighted EEZ (slateblue, grey border lines)
      geom_sf(col = "darkgoldenrod", size = 0.25) +
      #maptheme+
      coord_sf(xlim = c(-180,180), ylim = c(-90,90))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
    
    #%>% dplyr::filter(eez_codes == input$EEZ_for_profile)
    
 ####Need to make countries and lines reactive   
    
    
  })
  
  
  
  
 
  
  ### Pacific Islands --------
  
  
  ### Map of Pacific countries/EEZs highlighted for which we have DW fishing effort
  output$pacific_map <- renderLeaflet({
    
    # Filter data
    pacific_eezs <- eez_map %>%
      right_join(ACP_codes %>% dplyr::filter(region == "Pacific"), by = c("mrgid" = "eez_id"))
    
    
    
    # Map
    leaflet('pacific_map') %>%
      #options = leafletOptions(worldCopyJump = TRUE) %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
      addPolygons(data = pacific_eezs, 
                  fillColor = "coral",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = pacific_eezs$geoname, 
                  layerId = pacific_eezs$mrgid, # need this to link to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(-170,17, zoom = 2) 
    
  }) # Close render leaflet
      
  ### Register user clicks on map - change select input from widget
  observeEvent(input$pacific_map_shape_click, {
    
    updateSelectizeInput(session, "pacific_eez_select",
                         selected = input$pacific_map_shape_click$id
    )
    
  })
  
  ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  pacific_proxy <- leafletProxy("pacific_map")
  
  observeEvent(input$pacific_eez_select, {
    
    if(input$pacific_eez_select == "Select an EEZ..."){
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      pacific_proxy %>% setView(lng=-75, lat=20, zoom=3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_map, eez_map$mrgid == input$pacific_eez_select)
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      pacific_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = "darkred",
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 0.3,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      group = "highlighted_eez") %>%
        setView(lng=selected_eez$x_1, lat=selected_eez$y_1, zoom=2)
    }
    
  }) # Close observe event
  
  ## Pacific Connection Map
  
  output$pacific_connection_map <- renderLeaflet({
    
    crs4326 <-  leafletCRS(
      crsClass = "L.CRS.EPSG3857")
    
    #Require EEZ selection
    req(input$pacific_eez_select)
    req(input$pacific_eez_select != "Select an EEZ...")
    
    #Data filter
    ACP_codes <- eez_map %>% 
      dplyr::filter(mrgid %in% ACP_codes$eez_id)
    
    ACP_codes_filtered_pacific <- ACP_codes %>% 
      dplyr::filter(mrgid == input$pacific_eez_select)
    
    
    connectivity_data_filter_pacific <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$pacific_eez_select)
    
    country_map_filtered_pacific <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_filter_pacific$flag)
    
    #  Hover Text
    flag_subsidy_atlas_text_pacific <- paste0(
      "<b>","State: ", country_map_filtered_pacific$cntry_l,
      "<br/>",
      "<b>", "# of Vessels: ", format(round(dw_effort_final_pacific$number_vessels, 0), big.mark = ","), # Not matching up
      "</br>",
      "<b>", "Fishing hours per year in EEZ: ", format(round(dw_effort_final_pacific$fishing_hours, 0), big.mark = ","), # not matching up
      "</br>",
      "<b>", "Fishing kwhr in EEZ: ", format(round(dw_effort_final_pacific$fishing_KWh, 0), big.mark = ","))  %>% # not matching up
      lapply(htmltools::HTML)
    
    #Leaflet map
    
    leaflet('pacific_connection_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = ACP_codes_filtered_pacific, 
                  fillColor = "coral",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", ACP_codes_filtered_pacific$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
      #setView()) %>%
      
      addPolygons(data = country_map_filtered_pacific,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_subsidy_atlas_text_pacific,
                  # label = (paste0("<b>", country_map_filtered_pacific$cntry_l, "</b>") %>%
                  #            lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_filter_pacific,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(-75,20, zoom = 1.75)
      #setView(lng = input$lng, lat = input$lat, zoom = 2)
  })
  
  output$pacific_summary_text <- renderUI({
    
    connectivity_data_filter_pacific <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$pacific_eez_select)
    
    country_map_filtered_pacific <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_filter_pacific$flag) %>% 
      filter(iso3 == input$pacific_eez_select)
    
    #  Click Text
    flag_subsidy_atlas_text_pacific <- paste0(
      "<b>","State: FISH", country_map_filtered_pacific$cntry_l,
      "<br/>",
      "<b>", "# of Vessels: #####",
      "</br>",
      "<b>", "Fishing hours per year in EEZ: ######", 
      "</br>",
      "<b>", "Fishing kwhr in EEZ: ######")  %>% 
      lapply(htmltools::HTML)
    
    
    req(input$pacific_connection_map_shape_click)
    
    if (is.null(click)) return()
    text2 <- flag_subsidy_atlas_text_pacific %>% 
      lapply(htmltools::HTML)
      
      
      # paste0("<b>","State: ",  pacific_connection_map_shape_click$country_map_filtered_pacific$cntry_l ,
      #               "<br/>",
      #               "<b>", "# of Vessels: #####",
      #               "</br>",
      #               "<b>", "Fishing hours per year in EEZ: ######",
      #               "</br>",
      #               "<b>", "Fishing kwhr in EEZ: ######")  %>%
      # lapply(htmltools::HTML)
  })
  
    
  })
  
  


### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

