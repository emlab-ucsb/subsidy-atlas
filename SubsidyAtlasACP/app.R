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
library(shinyBS)

library(tidyverse)
library(sf)
library(png)

library(sf) # spatial manipulation
library(lwgeom) # needed for curved lines
library(here) # path names
library(knitr) # knit document
library(bigrquery) # access GFW data
library(DBI) # access GFW data
library(countrycode) # country names 
library(tidyverse) # data manipulation
library(ggalt) # transform to robinson projection
library(RColorBrewer) # custom color palettes
library(colorRamps)
library(scales) # scales for plotting
library(ggpubr) # plot arranging
library(gridExtra)
library(grid)
library(countrycode)


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
  mutate(flag = countrycode(territory_iso3, "iso3c", "country.name")) 
  #na.omit()
#Encoding <- (ACP_codes, "UTF-8") 

# Load csv of FAO RFMO codes and links
RFMO_links <- read_csv("./data/RMFO_links.csv")

# Load spatial data frame with lines linking countries and EEZs
connectivity_data <- read_sf("./data/eez_results/ACP/eez_mapping_with_lines.shp") 
   
### Shapefiles ###

# Custom EEZ shapefile
# eez_shp <- read_sf(dsn = "./data/shapefiles_edit/World_EEZ_v10_custom_ACP", layer = "World_EEZ_v10_custom_ACP") %>%
#   st_transform(crs = 4326) %>%
#   dplyr::filter(pol_type == "200NM")

 
### 1. Simplified EEZ shapefile (-360 to 360 degrees: crop appropriately for each region)
eez_map <- read_sf(dsn = "./data/shapefiles_edit/World_EEZ_v10_20180221_LR_-360_360", layer = "World_EEZ_v10_2018021_LR_-360_360") %>%
  setNames(tolower(names(.))) %>%
  st_transform(crs = 4326)

### 2. Simplified land shapefile 
land_map <- read_sf(dsn = "./data/shapefiles_edit/world_happy_180", layer="world_happy_180") %>%
  st_transform(crs = 4326)

### 3. Simplified combined land/EEZ shapefile (-360 to 360 degrees: crop appropriately for each region)
land_eez_map <- read_sf(dsn = "./data/shapefiles_edit/EEZ_land_v2_201410_-360_360", layer = "EEZ_land_v2_201410_-360_360") %>%
  setNames(tolower(names(.))) %>%
  st_transform(crs = 4326) %>% 
  dplyr::select(iso3 = iso_3digit,
                country,
                geometry) %>%
  dplyr::filter(!is.na(iso3))

### Widget choice values that depend on a dataset ------------------------------

# Put this here so we only have to load datasets in one place
africa_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])
names(africa_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])

caribbean_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])  
names(caribbean_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])

pacific_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])
names(pacific_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])

flag_state_choices <- unique(connectivity_data$flag)
names(flag_state_choices) <- countrycode(flag_state_choices, "iso3c", "country.name")
names(flag_state_choices)[is.na(names(flag_state_choices))] <- "Unknown flag"
  
#Map theme
eezmaptheme <- theme_minimal()+
  theme(strip.background = element_rect(fill = "#262626", color = NA),
        plot.background = element_rect(fill = "#262626", color = NA),
        panel.background = element_rect(fill = "#262626", color = NA),
        panel.grid.major = element_line(colour = "#262626"),
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "#262626", fill = NA),
        plot.margin = margin(t = 0.2, r = 0.1, b = 0, l = 0, unit = "cm"),
        legend.margin = margin(t = 0.1, r = 0, b = 0.2, l = 0, unit = "cm"),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.text = element_text(color = "white"))
  
### ----------
### Section 2: UI
### ----------

ui <- shinyUI(
  dashboardPage(
    
    # Header bar
    dashboardHeader(title = "ACP Atlas of Distant Water Fishing",
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
                    collapsed = TRUE,
                    sidebarMenu(id = "tabs",
                                
                                # # Introduction                     
                                # menuItem("Introduction", 
                                #          tabName = "introduction", 
                                #          icon = NULL,
                                #          selected = TRUE),
                                
                                # Regional Map
                                menuItem("Select a region",
                                         tabName = "selectregion",
                                         icon = NULL,
                                         selected = TRUE),
                                
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
               africa(africa_eez_choices, flag_state_choices)
       ),
       
       # Caribbean
       tabItem(tabName = "caribbean",
               caribbean(caribbean_eez_choices, flag_state_choices)
       ),
       
       # Pacific Islands
       tabItem(tabName = "pacific",
               pacific(pacific_eez_choices, flag_state_choices)
       )
       
     ) # close tabItems
   ) # close dashboardBody
   
  ) # close dashboardPage
) # close shinyUI
       

### ----------
### Section 3: Server
### ----------

server <- shinyServer(function(input, output, session) {
  
  ### Introduction / Select a region -----------
  
  region_pal <- colorFactor(
    palette = "Set2",
    domain = ACP_codes$region
  )
  
  ## Leaflet output: map of ACP countries aggregated by region
  output$regional_map <- renderLeaflet({
    
    # Make aggregated regional map data
    regions <- ACP_codes %>%
      distinct(territory_iso3, region)
    
    regional_dat <- land_eez_map %>%
      st_crop(c(xmin=-30, xmax=330, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      inner_join(regions, by = c("iso3" = "territory_iso3")) %>%
      group_by(region) %>%
      summarize(geometry = st_union(geometry))
    
    # Formatting for semi-transparent title box over map
    intro_overlay_formatting <- tags$style(HTML("
  .leaflet-control.map-title {

    transform: translate(-50%, 20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding: 20px;
    background: rgba(255,255,255,0.6);
    font-size: 14px;
    color:black;

    -webkit-box-shadow: 0px 2px 2px 0px rgba(0, 0, 0, 0.5);
    box-shadow: 0px 2px 2px 0px rgba(0, 0, 0, 0.5);
  }
"))
    
    # Text for semi-transparent title box over map
    intro_overlay <- tags$div(
      intro_overlay_formatting, 
      HTML("<b>", "Select a region to view more information about distant water fishing by country in that region", "</b>",
           "<br>",
           "Text",
           "<br>",
           "Text")
    )  
      
    # Leaflet map
    leaflet("regional_map", 
            options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>%
      
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
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
      setView(145, 0, zoom = 2) %>%
      setMaxBounds(lng1 = -30, lat1 = -90, lng2 = 360, lat2 = 90)
      
  })
  
  
  ### Based on where the user clicks on regional map, change tab
  observeEvent(input$regional_map_shape_click, {
    
    tab_navigation <- switch(input$regional_map_shape_click$group,
                             "Africa" = list("africa"),
                             "Caribbean" = list("caribbean"),
                             "Pacific" = list("pacific"))
    
    updateTabItems(session, "tabs", tab_navigation[[1]])
    
  })
  
  ###------------
  ### Africa ----------
  ###-----------------
  
  ### Map of African EEZs for which we have DW fishing effort
  output$africa_map <- renderLeaflet({
    
    # Filter data
    africa_eezs <- eez_map %>%
      st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      inner_join(ACP_codes %>% dplyr::filter(region == "Africa"), by = c("mrgid")) %>%
      mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
    
   # Deal with multiple EEZs for the same coastal state
   africa_eezs_merged <- africa_eezs %>%
      group_by(territory_iso3, geoname, region) %>%
      summarize(geometry = st_union(geometry))
   
#    tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title-africa { 
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: right;
#     padding-left: 10px; 
#     padding-right: 10px; 
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 16px;
#     color:black;
#   }
# "))
#    
#    map_title_africa <- tags$div(
#      tag.map.title, HTML("Click on an EEZ to begin")
#    )  
   
    # Map
    leaflet('africa_map', options = leafletOptions(zoomControl = FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldTopoMap") %>%
      #addControl(map_title_africa, position = "topright", className = "map-title-africa") %>%
      addPolygons(data = africa_eezs_merged, 
                  fillColor = ~region_pal(region),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = africa_eezs_merged$geoname,
                  layerId = africa_eezs_merged$territory_iso3, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = 15, lat = -13, zoom = 2)
  
  })
  
  ### ----------------------------------------------------
  ### Africa: update EEZ selectInput based on click on map
  ### ----------------------------------------------------
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$africa_map_shape_click, {

    updateSelectizeInput(session, "africa_eez_select",
                         selected = input$africa_map_shape_click$id
    )

  })
  
  ### -----------------
  ### Africa: proxy map
  ### -----------------
  
  africa_proxy <- leafletProxy("africa_map")
  
  # When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  observeEvent(input$africa_eez_select, {
    
    if(input$africa_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      africa_proxy %>% setView(lng=15, lat=-13, zoom=2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_map, eez_map$iso_ter1 == input$africa_eez_select)
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      africa_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = "mediumseagreen",
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 0.3,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      group = "highlighted_eez") %>%
        setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=4)
    }
    
  }) # close observe event
  
  

  # ###------------
  # ### Africa: Links to Online references
  # ###------------

  output$africa_online_text <- renderUI({

    req(input$africa_eez_select != "Select a coastal state...")
    
    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$africa_eez_select) %>%
      rename(territory_iso3 = ez_tr_3) %>%
      mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
    
    # Total stats by coastal state
    total_stats_africa <- connectivity_data_filter_africa %>%
      as.data.frame() %>%
      select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>%
      group_by(territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacty = sum(capacty, na.rm = T),
                fshng_h = sum(fshng_h, na.rm = T),
                fshn_KW = sum(fshn_KW, na.rm = T)) %>%
      arrange(territory_iso3)
    
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$africa_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>%
      separate_rows(fao_memberships, sep = ",")
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)

    browser()
    
    EEZ_info <- paste0("<h3 style = 'margin-top: 0px;'>", names(africa_eez_choices[africa_eez_choices == input$africa_eez_select]), "</h3>",
                       "Fisheries management agency:  ", 
                       "<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "'>", 
                       unique(ACP_codes_links$fishery_org_eng[!is.na(ACP_codes_links$fishery_org_eng)]), "</a>",
                       "<br>",
                       
                       "Country profile: ",
                       "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "'>", 
                       "FAO", "</a>",
                       " | ",
                       "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "'>", 
                       "World Bank", "</a>",
                       
                       "<br>",
                       
                       "Treaties and conventions: ",
                       "<a href='", unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]), "'>", 
                       "Fishbase", "</a>",
                       "<br>",
                       
                       "Foreign access agreements: ",
                       "<a href='", unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), "'>", 
                       "Sea Around Us", "</a>",
                       "<br>",
                       
                       "FAO Regional Fisheries Body Memberships: ",
                       paste0("<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>", "</br>"),
                       "<br>",

                       "<hr>",
                       "<b>", "Distant water fishing in the ", total_stats_africa$eez_nam, "</b>",
                       "<br>",
                       "Vessels: ", format(round(total_stats_africa$vessels, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (hours): ", format(round(total_stats_africa$fshng_h, 0), big.mark = ","),
                       "<br>",
                       "Fishing effort (KWh): ", format(round(total_stats_africa$fshn_KW, 0), big.mark = ",")) %>%
      lapply(htmltools::HTML)

    # EEZ_info <- tags$div(
    #   tags$h4(total_stats_africa$eez_nam),
    #   tags$h5("Total Number of Vessels in EEZ: ", format(round(total_stats_africa$vessels, 0), big.mark = ",")),
    #   tags$h5("Total Fishing hours per year in EEZ: ", format(round(total_stats_africa$fshng_h, 0), big.mark = ",")),
    #   tags$h5("Total Fishing kwhr in EEZ: ", format(round(total_stats_africa$fshn_KW, 0), big.mark = ",")),
    #   tags$hr(),
    #   tags$h3("EEZ Information"),
    #   tags$a(href = unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "FAO Country Profile"),
    #   tags$br(),
    #   tags$a(href = unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]), "Treaties and Conventions"),
    #   tags$br(),
    #   tags$a(href = unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), "Internal Fishing Access Agreements"),
    #   tags$hr(),
    #   tags$h4("Fishery organization: "),
    #   tags$a(unique(ACP_codes_links$fishery_org_eng[!is.na(ACP_codes_links$fishery_org_eng)]), href = unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])),
    #   tags$hr(),
    #   tags$h3("FAO Membership Information"),
    #   paste0("<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>", "</br>") %>%
    #     lapply(htmltools::HTML),
    #   tags$br()
    #   
    # )
    
    # Return
    EEZ_info

  })

  
  ### ------------------------
  ### Africa: connectivity Map
  ### ------------------------
  
  output$africa_connection_map <- renderLeaflet({
    
    #Require coastal state selection
    req(input$africa_eez_select != "Select a coastal state...")
    
    selected_eez <- eez_map %>% 
      st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(iso_ter1 == input$africa_eez_select) %>%
      rename(territory_iso3 = iso_ter1)
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$africa_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3) %>%
      arrange(flag) 
    
    flag_states_for_selected_eez <- land_eez_map %>% 
      st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    # Should be able to remove this step eventually. Ideally we need a land/eez map with all flag states represented. 
    connectivity_data_edit <- connectivity_data_for_selected_eez %>%
      dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
   
  # Connectivity stats with no geometry
  no_geometry <- connectivity_data_edit %>%
    group_by(territory_iso3, flag) %>%
    summarize(vessels = sum(vessels, na.rm = T),
              capacity = sum(capacty, na.rm = T),
              fishing_h = sum(fshng_h, na.rm = T),
              fishing_KWh = sum(fshn_KW, na.rm = T))
  st_geometry(no_geometry) <- NULL
   
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(no_geometry, by = "flag") %>%
      mutate(name = countrycode(flag, "iso3c", "country.name"))
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary$name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    #browser()
    
    
    # Leaflet map
    leaflet('africa_connection_map', options = leafletOptions(zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = flag_states_for_selected_eez,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_states_for_selected_eez$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      addPolygons(data = selected_eez, 
                  fillColor = "mediumseagreen",
                  fillOpacity = 0.8,
                  color= "white",
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
      
       
      
      addPolylines(data = connectivity_data_edit,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      
      setView(15, -13, zoom = 2)
  
  })
  
  ### -----------------------------------------------------------
  ### Africa: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$africa_eez_select, {
   
    req(input$africa_eez_select != "Select a coastal state...")
    
    connectivity_data_for_selected_eez <- connectivity_data %>%
      dplyr::filter(ez_tr_3 == input$africa_eez_select) %>% 
      arrange(flag) %>%
      dplyr::filter(flag != "BES") %>% 
      dplyr::filter(flag != "UNK")
    
    flag_state_choices_africa <- unique(connectivity_data_for_selected_eez$flag)
    names(flag_state_choices_africa) <- unique(countrycode(flag_state_choices_africa, "iso3c", "country.name"))
    
    updateSelectizeInput(session, "africa_flag_state_select",
                         choices = c("All flag states", flag_state_choices_africa))
    
  })
  
  
  ### --------------------------------------------------------------------------
  ### Africa: switch tab and update flag state selectInput based on click on map
  ### --------------------------------------------------------------------------
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$africa_connection_map_shape_click, {
    
    req(input$africa_connection_map_shape_click$id != input$africa_eez_select)
    
    updateTabItems(session, "africa_tabs", "Fishing effort and subsidy intensity of distant water vessels")
    
    updateSelectizeInput(session, "africa_flag_state_select",
                         selected = input$africa_connection_map_shape_click$id
    )
    
  })
  
  ### ------------------------------
  ### Africa: load eez specific data
  ### ------------------------------

  africa_eez_data <- eventReactive(input$africa_eez_select, {
    
    # Require EEZ selection
    req(input$africa_eez_select != "Select a coastal state...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$africa_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(africa_eez_choices[africa_eez_choices == input$africa_eez_select])) %>%
      arrange(name)
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -------------------------
  ### Africa: subsidy heat map
  ### ------------------------
  
  output$africa_subsidy_map <- renderPlot(bg = "#262626", {
    
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
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$africa_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- africa_eez_data() %>%
        dplyr::filter(flag == input$africa_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
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
  
  
  ### -------------------------
  ### Africa: effort heat map
  ### ------------------------
  
  output$africa_effort_map <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
    ### Get totals for data
    eez_totals <- africa_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    #browser()
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$africa_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- africa_eez_data() %>%
        dplyr::filter(flag == input$africa_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$fishing_KWh/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
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
    #browser()
  })
  

  ###------------
  ### Caribbean 
  ###-----------

  ### Map of Caribbean EEZs for which we have DW fishing effort
  output$caribbean_map <- renderLeaflet({
    
    # Filter data
    caribbean_eezs <- eez_map %>%
      st_crop(c(xmin=-260, xmax=100, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      inner_join(ACP_codes %>% dplyr::filter(region == "Caribbean"), by = c("mrgid")) %>%
      mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
    
    # Deal with multiple EEZs for the same coastal state
    caribbean_eezs_merged <- caribbean_eezs %>%
      group_by(territory_iso3, geoname, region) %>%
      summarize(geometry = st_union(geometry))
    
    # Map
    leaflet('caribbean_map', options = leafletOptions(zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
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
                  layerId = caribbean_eezs_merged$territory_iso3, # need this to link to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng= -75, lat = 17, zoom = 3)
    
  }) # Close render leaflet
  
  ###----------------------
  ### Caribbean: update EEZ selectInput based on click on map
  ###-----------------------
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$caribbean_map_shape_click, {
    
    updateSelectizeInput(session, "caribbean_eez_select",
                          selected = input$caribbean_map_shape_click$id
    )

  }) 
  
  ###--------------------
  ### Caribbean: Proxy Map
  ###--------------------
  
  ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  
  caribbean_proxy <- leafletProxy("caribbean_map")
  
  observeEvent(input$caribbean_eez_select, {
    
    if(input$caribbean_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      caribbean_proxy %>% setView(lng=-75, lat=17, zoom=3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_map, eez_map$iso_ter1 == input$caribbean_eez_select) %>%
        mutate(x_1 = ifelse(x_1 > 100, -180 - (180 - x_1), x_1))
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      caribbean_proxy %>% addPolygons(data = selected_eez,
                                   fillColor = "tomato",
                                   fillOpacity = 1,
                                   color= "white",
                                   weight = 0.3,
                                   highlight = highlightOptions(weight = 5,
                                                                color = "#666",
                                                                fillOpacity = 1,
                                                                bringToFront = TRUE),
                                   group = "highlighted_eez") %>%
        setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=4)
    }
    
  }) # Close observe event
  
  ### Countries with no data observe event text
  
  # caribbean_no_data <- renderUI("no_data_text")
  # 
  # observeEvent(input$caribbean_eez_select)
  
  # # 
  # # ###------------
  # # ### Caribbean: Links to Online references
  # # ###------------
  # # 
  output$caribbean_online_text <- renderUI({
  
      req(input$caribbean_eez_select != "Select a coastal state...")
      
      connectivity_data_filter_caribbean <- connectivity_data %>% # load this in up above
        dplyr::filter(ez_tr_3 == input$caribbean_eez_select) %>%
        rename(territory_iso3 = ez_tr_3) %>%
        mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
      
      # Total stats by coastal state
      total_stats_caribbean <- connectivity_data_filter_caribbean %>%
        as.data.frame() %>%
        select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>%
        group_by(territory_iso3, eez_nam) %>%
        summarize(vessels = sum(vessels, na.rm = T),
                  capacity = sum(capacty, na.rm = T),
                  fishing_h = sum(fshng_h, na.rm = T),
                  fishing_KWh = sum(fshn_KW, na.rm = T)) %>%
        arrange(territory_iso3)
      
      
      ACP_codes_links <- ACP_codes %>%
        dplyr::filter(territory_iso3 == input$caribbean_eez_select)
      
      #browser()
      
      ACP_fao_membership <- ACP_codes_links %>%
        separate_rows(fao_memberships, sep = ",")
      
      RFMO_links_eez <- RFMO_links %>%
        dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
      
      
      EEZ_info <- 
        
        if(nrow(total_stats_caribbean) == 0) {
        
        tags$div( 
          
          tags$h3("EEZ Summary Statistics"),
          tags$h4("**There is either, no distant water fishing data available for", ACP_codes_links$territory, "or the fishing effort did not meet the minimum thresholds of our analysis."), #"Text about no data asdkfas;dfkjaskd",  %in% input$caribbean_eez_select), # need to add full EEZ name, currently just iso3 code
          tags$h5("For more information about how we conducted our analysis please click here:"),
          tags$hr(),
          tags$h3("EEZ Information"),
          tags$a(href = unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "FAO Country Profile"),
          tags$br(),
          tags$a(href = unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]), "Treaties and Conventions"),
          tags$br(),
          tags$a(href = unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), "Internal Fishing Access Agreements"),
          tags$hr(),
          tags$h3("Fishery organization: "),
          tags$a(unique(ACP_codes_links$fishery_org_eng[!is.na(ACP_codes_links$fishery_org_eng)]), href = unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])),
          tags$hr(),
          tags$h3("FAO Membership Information"),
          paste0("<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>", "</br>") %>%
            lapply(htmltools::HTML),
          tags$br())
          
        }else{
        
        tags$div(
        
            tags$h3("EEZ Summary Statistics"),
            tags$h4(total_stats_caribbean$eez_nam),
            tags$h5("Total Number of Vessels in EEZ: ", format(round(total_stats_caribbean$vessels, 0), big.mark = ",")),
            tags$h5("Total Fishing hours per year in EEZ: ", format(round(total_stats_caribbean$fishing_h, 0), big.mark = ",")),
            tags$h5("Total Fishing kwhr in EEZ: ", format(round(total_stats_caribbean$fishing_KWh, 0), big.mark = ",")),
            tags$hr(),
            tags$h3("EEZ Information"),
            tags$a(href = unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "FAO Country Profile"),
            tags$br(),
            tags$a(href = unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]), "Treaties and Conventions"),
            tags$br(),
            tags$a(href = unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), "Internal Fishing Access Agreements"),
            tags$hr(),
            tags$h3("Fishery organization: "),
            tags$a(unique(ACP_codes_links$fishery_org_eng[!is.na(ACP_codes_links$fishery_org_eng)]), href = unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])),
            tags$hr(),
            tags$h3("FAO Membership Information"),
            paste0("<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>", "</br>") %>%
              lapply(htmltools::HTML),
            tags$br()
        
      ) #close tag$div
}#close if else

  })# close render UI
  
  # 
  ###---------------------
  ##Caribbean: Connectivity map
  ###---------------------
  
  output$caribbean_connection_map <- renderLeaflet({
    
    
    #Require EEZ selection
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    selected_eez <- eez_map %>% 
      st_crop(c(xmin=-260, xmax=100, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(iso_ter1 == input$caribbean_eez_select) %>%
      rename(territory_iso3 = iso_ter1)
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$caribbean_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3) %>%
      arrange(flag) 
    
    flag_states_for_selected_eez <- land_eez_map %>% 
      st_crop(c(xmin=-260, xmax=100, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    # Should be able to remove this step eventually. Ideally we need a land/eez map with all flag states represented. 
    connectivity_data_edit <- connectivity_data_for_selected_eez %>%
      dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
    
    # Connectivity stats with no geometry
    no_geometry <- connectivity_data_edit %>%
      group_by(territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacty, na.rm = T),
                fishing_h = sum(fshng_h, na.rm = T),
                fishing_KWh = sum(fshn_KW, na.rm = T))
    st_geometry(no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(no_geometry, by = "flag") %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>% 
      as.data.frame()
    
    #### If not data do not draw map
    #browser()
    if(nrow(connectivity_data_edit) == 0) {
      
      
      caribbean_connection_map <-  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
        addProviderTiles("Esri.WorldTopoMap") %>% 
        addPolygons(data = selected_eez, 
                    fillColor = "tomato",
                    fillOpacity = 0.8,
                    color= "white",
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
                                                direction = "auto"))
      
    }else{
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>",  flag_state_summary$name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    

    #Leaflet map
   caribbean_connection_map <-  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
     htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = flag_states_for_selected_eez,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_states_for_selected_eez$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolygons(data = selected_eez, 
                  fillColor = "tomato",
                  fillOpacity = 0.8,
                  color= "white",
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
                   color = "darkgoldenrod") %>% 
      setView(lng = -75, lat = 17, zoom = 2)
      
    } #close if statement
    
  })
  
  ### -----------------------------------------------------------
  ### Caribbean: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$caribbean_eez_select, {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    connectivity_data_for_selected_eez <- connectivity_data %>%
      dplyr::filter(ez_tr_3 == input$caribbean_eez_select) %>% 
      arrange(flag)
    
    flag_state_choices_caribbean <- unique(connectivity_data_for_selected_eez$flag)
    names(flag_state_choices_caribbean) <- unique(countrycode(flag_state_choices_caribbean, "iso3c", "country.name"))
    
    updateSelectizeInput(session, "caribbean_flag_state_select",
                         choices = c("All flag states", flag_state_choices_caribbean)
    )
    
  })
  
  
  ### ----------------------------------------------------
  ### caribbean: switch tab and update flag state selectInput based on click on map
  ### ----------------------------------------------------
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$caribbean_connection_map_shape_click, {
    
    req(input$caribbean_connection_map_shape_click$id != input$caribbean_eez_select)
    
    updateTabItems(session, "caribbean_tabs", "Fishing effort and subsidy intensity of distant water vessels")
    
    updateSelectizeInput(session, "caribbean_flag_state_select",
                         selected = input$caribbean_connection_map_shape_click$id
    )
    
  })
  
  ### ------------------------------
  ### Caribbean: load eez specific data
  ### ------------------------------
  
  caribbean_eez_data <- eventReactive(input$caribbean_eez_select, {
    
    # Require EEZ selection
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$caribbean_eez_select]
    
    if(length(matching_file) == 0){
      
      tibble(dat = numeric(0))
      
    }else{
      
      out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnn")
      
      clean_out <- out %>%
        dplyr::filter(year == 2018) %>%
        mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
        mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
        dplyr::filter(name != names(caribbean_eez_choices[caribbean_eez_choices == input$caribbean_eez_select])) %>%
        arrange(name)
      
      clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
      
      clean_out
      
    }

  })
  
  ### -----------------------------------------------------------
  ### caribbean: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$caribbean_eez_select, {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    
    flag_state_choices_caribbean <- unique(caribbean_eez_data()$flag)
    names(flag_state_choices_caribbean) <- unique(caribbean_eez_data()$name)
    
    updateSelectizeInput(session, "caribbean_flag_state_select",
                         choices = c("All flag states", flag_state_choices_caribbean)
    )
    
  })
  
  
  
  ### -------------------------
  ### caribbean: subsidy heat map
  ### ------------------------
  
  output$caribbean_subsidy_map <- renderPlot(bg = "#262626", {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    
    ### Get totals for data
    eez_totals <- caribbean_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$caribbean_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- caribbean_eez_data() %>%
        dplyr::filter(flag == input$caribbean_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
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
  
  
  ### -------------------------
  ### caribbean: effort heat map
  ### ------------------------
  
  output$caribbean_effort_map <- renderPlot(bg = "#262626", {
    
    req(input$caribbean_eez_select != "Select a coastal state...")
    req(nrow(caribbean_eez_data()) > 0)
    
    ### Get totals for data
    eez_totals <- caribbean_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$caribbean_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- caribbean_eez_data() %>%
        dplyr::filter(flag == input$caribbean_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$fishing_KWh/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
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
  

  ###------------
  ### pacific 
  ###-----------
  
  ### Map of pacific EEZs for which we have DW fishing effort
  output$pacific_map <- renderLeaflet({
    
    # Filter data
    pacific_eezs <- eez_map %>%
      st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      inner_join(ACP_codes %>% dplyr::filter(region == "Pacific"), by = c("mrgid")) %>%
      mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
    
    # Deal with multiple EEZs for the same coastal state
    pacific_eezs_merged <- pacific_eezs %>%
      group_by(territory_iso3, geoname, region) %>%
      summarize(geometry = st_union(geometry))
    
    # Map
    leaflet('pacific_map', options = leafletOptions(zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
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
                  layerId = pacific_eezs_merged$territory_iso3, # need this to link to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>% 
      setView(lng = 175, lat = 0, zoom = 2)
    
  }) # Close render leaflet
  
  ###----------------------
  ### pacific: update EEZ selectInput based on click on map
  ###-----------------------
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$pacific_map_shape_click, {
    
    updateSelectizeInput(session, "pacific_eez_select",
                         selected = input$pacific_map_shape_click$id
    )
    
  }) 
  
  ###--------------------
  ### pacific: Proxy Map
  ###--------------------
  
  ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  
  pacific_proxy <- leafletProxy("pacific_map")
  
  observeEvent(input$pacific_eez_select, {
    
    if(input$pacific_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      pacific_proxy %>% setView(lng = 175, lat = 0, zoom=2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_map, eez_map$iso_ter1 == input$pacific_eez_select) %>%
        mutate(x_1 = ifelse(x_1 < 0, 360 + x_1, x_1))
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      pacific_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = "mediumpurple",
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 0.3,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      group = "highlighted_eez") %>%
        setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=4)
    }
    
  }) # Close observe event
  
  
  
  # 
  # ###------------
  # ### Pacific: Links to Online references
  # ###------------
  # 
  output$pacific_online_text <- renderUI({
    
    req(input$pacific_eez_select != "Select a coastal state...")
    
    connectivity_data_filter_pacific <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$pacific_eez_select) %>%
      rename(territory_iso3 = ez_tr_3) %>%
      mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
    
    # Total stats by coastal state
    total_stats_pacific <- connectivity_data_filter_pacific %>%
      as.data.frame() %>%
      select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>%
      group_by(territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacty, na.rm = T),
                fishing_h = sum(fshng_h, na.rm = T),
                fishing_KWh = sum(fshn_KW, na.rm = T)) %>%
      arrange(territory_iso3)
    
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$pacific_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>%
      separate_rows(fao_memberships, sep = ",")
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    EEZ_info <- tags$div(
      tags$h3("EEZ Summary Statistics"),
      tags$h4(total_stats_pacific$eez_nam),
      tags$h5("Total Number of Vessels in EEZ: ", format(round(total_stats_pacific$vessels, 0), big.mark = ",")),
      tags$h5("Total Fishing hours per year in EEZ: ", format(round(total_stats_pacific$fishing_h, 0), big.mark = ",")),
      tags$h5("Total Fishing kwhr in EEZ: ", format(round(total_stats_pacific$fishing_KWh, 0), big.mark = ",")),
      tags$hr(),
      tags$h3("EEZ Information"),
      tags$a(href = unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "FAO Country Profile"),
      tags$br(),
      tags$a(href = unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]), "Treaties and Conventions"),
      tags$br(),
      tags$a(href = unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), "Internal Fishing Access Agreements"),
      tags$hr(),
      tags$h4("Fishery organization: "),
      tags$a(unique(ACP_codes_links$fishery_org_eng[!is.na(ACP_codes_links$fishery_org_eng)]), href = unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])),
      tags$hr(),
      tags$h3("FAO Membership Information"),
      paste0("<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>", "</br>") %>%
        lapply(htmltools::HTML)
      
      
      
    )
    
  })
  
  ###---------------------
  ##pacific: Connectivity map
  ###---------------------
  
  output$pacific_connection_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$pacific_eez_select != "Select a coastal state...")
    
    selected_eez <- eez_map %>% 
      st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(iso_ter1 == input$pacific_eez_select) %>%
      rename(territory_iso3 = iso_ter1)
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$pacific_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3) %>%
      arrange(flag) 
    
    flag_states_for_selected_eez <- land_eez_map %>% 
      st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    # Should be able to remove this step eventually. Ideally we need a land/eez map with all flag states represented. 
    connectivity_data_edit <- connectivity_data_for_selected_eez %>%
      dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
    
    # Connectivity stats with no geometry
    no_geometry <- connectivity_data_edit %>%
      group_by(territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacty, na.rm = T),
                fishing_h = sum(fshng_h, na.rm = T),
                fishing_KWh = sum(fshn_KW, na.rm = T))
    st_geometry(no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(no_geometry, by = "flag") %>%
      mutate(name = countrycode(flag, "iso3c", "country.name"))
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>",  flag_state_summary$name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    
    #Leaflet map
    
    leaflet('pacific_connection_map', options = leafletOptions(zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = flag_states_for_selected_eez,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_states_for_selected_eez$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolygons(data = selected_eez, 
                  fillColor = "mediumpurple",
                  fillOpacity = 0.8,
                  color= "white",
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
                   color = "darkgoldenrod") %>% 
      setView(lng = 175, lat = 0, zoom = 1)
  })
  
  ### -----------------------------------------------------------
  ### pacific: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$pacific_eez_select, {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    
    connectivity_data_for_selected_eez <- connectivity_data %>%
      dplyr::filter(ez_tr_3 == input$pacific_eez_select) %>% 
      arrange(flag)
    
    flag_state_choices_pacific <- unique(connectivity_data_for_selected_eez$flag)
    names(flag_state_choices_pacific) <- unique(countrycode(flag_state_choices_pacific, "iso3c", "country.name"))
    
    updateSelectizeInput(session, "pacific_flag_state_select",
                         choices = c("All flag states", flag_state_choices_pacific)
    )
    
  })
  
  
  ### ----------------------------------------------------
  ### pacific: switch tab and update flag state selectInput based on click on map
  ### ----------------------------------------------------
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$pacific_connection_map_shape_click, {
    
    req(input$pacific_connection_map_shape_click$id != input$pacific_eez_select)
    
    updateTabItems(session, "pacific_tabs", "Fishing effort and subsidy intensity of distant water vessels")
    
    updateSelectizeInput(session, "pacific_flag_state_select",
                         selected = input$pacific_connection_map_shape_click$id
    )
    
  })
  
  ### ------------------------------
  ### pacific: load eez specific data
  ### ------------------------------
  
  pacific_eez_data <- eventReactive(input$pacific_eez_select, {
    
    # Require EEZ selection
    req(input$pacific_eez_select != "Select a coastal state...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$pacific_eez_select]
    
    if(length(matching_file) == 0){
      
      tibble(dat = numeric(0))
      
    }else{
      
      out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnn")
      
      clean_out <- out %>%
        dplyr::filter(year == 2018) %>%
        mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
        mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
        dplyr::filter(name != names(pacific_eez_choices[pacific_eez_choices == input$pacific_eez_select])) %>%
        arrange(name)
      
      clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
      
      clean_out
      
    }
    
  })
  
  ### -----------------------------------------------------------
  ### pacific: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$pacific_eez_select, {
    
    req(input$pacific_eez_select != "Select a coastal state...")
    
    flag_state_choices_pacific <- unique(pacific_eez_data()$flag)
    names(flag_state_choices_pacific) <- unique(pacific_eez_data()$name)
    
    updateSelectizeInput(session, "pacific_flag_state_select",
                         choices = c("All flag states", flag_state_choices_pacific)
    )
    
  })
  
  
  
  ### -------------------------
  ### pacific: subsidy heat map
  ### ------------------------
  
  output$pacific_subsidy_map <- renderPlot(bg = "#262626", {
    
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
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$pacific_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- pacific_eez_data() %>%
        dplyr::filter(flag == input$pacific_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
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
  
  
  ### -------------------------
  ### pacific: effort heat map
  ### ------------------------
  
  output$pacific_effort_map <- renderPlot(bg = "#262626", {
    
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
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$pacific_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- pacific_eez_data() %>%
        dplyr::filter(flag == input$pacific_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    #browser()
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$fishing_KWh/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
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
  
  ### -------------------
  ### Navigation buttons
  ### -------------------
  
  # return to regional map buttons
  observeEvent(input$africa_return_to_region, {
    updateTabItems(session, "tabs", "selectregion")
  })
  
  observeEvent(input$caribbean_return_to_region, {
    updateTabItems(session, "tabs", "selectregion")
  })
  
  observeEvent(input$pacific_return_to_region, {
    updateTabItems(session, "tabs", "selectregion")
  })
  
})

### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

