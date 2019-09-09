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

### Load packages -----

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


### Source UI files -----

# The user interface content for each tab is stored in a separate file - Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)

### Data -----

# Load csv of ACP EEZ and iso3 codes
ACP_codes <- read_csv("./data/ACP_eez_codes.csv") %>%
  mutate(flag = countrycode(territory_iso3, "iso3c", "country.name")) 

eez_regions <- ACP_codes %>%
  distinct(mrgid, region)

flag_regions <- ACP_codes %>%
  distinct(territory_iso3, region)

# Load csv of FAO RFMO codes and links
RFMO_links <- read_csv("./data/RMFO_links.csv")

# Load spatial data frame with lines linking countries and EEZs
connectivity_data <- read_sf("./data/eez_results/ACP/eez_mapping_with_lines.shp") %>%
  rename(eez_code = eez_cod,
         eez_territory_iso3 = ez_tr_3,
         capacity = capacty,
         fishing_h = fshng_h,
         fishing_KWh = fshn_KW)

#browser()

### Shapefiles -----

### 1. Simplified EEZ shapefile (-360 to 360 degrees: crop appropriately for each region)
eez_map <- read_sf(dsn = "./data/shapefiles_edit/World_EEZ_v10_SubsidyAtlasACP", layer = "eez_v10") %>%
    setNames(tolower(names(.))) %>%
    st_transform(crs = 4326) %>%
    left_join(eez_regions, by = "mrgid")
# Africa
africa_eez_map <- eez_map %>%
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) 
# Caribbean
# caribbean_eez_map <- eez_map %>%
#   st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90), warn = FALSE) %>%
#   st_collection_extract(type = c("POLYGON"), warn = FALSE) 
# Pacific
pacific_eez_map <- eez_map %>%
  st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) 

### 2. Simplified land shapefile 
# land_map <- read_sf(dsn = "./data/shapefiles_edit/world_happy_180", layer="world_happy_180") %>%
#   st_transform(crs = 4326) %>%
#   left_join(flag_regions, by = c("iso3" = "territory_iso3"))
# land_map <- read_sf(dsn = "./data/shapefiles_edit/ne_10m_admin_0_map_subunits_-360_360")

### 3. Simplified combined land/EEZ shapefile (-360 to 360 degrees: crop appropriately for each region)
land_eez_map <- read_sf(dsn = "./data/shapefiles_edit/EEZ_land_v2_201410_-360_360", layer = "EEZ_land_v2_201410_-360_360") %>%
  setNames(tolower(names(.))) %>%
  st_transform(crs = 4326) %>% 
  dplyr::select(iso3 = iso_3digit,
                country,
                geometry) %>%
  dplyr::filter(!is.na(iso3)) %>%
  left_join(flag_regions, by = c("iso3" = "territory_iso3"))

# Africa
africa_land_eez_map <- land_eez_map %>% 
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
  st_collection_extract(type = c("POLYGON"))
# Caribbean
caribbean_land_eez_map <- land_eez_map %>% 
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
  st_collection_extract(type = c("POLYGON"))
# Pacific
pacific_land_eez_map <- land_eez_map %>% 
  st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
  st_collection_extract(type = c("POLYGON"))

### Widget choice values that depend on a dataset -----

# Define EEZ and flag state choices
africa_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])
names(africa_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])

caribbean_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])  
names(caribbean_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])

pacific_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])
names(pacific_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])

flag_state_choices <- unique(connectivity_data$flag)
names(flag_state_choices) <- countrycode(flag_state_choices, "iso3c", "country.name")
names(flag_state_choices)[is.na(names(flag_state_choices))] <- "Unknown flag"
  
### Themes -----

# Standard map ggplot theme
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
                                src = 'emLab-logo-white.png',
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
                                
                                # Introduction/select a region
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
                                
                                # Pacific 
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
    palette = "Dark2",
    domain = ACP_codes$region,
    na.color = "grey"
  )
  region_pal_light <- colorFactor(
    palette = "Set2",
    domain = ACP_codes$region,
    na.color = "grey"
  )
  
  ## Leaflet output: map of ACP countries aggregated by region
  output$regional_map <- renderLeaflet({
    
    # Make aggregated regional map data
    regional_dat <- land_eez_map %>%
      st_crop(c(xmin=-30, xmax=330, ymin=-90, ymax=90)) %>%
      st_collection_extract(type = c("POLYGON")) %>%
      dplyr::filter(!is.na(region)) %>%
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
            options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>%
      
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      # Title box
      addControl(intro_overlay, 
                 position = "topleft", 
                 className = "map-title") %>%
      # Bottom left box
      # addControl(intro_overlay, 
      #            position = "topleft") %>%
       
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
  
  ###---------------------------------------------------------------------------------------
  ### Africa -------------------------------------------------------------------------------
  ###---------------------------------------------------------------------------------------
  
  ### -----
  ### Leaflet output: Map of Africa ACP EEZs for which we have DW fishing effort
  ### -----
  output$africa_map <- renderLeaflet({
    
    # Extract ACP EEZs
    africa_eezs <- africa_eez_map %>%
      dplyr::filter(region == "Africa") %>%
      mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
    
    # Merge non-contiguous EEZs for the same coastal state (South Africa)
    africa_eezs_merged <- africa_eezs %>%
      group_by(iso_ter, geoname, region) %>%
      summarize(geometry = st_union(geometry))
    
    # Also extract disputed areas/joint management areas involving ACP coastal states in Africa
    africa_disputed_joint <- africa_eez_map %>%
      dplyr::filter(pol_type != "200NM" & iso_ter %in% africa_eezs$iso_ter) %>%
      mutate(region = "Africa")
    
    # Map
    leaflet('africa_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.OceanBasemap") %>% 
      
      addPolygons(data = africa_disputed_joint, 
                  fillColor = "grey",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = africa_disputed_joint$geoname,
                  layerId = NULL, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      
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
                  layerId = africa_eezs_merged$iso_ter, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng= 15, lat = 0, zoom = 2)
    
})
  
  ### -----
  ### Update selectInput: Register user clicks on Africa map - change selected value of widget
  ### -----
  
  observeEvent(input$africa_map_shape_click, {
    
    # Don't register clicks on the disputed areas/joint areas
    req(!is.null(input$africa_map_shape_click$id))
    
    updateSelectizeInput(session, "africa_eez_select",
                         selected = input$africa_map_shape_click$id)
    
  })
  
  ### -----
  ### Leaflet proxy: Create proxy for the Africa map of ACP countries
  ### -----
  
  africa_proxy <- leafletProxy("africa_map")
  
  ### -----
  ### Leaflet proxy: When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  ### -----
  
  observeEvent(input$africa_eez_select, {
    
    if(input$africa_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      africa_proxy %>% setView(lng= 15, lat = 0, zoom = 2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(africa_eez_map, africa_eez_map$iso_ter == input$africa_eez_select)
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      africa_proxy %>% addPolygons(data = selected_eez,
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
    
    # Selected Africa ACP coastal state
    selected_eez <- africa_eez_map %>% 
      dplyr::filter(iso_ter == input$africa_eez_select) %>%
      rename(territory_iso3 = iso_ter)
    
    # Connectivity data for the entire region
    connectivity_data_region <- connectivity_data %>% # load this in up above
      dplyr::filter(region == "Africa")
    
    # Connectivity data for the EEZ of the selected ACP coastal state
    connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$africa_eez_select) %>% 
      arrange(flag)%>%
      dplyr::filter(flag != "UNK")
    
    flag_states_for_selected_eez <- africa_land_eez_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    # Filter out flag states that may not show up on map 
    if(length(unique(connectivity_data_for_selected_eez$flag)) != length(unique(flag_states_for_selected_eez$flag))) {
      
      connectivity_data_for_selected_eez <- connectivity_data_for_selected_eez %>%
        dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
    }
    
    # Connectivity stats with no geometry
    connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
      group_by(eez_territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T))
    st_geometry(connectivity_data_no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag") %>%
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
    
    # Leaflet map
    leaflet('africa_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
      
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
    
    # Selected Caribbean ACP coastal state
    selected_eez <- caribbean_eez_map %>% 
      dplyr::filter(iso_ter == input$caribbean_eez_select) %>%
      rename(territory_iso3 = iso_ter)
    
    # Connectivity data for the entire region
    connectivity_data_region <- connectivity_data %>% # load this in up above
      dplyr::filter(region == "Caribbean")
    
    # Connectivity data for the EEZ of the selected ACP coastal state
    connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$caribbean_eez_select) %>% 
      arrange(flag)%>%
      dplyr::filter(flag != "UNK")
    
    flag_states_for_selected_eez <- caribbean_land_eez_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    # Filter out flag states that may not show up on map 
    if(length(unique(connectivity_data_for_selected_eez$flag)) != length(unique(flag_states_for_selected_eez$flag))) {
      
      connectivity_data_for_selected_eez <- connectivity_data_for_selected_eez %>%
        dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
    }
    
    # Connectivity stats with no geometry
    connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
      group_by(eez_territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T))
    st_geometry(connectivity_data_no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag") %>%
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
    
    # Leaflet map
    leaflet('caribbean_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
      
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
    
    # Selected Pacific ACP coastal state
    selected_eez <- pacific_eez_map %>% 
      dplyr::filter(iso_ter == input$pacific_eez_select) %>%
      rename(territory_iso3 = iso_ter)
    
    # Connectivity data for the entire region
    connectivity_data_region <- connectivity_data %>% # load this in up above
      dplyr::filter(region == "Pacific")
    
    # Connectivity data for the EEZ of the selected ACP coastal state
    connectivity_data_for_selected_eez <- connectivity_data_region %>% # load this in up above
      dplyr::filter(eez_territory_iso3 == input$pacific_eez_select) %>% 
      arrange(flag)%>%
      dplyr::filter(flag != "UNK")
    
    flag_states_for_selected_eez <- pacific_land_eez_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    # Filter out flag states that may not show up on map 
    if(length(unique(connectivity_data_for_selected_eez$flag)) != length(unique(flag_states_for_selected_eez$flag))) {
      
      connectivity_data_for_selected_eez <- connectivity_data_for_selected_eez %>%
        dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
    }
    
    # Connectivity stats with no geometry
    connectivity_data_no_geometry <- connectivity_data_for_selected_eez %>%
      group_by(eez_territory_iso3, flag) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacity = sum(capacity, na.rm = T),
                fishing_h = sum(fishing_h, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T))
    st_geometry(connectivity_data_no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(connectivity_data_no_geometry, by = "flag") %>%
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
    
    # Leaflet map
    leaflet('pacific_connection_map', options = leafletOptions(minZoom = 2, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "basemap") %>% 
      
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
                opacity=0.9, title = input$pacific_connection_fill, position = "bottomleft" ) %>%
      setView(lng = 175, lat = 0, zoom = 2)
    
})
  
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

  # ###--------------
  # ### Pacific -----
  # ###--------------
  # 
  # ### Leaflet Map: Pacific EEZs for which we have DW fishing effort -----
  # 
  # output$pacific_map <- renderLeaflet({
  #   
  #   # Filter data
  #   pacific_eezs <- eez_map %>%
  #     st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
  #     st_collection_extract(type = c("POLYGON")) %>%
  #     dplyr::filter(region == "Pacific") %>%
  #     mutate(geoname = str_replace(geoname, " \\(.*\\)", ""))
  #   
  #   # Deal with multiple EEZs for the same coastal state
  #   pacific_eezs_merged <- pacific_eezs %>%
  #     group_by(iso_ter1, geoname, region) %>%
  #     summarize(geometry = st_union(geometry))
  #   
  #   # Map
  #   leaflet('pacific_map', options = leafletOptions(zoomControl = FALSE)) %>% 
  #     htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
  #     
  #     addProviderTiles("Esri.OceanBasemap") %>% 
  #     
  #     addPolygons(data = pacific_eezs_merged, 
  #                 fillColor = ~region_pal(region),
  #                 fillOpacity = 0.8,
  #                 color= "white",
  #                 weight = 0.3,
  #                 highlight = highlightOptions(weight = 5,
  #                                              color = "#666",
  #                                              fillOpacity = 1,
  #                                              bringToFront = TRUE),
  #                 label = pacific_eezs_merged$geoname,
  #                 layerId = pacific_eezs_merged$iso_ter1, # need this to link to select input below
  #                 labelOptions = labelOptions(style = list("font-weight" = "normal",
  #                                                          padding = "3px 8px"),
  #                                             textsize = "13px",
  #                                             direction = "auto")
  #     ) %>% 
  #     setView(lng = 175, lat = 0, zoom = 2)
  #   
  # }) # Close render leaflet
  # 
  # 
  # ### Update Select Input: update selected Pacific coastal state -----
  # 
  # observeEvent(input$pacific_map_shape_click, {
  #   
  #   # Register user clicks on map - change selected value on input widget
  #   updateSelectizeInput(session, "pacific_eez_select",
  #                        selected = input$pacific_map_shape_click$id
  #   )
  #   
  # }) 
  # 
  # ###--------------------
  # ### Pacific: Proxy Map
  # ###--------------------
  # 
  # ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  # 
  # pacific_proxy <- leafletProxy("pacific_map")
  # 
  # observeEvent(input$pacific_eez_select, {
  #   
  #   if(input$pacific_eez_select == "Select a coastal state..."){
  #     
  #     # Remove any previously highlighted polygon
  #     pacific_proxy %>% clearGroup("highlighted_eez")  
  #     
  #     # Reset view to entire region
  #     pacific_proxy %>% setView(lng = 175, lat = 0, zoom=2)
  #     
  #   }else{
  #     
  #     # Get code for selected EEZ
  #     selected_eez <- subset(eez_map, eez_map$iso_ter1 == input$pacific_eez_select) %>%
  #       mutate(x_1 = ifelse(x_1 < 0, 360 + x_1, x_1))
  # 
  #     if(input$pacific_eez_select == "FJI"){
  #       # The coordinates for Fiji are just wrong in the EEZ file for some reason
  #       selected_eez$x_1[selected_eez$iso_ter1 == "FJI"] <- 177.956
  #     
  #     }else if(input$pacific_eez_select == "TUV"){
  #       # Same for Tuvalu
  #       selected_eez$x_1[selected_eez$iso_ter1 == "TUV"] <- 177.54
  #     }
  # 
  #     # Remove any previously highlighted polygon
  #     pacific_proxy %>% clearGroup("highlighted_eez")
  #     
  #     # Add a different colored polygon on top of map
  #     pacific_proxy %>% addPolygons(data = selected_eez,
  #                                     fillColor = ~region_pal_light(region),
  #                                     fillOpacity = 1,
  #                                     color= "white",
  #                                     weight = 2,
  #                                     highlight = highlightOptions(weight = 5,
  #                                                                  color = "#666",
  #                                                                  fillOpacity = 1,
  #                                                                  bringToFront = TRUE),
  #                                     group = "highlighted_eez") %>%
  #       setView(lng=mean(selected_eez$x_1, na.rm = T), lat=mean(selected_eez$y_1, na.rm = T), zoom=4)
  #   }
  #   
  # }) # Close observe event
  # 
  # 
  # 
  #  
  # ###------------
  # ### Pacific: Links to Online references
  # ###------------
  #  
  # output$pacific_country_profile <- renderUI({
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   
  #   connectivity_data_filter_pacific <- connectivity_data %>% # load this in up above
  #     dplyr::filter(ez_tr_3 == input$pacific_eez_select) %>%
  #     rename(territory_iso3 = ez_tr_3) %>%
  #     mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
  #   
  #   # Total stats by coastal state
  #   total_stats_pacific <- connectivity_data_filter_pacific %>%
  #     as.data.frame() %>%
  #     group_by(territory_iso3, eez_nam) %>%
  #     summarize(vessels = sum(vessels, na.rm = T),
  #               capacity = sum(capacty, na.rm = T),
  #               fishing_h = sum(fshng_h, na.rm = T),
  #               fishing_KWh = sum(fshn_KW, na.rm = T)) %>%
  #     arrange(territory_iso3)
  #   
  #   # Filter and format Country profile data
  #   ACP_codes_links <- ACP_codes %>%
  #     dplyr::filter(territory_iso3 == input$pacific_eez_select)
  #   
  #   ACP_fao_membership <- ACP_codes_links %>%
  #     separate_rows(fao_memberships, sep = ",")
  #   
  #   RFMO_links_eez <- RFMO_links %>%
  #     dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
  #   
  #   # Treaties and Conventions
  #   treaties_conventions <- paste0(
  #     "<a href= '",
  #     unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]),
  #     "'>",
  #     unique(ACP_codes_links$territory[!is.na(ACP_codes_links$treaties_conventions)]),
  #     "</a>",
  #     collapse = " | "
  #   )
  #   
  #   # Foreign access agreements by EEZ sector
  #   foreign_access_agreements <- paste0(
  #     "<a href= '", 
  #     unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
  #     "'>", 
  #     unique(ACP_codes_links$territory[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
  #     "</a>", 
  #     collapse = " | ")
  #   
  #   # FAO Regional Fisheries Body Memberships
  #   regional_body_memberships <- paste0(
  #     "<a href= '", 
  #     unique(RFMO_links_eez$link[!is.na(RFMO_links_eez$link)]),
  #     "'>",
  #     unique(RFMO_links_eez$rfmo_name[!is.na(RFMO_links_eez$link)]),
  #     "</a>",
  #     collapse = " | ")
  #   
  #   # Combine and format for country profile/summary
  #   EEZ_info <- paste0("<h3 style = 'margin-top: 0px;'>", names(pacific_eez_choices[pacific_eez_choices == input$pacific_eez_select]), "</h3>",
  #                      "Fisheries management agency:  ", 
  #                      ifelse(length(unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])) > 0,
  #                             paste0("<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "'>", 
  #                                    ACP_codes_links$fishery_org_eng, "</a>")),
  #                             #ACP_codes_links$fishery_org_eng),
  #                      
  #                      # "<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "'>", 
  #                      # unique(ACP_codes_links$fishery_org_eng[!is.na(ACP_codes_links$fishery_org_eng)]), "</a>",
  #                      "<br>",
  #                      
  #                      "Country profile: ",
  #                      "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "'>", 
  #                      "FAO", "</a>",
  #                      " | ",
  #                      ifelse(length(unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)])) > 0,
  #                             paste0("<a href='", unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)]), "'>", 
  #                             "World Bank", "</a>"),
  #                             "World Bank"),
  #                      " | ",
  #                      ifelse(length(unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)])) > 0,
  #                             paste0("<a href='", unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)]), "'>", 
  #                             "United Nations", "</a>"),
  #                             "United Nations"),
  #                      
  #                      "<br>",
  #                      
  #                      "Treaties and conventions: ",
  #                      treaties_conventions,
  #                      "<br>",
  #                      
  #                      "Foreign access agreements: ",
  #                      foreign_access_agreements,
  #                      "<br>",
  #                      
  #                      "FAO Regional Fisheries Body Memberships: ",
  #                      regional_body_memberships,
  #                      "<br>",
  #                      
  #                      "<hr>",
  #                      "<b>", "Distant water fishing in the ", total_stats_pacific$eez_nam, " (2018)", "</b>",
  #                      "<br>",
  #                      "Vessels: ", format(round(total_stats_pacific$vessels, 0), big.mark = ","),
  #                      "<br>",
  #                      "Total engine capacity (KW): ", format(round(total_stats_pacific$capacity, 0), big.mark = ","),
  #                      "<br>",
  #                      "Fishing effort (hours): ", format(round(total_stats_pacific$fishing_h, 0), big.mark = ","),
  #                      "<br>",
  #                      "Fishing effort (KWh): ", format(round(total_stats_pacific$fishing_KWh, 0), big.mark = ",")) %>%
  #     lapply(htmltools::HTML)
  #   
  #   # Return
  #   EEZ_info
  #   
  # })
  # 
  # ###---------------------
  # ##pacific: Connectivity map
  # ###---------------------
  # 
  # output$pacific_connection_map <- renderLeaflet({
  #   
  #   #Require EEZ selection
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   
  #   selected_eez <- eez_map %>% 
  #     st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
  #     st_collection_extract(type = c("POLYGON")) %>%
  #     dplyr::filter(iso_ter1 == input$pacific_eez_select) %>%
  #     rename(territory_iso3 = iso_ter1)
  #   
  #   connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
  #     dplyr::filter(ez_tr_3 == input$pacific_eez_select) %>% 
  #     rename(territory_iso3 = ez_tr_3) %>%
  #     arrange(flag) 
  #   
  #   flag_states_for_selected_eez <- land_eez_map %>% 
  #     st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
  #     st_collection_extract(type = c("POLYGON")) %>%
  #     dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
  #     rename(flag = iso3) %>% 
  #     arrange(flag)
  #   
  #   # Should be able to remove this step eventually. Ideally we need a land/eez map with all flag states represented. 
  #   connectivity_data_edit <- connectivity_data_for_selected_eez %>%
  #     dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
  #   
  #   # Connectivity stats with no geometry
  #   no_geometry <- connectivity_data_edit %>%
  #     group_by(territory_iso3, flag) %>%
  #     summarize(vessels = sum(vessels, na.rm = T),
  #               capacity = sum(capacty, na.rm = T),
  #               fishing_h = sum(fshng_h, na.rm = T),
  #               fishing_KWh = sum(fshn_KW, na.rm = T))
  #   st_geometry(no_geometry) <- NULL
  #   
  #   #Summary of all info for pacific
  #   regional_summary <- connectivity_data %>%
  #     rename(territory_iso3 = ez_tr_3) %>%
  #     filter(region == "Pacific") %>% 
  #   group_by(territory_iso3, flag) %>%
  #     summarize(vessels = sum(vessels, na.rm = T),
  #               capacity = sum(capacty, na.rm = T),
  #               fishing_h = sum(fshng_h, na.rm = T),
  #               fishing_KWh = sum(fshn_KW, na.rm = T)) #%>% 
  #     #select(fishing_KWh)
  #     
  #   st_geometry(regional_summary) <- NULL
  #   
  #   
  #   #  Hover Text
  #   flag_state_summary <- flag_states_for_selected_eez %>% 
  #     left_join(no_geometry, by = "flag") %>%
  #     mutate(name = countrycode(flag, "iso3c", "country.name"))
  #   
  #   flag_state_summary_text <- paste0(
  #     "<b>", "Flag state: ", "</b>",  flag_state_summary$name,
  #     "<br/>",
  #     "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
  #     "</br>",
  #     "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacity, 0), big.mark = ","), 
  #     "</br>",
  #     "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fishing_h, 0), big.mark = ","), 
  #     "</br>",
  #     "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fishing_KWh, 0), big.mark = ",")) %>% 
  #     lapply(htmltools::HTML)
  #   
  #   #bins <- c(0,1000,10000,100000,1000000,10000000,50000000,100000000,Inf)
  #   #vessels <- flag_state_summary$vessels
  #   
  #   
  #   
  #   max_effort_pacific <- regional_summary %>% 
  #     arrange(desc(fishing_KWh)) %>% 
  #     top_n(n=1)
  #   
  #   max_effort_pacific <- max(max_effort_pacific$fishing_KWh)
  #     
  #   
  #   effort <- flag_state_summary$fishing_KWh
  #   pal <- colorNumeric("YlOrRd", 0:max_effort_pacific)
  #   
  #   
  #   #Leaflet map
  #   
  #   leaflet('pacific_connection_map', options = leafletOptions(zoomControl = FALSE)) %>% 
  #     htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
  #     addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
  #     addPolygons(data = flag_states_for_selected_eez,
  #                 fillColor = ~pal(effort),
  #                 fillOpacity = 0.8,
  #                 color= "white",
  #                 weight = 0.3,
  #                 highlight = highlightOptions(weight = 5,
  #                                              color = "#666",
  #                                              fillOpacity = 1,
  #                                              bringToFront = TRUE),
  #                 label = flag_state_summary_text,
  #                 layerId = flag_states_for_selected_eez$flag,
  #                 labelOptions = labelOptions(style = list("font-weight" = "normal",
  #                                                          padding = "3px 8px"),
  #                                             textsize = "13px",
  #                                             direction = "auto")) %>% 
  #     addPolygons(data = selected_eez, 
  #                 fillColor = "mediumpurple",
  #                 fillOpacity = 0.8,
  #                 color= "white",
  #                 weight = 0.3,
  #                 highlight = highlightOptions(weight = 5,
  #                                              color = "#666",
  #                                              fillOpacity = 1,
  #                                              bringToFront = TRUE),
  #                 label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
  #                            lapply(htmltools::HTML)),
  #                 labelOptions = labelOptions(style = list("font-weight" = "normal",
  #                                                          padding = "3px 8px"),
  #                                             textsize = "13px",
  #                                             direction = "auto")) %>% 
  #     
  #     addPolylines(data = connectivity_data_edit,
  #                  fillColor = "goldenrod",
  #                  fillOpacity = 1,
  #                  weight = 1,
  #                  color = "darkgoldenrod") %>% 
  #     addLegend(pal = pal, values = 0:max_effort_pacific, opacity=0.9, title = "Fishing Effort (Kwh)", position = "bottomleft" ) %>%
  #     setView(lng = 175, lat = 0, zoom = 1)
  # })
  # 
  # ### -----------------------------------------------------------
  # ### pacific: filter flag state selectInput choices based on selected EEZ
  # ### -----------------------------------------------------------
  # 
  # observeEvent(input$pacific_eez_select, {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   
  #   connectivity_data_for_selected_eez <- connectivity_data %>%
  #     dplyr::filter(ez_tr_3 == input$pacific_eez_select) %>% 
  #     arrange(flag) %>%
  #     dplyr::filter(flag != "UNK")
  #   
  #   flag_state_choices_pacific <- unique(connectivity_data_for_selected_eez$flag)
  #   names(flag_state_choices_pacific) <- unique(countrycode(flag_state_choices_pacific, "iso3c", "country.name"))
  #   
  #   updateSelectizeInput(session, "pacific_flag_state_select",
  #                        choices = c("All flag states", flag_state_choices_pacific)
  #   )
  #   
  # })
  # 
  # 
  # ### ----------------------------------------------------
  # ### pacific: switch tab and update flag state selectInput based on click on map
  # ### ----------------------------------------------------
  # 
  # ### Register user clicks on connectivity map - change select input from widget
  # observeEvent(input$pacific_connection_map_shape_click, {
  #   
  #   req(input$pacific_connection_map_shape_click$id != input$pacific_eez_select)
  #   
  #   updateTabItems(session, "pacific_tabs", "Distant water fishing effort")
  #   
  #   updateSelectizeInput(session, "pacific_flag_state_select_effort",
  #                        selected = input$pacific_connection_map_shape_click$id
  #   )
  #   
  #   updateSelectizeInput(session, "pacific_flag_state_select_subsidy",
  #                        selected = input$pacific_connection_map_shape_click$id
  #   )
  #   
  # })
  # 
  # ### ------------------------------
  # ### pacific: load eez specific data
  # ### ------------------------------
  # 
  # pacific_eez_data <- eventReactive(input$pacific_eez_select, {
  #   
  #   # Require EEZ selection
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   
  #   # Find matching data file and load
  #   all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
  #   coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
  #   matching_file <- all_data_files[coastal_state_codes == input$pacific_eez_select]
  #   
  #   if(length(matching_file) == 0){
  #     
  #     tibble(dat = numeric(0))
  #     
  #   }else{
  #     
  #     out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnn")
  #     
  #     clean_out <- out %>%
  #       dplyr::filter(year == 2018) %>%
  #       mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
  #       mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
  #       dplyr::filter(name != names(pacific_eez_choices[pacific_eez_choices == input$pacific_eez_select])) %>%
  #       arrange(name)
  #     
  #     clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
  #     
  #     clean_out %>%
  #       mutate(lon_cen = ifelse(lon_cen < 0, 360 + lon_cen, lon_cen)) # correction for 0 to 360 range
  #     
  #   }
  #   
  # })
  # 
  # ### -----------------------------------------------------------
  # ### pacific: filter flag state selectInput choices based on selected EEZ
  # ### -----------------------------------------------------------
  # 
  # observeEvent(input$pacific_eez_select, {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   
  #   flag_state_choices_pacific <- unique(pacific_eez_data()$flag)
  #   names(flag_state_choices_pacific) <- unique(pacific_eez_data()$name)
  #   
  #   updateSelectizeInput(session, "pacific_flag_state_select_subsidy",
  #                        choices = c("Select a flag state...", flag_state_choices_pacific)
  #   )
  #   
  # })
  # 
  # observeEvent(input$pacific_eez_select, {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   
  #   flag_state_choices_pacific <- unique(pacific_eez_data()$flag)
  #   names(flag_state_choices_pacific) <- unique(pacific_eez_data()$name)
  #   
  #   updateSelectizeInput(session, "pacific_flag_state_select_effort",
  #                        choices = c("Select a flag state...", flag_state_choices_pacific)
  #   )
  #   
  # })
  # 
  # 
  # ### -----
  # ### Pacific: reactive bounding box coordinates for selected EEZ
  # ### -----
  # 
  # pacific_eez_bbox <- eventReactive(input$pacific_eez_select, {
  #   
  #   eez_map_subsidy <- eez_map %>% 
  #     st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
  #     st_collection_extract(type = c("POLYGON")) %>%
  #     dplyr::filter(iso_ter1 == input$pacific_eez_select) %>%
  #     group_by(iso_ter1) %>%
  #     summarize(geometry = st_union(geometry))
  #   
  #   st_bbox(eez_map_subsidy)
  #   
  # })
  # 
  # ### -------------------------
  # ### Pacific: subsidy heat map (All flag states)
  # ### ------------------------
  # 
  # output$pacific_subsidy_map_all <- renderPlot(bg = "#262626", {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   req(nrow(pacific_eez_data()) > 0)
  #   
  #   # Get totals for data
  #   eez_totals <- pacific_eez_data() %>%
  #     group_by(lon_cen, lat_cen) %>%
  #     summarize(fishing_hours = sum(fishing_hours, na.rm = T),
  #               fishing_KWh = sum(fishing_KWh, na.rm = T),
  #               subs = sum(subs, na.rm = T)) %>%
  #     mutate(subsidy_intensity = subs/fishing_KWh)
  #   
  #   # Set limits for map area
  #   x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
  #   y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
  #   
  #   # Define plot data  
  #   eez_plot_data <- eez_totals
  #   
  #   # Get data quntiles to set fil scale limit appropriately
  #   intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
  #   scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
  #   
  #   # Map of subsidy intensity
  #   ggplot()+
  #     geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
  #     scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
  #                          limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
  #                          labels=scale_labels,
  #                          breaks=scale_labels,
  #                          oob=scales::squish)+
  #     geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
  #     geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
  #     labs(x = "", y = "")+
  #     coord_sf(xlim = x_lim, ylim = y_lim)+
  #     guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
  #     scale_x_continuous(expand = c(0,0))+
  #     scale_y_continuous(expand = c(0,0))+
  #     eezmaptheme
  #   
  # })
  # 
  # ### -------------------------
  # ### Pacific: subsidy heat map (Selected flag state)
  # ### ------------------------
  # 
  # output$pacific_subsidy_map <- renderPlot(bg = "#262626", {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   req(nrow(pacific_eez_data()) > 0)
  #   req(input$pacific_flag_state_select_subsidy != "Select a flag state...")
  #   
  #   ### Get totals for data
  #   eez_totals <- pacific_eez_data() %>%
  #     group_by(lon_cen, lat_cen) %>%
  #     summarize(fishing_hours = sum(fishing_hours, na.rm = T),
  #               fishing_KWh = sum(fishing_KWh, na.rm = T),
  #               subs = sum(subs, na.rm = T)) %>%
  #     mutate(subsidy_intensity = subs/fishing_KWh)
  #   
  #   ### Get limits for map area
  #   x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
  #   y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
  #   
  #   ### Filter data for selected flag state(s) and aggregate
  #     eez_plot_data <- pacific_eez_data() %>%
  #       dplyr::filter(flag == input$pacific_flag_state_select_subsidy) %>%
  #       group_by(lon_cen, lat_cen) %>%
  #       summarize(fishing_hours = sum(fishing_hours, na.rm = T),
  #                 fishing_KWh = sum(fishing_KWh, na.rm = T),
  #                 subs = sum(subs, na.rm = T)) %>%
  #       mutate(subsidy_intensity = subs/fishing_KWh)
  #   
  #   # Get data quntiles to set fil scale limit appropriately
  #   intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
  #   scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
  #   
  #   # Map of subsidy intensity
  #   ggplot()+
  #     geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
  #     scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
  #                          limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
  #                          labels=scale_labels,
  #                          breaks=scale_labels,
  #                          oob=scales::squish)+
  #     geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
  #     geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
  #     labs(x = "", y = "")+
  #     coord_sf(xlim = x_lim, ylim = y_lim)+
  #     guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
  #     scale_x_continuous(expand = c(0,0))+
  #     scale_y_continuous(expand = c(0,0))+
  #     eezmaptheme
  #   
  # })
  # 
  # 
  # ### -------------------------
  # ### pacific: effort heat map
  # ### ------------------------
  # 
  # output$pacific_effort_map_all <- renderPlot(bg = "#262626", {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   req(nrow(pacific_eez_data()) > 0)
  #   
  #   ### Get totals for data
  #   eez_totals <- pacific_eez_data() %>%
  #     group_by(lon_cen, lat_cen) %>%
  #     summarize(fishing_hours = sum(fishing_hours, na.rm = T),
  #               fishing_KWh = sum(fishing_KWh, na.rm = T),
  #               subs = sum(subs, na.rm = T)) %>%
  #     mutate(subsidy_intensity = subs/fishing_KWh)
  #   
  #   ### Get limits for map area
  #   x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
  #   y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
  #   
  #   ### Filter data for selected flag state(s) and aggregate
  #   eez_plot_data <- eez_totals
  #     
  #   # Get data quntiles to set fil scale limit appropriately
  #   intensity_quantile <- quantile(eez_plot_data$fishing_KWh/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
  #   scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
  #   
  #   # Map of fishing effort
  #   ggplot()+
  #     geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
  #     scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
  #     geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
  #     geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
  #     labs(x = "", y = "")+
  #     coord_sf(xlim = x_lim, ylim = y_lim) +
  #     guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
  #     scale_x_continuous(expand = c(0,0))+
  #     scale_y_continuous(expand = c(0,0))+
  #     eezmaptheme
  #   
  # })
  # 
  # ### -------------------------
  # ### pacific: effort heat map (Selected flag state)
  # ### ------------------------
  # 
  # output$pacific_effort_map <- renderPlot(bg = "#262626", {
  #   
  #   req(input$pacific_eez_select != "Select a coastal state...")
  #   req(nrow(pacific_eez_data()) > 0)
  #   req(input$pacific_flag_state_select_effort != "Select a flag state...")
  #   
  #   ### Get totals for data
  #   # eez_totals <- pacific_eez_data() %>%
  #   #   mutate(lon_cen = ifelse(lon_cen < 0, 360 + lon_cen, lon_cen)) %>%
  #   #   group_by(lon_cen, lat_cen) %>%
  #   #   summarize(fishing_hours = sum(fishing_hours, na.rm = T),
  #   #             fishing_KWh = sum(fishing_KWh, na.rm = T),
  #   #             subs = sum(subs, na.rm = T)) %>%
  #   #   mutate(subsidy_intensity = subs/fishing_KWh)
  #   
  #   ### Get limits for map area
  #   x_lim <- c(pacific_eez_bbox()$xmin - 0.5, pacific_eez_bbox()$xmax + 0.5)
  #   y_lim <- c(pacific_eez_bbox()$ymin - 0.5, pacific_eez_bbox()$ymax + 0.5)
  #   
  #   ### Filter data for selected flag state(s) and aggregate
  #   eez_plot_data <- pacific_eez_data() %>%
  #       dplyr::filter(flag == input$pacific_flag_state_select_effort) %>%
  #       group_by(lon_cen, lat_cen) %>%
  #       summarize(fishing_hours = sum(fishing_hours, na.rm = T),
  #                 fishing_KWh = sum(fishing_KWh, na.rm = T),
  #                 subs = sum(subs, na.rm = T)) %>%
  #       mutate(subsidy_intensity = subs/fishing_KWh)
  #     
  #   # Get data quntiles to set fil scale limit appropriately
  #   intensity_quantile <- quantile(eez_plot_data$fishing_KWh/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
  #   scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
  #   
  #   # Map of fishing effort
  #   ggplot()+
  #     geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
  #     scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
  #     geom_sf(data = eez_map, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
  #     geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
  #     labs(x = "", y = "")+
  #     coord_sf(xlim = x_lim, ylim = y_lim) +
  #     guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
  #     scale_x_continuous(expand = c(0,0))+
  #     scale_y_continuous(expand = c(0,0))+
  #     eezmaptheme
  #   
  # })
  
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
  
})

### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

