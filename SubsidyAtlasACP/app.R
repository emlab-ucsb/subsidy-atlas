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

### Source files and data
# The content for each tab is stored in a separate file - Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)

# The data for each region was saved separately to try to make the file sizes smaller. Reach in each of those here. 
# sapply(list.files(
#   pattern = "[.]csv$",
#   path = "./data/",
#   full.names = TRUE
# ),
# read_csv)

# Load csv of ACP eez's

ACP_codes <- read_csv("./ACP_eez_codes.csv") %>%
  na.omit()

#connectivity_data <- read_csv("./data/ACP_eez_results/ACP_eez_mapping_wlines.csv")

concectivity_data <- read_sf("./data/ACP_eez_results/ACP_eez_mapping_wlines.shp") %>% 
  rename(eez_territory_iso3 = ez_tr_3) 


# Load shapefiles
eez_fao <- read_sf(dsn = "./data/eez_v10_fao_combined_simple", layer="eez_v10_fao_combined_simple") %>%
  dplyr::filter(is.na(zone))
eez_codes <- unique(eez_fao$mrgid)

world <- read_sf(dsn = "./data/world_happy_180/world_happy_180.shp", layer="world_happy_180")

## Load maps

# EEZ map
EEZ_map <- read_sf(dsn = here::here("SubsidyAtlasACP", "data", "eez_v10_fao_combined_simple"), layer = "eez_v10_fao_combined_simple") %>% 
  st_transform(crs = 4326) 

# Country map
country_map <- read_sf(dsn = here::here("SubsidyAtlasACP", "data", "world_happy_180"), layer = "world_happy_180") %>%  
  st_transform(crs = 4326) 

### Widget choice values that depend on a dataset ------------------------------
# Put this here so we only have to load datasets in one place
country_choices <- unique(ACP_codes$territory_iso3)
names(country_choices) <- unique(ACP_codes$flag)

ACP_choices <- unique(ACP_codes$territory_iso3)
names(ACP_choices) <- unique(ACP_codes$flag)
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
                    )
                    ), 
                    
   # Sidebar menu
   dashboardSidebar(width = "250px",
                    sidebarMenu(id = "tabs",
                                
                                # Introduction                     
                                menuItem("Background", 
                                         tabName = "background", 
                                         icon = NULL,
                                         selected = TRUE),
                                
                                #Leaflet
                                menuItem("Leaflet EEZ",
                                         tabName = 'leaflet_EEZ',
                                         icon = NULL, 
                                         selected = TRUE),
                                
                                
                                # EEZ
                                menuItem("EEZ",
                                         tabName = "EEZ",
                                         icon = NULL,
                                         selected = TRUE),
                                                                 
                                # Africa
                                menuItem("Africa", 
                                         tabName = "africa", 
                                         icon = NULL,
                                         selected = TRUE),
                                
                                # Caribbean
                                menuItem("Caribbean", 
                                         tabName = "caribbean", 
                                         icon = NULL,
                                         selected = TRUE),
                                
                                # Pacific Islands
                                menuItem("Pacific Islands", 
                                         tabName = "pacific_islands", 
                                         icon = NULL,
                                         selected = TRUE)
                                
                                # # Europe
                                # menuItem("Europe", 
                                #          tabName = "europe", 
                                #          icon = NULL,
                                #          selected = TRUE),
                                # 
                                # # North America and the Caribbean 
                                # menuItem("North America &\n the Caribbean", 
                                #          tabName = "north_america", 
                                #          icon = NULL,
                                #          selected = TRUE),
                                # 
                                # # South America
                                # menuItem("South America", 
                                #          tabName = "south_america", 
                                #          icon = NULL,
                                #          selected = TRUE),
                                # 
                                
                                
                                
                                
                    ) # close sidebarMenu
                    
   ), # close dashboardSidebar
   
   # Main panel
   dashboardBody(
     
     # Custom stylesheet
     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ACP_custom.css")),
     
     # Tabs
     tabItems(
       
       # Background
       tabItem(tabName = "background",
               introduction()
       ),
       
       #Leaflet EEZ
       tabItem(tabName = "leaflet_EEZ",
               leaflet_EEZ(ACP_choices)
       ),
       
       #EEZ Connectivity
       tabItem(tabName = "EEZ",
               EEZ(country_choices)
        ),
       
       
       # Africa
       tabItem(tabName = "africa",
               africa()
       ),
       
       # 
       # North America
       tabItem(tabName = "caribbean",
               caribbean()
       ),
       
       # Pacific Islands
       tabItem(tabName = "pacific_islands",
               pacific()
       )#,
       
       # # Asia
       # tabItem(tabName = "asia"
       #         #asia()
       # ),
       # 
       # # Europe
       # tabItem(tabName = "europe"
       #         #europe()
       # ),
       
       # # South America
       # tabItem(tabName = "south_america"
       #         #south_america()
       # ),
       # 
       
     ) # close tabItems
   ) # close dashboardBody
   
  ) # close dashboardPage
) # close shinyUI
       

### ----------
### Section 3: Server
### ----------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### Background ----------
  
  
  ### Leaflet Version of EEZ Connectivity Map
  
  
  
  ACP_codes %>% 
    select(eez_id)
  
  output$leaflet_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$ACP_for_profile)
    
    #filter data
    
    # ACP_codes_filtered <- connectivity_data %>% # load this in up above
    #   dplyr::filter(eez_territory_iso3 == input$ACP_for_profile)
    
     ACP_codes <- eez_fao %>% 
       dplyr::filter(mrgid %in% ACP_codes$eez_id)
     
     ACP_codes_filtered <- ACP_codes %>% 
       dplyr::filter(ez_hs_c == input$ACP_for_profile)
     
     
     country_map_filtered <- country_map %>% 
       dplyr::filter(iso3 == input$ACP_for_profile)
    
    # connectivity_data_filter <- connectivity_data %>% # load this in up above
    #   dplyr::filter(eez_territory_iso3 == input$EEZ_for_profile)
    
    leaflet('leaflet_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = ACP_codes_filtered, 
                  fillColor = "blue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", ACP_codes$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
      addPolygons(data = country_map_filtered,
                  fillColor = "red",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", country_map$cntry_l, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto"))
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
      geom_sf(data = EEZ_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.1)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = country_map, fill = "grey2", color = "grey40", size = 0.1)+ # world countries (dark grey, white border lines)
      geom_sf(data = country_map %>% dplyr::filter(iso3 %in% connectivity_data_filter$flag), fill = "darkmagenta", alpha = 0.5, color = NA, size = 0.1) + # highlighted flag states (magenta)
      geom_sf(data = EEZ_map %>% dplyr::filter(ez_hs_c == input$EEZ_for_profile), fill = "slateblue", color = "grey40", size = 0.1) + # highlighted EEZ (slateblue, grey border lines)
      geom_sf(col = "darkgoldenrod", size = 0.25) +
      maptheme+
      coord_sf(xlim = c(-180,180), ylim = c(-90,90))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
    
    #%>% dplyr::filter(eez_codes == input$EEZ_for_profile)
    
 ####Need to make countries and lines reactive   
    
    
  })
  
  
  
  
  ### Africa ----------
  
  africa_eezs <- ACP_codes %>% 
    dplyr::filter(region == "Africa") %>% 
    select(eez_id)
    
  
  
  ### Map of African countries/EEZs highlighted for which we have DW fishing effort
  output$africa_map <- renderLeaflet({
    
    # Filter data
    africa_eezs <- eez_fao %>%
      dplyr::filter(mrgid %in% africa_eezs$eez_id)
    
    # Map
    leaflet('africa_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = africa_eezs, 
                  fillColor = "blue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", africa_eezs$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
                  ) %>%
      setView(20,-2, zoom = 3)
      # addLegend("bottomright", pal = pal_global,
      #           values = log10(tot_subs$value),
      #           labels = round(tot_subs$value, 0),
      #           title = "Est. fisheries subsidies<br>(2018 $USD)",
      #           opacity = 1,
      #           labFormat = labelFormat(prefix = "$",
      #                                   transform = function(x) 10^(x)
      #           )
      #)
    
  })
  
  
  ### Caribbean ----------
  
  caribbean_eezs <- ACP_codes %>% 
    dplyr::filter(region == "Caribbean") %>% 
    select(eez_id)
  
  
  
  ### Map of Caribbean countries/EEZs highlighted for which we have DW fishing effort
  output$caribbean_map <- renderLeaflet({
    
    # Filter data
    caribbean_eezs <- eez_fao %>%
      dplyr::filter(mrgid %in% caribbean_eezs$eez_id)
    
    # Map
    leaflet('caribbean_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = caribbean_eezs, 
                  fillColor = "blue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", caribbean_eezs$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(-75,20, zoom = 4)
    # addLegend("bottomright", pal = pal_global,
    #           values = log10(tot_subs$value),
    #           labels = round(tot_subs$value, 0),
    #           title = "Est. fisheries subsidies<br>(2018 $USD)",
    #           opacity = 1,
    #           labFormat = labelFormat(prefix = "$",
    #                                   transform = function(x) 10^(x)
    #           )
    #)
    
  })
  
  
  ### Pacific Islands --------
  
  pacific_eezs <- ACP_codes %>% 
    dplyr::filter(region == "Pacific") %>% 
    select(eez_id)
  
  
  ### Map of Pacific countries/EEZs highlighted for which we have DW fishing effort
  output$pacific_map <- renderLeaflet({
    
    # Filter data
    pacific_eezs <- eez_fao %>%
      dplyr::filter(mrgid %in% pacific_eezs$eez_id)
    
    # Map
    leaflet('pacific_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = pacific_eezs, 
                  fillColor = "blue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", pacific_eezs$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(-75,20, zoom = 4)
    # addLegend("bottomright", pal = pal_global,
    #           values = log10(tot_subs$value),
    #           labels = round(tot_subs$value, 0),
    #           title = "Est. fisheries subsidies<br>(2018 $USD)",
    #           opacity = 1,
    #           labFormat = labelFormat(prefix = "$",
    #                                   transform = function(x) 10^(x)
    #           )
    #)
    
  })
  
  
  
  
}

### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

