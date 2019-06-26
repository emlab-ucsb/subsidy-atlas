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

# Load csz of ACP eez's

ACP_codes <- read_csv("ACP_eez_codes.csv")

# Load shapefiles
eez_fao <- read_sf(dsn = "./data/eez_v10_fao_combined_simple", layer="eez_v10_fao_combined_simple") %>%
  dplyr::filter(is.na(zone))
eez_codes <- unique(eez_fao$mrgid)

world <- read_sf(dsn = "./data/world_happy_180/world_happy_180.shp", layer="world_happy_180") 

### Widget choice values that depend on a dataset ------------------------------
# Put this here so we only have to load datasets in one place 
country_choices <- c(unique(ACP_codes$flag))
names(country_choices) <- unique(ACP_codes$flag)
### Widget choice values that are text heavy  ------------------------------
# Put this here so it's easier to edit text throughout

### ----------
### Section 2: UI
### ----------

ui <- shinyUI(
  dashboardPage(
    
    # Header bar
    dashboardHeader(title = "Global Atlas of Distant Water Fishing",
                    titleWidth = "350px"), # Hide title next to the menu button
                    
   # Sidebar menu
   dashboardSidebar(width = "250px",
                    sidebarMenu(id = "tabs",
                                
                                # Introduction                     
                                menuItem("Background", 
                                         tabName = "background", 
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
                                
                                # Asia
                                menuItem("Caribbean", 
                                         tabName = "caribbean", 
                                         icon = NULL,
                                         selected = TRUE),
                                
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
                                # Pacific Islands
                                menuItem("Pacific Islands", 
                                         tabName = "pacific_islands", 
                                         icon = NULL,
                                         selected = TRUE)
                                
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
       
       #EEZ Connectivity
       tabItem(tabName = "EEZ",
               EEZ()
        ),
       
       # Africa
       tabItem(tabName = "africa",
               africa()
       ),
       
       # # Asia
       # tabItem(tabName = "asia"
       #         #asia()
       # ),
       # 
       # # Europe
       # tabItem(tabName = "europe"
       #         #europe()
       # ),
       # 
       # North America
       tabItem(tabName = "caribbean",
               caribbean()
       ),
       
       # # South America
       # tabItem(tabName = "south_america"
       #         #south_america()
       # ),
       # 
       # Pacific Islands
       tabItem(tabName = "pacific_islands",
               pacific()
       )#,
       
       ## EEZ Connectivity maps
      # tabitem(tabName = "EEZ_Conectivity",
               #EEZConnectivity()
      #)
       
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
  
  
  ### EEZ Connectivity Maps ---------
  
  
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

