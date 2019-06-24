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

# Load shapefiles
eez_fao <- read_sf(dsn = "./data/eez_v10_fao_combined_simple", layer="eez_v10_fao_combined_simple") %>%
  dplyr::filter(is.na(zone))
eez_codes <- unique(eez_fao$mrgid)

world <- read_sf(dsn = "./data/world_happy_180/world_happy_180.shp", layer="world_happy_180") 

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
       tabItem(tabName = "pacific_islands"
               #pacific_islands()
       )
       
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
  
  
  
  
  ### Africa ----------
  
  africa_eezs <- c(8379, # Ascension
                   8380, # St. Helena
                   8382, # Tristan Da Cunha
                   8334, # Comoros
                   8348, # Madagascar
                   8396, # South Africa
                   8343, # Mauritius
                   8395, # Namibia
                   8394, # Congo
                   8478, # Angola
                   8347, # Mozambique
                   8349, # Kenya
                   8384, # Prince Edward Islands (South Africa)
                   8479, # Tanzania
                   8362, # Cape Verde
                   8370, # Gambia
                   8390, # Sierra Leone
                   8369, # Mauritania
                   48964, # Joint regime: Senegal/Guinea Bissau
                   8371, # Senegal
                   8471, # Guinea Bissau
                   8472, # Guinea
                   8473, # Ivory coast
                   8391, # Liberia
                   48956, # Overlapping claim: Djibouti/?
                   8490, # Egypt
                   8351, # Eritrea
                   8372, # Libya
                   8355, # Sudan
                   8392, # Togo
                   8366, # Tunisia
                   8352, # Djibouti
                   8474, # Nigeria
                   8475, # Cameroon
                   8397, # Sao Tome and Principe
                   21797 # Joint regime
  )
  
  ### Map of African countries/EEZs highlighted for which we have DW fishing effort
  output$africa_map <- renderLeaflet({
    
    # Filter data
    africa_eezs <- eez_fao %>%
      dplyr::filter(mrgid %in% africa_eezs)
    
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
  
  caribbean_eezs <- c(8412, # Anguilla
                      8414, # Antigua and Barbuda
                      26519, # Aruba
                      8404, # Bahamas
                      8418, # Barbados
                      8429, # Mexico
                      8402, # Bermuda
                      26520, # Bonaire
                      8411, # BVI
                      8407, # Cayman Islands
                      26517, # Curacao
                      8417, # Dominica
                      8409, # Dominican Republic
                      8419, # Grenada
                      33177, # Guadeloupe
                      8459, # Jamaica
                      8415, # Montserrat
                      33179, # Puerto Rico
                      26518, # Saba
                      48952, # Saint Bartholemy
                      26526, # Sint Eustatius
                      8413, # Saint Kitts and Nevis
                      8416, # Saint Lucia
                      8495, # Saint Martin
                      8421, # Saint Vincent and the Grenadines
                      21803, # Sint Maarten
                      8420, # Trinidad and Tobago
                      8405, # Turks and Caicos
                      33180, # USVI
                      8406, # Cuba
                      8408, # Haiti
                      33178, # Martinique
                      48969, # Joint regime: Guyana/Barbados
                      48970, # Joint regime: Colombia/Dominican Republic
                      8424, # Costa Rica
                      8430, # Guatemala
                      8428, # El Salvador
                      8457, # Belize
                      48968, # Galapagos 
                      8425, # Nicaragua
                      8460, # Guyana
                      8431, # Ecuador
                      48985, # Colombia
                      8403, # Ecuador (galapagos)
                      48984, # colombia
                      33184, # Colombia
                      33183, # colombia
                      8426, # colombia
                      8456, # United States
                      8381, # Brazil
                      8464, # Brazil
                      48982 # overlapping claim
                      
  )
  
  ### Map of Caribbean countries/EEZs highlighted for which we have DW fishing effort
  output$caribbean_map <- renderLeaflet({
    
    # Filter data
    caribbean_eezs <- eez_fao %>%
      dplyr::filter(mrgid %in% caribbean_eezs)
    
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
  
   
   
}

### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

