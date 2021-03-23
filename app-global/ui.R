### ----------------------------------------------------------------------
# 
# Distant Water Fishing Atlas - Server Logic
# This app allows users to visualize distant-water fishing effort in the EEZs of ACP countries
#
### ----------------------------------------------------------------------

### ----------
### Initialize app
### ----------

rm(list = ls())
set.seed(123)

dir.create('~/.fonts')
file.copy("www/Avenir-Book.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

### Load packages -----

library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyBS)
library(shinyjs)
library(shinyalert)

library(tidyverse)
library(sf)
library(png)
library(htmltools)
library(DT)

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

# Silence new dplyr grouping messages
options(dplyr.summarise.inform=F)

### Source files -----

# Data is loaded and wrangled in a separate script
source("00_initialize_app.R")

# The user interface content for each tab is stored in a separate file - Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)

# The server functions are stored in separate files - Source all .R files in the functions directory:  
sapply(list.files(
  path = "./functions/",
  full.names = TRUE
),
source)
  
### ---------
### UI ------
### ---------

shinyUI(
  dashboardPage(
    
    # Header bar
    dashboardHeader(
      
      # Title - none for this application
      title = "",
      
      # Title width - none for this application
      titleWidth = "0%",
      
      # Title
      tags$li(
        id = "custom-title-container",
        class = "dropdown",
        tags$button(id = "ab_home",
                    tags$h3("Distant Water Fishing Atlas",
                            style = "margin: 0;"),
                    class = "btn action-button") # /h3
      ), # /tags$li

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
                                menuItem("Select a Region",
                                         tabName = "selectregion",
                                         icon = NULL,
                                         selected = TRUE),
                                
                                # East Asia & Pacific 
                                menuItem("East Asia & Pacific", 
                                         tabName = "east-asia-pacific", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Europe & Central Asia 
                                menuItem("Europe & Central Asia", 
                                         tabName = "europe-central-asia", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Latin America & Caribbean
                                menuItem("Latin America & Caribbean", 
                                         tabName = "latin-america-caribbean", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Middle East & North Africa 
                                menuItem("Middle East & North Africa", 
                                         tabName = "middle-east-north-africa", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # North America 
                                menuItem("North America", 
                                         tabName = "north-america", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # South Asia 
                                menuItem("South Asia", 
                                         tabName = "south-asia", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Sub-Saharan Africa 
                                menuItem("Sub-Saharan Africa", 
                                         tabName = "sub-saharan-africa", 
                                         icon = NULL,
                                         selected = NULL)
                                

                    ) # close sidebarMenu
                    
   ), # close dashboardSidebar
   
   # Main panel
   dashboardBody(
     
     # Custom stylesheet
     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom_global_atlas.css")),
     
     # Allow js functionality
     useShinyjs(),
     
     # Allow for popups
     useShinyalert(),
     
     # Tabs
     tabItems(
       
       # Select a region
       tabItem(tabName = "selectregion",
               selectregion()
       ),
       
       # East Asia & Pacific
       tabItem(tabName = "east-asia-pacific",
               EastAsiaPacific(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       ),
       
       # Europe & Central Asia
       tabItem(tabName = "europe-central-asia",
               EuropeCentralAsia(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       ),
       
       # Latin America & Caribbean
       tabItem(tabName = "latin-america-caribbean",
               LatinAmericaCaribbean(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       ),
       
       # Middle East & North Africa
       tabItem(tabName = "middle-east-north-africa",
               MiddleEastNorthAfrica(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       ),
       
       # North America
       tabItem(tabName = "north-america",
               NorthAmerica(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       ),
       
       # South Asisa
       tabItem(tabName = "south-asia",
               SouthAsia(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       ),
       
       # South Asisa
       tabItem(tabName = "sub-saharan-africa",
               SubSaharanAfrica(vessel_origins_fill_choices, vessel_origins_fill_scale, flag_state_choices)
       )
       
     ) # close tabItems
   ) # close dashboardBody
   
  ) # close dashboardPage
) # close shinyUI
       