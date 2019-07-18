### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------


### Section 1: Function



### Function
africa = function(ACP_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",

    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "padding: 0; margin: 0;", "Africa")
           
    ), 
    
    # Leaflet Map
    
    column(6, stlye = "padding: 0px;",
           
           leafletOutput("africa_map", width = "auto", height = "40vh")
           
           #selectizeInput()
           ),
    
    ##Drop down menu
    
    fluidRow(style = "padding: 0px 25px 0px;",
             column(6, style = "padding: 0 5px 0 20px;",
                    selectizeInput("Africa_for_profile",
                                   label = "EEZ:",
                                   choices = ACP_choices,
                                   selected = NULL,
                                   width = "100%",
                                   options = list(placeholder = 'Select...'))
             ))
    
    
  ) # close fluidPage

