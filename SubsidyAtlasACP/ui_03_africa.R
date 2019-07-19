### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------


### Function
africa = function(eez_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",

    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "padding: 0; margin: 0;", "Africa")
           
    ), 
    
    # Map and widget to select a country 
    column(6, stlye = "padding: 0px;",
           
           column(12, style = "padding: 0 10px;",
                  
                  selectizeInput("africa_eez_select",
                                 label = NULL,
                                 choices = c("Select an EEZ...", eez_choices),
                                 selected = "Select an EEZ...",
                                 width = "100%",
                                 options = list(placeholder = 'Select...'))
                  
           ),
           
           column(12, style = "padding: 0",
                  
                  leafletOutput("africa_map", width = "auto", height = "40vh")
                  
           )
           
    ),
    
    #   ## Leaflet map of ACP EEZs
    conditionalPanel(
      condition = "input.africa_eez_select.length > 0",
      #  
      column(12, stlye = "padding: 0px;",
             leafletOutput("africa_connection_map", width = "auto", height = "80vh")
      ))
    

    # fluidRow(style = "padding: 0px 25px 0px;",
    #          column(6, style = "padding: 0 5px 0 20px;",
    #                 
    #          ))
    
    
  ) # close fluidPage

