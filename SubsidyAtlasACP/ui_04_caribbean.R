### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Function
caribbean = function(eez_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "padding: 0; margin: 0;", "Caribbean")
           
    ),
    
    # Map and widget to select a country 
    column(6, stlye = "padding: 0px;",
           
           column(12, style = "padding: 0 10px;",
                  
                  selectizeInput("caribbean_eez_select",
                                 label = NULL,
                                 choices = c("Select an EEZ...", eez_choices),
                                 selected = "Select an EEZ...",
                                 width = "100%",
                                 options = list(placeholder = 'Select...'))
                  
           ),
           
           column(12, style = "padding: 0",
                  
                  leafletOutput("caribbean_map", width = "auto", height = "40vh")
                  
           )
           
    )
    
  ) # close fluidPage