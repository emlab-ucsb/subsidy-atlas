### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 1: Function

africa = function()
  fluidPage(
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "Africa")
    ), 
    
    # Select an EEZ - map and dropdown
    
    column(6, stlye = "padding: 0px;",
           
           leafletOutput("africa_map", width = "auto", height = "40vh"),
           
           selectizeInput()
           )
    
    
  ) # close fluidPage

