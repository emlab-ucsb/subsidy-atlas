### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 2: Function

caribbean = function()
  fluidPage(
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "Caribbean")
    ),
    
    # Leaflet overview map
    column(12, stlye = "padding: 0px;",
           leafletOutput("caribbean_map", width = "auto", height = "60vh")
    )
    
    
  ) # close fluidPage