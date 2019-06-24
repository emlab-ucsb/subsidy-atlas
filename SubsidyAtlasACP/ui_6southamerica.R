### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 2: Function

south_america = function()
  fluidPage(
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #000000; padding: 0; margin: 0;", "South America")
    )
    
    
  ) # close fluidPage