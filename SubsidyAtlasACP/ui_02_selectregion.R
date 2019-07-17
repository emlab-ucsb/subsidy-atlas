### -----------------------------------
#
# UI - Select a region
# 
### ------------------------------------

### Function
selectregion = function()
  fluidPage(
    
    # Header and introductory text
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "ACP Regions")
    ), 
    
    # Leaflet overview map
    column(12, stlye = "padding: 0px;",
           leafletOutput("regional_map", width = "auto", height = "60vh")
    )
    
    
  ) # close fluidPage
