### -----------------------------------
#
# UI - Select a region
# 
### ------------------------------------

### Function
selectregion = function()
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
    
    ### Header and introductory text
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           # Title
           tags$h3(style = "padding: 0; margin: 0;", "Select a region"),
           
           br(),
           
           # Text
           includeHTML("./text/02_selectregion_intro.html")
    ), 
    
    ### Leaflet regions map
    column(12, stlye = "padding: 0px;",
           
           leafletOutput("regional_map", width = "auto", height = "60vh")
           
    )
    
    
  ) # close fluidPage
