### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 1: Introduction/Select a region
# 
### ------------------------------------

### Function
selectregion = function(){
  
  fluidPage(style = "color: #ffffff;",
            
    ### Leaflet map with selectable regions
    column(12, stlye = "padding: 0px;",
           
           leafletOutput("regional_map", width = "auto", height = "90vh")
           
    )
  
  ) # close fluidPage
}
