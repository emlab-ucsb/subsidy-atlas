### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 1: Introduction/Select a region
# 
### ------------------------------------

### Function
selectregion = function(){
  
  fluidPage(
            
    ### Leaflet map with selectable regions
    column(12, stlye = "padding: 0px;",
           
           # Map
           leafletOutput("regional_map", width = "auto", height = "90vh"),
           
           # Text panel
           # Button to hide instructions 
           absolutePanel(id = "regional_map_hide_text_arrow",
                                         
                                         tags$button(id = "ab_regional_map_hide_text",
                                                     class = "btn action-button",
                                                     "Instructions", icon("caret-down"))
           ),
           
           # Button to show instructions
           shinyjs::hidden(absolutePanel(id = "regional_map_expand_text_arrow",
                         
                         tags$button(id = "ab_regional_map_expand_text",
                                     class = "btn action-button",
                                     "Instructions", icon("caret-up"))
           )),
           
           # Instructions
           absolutePanel(id = "regional_map_text_panel",
                                         
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                tags$p("Select a region by clicking on the map or using the links in the left panel to view more information about distant water fishing activity in that area. Please be patient if nothing is visible – the map may take a couple of seconds to load.")
                                #includeHTML("./text/intro_overlay.html")
                                                
                         )
                                         
           )
           
    )
  
  ) # close fluidPage
}
