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
           # Button to hide disclaimer 
           absolutePanel(id = "regional_map_hide_text_arrow",
                                         
                                         tags$button(id = "ab_regional_map_hide_text",
                                                     class = "btn action-button",
                                                     icon("caret-down"))
           ),
           
           # Button to open left panel
           shinyjs::hidden(absolutePanel(id = "regional_map_expand_text_arrow",
                         
                         tags$button(id = "ab_regional_map_expand_text",
                                     class = "btn action-button",
                                     icon("caret-up"))
           )),
           
           absolutePanel(id = "regional_map_text_panel",
                                         
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                includeHTML("./text/intro_overlay.html")
                                                
                         )
                                         
           )
           
    )
  
  ) # close fluidPage
}
