### -----------------------------------
# User interface - Leaflet Connectivity Map
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 1: Function

# leaflet_EEZ = function(country_choices)
#    fluidPage(
#      
#       # Top header
#       column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
#              tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "leaflet_EEZ")
#      ), 
#      
#      fluidRow()
#      column(12, style = "padding: 0px 25px; color: #ffffff;",
#             selectizeInput("EEZ_for_profile",
#                            label = "EEZ:",
#                            choices = country_choices,
#                            selected = NULL,
#                            width = "100%",
#                            options = list(placeholder = 'Select...'))
#      ),
#       
#       # # Leaflet overview map
#       # column(12, stlye = "padding: 0px;",
#       #        leafletOutput("leaflet_EEZ", width = "auto", height = "80vh")
#       # )
#       
#      
#    ) # close fluidPage


