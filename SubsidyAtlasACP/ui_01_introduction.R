### -----------------------------------
# User interface - Introduction
#
# This script controls the layout of the user interface for the introduction tab
# 
### ------------------------------------

introduction = function()
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
    
    ### Header and introductory text
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           # Header
           tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "Global atlas of distant water fishing"),
           
           br(),
           
           # Text
           tags$p("Explain distant water fishing, etc. etc.")
    ),
    
  
    ### Connectivity Map
    column(12, style = "padding: 0;",
      img(src = "_ACP_connections_map.png", width = "120%", align = "center")
    )
            
            
  ) # /fluidPage

