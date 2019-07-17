### -----------------------------------
# User interface - Introduction
#
# This script controls the layout of the user interface for the introduction tab
# 
### ------------------------------------

introduction = function()
  fluidPage(
    
    ##Title
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h2(style = "color: #ffffff; padding: 0; margin: 0;", "Subsidy Atlas")
           
           
    ),
    
    
    # Text
    column(12, style = "padding:15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "Maps we made"),
           
           #text
           tags$h4(style = "color: #ffffff;" , "This map shows all of the possible connections between ACP EEZ's and flag states using Global Fishing Watch Data blah blah blah")
    ),
    
    ## Connectivity Map
    mainPanel(
      img(src = "_ACP_connections_map.png", width = "150%", align = "center")
    )
            
            # # Picture container
            # tags$div(class = "picture-container", style = "height:60%; border-bottom: 4px solid #3c8dbc;",
            #          tags$img(class = "picture-full", src = "intro-background.jpeg"),
            #          tags$div(class = "overlay-black",
            #                   
            #                   # Title and subtitle text
            #                   tags$div(class = "overlay-center-text",
            #                            tags$h1("SubsidyExplorer"),
            #                            tags$h4("An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines")
            #                            
            #                   ) # /div overlay-center-text
            #                   
            #                   
            #          ) #/div overlay-black
            # ) # /div picture-container
            
            # Text 
            
            
  ) # /fluidPage

