### -----------------------------------
# User interface - Introduction
#
# This script controls the layout of the user interface for the introduction tab
# 
### ------------------------------------

introduction = function()
  fluidPage(style = "color: #ffffff;",
            
            # Picture container
            tags$div(class = "picture-container", style = "height:60%; border-bottom: 4px solid #3c8dbc;",
                     tags$img(class = "picture-full", src = "fishing_fleet.jpg"),
                     tags$div(class = "overlay-black",
                              
                              # Title and subtitle text
                              tags$div(class = "overlay-center-text",
                                       tags$h1("SubsidyExplorer"),
                                       tags$h4("An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines")
                                       
                              ) # /div overlay-center-text
                              
                              
                     ) #/div overlay-black
            ), # /div picture-container
            
            # Text
            column(12, style = "padding: 20px 25px 15px;",
                   
                   #source file with text for introduction page
                   includeHTML("./text/01_intro_paragraph_atlas.html"))
    
    # ### Header and introductory text
    # column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
    #        
    #        # Header
    #        tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "Global atlas of distant water fishing"),
    #        
    #        br(),
    #        
    #        # Text
    #        tags$p("Explain distant water fishing, etc. etc.")
    # )#,
    
  
    # ### Connectivity Map
    # column(12, style = "padding: 0;",
    #   img(src = "intro-background.jpeg", width = "100%", align = "center")
    # )
            
            
  ) # /fluidPage

