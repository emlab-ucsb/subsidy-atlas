### -----------------------------------
#
# UI - Select a region
#https://shiny.rstudio.com/articles/layout-guide.html
# 
### ------------------------------------

### Function
selectregion = function()
  
  # navbarPage("Navbar!",
  #           tabPanel("Select region"),
  #           tabPanel("Africa"),
  #           tabPanel("Caribbean"),
  #           tabPanel("Pacific"))
            
  fluidPage(style = "color: #ffffff;",
            
            # # Picture container
            # tags$div(class = "picture-container", style = "height:60%; border-bottom: 4px solid #3c8dbc;",
            #          
            #          tags$div(class = "overlay-black",
            #                   
            #                   # Title and subtitle text
            #                   tags$div(class = "overlay-center-text",
            #                            tags$h1("Subsidy Atlas"),
            #                            tags$h4("An interactive global atlas to explore the extent of distant water fishing in ACP country states")
            #                            
            #                   ) # /div overlay-center-text
            #                   
            #                   
            #          ),
            #          tags$img(class = "picture-full", src = "fishing_fleet.jpg")#/div overlay-black
            # ), # /div pictu
    
    ### Header and introductory text
    # column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
    #        
    #        # Title
    #        tags$h3(style = "padding: 0; margin: 0;", "Select a region"),
    #        
    #        br(),
    #        
    #        # Text
    #        includeHTML("./text/02_selectregion_intro.html")
    # ), 
    
    ### Leaflet regions map
    column(12, stlye = "padding: 0px;",
           
           leafletOutput("regional_map", width = "auto", height = "60vh")
           
    ),
    
    # Text
    column(12, style = "padding: 20px 25px 15px;",
           
           #source file with text for introduction page
           includeHTML("./text/01_intro_paragraph_atlas.html"))
    
    
  ) # close fluidPage
