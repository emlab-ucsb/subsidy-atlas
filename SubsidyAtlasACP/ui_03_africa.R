### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Function
africa = function(africa_eez_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",

    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "padding: 0; margin: 0;", "Africa"),
           
           br(),
           
           # Text
           includeHTML("./text/03_africa_intro.html")
           
    ), 
    
    # Map and widget to select a country 
    column(6, stlye = "padding: 0px;",
           
           column(12, style = "padding: 0 10px;",
                  
                  selectizeInput("africa_eez_select",
                                 label = NULL,
                                 choices = c("Select an EEZ...", africa_eez_choices),
                                 selected = "Select an EEZ...",
                                 width = "100%",
                                 options = list(placeholder = 'Select...'))
                  
           ),
           
           column(12, style = "padding: 0",
                  
                  leafletOutput("africa_map", width = "auto", height = "40vh")
           )
                  
           
    ), # close column
    
          
    
    #   ## Leaflet map of ACP EEZs
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/03_africa_connectivity.html")
           
           ),
      #  
      column(12, style = "padding: 0px;",
             
             leafletOutput("africa_connection_map", width = "auto", height = "80vh")
      ),
    
    
      # column(12, style = "padding: 10px 25px 0px;",
      #        
      #        plotOutput("africa_heat_map", width = "auto")
      #        
      # ),
    

      column(12, style = "padding: 15px 25px; background-color: #262626;",
             
             plotOutput("africa_subsidy_map", width = "100%")
             
      ),
    
    
      column(12, style = "padding: 15px 25px; background-color: #262626;",
             
             plotOutput("africa_effort_map", width = "100%")
             
      ),
    
    # column(12, style = "padding: 0px;",
    # 
    #        imageOutput("africa_heat_map", width = "100", height = "100")
    # 
    #        ),
  
    
    
    
    fluidRow(uiOutput("africa_summary_text")
           
           
             
             
             
    )
    
    
    
  
  
 ) # close fluidpage

