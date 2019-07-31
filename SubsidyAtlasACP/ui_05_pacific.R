### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 2: Function

pacific = function(pacific_eez_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
            
            # Top header
            column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
                   
                   tags$h3(style = "padding: 0; margin: 0;", "Pacific"),
                   
                   br(),
                   
                   # Text
                   includeHTML("./text/05_pacific_intro.html")
    ),
    
    fluidRow(
        # Map and widget to select a country 
        column(6, style = "padding: 0px;",
               
               column(12, style = "padding: 0 10px;",
                      
                      selectizeInput("pacific_eez_select",
                                     label = NULL,
                                     choices = c("Select an EEZ...", pacific_eez_choices),
                                     selected = "Select an EEZ...",
                                     width = "100%",
                                     options = list(placeholder = 'Select...'))
                      
               ),
               
               column(12, style = "padding: 0",
                      
                      leafletOutput("pacific_map", width = "auto", height = "40vh")
                      
               )
          ), #close column 
        
        column(6, style = "padding: 0px",
               
               uiOutput("pacific_summary_text")
               
          ) #close column
        
        ), #close fluid row
    
    #   ## Leaflet map of ACP EEZs
    
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/05_pacific_connectivity.html")
           
    ),
    #  
    column(12, style = "padding: 0px;",
           
           leafletOutput("pacific_connection_map", width = "auto", height = "80vh")
    ),
    
    ##Heat Maps
    
    fluidRow(
    
        column(6, style = "padding: 15px 25px; background-color: #262626;",
               
               plotOutput("pacific_subsidy_map", width = "100%")
               
        ),
        
        
        column(6, style = "padding: 15px 25px; background-color: #262626;",
               
               plotOutput("pacific_effort_map", width = "100%")
        )      
        ) #close fluid row
    
    
    
  ) #close fluid page