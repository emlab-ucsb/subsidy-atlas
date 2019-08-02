### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Function
africa = function(africa_eez_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
            
            tags$style("
                                
                                .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                                background-color: transparent;
                                border-color: transparent;
                                }
                                
                                .nav-tabs-custom .nav-tabs li.active {
                                border-top-color: #FFF;
                                }"),
            

    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "padding: 0; margin: 0;", "Africa")
           
           #br(),
           
           # # Text
           # includeHTML("./text/03_africa_intro.html")
           
    ), 
  
  fluidRow(  
    # Map and widget to select a country 
    column(6, style = "padding: 0px;",
           
           column(12, style = "padding: 0 10px;",
                  
                  selectizeInput("africa_eez_select",
                                 label = NULL,
                                 choices = c("Select an EEZ...", africa_eez_choices),
                                 selected = "Select an EEZ...",
                                 width = "100%",
                                 options = list(placeholder = 'Select...')),
                  
                  leafletOutput("africa_map", width = "auto", height = "40vh")
           )
                  
           
    ), # close column
    
    column(6, style = "padding: 0px",
           
           uiOutput("africa_summary_text")
           
           ) #close column
    
    
  ), #close fluid row 
  
  ### Tab box with results
  
  tabBox(width = 12, id = "africa_tabs", 
         
         
         # First tab - connectivity map
         tabPanel("Origins of distant water fishing vessels", 
                  
                  column(12, style = "padding: 15px 0;",
                         
                         leafletOutput("africa_connection_map",
                                       width = "95%",
                                       height = "50vh")
                  ),
                  
                fluidRow(
                  
                  column(6, style = "padding: 15px 25px; background-color: #262626;",
                         
                         plotOutput("africa_flag_subsidy_map",
                                    width= "auto")
                         ),
                  
                  column(6, style = "padding: 15px 25px; background-color: #262626;",

                         plotOutput("africa_flag_effort_map",
                                    width= "auto")
                  )#close column
               )#close fluid row
            ), #close tab panel
         
         # Second tab - heat maps   
         tabPanel("Fishing effort and subsidy intensity of distant water vessels",
                  fluidRow(
                    
                    column(6, style = "padding: 15px 25px; background-color: #262626;",
                           
                           plotOutput("africa_subsidy_map", 
                                      width = "auto")
                           
                    ),
                    
                    column(6, style = "padding: 15px 25px; background-color: #262626;",
                           
                           plotOutput("africa_effort_map",
                                      width = "auto")
                           
                    )   
                  ) #/fluidRow
         ) #/tabPanel #2
         
  ) # /tabBox
  
  ) #close fluid page