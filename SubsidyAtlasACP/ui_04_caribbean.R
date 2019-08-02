### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Function
caribbean = function(caribbean_eez_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
            
            # Tabs custom styling - for some reason this has to be here
            tags$style(
".nav-tabs {
background: black;
}

.nav-tabs-custom .nav-tabs {
border-bottom-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
color: #ffffff;
background: #1A1B1D;
}

.nav-tabs-custom .nav-tabs li.active {
border-top-color: #3c8dbc;
}

.nav-tabs-custom .nav-tabs li.active a {
background: #1A1B1D;
color: #ffffff;
border-left-color: transparent;
border-right-color: transparent;}"

            ),
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "padding: 0; margin: 0;", "Caribbean")
           
           # br(),
           # 
           # # Text
           # includeHTML("./text/04_caribbean_intro.html")
           
    ),
    
    fluidRow( 
       # Map and widget to select a country 
        column(6, style = "padding: 0 0 30px 0;",
               
               column(12, style = "padding: 0 20px;",
                      
                      selectizeInput("caribbean_eez_select",
                                     label = NULL,
                                     choices = c("Select an EEZ...", caribbean_eez_choices),
                                     selected = "Select an EEZ...",
                                     width = "100%",
                                     options = list(placeholder = 'Select...')),
                      
                      leafletOutput("caribbean_map", width = "auto", height = "40vh")
                      
               )
               
        ), #close column
        
        column(6, style = "padding: 0px",
               
               uiOutput("caribbean_summary_text")
               
        ) #close column
        
    ),#close fluid row
    
    ### Tab box with results
    
    column(12, style = "width: 95%; margin: 0 2.5% 0 2.5%;",
           
    tabBox(width = 12, id = "caribbean_tabs", 
           
           # First tab - connectivity map
           tabPanel("Origins of distant water fishing vessels", 
                         
                    column(12, style = "padding: 15px 0; 
                           border-style: solid;
                           border-width: 2px 1px 1px 1px;
                           border-color: #1A1B1D;",
                           
                           leafletOutput("caribbean_connection_map",
                                         width = "100%",
                                         height = "50vh")
                    )
           ),
             
           # Second tab - heat maps   
           tabPanel("Fishing effort and subsidy intensity of distant water vessels",
                    column(12, style = "padding: 15px 25px; 
                           border-style: solid;
                           border-width: 2px 1px 1px 1px;
                           border-color: #1A1B1D;",
                           
                    fluidRow(
                           
                      column(6,
                                
                             plotOutput("caribbean_subsidy_map", 
                                        width = "auto")
                                
                      ),
                         
                      column(6,
                                
                             plotOutput("caribbean_effort_map",
                                        width = "auto")
                                
                      )   
                    ) #/fluidRow
                    )
           ) #/tabPanel #2
                
    ) # /tabBox
    )
    
  ) #close fluid page