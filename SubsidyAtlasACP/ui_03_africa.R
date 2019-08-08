### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Function
africa = function(africa_eez_choices, flag_state_choices)
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
    column(12, style = "padding: 15px 0; border-top: 4px solid #3c8dbc;",
           
           tags$h3(style = "margin: 0 2.5%;", "Africa")
           
           #br(),
           
           # # Text
           # includeHTML("./text/03_africa_intro.html")
           
    ), 
  
  fluidRow(  
    # Map and widget to select a country 
    column(6, style = "padding: 0px 1vw;",
           
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
           
           # Title
           tags$h3(style = "padding: 0; margin: 0;", "EEZ Summary Statistics"),
           
           uiOutput("africa_summary_text"),
           
           br(),
           
           # Title
           tags$h3(style = "padding: 0x; margin: 0;", "EEZ FAO Memberships and Information"),
           
           uiOutput("africa_online_text")
           
           )#, #close column
    
    # column(6, style = "padding: 0px",
    #        
    #        uiOutput("africa_online_text")
    #        
    #        ) #close column
    
    
  ), #close fluid row 
  
  ### Tab box with results
  
  column(12, style = "width: 96%; margin: 2%;",
         
         tabBox(width = 12, id = "africa_tabs", 
                
                # First tab - connectivity map
                tabPanel("Origins of distant water fishing vessels", 
                         
                         column(12, style = "padding: 15px 0; 
                                border-style: solid;
                                border-width: 2px 1px 1px 1px;
                                border-color: #1A1B1D;",
                                
                                leafletOutput("africa_connection_map",
                                              width = "100%",
                                              height = "50vh")
                         )
                ),
                
                # Second tab - heat maps   
                tabPanel("Fishing effort and subsidy intensity of distant water vessels",
                         column(12, style = "padding: 15px 10px; 
                                border-style: solid;
                                border-width: 2px 1px 1px 1px;
                                border-color: #1A1B1D;",
                                
                                selectizeInput("africa_flag_state_select",
                                               label = "Filter fishing activity by flag state...",
                                               choices = c("All flag states", flag_state_choices),
                                               selected = "All flag states",
                                               width = "100%"),
                                
                                fluidRow(
                                  
                                  column(6,
                                         
                                         plotOutput("africa_subsidy_map", 
                                                    width = "100%")
                                         
                                  ),
                                  
                                  column(6,
                                         
                                         plotOutput("africa_effort_map",
                                                    width = "100%")
                                         
                                  )   
                                ) #/fluidRow
                         )
                ) #/tabPanel #2
                
         ) # /tabBox
  ) # /column (for tabBox)
  
  ) #close fluid page