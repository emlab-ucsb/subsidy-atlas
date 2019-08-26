### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 2: Africa
# 
### ------------------------------------

### Function
africa = function(africa_eez_choices, flag_state_choices)
  fluidPage(style = "color: #ffffff; padding-bottom: 40px;",
            
            # Custom styling for the tabs on this page
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
    fluidRow(
      column(12, style = "padding: 15px 25px 0px; border-top: 4px solid #3c8dbc;",
             
             column(6,
                    # Return to regional map
                    actionButton("africa_return_to_region", "Return to regional map",
                                 style="color: #fff; 
                                 background-color: #3c8dbc; 
                                 border-color: #2e6da4; 
                                 width:100%; 
                                 font-size: 14px")
             )
                    
      )
    ),
    
    
    ### Map with controls to select an EEZ as well as stats for the selected EEZ 
    fluidRow( 
  
      column(12, style = "padding: 15px 25px;",
             
             # Left column: Map and dropdown widget
             column(6, 
             
                    # Africa map with selectable EEZs
                    leafletOutput("africa_map", width = "auto", height = "50vh"), 
                
                    
                    # Panel for dropdown widget 
                    absolutePanel(top = 10, left = 140, width = "50%", fixed = FALSE,

                                  selectizeInput("africa_eez_select",
                                                 label = NULL,
                                                 choices = c("Select a coastal state...", africa_eez_choices),
                                                 selected = "Select a coastal state...",
                                                 width = "100%")
                    )
             ), # /left column
      
             
             # Right column: Stats for selected EEZ
             column(6, style = "padding: 0 0 0 25px;",
             
                    # Interactive text
                    uiOutput("africa_country_profile")
             
             ) # /right column
      ) # close column
      
    ), #close fluid row
                    
  ### Tab box with results
  
  column(12, style = "width: 96%; margin: 2%;",
         
         tabBox(width = 12, id = "africa_tabs", 
                
                # First tab - connectivity map
                
                
          
                tabPanel("Origins of distant water fishing vessels", 
                         
                         
                         ### Title and introductory text 
                         column(12, style = "padding: 15px 25px; color: #ffffff;",
                                
                                # Header with info button
                                tags$h3(style = "padding: 0; margin: 0;", 
                                        tagList("Global Connectivity Map",
                                                tags$button(id = "africa_distant_water_info",
                                                            class = "btn action-button info-button",
                                                            icon("info"),
                                                            style = "color: #fff; background-color: #3c8dbc; border-width: 0px; padding: 0px; width:20px; height:20px; font-size: 10px; margin: 0px 5px; border-radius: 50%;")),
                         
                                 
                                
                                # Text
                                includeHTML("./text/03_africa_flag_info.html")
                                )
                         ), # /column
                         
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
                tabPanel("Distant water fishing effort",
                         column(12, style = "padding: 15px 25px; 
                                border-style: solid;
                                border-width: 2px 1px 1px 1px;
                                border-color: #1A1B1D;",
                                
                                # selectizeInput("pacific_flag_state_select",
                                #                label = "Filter fishing activity by flag state...",
                                #                choices = c("All flag states", flag_state_choices),
                                #                selected = "All flag states",
                                #                width = "100%"),
                                
                                fluidRow(
                                  
                                  column(6, align = "center",
                                         
                                         h4("All flag states"),
                                         
                                         plotOutput("africa_effort_map_all", 
                                                    width = "auto")
                                         
                                  ),
                                  
                                  column(6, align = "center",
                                         
                                         selectizeInput("africa_flag_state_select_effort",
                                                        label = tagList(tags$b("Filter fishing activity by flag state..."),
                                                        
                                                                  # Info button: subsidy types
                                                                  tags$button(id = "africa_effort_info",
                                                                              class = "btn action-button info-button",
                                                                              icon("info"),
                                                                              style = "color: #fff; background-color: #3c8dbc; border-width: 0px; padding: 0px; width:20px; height:20px; font-size: 10px; margin: 0px 5px; border-radius: 50%;")),
                                                                  
                                                        
                                                        choices = c("Select a flag state...", flag_state_choices),
                                                        selected = "Select a flag state...",
                                                        width = "100%"),
                                         
                                         plotOutput("africa_effort_map",
                                                    width = "auto")
                                         
                                  ) # close column   
                                ) #/fluidRow
                         ) # close column
                ), #/tabPanel #2
                
                # Third tab - Subsidy heat maps   
                tabPanel("Subsidy intensity of distant water vessels",
                         column(12, style = "padding: 15px 25px; 
                                  border-style: solid;
                                  border-width: 2px 1px 1px 1px;
                                  border-color: #1A1B1D;",
                                
                                fluidRow(
                                  
                                  column(6, align = "center",
                                         
                                         h4("All flag states"),
                                         
                                         plotOutput("africa_subsidy_map_all", 
                                                    width = "auto")
                                         
                                  ),
                                  
                                  column(6, align = "center",
                                         
                                         selectizeInput("africa_flag_state_select_subsidy",
                                                        label = tagList(tags$b("Filter fishing activity by flag state..."),
                                                        
                                                              # Info button: subsidy types
                                                              tags$button(id = "africa_subsidy_info",
                                                                          class = "btn action-button info-button",
                                                                          icon("info"),
                                                                          style = "color: #fff; background-color: #3c8dbc; border-width: 0px; padding: 0px; width:20px; height:20px; font-size: 10px; margin: 0px 5px; border-radius: 50%;")),
                                               
                                         
                                                        choices = c("Select a flag state...", flag_state_choices),
                                                        selected = "Select a flag state...",
                                                        width = "100%"),
                                         
                                         plotOutput("africa_subsidy_map",
                                                    width = "auto")
                                         
                                  )   
                                ) #/fluidRow
                         )
                ) #/tabPanel #2
                
         ) # /tabBox
  ) # /column (for tabBox)
  
  ) #close fluid page