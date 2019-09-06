### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 2: Caribbean
# 
### ------------------------------------

### Function
caribbean = function(caribbean_eez_choices, flag_state_choices)
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
                    actionButton("caribbean_return_to_region", "Return to regional map",
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
             
                    # Caribbean map with selectable EEZs
                    leafletOutput("caribbean_map", width = "auto", height = "50vh"), 
                
                    
                    # Panel for dropdown widget 
                    absolutePanel(top = 10, left = 140, width = "50%", fixed = FALSE,

                                  selectizeInput("caribbean_eez_select",
                                                 label = NULL,
                                                 choices = c("Select a coastal state...", caribbean_eez_choices),
                                                 selected = "Select a coastal state...",
                                                 width = "100%")
                    )
             ), # /left column
      
             
             # Right column: Stats for selected EEZ
             column(6, style = "padding: 0 0 0 25px;",
             
                    # Interactive text
                    uiOutput("caribbean_country_profile")
             
             ) # /right column
      ) # close column
      
    ), #close fluid row
                    
  ### Tab box with results
  
  column(12, style = "width: 96%; margin: 2%;",
         
         tabBox(width = 12, id = "caribbean_tabs", 
                
                # First tab - connectivity map
                tabPanel("Origins of distant water fishing vessels", 
                         
                  # Column container for tab panel        
                  column(12, style = "border-style: solid;
                                      border-width: 2px 1px 1px 1px;
                                      border-color: #1A1B1D;",
                         
                         ### Introductory text and info button
                         column(12, style = "padding: 15px 25px; color: #ffffff;",
                                
                                # Info button
                                tagList("This figure shows all flag states fishing in the EEZ of the selected ACP coastal state. If no map is visible, please select a coastal state from the map above. Flag states are filled based on the metric of fishing effort selected below. Hover over each flag stay to view more about distant water fishing activity by vessels flagged to that state in the selected EEZ."
                                  
                                        # tags$button(id = "caribbean_distant_water_info",
                                        #             class = "btn action-button info-button",
                                        #             icon("info"),
                                        #             style = "color: #fff; background-color: #3c8dbc; border-width: 0px; padding: 0px; width:20px; height:20px; font-size: 10px; margin: 0px 5px; border-radius: 50%;")
                                               
                                       )
                                
                         ), # /column12
                         
                         fluidRow(
                           
                           column(6, style = "padding: 0 5px 0 15px;", align = "center",
                                  
                                  # Variable by which to fill flag states on map
                                  selectizeInput("caribbean_connection_fill",
                                                 label = "Fill flag state(s) by...",
                                                 choices = c("# of Different Vessels", 
                                                             "Total Engine Capacity (KW)",
                                                             "Total Fishing Effort (hours)", 
                                                             "Total Fishing Effort (KWh)"),
                                                 selected = "Total Fishing Effort (KWh)",
                                                 width = "100%")
                           ),
                           
                           column(6, style = "padding: 0 15px 0 5px;", align = "center",
                                  
                                  radioButtons("caribbean_connection_fill_rescale",
                                               label = "Fill scale is based on...",
                                               choices = c("All distant water fishing in the region (default)",
                                                           "Selected EEZ only"),
                                               selected = "All distant water fishing in the region (default)",
                                               inline = T,
                                               width = "100%")
                                  )
                         ),
                                
                         
                         ### Connectivity map 
                         column(12, style = "padding: 0;", align = "left",
                                
                                leafletOutput("caribbean_connection_map",
                                              width = "100%",
                                              height = "60vh")
                                
                         ) # /column
                         
                  ) # / encompassing column
                ), # / tabPanel
                
                # Second tab - heat maps   
                tabPanel("Distant water fishing effort",
                         
                         column(12, style = "border-style: solid;
                                border-width: 2px 1px 1px 1px;
                                border-color: #1A1B1D;",
                                
                                ### Introductory text and info button
                                column(12, style = "padding: 15px 25px; color: #ffffff;",
                                       
                                       # Info button
                                       tagList("This figure shows distant water fishing effort (in KWh) in the EEZ of the selected ACP coastal state. Fishing effort is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a coastal state from the map above. The figure on the left shows total fishing effort for distant water vessels from all flag states and the figure on the right shows fishing effort for distant water vessels from the selected flag states."
                                               
                                               # tags$button(id = "caribbean_effort_info",
                                               #             class = "btn action-button info-button",
                                               #             icon("info"),
                                               #             style = "color: #fff; background-color: #3c8dbc; border-width: 0px; padding: 0px; width:20px; height:20px; font-size: 10px; margin: 0px 5px; border-radius: 50%;")
                                               
                                       )
                                       
                                ), # /column12
                                
                                ### Header row for heat maps w/select input for flag state
                                column(12, style = "padding: 0px 25px; color: #ffffff;",
                                       
                                       fluidRow(
                                         
                                         # All flag states
                                         column(6, align = "center",
                                                
                                                h4("All flag states")
                                                
                                                ),
                                         
                                         # Select a flag state
                                         column(6, align = "center",
                                                
                                                selectizeInput("caribbean_flag_state_select_effort",
                                                               label ="Filter fishing activity by flag state...",
                                                               choices = c("Select a flag state...", flag_state_choices),
                                                               selected = "Select a flag state...",
                                                               width = "100%")
                                                
                                                )
                                       )
                                       
                                ),
                                
                                ### Heat maps
                                column(12, style = "padding: 5px 0; color: #ffffff;",
                                       
                                       fluidRow(
                                         
                                         # All flag states
                                         column(6, align = "center",
                                                
                                                plotOutput("caribbean_effort_map_all", 
                                                           width = "auto")
                                                
                                         ),
                                         # Selected flag state
                                         column(6, align = "center",
                                                
                                                plotOutput("caribbean_effort_map",
                                                           width = "auto")
                                                
                                         )
                                       )
                                       
                                )
                                
                         ) # close column12
                ), #/tabPanel #2
                
                # Third tab - Subsidy heat maps   
                tabPanel("Subsidy intensity of distant water vessels",
                         
                         column(12, style = "border-style: solid;
                                  border-width: 2px 1px 1px 1px;
                                  border-color: #1A1B1D;",
                                
                                ### Introductory text and info button
                                column(12, style = "padding: 15px 25px; color: #ffffff;",
                                       
                                       # Info button
                                       tagList("This figure shows the estimated magnitude of capacity-enhancing subsidies (in 2018 US$) supporting distant water fishing in the EEZ of the selected ACP coastal state. Subsidy magnitude is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a coastal state from the map above. The figure on the left shows total subsidy intensity for distant water vessels from all flag states and the figure on the right shows subsidy intensity for distant water vessels from the selected flag states."
                                               
                                               # tags$button(id = "caribbean_subsidy_info",
                                               #             class = "btn action-button info-button",
                                               #             icon("info"),
                                               #             style = "color: #fff; background-color: #3c8dbc; border-width: 0px; padding: 0px; width:20px; height:20px; font-size: 10px; margin: 0px 5px; border-radius: 50%;")
                                               
                                       )
                                       
                                ), # /column12
                                
                                ### Header row for heat maps w/select input for flag state
                                column(12, style = "padding: 0px 25px; color: #ffffff;",
                                       
                                       fluidRow(
                                         
                                         # All flag states
                                         column(6, align = "center",
                                                
                                                h4("All flag states")
                                                
                                         ),
                                         
                                         # Select a flag state
                                         column(6, align = "center",
                                                
                                                selectizeInput("caribbean_flag_state_select_subsidy",
                                                               label ="Filter fishing activity by flag state...",
                                                               choices = c("Select a flag state...", flag_state_choices),
                                                               selected = "Select a flag state...",
                                                               width = "100%")
                                                
                                         )
                                       )
                                       
                                ),
                                
                                ### Heat maps
                                column(12, style = "padding: 5px 0; color: #ffffff;",
                                       
                                       fluidRow(
                                         
                                         # All flag states
                                         column(6, align = "center",
                                                
                                                plotOutput("caribbean_subsidy_map_all", 
                                                           width = "auto")
                                                
                                         ),
                                         # Selected flag state
                                         column(6, align = "center",
                                                
                                                plotOutput("caribbean_subsidy_map",
                                                           width = "auto")
                                                
                                         )
                                       )
                                       
                                )
                                
                                
                         ) # /column12
                ) #/tabPanel #3
                
         ) # /tabBox
  ) # /column (for tabBox)
  
  ) #close fluid page