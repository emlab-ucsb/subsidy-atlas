### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 2: Africa
# 
### ------------------------------------

### Function
EastAsiaPacific = function(east_asia_pacific_eezs, flag_state_choices)
  fluidPage(
            
            # Page style
            style = "background-color: #ffffff; color: #000000;",
            
            ## Content
            column(12,
                   
                   fluidRow(
                     
                     ####---------------------------------------------
                     #### Left section - Navigation and country info
                     #### --------------------------------------------
                     
                     #### Fixed absolute panel to provide shading in background
                     column(4, id = "left-column-placeholder",
                            
                     ),
            
                     #### Actual left column that will scroll appropriately
                     column(4, id = "left-column-real",
                   
                            
                            column(12, id = "lr-spaced-div",
                          
                                   # Return to regional map button
                                   column(12, id = "t-spaced-div",
                                 
                                          tags$button(id = "africa_return_to_region",
                                                      tags$b("Return to Regional Map"),
                                                      class = "btn action-button rounded-button")
                                   ),
                          
                                   # Small navigational map of the region
                                   column(12, id = "tb-spaced-div",
                                          
                                          # Africa map with selectable EEZs
                                          leafletOutput("east_asia_pacific_nav_map", width = "auto", height = "250px")
                                          
                                          # # Panel for dropdown widget 
                                          # absolutePanel(top = 10, left = 25, right = 25, fixed = FALSE,
                                          #               
                                          #               
                                          # )
                                          
                                   ),
                                   
                                   # Select coastal state widget
                                   column(12, id = "b-spaced-div",
                                          
                                          selectizeInput("east_asia_pacific_eez_select",
                                                         label = NULL,
                                                         choices = c("Select a coastal state...", east_asia_pacific_eezs),
                                                         selected = "Select a coastal state...",
                                                         width = "100%")
    
                                   )
                          )
                          
                     ), # /column 4 - left-column-real
                            
            ####------------------------
            #### Right section - results
            #### -----------------------
             
            # Right Panel
            column(8, id = "right-column",
                   
                   ### tabBox container
                   column(12,
                          
                          # tabBox
                          tabBox(width = 12, id = "results-tabs", 
                                 
                                 ### --------------------------
                                 ### Tab # 1  - Stats overview
                                 ### --------------------------
                                 
                                 tabPanel(value = "east-asia-pacific-summary",
                                   
                                          # Title
                                          tags$b("Summary"),
                                          
                                          # Content
                                          column(12, id = "tblr-spaced-div",
                                                 
                                                 # Interactive text
                                                 uiOutput("africa_country_profile")
                                                 
                                          )
                                 ),
                                 
                                 ### --------------------------
                                 ### Tab # 2  - Origins of distant water vessels
                                 ### --------------------------
                                 
                                 tabPanel(value = "east-asia-pacific-origins",
                                          
                                          # Title
                                          tags$b("Vessel Origins"),
                                          
                                          # Content
                                          column(12, id = "lr-spaced-div",
                                                 
                                                 # ### Introductory text and info button
                                                 # column(12, style = "padding: 15px 25px; color: #ffffff;",
                                                 #        
                                                 #        # Info button
                                                 #        tagList("This figure shows all flag states fishing in the EEZ of the selected ACP coastal state. If no map is visible, please select a coastal state from the map above. Flag states are filled based on the metric of fishing effort selected below. Hover over each flag stay to view more about distant water fishing activity by vessels flagged to that state in the selected EEZ.")
                                                 #        
                                                 # ), # /column12
                                                 
                                                 column(12, id = "t-spaced-div",
                                                        
                                                        fluidRow(
                                                   
                                                          column(6, align = "center",
                                                          
                                                                 # Variable by which to fill flag states on map
                                                                 selectizeInput("africa_connection_fill",
                                                                                label = "Fill flag state(s) by...",
                                                                                choices = c("# of Different Vessels", 
                                                                                            "Total Engine Capacity (KW)",
                                                                                            "Total Fishing Effort (hours)",
                                                                                            "Total Fishing Effort (KWh)"),
                                                                                selected = "Total Fishing Effort (KWh)",
                                                                                width = "100%")
                                                          ),
                                                   
                                                          column(6, align = "center",
                                                          
                                                                 radioButtons("africa_connection_fill_rescale",
                                                                              label = "Fill scale is based on...",
                                                                              choices = c("All distant water fishing in the region (default)",
                                                                                   "Selected EEZ only"),
                                                                              
                                                                              selected = "All distant water fishing in the region (default)",
                                                                              inline = T,
                                                                              width = "100%")
                                                                 
                                                          )
                                                          
                                                        )
                                                 ), # /column 12
                                                 
                                                 
                                                 ### Connectivity map 
                                                 column(12, id = "b-spaced-div",
                                                        
                                                        leafletOutput("africa_connection_map",
                                                                      width = "auto")
                                                        
                                                 ) # /column
                                                 
                                          ) # /content
                                 ),
                                 
                                 ### --------------------------
                                 ### Tab # 3  - Fishing Effort
                                 ### --------------------------
                                 
                                 tabPanel(value = "east-asia-pacific-effort",
                                          
                                          # Title
                                          tags$b("Fishing Effort"),
                                          
                                          # Content
                                          column(12, id = "lr-spaced-div",
                                                 
                                                 # ### Introductory text and info button
                                                 # column(12, style = "padding: 15px 25px; color: #ffffff;",
                                                 #        
                                                 #        # Info button
                                                 #        tagList("This figure shows distant water fishing effort (in KWh) in the EEZ of the selected ACP coastal state. Fishing effort is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a coastal state from the map above. The figure on the left shows total fishing effort for distant water vessels from all flag states and the figure on the right shows fishing effort for distant water vessels from the selected flag states.")
                                                 #        
                                                 # ), # /column12
                                                 
                                                 ### Header row for heat maps w/select input for flag state
                                                 column(12, id = "t-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 h4("All flag states")
                                                                 
                                                          ),
                                                          
                                                          # Select a flag state
                                                          column(6, align = "center",
                                                                 
                                                                 selectizeInput("africa_flag_state_select_effort",
                                                                                label ="Filter fishing activity by flag state...",
                                                                                choices = c("Select a flag state...", flag_state_choices),
                                                                                selected = "Select a flag state...",
                                                                                width = "100%")
                                                                 
                                                          )
                                                        )
                                                        
                                                 ),
                                                 
                                                 ### Heat maps
                                                 column(12, id = "b-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("africa_effort_map_all", 
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("africa_effort_summary_all")
                                                                 
                                                          ),
                                                          # Selected flag state
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("africa_effort_map",
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("africa_effort_summary")
                                                                 
                                                                 
                                                          )
                                                        )
                                                        
                                                 )
                                                 
                                          ) # /content
                                 ),
                                 
                                 ### --------------------------
                                 ### Tab # 4  - Global fishing footprint
                                 ### --------------------------
                                 
                                 tabPanel(value = "east-asia-pacific-subsidies",
                                          
                                          # Title
                                          tags$b("Subsidy Intensity"),
                                          
                                          # Content
                                          column(12, id = "lr-spaced-div",
                                                 
                                                 # ### Introductory text and info button
                                                 # column(12, style = "padding: 15px 25px; color: #ffffff;",
                                                 #        
                                                 #        # Info button
                                                 #        tagList("This figure shows the estimated magnitude of capacity-enhancing subsidies (in 2018 US$) supporting distant water fishing in the EEZ of the selected ACP coastal state. Subsidy magnitude is aggregated by 0.1 x 0.1 degree latitude/longitude. If no figure(s) are visible, please select a coastal state from the map above. The figure on the left shows total subsidy intensity for distant water vessels from all flag states and the figure on the right shows subsidy intensity for distant water vessels from the selected flag states.")
                                                 #        
                                                 # ), # /column12
                                                 
                                                 ### Header row for heat maps w/select input for flag state
                                                 column(12, id = "t-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 h4("All flag states")
                                                                 
                                                          ),
                                                          
                                                          # Select a flag state
                                                          column(6, align = "center",
                                                                 
                                                                 selectizeInput("africa_flag_state_select_subsidy",
                                                                                label ="Filter fishing activity by flag state...",
                                                                                choices = c("Select a flag state...", flag_state_choices),
                                                                                selected = "Select a flag state...",
                                                                                width = "100%")
                                                                 
                                                          )
                                                        )
                                                        
                                                 ),
                                                 
                                                 ### Heat maps
                                                 column(12, id = "b-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("africa_subsidy_map_all", 
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("africa_subsidy_summary_all")
                                                                 
                                                                 
                                                          ),
                                                          # Selected flag state
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("africa_subsidy_map",
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("africa_subsidy_summary")
                                                                 
                                                                 
                                                          )
                                                        )
                                                        
                                                 ) # /heat maps
                                                 
                                          ) # /content
                                 ) # /tabPanel 4
                          ) # /tabBox
                          
                   ) #/column 12 - tabBox container
                   
            ) # /column 8 - right panel
            
            
                   ) # /fluidRow
            
            ) # /column 12 - all content
            
  ) # /fluidPage