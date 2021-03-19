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

                                   ),
                                   
                                   # Select coastal state widget
                                   column(12, id = "b-spaced-div",
                                          
                                          selectizeInput("east_asia_pacific_eez_select",
                                                         label = tags$b("Select a coastal state:"),
                                                         choices = c("Select..." = "Select a coastal state...", east_asia_pacific_eezs),
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
                                          tagList(
                                            # Text 
                                            tags$b("Summary"),
                                            # Info button
                                            tags$button(id = "info_summary",
                                                        class = "btn action-button info-button",
                                                        icon("info"))
                                          ),

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
                                          tagList(
                                            # Text 
                                            tags$b("Vessel Origins"),
                                            # Info button
                                            tags$button(id = "east_asia_pacific_info_vessel_origins",
                                                        class = "btn action-button info-button",
                                                        icon("info"))
                                            ),
                                          
                                          # Content
                                          column(12, id = "lr-spaced-div",
                                                 
                                                 column(12, id = "t-spaced-div",
                                                        
                                                        fluidRow(
                                                   
                                                          column(6, align = "center",
                                                          
                                                                 # Variable by which to fill flag states on map
                                                                 selectizeInput(
                                                                   "east_asia_pacific_vessel_origins_fill",
                                                                   label = "Fill flag state(s) by...",
                                                                   choices = c("# of Different Vessels", 
                                                                               "Total Engine Capacity (KW)",
                                                                               "Total Fishing Effort (hours)",
                                                                               "Total Fishing Effort (KWh)"),
                                                                   selected = "Total Fishing Effort (KWh)",
                                                                   width = "100%")
                                                          ),
                                                   
                                                          column(6, align = "center",
                                                          
                                                                 radioButtons(
                                                                   "east_asia_pacific_vessel_origins_fill_rescale",
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
                                                        
                                                        leafletOutput("east_asia_pacific_vessel_origins_map", width = "auto")
                                                        
                                                 ) # /column
                                                 
                                          ) # /content
                                 ),
                                 
                                 ### --------------------------
                                 ### Tab # 3  - Fishing Effort
                                 ### --------------------------
                                 
                                 tabPanel(value = "east-asia-pacific-effort",
                                          
                                          # Title
                                          tagList(
                                            # Text 
                                            tags$b("Fishing Effort"),
                                            # Info button
                                            tags$button(id = "east_asia_pacific_info_effort",
                                                        class = "btn action-button info-button",
                                                        icon("info"))
                                          ),

                                          # Content
                                          column(12, id = "lr-spaced-div",

                                                 ### Header row for heat maps w/select input for flag state
                                                 column(12, id = "t-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 h4("All flag states")
                                                                 
                                                          ),
                                                          
                                                          # Select a flag state
                                                          column(6, align = "center",
                                                                 
                                                                 selectizeInput(
                                                                   "east_asia_pacific_effort_select_flag_state",
                                                                   label = "Filter fishing activity by flag state...",
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
                                                                 
                                                                 plotOutput("east_asia_pacific_effort_map_all", 
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("east_asia_pacific_effort_summary_all")
                                                                 
                                                          ),
                                                          # Selected flag state
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("east_asia_pacific_effort_map_selected",
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("east_asia_pacific_effort_summary_selected")
                                                                 
                                                                 
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
                                          tagList(
                                            # Text 
                                            tags$b("Subsidy Intensity"),
                                            # Info button
                                            tags$button(id = "east_asia_pacific_info_subsidies",
                                                        class = "btn action-button info-button",
                                                        icon("info"))
                                            ),

                                          # Content
                                          column(12, id = "lr-spaced-div",

                                                 ### Header row for heat maps w/select input for flag state
                                                 column(12, id = "t-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 h4("All flag states")
                                                                 
                                                          ),
                                                          
                                                          # Select a flag state
                                                          column(6, align = "center",
                                                                 
                                                                 selectizeInput(
                                                                   "east_asia_pacific_subsidies_select_flag_state",
                                                                   label = "Filter fishing activity by flag state...",
                                                                   choices = c("Select a flag state...", flag_state_choices),
                                                                   selected = "Select a flag state...",
                                                                   width = "100%"
                                                                 )
                                                                 
                                                          )
                                                        )
                                                        
                                                 ),
                                                 
                                                 ### Heat maps
                                                 column(12, id = "b-spaced-div",
                                                        
                                                        fluidRow(
                                                          
                                                          # All flag states
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("east_asia_pacific_subsidies_map_all", 
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("east_asia_pacific_subsidies_summary_all")
                                                                 
                                                                 
                                                          ),
                                                          # Selected flag state
                                                          column(6, align = "center",
                                                                 
                                                                 plotOutput("east_asia_pacific_subsidies_map_selected",
                                                                            width = "auto"),
                                                                 
                                                                 uiOutput("east_asia_pacific_subsidies_summary_selected")
                                                                 
                                                                 
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