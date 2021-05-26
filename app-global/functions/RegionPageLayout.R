
RegionPageLayout <- function(region_name, 
                             region_name_long,
                             vessel_origins_fill_choices, 
                             vessel_origins_fill_scale){
  
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

                                  # Return button
                                  tags$button(id = paste0(region_name, "_return_to_region"),
                                              tags$b("Return to Global Map"),
                                              class = "btn action-button rounded-button"),
                                  
                                  # Title
                                  paste0("<h4 style = 'margin-bottom:5px; margin-top:15px;'>", region_name_long, "</h4>") %>% 
                                    lapply(htmltools::HTML)
                                  #tags$h4(region_name_long)
                              
                           ),

                           # Small navigational map of the region
                           column(12, id = "tb-spaced-div",
                                  
                                  # Regional map with selectable EEZs
                                  leafletOutput(paste0(region_name, "_nav_map"), width = "auto", height = "250px"),

                                  # Select coastal state widget
                                  absolutePanel(top = "15px", left = "5px", right = "5px",

                                                uiOutput(paste0(region_name, "_eez_select"))
                                  )

                           ),

                           # Selected coastal state stats
                           column(12, id = "b-spaced-div",

                                  uiOutput(paste0(region_name, "_eez_select_stats"))

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
                           tabBox(width = 12, id = paste0(str_replace_all(region_name, "_", "-"), "-results-tabs"),

                                  ### --------------------------
                                  ### Tab # 1  - Stats overview
                                  ### --------------------------

                                  tabPanel(value = paste0(str_replace_all(region_name, "_", "-"), "-summary"),

                                           # Title
                                           tagList(
                                             # Text
                                             tags$b("Summary"),
                                             # Info button
                                             tags$button(id = paste0(region_name, "_info_summary"),
                                                         class = "btn action-button info-button",
                                                         icon("info"))
                                           ),
                                             
                                           # Content
                                          column(12, id = "tlr-spaced-div",
                                                    
                                                    # Interactive text
                                                    uiOutput(paste0(region_name, "_summary_ui"))
                                                
                                          ),
                                          
                                          conditionalPanel(condition = paste0("input.", region_name, "_eez_select != 'Select a coastal state...'"),
                                                           
                                          column(12,
                                                 
                                                 uiOutput(paste0(region_name, "_summary_ui_flag"))
                                                 
                                          ),                 
                                          
                                          # fluidRow(
                                          #   column(8, id = "tblr-small-spaced-div",
                                          #          
                                          #          paste0("<h5>By flag state</h5>") %>%
                                          #            lapply(htmltools::HTML)
                                          #          
                                          #          
                                          #   ),
                                          #   column(4, id = "tblr-small-spaced-div", align = "right",
                                          #          
                                          #          # Download CSV button
                                          #          downloadButton(
                                          #            paste0(region_name, "_download_data"),
                                          #            tags$b("Download Data (CSV)"))
                                          #          
                                          #   )
                                          # ),
                                          
                                          column(12, id = "tblr-spaced-div",
                                                    
                                                 # Data table
                                                 dataTableOutput(paste0(region_name, "_summary_dt"))    
                                                    
                                          )
                                          
                                           ) # /conditional
                                  ),

                                  ### --------------------------
                                  ### Tab # 2  - Origins of distant water vessels
                                  ### --------------------------

                                  tabPanel(value = paste0(str_replace_all(region_name, "_", "-"), "-origins"),

                                           # Title
                                           tagList(
                                             # Text
                                             tags$b("Vessel Origins"),
                                             # Info button
                                             tags$button(id = paste0(region_name, "_info_vessel_origins"),
                                                         class = "btn action-button info-button",
                                                         icon("info"))
                                           ),

                                           conditionalPanel(condition = paste0("input.", region_name, "_eez_select != 'Select a coastal state...'"),
                                                            
                                           # Content
                                           column(12, id = "lr-spaced-div",

                                                  column(12, id = "t-spaced-div",

                                                         fluidRow(

                                                           column(6, align = "center",

                                                                  # Variable by which to fill flag states on map
                                                                  selectizeInput(
                                                                    paste0(region_name, "_vessel_origins_fill"),
                                                                    label = "Fill flag state(s) by...",
                                                                    choices = vessel_origins_fill_choices,
                                                                    selected = "n_vessels",
                                                                    width = "100%")

                                                           ),

                                                           column(6, align = "center",

                                                                  radioButtons(
                                                                    paste0(region_name, "_vessel_origins_fill_rescale"),
                                                                    label = "Fill scale is based on...",
                                                                    choices = vessel_origins_fill_scale,
                                                                    selected = "region",
                                                                    inline = T,
                                                                    width = "100%")

                                                           )

                                                         )
                                                  ), # /column 12


                                                  ### Connectivity map
                                                  column(12, id = "b-spaced-div",

                                                         leafletOutput(paste0(region_name, "_vessel_origins_map"), width = "auto")

                                                  ) # /column

                                           ) # /content
                                           )
                                  ),

                                  ### --------------------------
                                  ### Tab # 3  - Fishing Effort
                                  ### --------------------------

                                  tabPanel(value = paste0(str_replace_all(region_name, "_", "-"), "-effort"),

                                           # Title
                                           tagList(
                                             # Text
                                             tags$b("Fishing Effort"),
                                             # Info button
                                             tags$button(id = paste0(region_name, "_info_effort"),
                                                         class = "btn action-button info-button",
                                                         icon("info"))
                                           ),

                                           conditionalPanel(condition = paste0("input.", region_name, "_eez_select != 'Select a coastal state...'"),
                                                            
                                           # Content
                                           column(12, id = "lr-spaced-div",

                                                  ### Header row for heat maps w/select input for flag state
                                                  column(12, id = "t-spaced-div",

                                                         fluidRow(

                                                           # All flag states
                                                           column(6, align = "center",

                                                                  h4("All DW vessels")

                                                           ),

                                                           # Select a flag state
                                                           column(6, align = "center",
                                                                  
                                                                  uiOutput(paste0(region_name, "_effort_select_flag_state"))

                                                           )
                                                         )

                                                  ),

                                                  ### Heat maps
                                                  column(12, id = "b-spaced-div",

                                                         fluidRow(

                                                           # All flag states
                                                           column(6,

                                                                  leafletOutput(
                                                                    paste0(region_name, "_effort_map_all"),
                                                                    height = "375px")

                                                           ),
                                                           # Selected flag state
                                                           column(6,

                                                                  leafletOutput(
                                                                    paste0(region_name, "_effort_map_selected"),
                                                                    height = "375px")


                                                           )
                                                         ),
                                                         
                                                         # fluidRow(
                                                         #   
                                                         #   column(12, align = "center",
                                                         #          
                                                         #          plotOutput(
                                                         #            paste0(region_name, "_effort_map_legend"),
                                                         #            height = "80px")
                                                         #          
                                                         #   )
                                                         #   
                                                         # ),
                                                         
                                                         shinyjs::hidden(fluidRow(

                                                           column(12, align = "center",

                                                                  switchInput(
                                                                    inputId = paste0(region_name, "_effort_high_seas"),
                                                                    label = "Show high seas activity",
                                                                    labelWidth = "180px",
                                                                    onLabel = "Yes",
                                                                    offLabel = "No"
                                                                  )

                                                           )

                                                         ))

                                                  )

                                           ) # /content
                                           )
                                  ),

                                  ### --------------------------
                                  ### Tab # 4  - Global fishing footprint
                                  ### --------------------------

                                  tabPanel(value = paste0(str_replace_all(region_name, "_", "-"), "-subsidies"),

                                           # Title
                                           tagList(
                                             # Text
                                             tags$b("Subsidies"),
                                             # Info button
                                             tags$button(id = paste0(region_name, "_info_subsidies"),
                                                         class = "btn action-button info-button",
                                                         icon("info"))
                                           ),

                                           conditionalPanel(condition = paste0("input.", region_name, "_eez_select != 'Select a coastal state...'"),
                                                            
                                           # Content
                                           column(12, id = "lr-spaced-div",

                                                  ### Header row for heat maps w/select input for flag state
                                                  column(12, id = "t-spaced-div",

                                                         fluidRow(

                                                           # All flag states
                                                           column(6, align = "center",

                                                                  h4("All DW vessels")

                                                           ),

                                                           # Select a flag state
                                                           column(6, align = "center",
                                                                  
                                                                  uiOutput(paste0(region_name, "_subsidies_select_flag_state"))

                                                           )
                                                         )
                                                         

                                                  ),

                                                  ### Heat maps
                                                  column(12, id = "b-spaced-div",

                                                         fluidRow(

                                                           # All flag states
                                                           column(6,

                                                                  leafletOutput(
                                                                    paste0(region_name, "_subsidies_map_all"),
                                                                    height = "375px")


                                                           ),
                                                           # Selected flag state
                                                           column(6,

                                                                  leafletOutput(
                                                                    paste0(region_name, "_subsidies_map_selected"),
                                                                    height = "375px")

                                                           )
                                                         ),
                                                         
                                                         # fluidRow(
                                                         #   
                                                         #   column(12, align = "center",
                                                         #          
                                                         #          plotOutput(
                                                         #            paste0(region_name, "_subsidies_map_legend"),
                                                         #            height = "80px")
                                                         #          
                                                         #   )
                                                         # 
                                                         # ),
                                                         
                                                         shinyjs::hidden(fluidRow(

                                                           column(12, align = "center",

                                                                  switchInput(
                                                                    inputId = paste0(region_name, "_subsidies_high_seas"),
                                                                    label = "Show high seas activity",
                                                                    labelWidth = "180px",
                                                                    onLabel = "Yes",
                                                                    offLabel = "No"
                                                                  )

                                                           )

                                                         ))

                                                  ) # /heat maps

                                           ) # /content
                                           )
                                  ) # /tabPanel 4
                           ) # /tabBox

                    ) #/column 12 - tabBox container

             ) # /column 8 - right panel


           ) # /fluidRow

    ) # /column 12 - all content

  ) # /fluidPage

}