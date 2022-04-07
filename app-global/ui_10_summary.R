### -----------------------------------
#
# ACP Atlas of Distant Water Fishing
# UI - Tab 10: Data Summaries
# 
### ------------------------------------

### Function
Summary = function(){
  
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Page Header
    column(12, id = "tblr-small-spaced-div",
           
           # Title
           tags$h4("Data Summaries")
           
    ),
    
    ### Data summaries
    column(12, id = "tblr-small-spaced-div",
           
           # Text
           includeHTML("./text/faqs-and-contact/summary.html")
           
    ),
    
    ### Tab Box? 
    column(12, 
           
           ### tabBox container
           column(12,
                  
                  # tabBox
                  tabBox(width = 12, id = "summary-tabs",
                         
                         ### --------------------------
                         ### Tab # 1  - By EEZ
                         ### --------------------------
                         
                         tabPanel(value = "summary-tabs-1",
                                  
                                  # Title
                                  tagList(
                                    # Text
                                    tags$b("By EEZ"),
                                    # Info button
                                    tags$button(id = "eez_summary_info",
                                                class = "btn action-button info-button",
                                                icon("info"))
                                  ),
                                  
                                  column(12, id = "tblr-spaced-div",
                                         
                                         column(3, offset = 9, align = "right", 
                                                
                                                downloadButton("db_data_summary_by_eez", 
                                                               "Download Summary Data\n(CSV)")
                                                
                                         ),
                                                          
                                         # Data table
                                         dataTableOutput("summary_table_by_EEZ")    
                                                          
                                  ),
                                  
                                  column(12, id = "tblr-spaced-div",
                                         
                                         tags$p("ᵝ This area is claimed by two or more states."),
                                         tags$p("ᵞ This area is jointly managed by two or more states."),
                                         tags$p("ᵟ This is an aggregated entry for the entire European Union area. Areas cooresponding to individual Member states are also listed separately."),
                                         tags$p("¹ This area is part of a non-contiguous EEZ administered by a single state."),
                                         tags$p("² The administering state of this EEZ area also lays claim to one or more contested area(s)."),
                                         tags$p("³ The administering state of this EEZ area also administers one or more jointly managed area(s).")
                                  )
                                                   
                         ),
                         
                         ### --------------------------
                         ### Tab # 2  - By Flag State (EEZs Only)
                         ### --------------------------
                         
                         tabPanel(value = "summary-tabs-2",
                                  
                                  # Title
                                  tagList(
                                    # Text
                                    tags$b("By Flag State (EEZs Only)"),
                                    # Info button
                                    tags$button(id = "flag_eez_only_summary_info",
                                                class = "btn action-button info-button",
                                                icon("info"))
                                  ),
                                  
                                  column(12, id = "tblr-spaced-div",
                                         
                                         column(3, offset = 9, align = "right",
                                                
                                                downloadButton("db_data_summary_by_flag", 
                                                               "Download Summary Data\n(CSV)"),
                                                
                                         ),
                                         
                                         # Data table
                                         dataTableOutput("summary_table_by_flag_EEZs_only")    
                                         
                                  )

                         ),
                         
                         ### --------------------------
                         ### Tab # 3  - By High Seas Area
                         ### --------------------------
                         
                  tabPanel(value = "summary-tabs-3",
                           
                           # Title
                           tagList(
                             # Text
                             tags$b("By High Seas Area"),
                             # Info button
                             tags$button(id = "hs_summary_info",
                                         class = "btn action-button info-button",
                                         icon("info"))
                           ),
                           
                           column(12, id = "tblr-spaced-div",
                                  
                                  column(3, offset = 9, align = "right",
                                         
                                         downloadButton("db_data_summary_by_region", 
                                                        "Download Summary Data\n(CSV)"),
                                         
                                  ),
                                  
                                  # Data table
                                  dataTableOutput("summary_table_by_hs_area")    
                                  
                           )
                           
                  ),  
                         
                         ### --------------------------
                         ### Tab # 4  - Global fishing footprint
                         ### --------------------------
                         
                  tabPanel(value = "summary-tabs-3",
                           
                           # Title
                           tagList(
                             # Text
                             tags$b("By Flag State (High Seas Only)"),
                             # Info button
                             tags$button(id = "flag_hs_only_summary_info",
                                         class = "btn action-button info-button",
                                         icon("info"))
                           ),
                           
                           column(12, id = "tblr-spaced-div",
                                  
                                  column(3, offset = 9, align = "right",
                                         
                                         downloadButton("db_data_summary_by_flag_hs", 
                                                        "Download Summary Data\n(CSV)")
                                         
                                  ),
                                  
                                  # Data table
                                  dataTableOutput("summary_table_by_flag_hs_only")    
                                  
                           )
                           
                  )
                  
                  ) # /tabBox
                  
           ) #/column 12 - tabBox container
           
    ) # /column 12 
    
  ) # /fluidPage
}
