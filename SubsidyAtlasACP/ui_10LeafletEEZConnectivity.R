### -----------------------------------
# User interface - Leaflet Connectivity Map
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 1: Function

 leaflet_EEZ = function(ACP_choices)
   fluidPage(
     
     # Top header
     column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
            tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "Leaflet")
     ), 
     
     #Input bar to choose EEZ
     
       fluidRow(style = "padding: 0px 25px 0px;",
               column(6, style = "padding: 0 5px 0 20px;",
                       selectizeInput("ACP_for_profile",
                                      label = "EEZ:",
                                      choices = ACP_choices,
                                      selected = NULL,
                                      width = "100%",
                                      options = list(placeholder = 'Select...'))
                )),
       
       column(12, style = "min-height: 20vh"),
      
    
      
   #   ## Leaflet map of ACP EEZs
     conditionalPanel(
       condition = "input.ACP_for_profile.length > 0",
    #  
      column(12, stlye = "padding: 0px;",
                        leafletOutput("leaflet_map", width = "auto", height = "80vh")
                 )
     
    # ##leaflet overvierw map 
    #  conditionalPanel(
    #    condition = "input.ACP_for_profile.length > 0", 
    #    
    #    
    #           
        #),
     
     
   )

    ) # close fluidPage
 
 
 
 #      
 #       
 #       
 #      

 # column(12, style = "padding: 0px 25px; color: #ffffff;",
 #        selectizeInput("subsidy_type_to_plot",
 #                       label = "Subsidy type(s) to include:",
 #                       choices = subsidy_types_all,
 #                       selected = 'B7',
 #                       width = "100%",
 #                       multiple = T)
 # ),
 # 
 # column(12, style = "padding: 0 0 30px;",
 #        
 #        #tableOutput("test"),
 #        leafletOutput('global_subsidies_map', width = "auto", height = "70vh")
 # )
