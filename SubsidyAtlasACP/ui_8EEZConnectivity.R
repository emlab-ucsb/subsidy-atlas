### -----------------------------------
# User interface - EEZ Connectivity
#
# This script controls the layout of the user interface for the EEZ tab
# 
### ------------------------------------

### Section 2: Function

EEZ = function(country_choices)
  fluidPage(
    
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #ffffff; padding: 0; margin: 0;", "EEZ Connectivity Maps")
    ),
    
    
    fluidRow(style = "padding: 0px 25px 0px;",
             column(6, style = "padding: 0 5px 0 20px;",
                    selectizeInput("EEZ_for_profile",
                                   label = "EEZ:",
                                   choices = country_choices,
                                   selected = NULL,
                                   width = "100%",
                                   options = list(placeholder = 'Select...'))
             )),
    
    column(12, style = "min-height: 20vh"),
    
    
conditionalPanel(
  condition = "input.EEZ_for_profile.length > 0", 
  
  column(12, style = "padding: 10px 25px 0px;",
         plotOutput("connectivity_plot", width = "auto")
  
  ))#,



#conditionalPanel(  
  #condition = "input.flag_for_profile > 0" ,
  
  #column(12, style = "padding:10px 25 px 0px;",
         #plotOutput("connectivity_plot", width = "auto")
         
  #))
  
    
    
  ) # close fluidPage