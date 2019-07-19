### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 2: Function

pacific = function(eez_choices)
  fluidPage(
    
    # Top header
    column(6, stlye = "padding: 0px;",
           
           column(12, style = "padding: 0 10px;",
                  
                  selectizeInput("pacific_eez_select",
                                 label = NULL,
                                 choices = c("Select an EEZ...", eez_choices),
                                 selected = "Select an EEZ...",
                                 width = "100%",
                                 options = list(placeholder = 'Select...'))
                  
           ),
           
           column(12, style = "padding: 0",
                  
                  leafletOutput("pacific_map", width = "auto", height = "40vh")
                  
           )
           
    )
    
  ) # close fluidPage
