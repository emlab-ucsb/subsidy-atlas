### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 1: Setup

region_eezs <- c(8361, # Azores (Portugal)
                 8364, # Canary Islands (Spain)
                 8435, # Faeroe
                 5680, # Iceland
                 8363, # Madiera (Portugal)
                 5688, # Portugal
                 48966, # Joint Regime: Spain/France
                 48967, # Joint Regime: UK/Denmark
                 5681 # Ireland
                 )

### Section 2: Function

europe = function()
  fluidPage(
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #000000; padding: 0; margin: 0;", "Europe")
    )
    
    
  ) # close fluidPage