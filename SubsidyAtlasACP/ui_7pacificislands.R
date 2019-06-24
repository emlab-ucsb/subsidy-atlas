### -----------------------------------
# User interface - Africa
#
# This script controls the layout of the user interface for the Africa tab
# 
### ------------------------------------

### Section 1: Setup

region_eezs <- c(8444, # American Samoa
                 8446, # Cook Islands
                 8440, # French Polynesia
                 8439, # Pitcairn Island
                 8445, # Samoa
                 8448, # Tonga
                 8447, # Niue
                 8309, # Christmas Island
                 8308, # Cocos Islands
                 8388, # Heard and McDonald
                 8312, # New Caledonia
                 8313, # Vanuatu
                 21791, # Oecusse
                 8758, # East Timor
                 21795, # Joint Regime: Australia/East Timor
                 8314, # Solomon Islands
                 8443 # Palmyra Atoll (USA)
                 
                 )

### Section 2: Function

south_america = function()
  fluidPage(
    
    # Top header
    column(12, style = "padding: 15px 25px; border-top: 4px solid #3c8dbc;",
           tags$h3(style = "color: #000000; padding: 0; margin: 0;", "South America")
    )
    
    
  ) # close fluidPage