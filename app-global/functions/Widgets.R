
WidgetEEZSelect <- function(region_dat,
                            widget_id){
  
  # Get data
  region_eez_data <- region_dat$connect %>%
    st_drop_geometry() %>%
    distinct(region, eez_ter_iso3, eez_ter_name) %>%
    arrange(eez_ter_name)
  
  # Unique choices
  eezs <- region_eez_data$eez_ter_iso3
  names(eezs) <- region_eez_data$eez_ter_name
  
  selectizeInput(widget_id,
                 label = NULL,
                 choices = c("Select a coastal state..." = "Select a coastal state...", eezs),
                 selected = "Select a coastal state...",
                 width = "100%")
  
}

WidgetFlagStateSelect <- function(region_dat,
                                  widget_id){
  
  
  
}