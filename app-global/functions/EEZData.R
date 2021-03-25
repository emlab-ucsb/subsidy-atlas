
LoadEEZData <- function(input_selected_eez){
  
  # Find matching data file and load
  all_data_files <- list.files(path = "./data/eez-effort-subs/", pattern = "*.csv")
  coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
  matching_file <- all_data_files[coastal_state_codes == input_selected_eez]
  
  out <- read_csv(paste0("./data/eez-effort-subs/", matching_file))
  
  out
  
}

EEZPlot <- function(region_dat,
                    input_selected_eez,
                    input_selected_flag_state,
                    type = "total",
                    plot_variable = "fishing_KWh",
                    eez_sf,
                    land_sf,
                    map_theme){
  
  if(type == "total"){
    
    ### Get totals for all flag states
    eez_totals <- region_dat$eez_dat %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(bad_subs, na.rm = T)) %>%
      ungroup() %>%
      mutate(bad_subs_per_fishing_KWh = subs/fishing_KWh)
    
  }else{
    
    ### Get totals for selected flag states
    eez_totals <- region_dat$eez_dat %>%
      dplyr::filter(flag_iso3 == input_selected_flag_state) %>%
      rename(subs = bad_subs)
    
  }
  
  req(nrow(eez_totals) > 0)
    
    ### Get limits for map area
    x_lim <- c(region_dat$eez_bb$xmin - 0.5, region_dat$eez_bb$xmax + 0.5)
    y_lim <- c(region_dat$eez_bb$ymin - 0.5, region_dat$eez_bb$ymax + 0.5)
    
    ### Scale
    if(plot_variable == "fishing_KWh"){
      
      legend_name = "Fishing effort \n(kWh)"
      legend_options = "A"
      
    }else{
      
      legend_name = "Subsidy intensity\n(2018 $US/kWh)"
      legend_options = "D"

    }
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_totals, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = get(plot_variable)))+
      scale_fill_viridis_c(na.value = NA, option = legend_options, name = legend_name, trans = log10_trans(), labels = comma)+
      geom_sf(data = eez_sf, fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_sf, fill = "#fafaf8", color = "#f4ebeb", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      map_theme

}