
### ----------------------------------------------------
### Data import function - Load EEZ level data
### ----------------------------------------------------

LoadEEZData <- function(input_selected_eez){
  
  # Find matching data file and load
  all_data_files <- list.files(path = "./data/eez-effort-subs/", pattern = "*.csv")
  coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
  matching_file <- all_data_files[coastal_state_codes == input_selected_eez]
  
  if(length(matching_file) > 0){
  
  out <- read_csv(paste0("./data/eez-effort-subs/", matching_file))
  
  }else{
    
    out <- NULL
    
  }
  
  out
  
}

### ----------------------------------------------------
### Plotting function - EEZ level maps (ggplot version)
### ----------------------------------------------------

# EEZPlot <- function(region_dat,
#                     input_selected_eez,
#                     input_selected_flag_state,
#                     input_hs,
#                     type = "total",
#                     plot_variable = "fishing_KWh",
#                     eez_sf,
#                     land_sf,
#                     map_theme,
#                     use_raster = T){
# 
#   browser()
#   ### Get totals for all flag states
#   eez_totals <- region_dat$eez_dat %>%
#     group_by(lon_cen, lat_cen) %>%
#     summarize(fishing_hours = sum(fishing_hours, na.rm = T),
#               fishing_KWh = sum(fishing_KWh, na.rm = T),
#               subs = sum(bad_subs, na.rm = T)) %>%
#     ungroup() %>%
#     mutate(bad_subs_per_fishing_KWh = subs/fishing_KWh)
# 
#   if(type == "total"){
# 
#     plot_totals <- eez_totals
# 
#     # ### Get high seas effort if relevant
#     # if(input_hs){
#     #
#     #   hs_totals <- region_dat$hs_dat %>%
#     #     group_by(lon_cen, lat_cen) %>%
#     #     summarize(fishing_hours = sum(fishing_hours, na.rm = T),
#     #               fishing_KWh = sum(fishing_KWh, na.rm = T),
#     #               subs = sum(bad_subs, na.rm = T)) %>%
#     #     ungroup() %>%
#     #     mutate(bad_subs_per_fishing_KWh = subs/fishing_KWh)
#     #
#     # }else{
#     #
#     #   hs_totals <- tibble(lat_cen = numeric(0),
#     #                       lon_cen = numeric(0),
#     #                       fishing_hours = numeric(0),
#     #                       fishing_KWh = numeric(0),
#     #                       subs = numeric(0),
#     #                       bad_subs_per_fishing_KWh = numeric(0))
#     # }
# 
#   }else{
# 
#     ### Get totals for selected flag states
#     plot_totals <- region_dat$eez_dat %>%
#       dplyr::filter(flag_iso3 == input_selected_flag_state) %>%
#       rename(subs = bad_subs)
# 
#     # ### Get high seas effort if relevant
#     # if(input_hs){
#     #
#     #   hs_totals <- region_dat$hs_dat %>%
#     #     dplyr::filter(flag_iso3 == input_selected_flag_state) %>%
#     #     rename(subs = bad_subs)
#     #
#     # }else{
#     #
#     #   hs_totals <- tibble(lat_cen = numeric(0),
#     #                       lon_cen = numeric(0),
#     #                       fishing_hours = numeric(0),
#     #                       fishing_KWh = numeric(0),
#     #                       subs = numeric(0),
#     #                       bad_subs_per_fishing_KWh = numeric(0))
#     # }
# 
#   }
# 
#   #browser()
#   req(nrow(plot_totals) > 0)
# 
#     ### Get limits for map area
#     x_lim <- c(region_dat$eez_bb$xmin - 1, region_dat$eez_bb$xmax + 1)
#     y_lim <- c(region_dat$eez_bb$ymin - 1, region_dat$eez_bb$ymax + 1)
# 
#     ### Scale
#     if(plot_variable == "fishing_KWh"){
# 
#       legend_name = "Fishing effort \n(kWh)"
#       legend_options = "A"
# 
#       ### Get legend limits
#       scale_breaks <- pretty(log10(c(eez_totals[[plot_variable]]
#                                      #hs_totals[[plot_variable]]
#                                      )), n = 5)
#       scale_labels <- format(round(10^scale_breaks, 0), big.mark = ",", scientific = F)
# 
#       # Caption
#       caption = paste0("Total DW fishing effort\n(kWh): ",
#                        format(round(sum(plot_totals$fishing_KWh, na.rm = T), 0), big.mark = ",", scientific = F))
# 
#       ### Log transform variable
#       plot_totals <- plot_totals %>%
#         dplyr::filter(fishing_KWh > 0) %>%
#         mutate(fishing_KWh = log10(fishing_KWh))
# 
#       # hs_totals <- hs_totals %>%
#       #   dplyr::filter(fishing_KWh > 0) %>%
#       #   mutate(fishing_KWh = log10(fishing_KWh))
# 
#     }else{
# 
#       legend_name = "Capacity-Enhancing Subsidies\n(2018 $US)"
#       legend_options = "D"
# 
#       ### Get legend limits
#       scale_breaks <- pretty(log10(c(eez_totals[[plot_variable]]
#                                #hs_totals[[plot_variable]]
#                                )), n = 5)
#       scale_labels <- format(round(10^scale_breaks, 0), big.mark = ",", scientific = F)
# 
#       # Caption
#       caption = paste0("Estimated DW subsidies\n(2018 $US): ",
#                        format(round(sum(plot_totals$subs, na.rm = T), 0), big.mark = ",", scientific = F))
#       ### Log transform variable
#       plot_totals <- plot_totals %>%
#         dplyr::filter(subs > 0 & !is.na(subs)) %>%
#         mutate(subs = log10(subs))
# 
#     }
# 
#     req(nrow(plot_totals) > 0)
# 
#     ### Map with high seas on --------------------------------------------------------------
# 
#     # if(input_hs
#     #    & nrow(hs_totals) > 0){
#     #
#     # # Map
#     # plot <- ggplot()+
#     #   geom_tile(data = plot_totals, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = get(plot_variable)))+
#     #   geom_tile(data = hs_totals, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = get(plot_variable)))+
#     #     scale_fill_gradientn(colors = viridis_pal(option = legend_options)(9),
#     #                          breaks = scale_breaks,
#     #                          limits = c(min(scale_breaks), max(scale_breaks)),
#     #                          labels = scale_labels,
#     #                          name = legend_name)+
#     #   guides(fill = guide_colourbar(title.position = "bottom", title.hjust = 0.5, barwidth = 20))+
#     #   geom_sf(data = eez_sf, fill = NA, color = "grey60", size = 0.5)+ # world EEZs
#     #   geom_sf(data = land_sf, fill = "#fafaf8", color = "#f4ebeb", size = 0.5)+ # world countries
#     #   labs(x = "", y = "")+
#     #   coord_sf(xlim = x_lim, ylim = y_lim) +
#     #   scale_x_continuous(expand = c(0,0))+
#     #   scale_y_continuous(expand = c(0,0))+
#     #   map_theme+
#     #   labs(caption=caption) +
#     #   theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
#     #
#     # }else{
# 
#     ### Map with high seas off --------------------------------------------------------------
# 
#       # Map
#       plot <- ggplot()+
#         geom_tile(data = plot_totals, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = get(plot_variable)))+
#         scale_fill_gradientn(colors = viridis_pal(option = legend_options)(9),
#                              breaks = scale_breaks,
#                              limits = c(min(scale_breaks), max(scale_breaks)),
#                              labels = scale_labels,
#                              name = legend_name)+
#         geom_sf(data = land_sf, fill = "#fafaf8", color = "#f4ebeb", size = 0.5)+ # world countries
#         geom_sf(data = eez_sf, fill = NA, color = "grey60", size = 0.5)+ # world EEZs
#         guides(fill = guide_colourbar(title.position = "bottom", title.hjust = 0.5, barwidth = 20, ticks.colour = "black", frame.colour = "black"))+
#         labs(x = "", y = "")+
#         coord_sf(xlim = x_lim, ylim = y_lim) +
#         scale_x_continuous(expand = c(0,0))+
#         scale_y_continuous(expand = c(0,0))+
#         map_theme+
#         labs(caption=caption) +
#         theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
# 
#     #}
# 
#     return(list(plot = plot + theme(legend.position = "none"),
#                 legend = cowplot::get_legend(plot)))
# 
# }

### ----------------------------------------------------
### Plotting function - EEZ level maps (Leaflet version)
### ----------------------------------------------------

EEZLeafletMap <- function(region_dat,
                         input_selected_eez,
                         map_id,
                         min_zoom = 1){
  
  if(map_id %in% c("high_seas_effort_map_all", "high_seas_effort_map_selected", "high_seas_subsidies_map_all", "high_seas_subsidies_map_selected")){
    
    ### Get limits for map area
    x_lim <- unname(c(region_dat$eez_bb$xmin - 1, region_dat$eez_bb$xmax + 1))
    y_lim <- unname(c(region_dat$eez_bb$ymin - 1, region_dat$eez_bb$ymax + 1))
    
    # Get selected EEZ so we can emphasize it
    selected_eez <- region_dat$eezs %>%
      mutate(zone = as.character(zone)) %>%
      dplyr::filter(zone == input_selected_eez)
    
    # Check to see if there are any manual zoom/extent corrections that need to be made
    if(input_selected_eez %in% names(manual_region)){
      
      eez_manual_info <- manual_region[[input_selected_eez]]
      
      lon_map <- unname(ifelse(!is.na(eez_manual_info["lon"]), 
                               eez_manual_info["lon"],
                               mean(x_lim, na.rm = T)))
      
      lat_map <- unname(ifelse(!is.na(eez_manual_info["lat"]), 
                               eez_manual_info["lat"],
                               mean(y_lim, na.rm = T)))
      
      zoom_map <- unname(ifelse(!is.na(eez_manual_info["zoom"]), 
                                eez_manual_info["zoom"],
                                min_zoom + 1))
      
      
    }else{
      
      lon_map <- mean(x_lim, na.rm = T)
      lat_map <- mean(y_lim, na.rm = T)
      zoom_map <- min_zoom + 1
      
    }
    
    # Map
    leaflet(map_id, 
            options = leafletOptions(minZoom = min_zoom, zoomControl = FALSE, 
                                     attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topleft' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldPhysical") %>% 
      
      addPolygons(data = region_dat$eezs,
                  fillOpacity = 0,
                  color= "white",
                  weight = 0.3,
                  label = NA,
      ) %>%
      addPolygons(data = selected_eez,
                  fillOpacity = 0,
                  color= "white",
                  weight = 2,
                  label = NA,
      ) %>%
      setView(lng = lon_map,
              lat = lat_map,
              zoom = zoom_map)
      # setMaxBounds(lng1 = x_lim[1],
      #              lat1 = y_lim[1],
      #              lng2 = x_lim[2],
      #              lat2 = y_lim[2])
    
  }else{
    
    
    ### Get limits for map area
    x_lim <- unname(c(region_dat$eez_bb$xmin - 1, region_dat$eez_bb$xmax + 1))
    y_lim <- unname(c(region_dat$eez_bb$ymin - 1, region_dat$eez_bb$ymax + 1))
    
    # Get selected EEZ so we can emphasize it
    selected_eez <- region_dat$eezs %>%
      dplyr::filter(eez_ter_iso3 == input_selected_eez)
    
    # Check to see if there are any manual zoom/extent corrections that need to be made
    if(input_selected_eez %in% names(manual_eez)){
      
      eez_manual_info <- manual_eez[[input_selected_eez]]
      
      lon_map <- unname(ifelse(!is.na(eez_manual_info["lon"]), 
                               eez_manual_info["lon"],
                               mean(x_lim, na.rm = T)))
      
      lat_map <- unname(ifelse(!is.na(eez_manual_info["lat"]), 
                               eez_manual_info["lat"],
                               mean(y_lim, na.rm = T)))
      
      zoom_map <- unname(ifelse(!is.na(eez_manual_info["zoom"]), 
                                eez_manual_info["zoom"],
                                min_zoom + 1))
      
      
    }else{
      
      lon_map <- mean(x_lim, na.rm = T)
      lat_map <- mean(y_lim, na.rm = T)
      zoom_map <- min_zoom + 1
      
    }

    # Map
    leaflet(map_id, 
            options = leafletOptions(minZoom = min_zoom, zoomControl = FALSE, 
                                     attributionControl=FALSE)) %>% 
      
      htmlwidgets::onRender("function(el, x) {
                            L.control.zoom({ position: 'topleft' }).addTo(this)}") %>%
      
      addProviderTiles("Esri.WorldPhysical") %>% 
      
      addPolygons(data = region_dat$eezs,
                  fillOpacity = 0,
                  color= "white",
                  weight = 0.3,
                  label = NA,
      ) %>%
      addPolygons(data = selected_eez,
                  fillOpacity = 0,
                  color= "white",
                  weight = 2,
                  label = NA,
      ) %>%
      setView(lng = lon_map,
              lat = lat_map,
              zoom = zoom_map)
    # setMaxBounds(lng1 = x_lim[1],
    #              lat1 = y_lim[1],
    #              lng2 = x_lim[2],
    #              lat2 = y_lim[2])
    
  }
  
}

### ----------------------------------------------------
### Data manipulation function - Rasterize EEZ level data
### ----------------------------------------------------

EEZDatRasterize <- function(region_dat,
                            input_selected_eez,
                            input_selected_flag_state,
                            type = "total",
                            plot_variable = "fishing_KWh",
                            is_hs = F){
  

  
  if(type == "total"){
    
    ### Aggregate spatial totals for all flag states
    eez_totals <- region_dat$eez_dat %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(bad_subs, na.rm = T)) %>%
      ungroup()
    
    plot_totals <- eez_totals
    
    if(!is_hs){
      
    ### Get EEZ-wide totals from connectivity data (removes rounding issues from rasters)
    connect_totals <- region_dat$connect %>%
      st_drop_geometry() %>%
      dplyr::filter(eez_ter_iso3 == input_selected_eez) %>% 
      group_by(eez_ter_iso3, eez_ter_name, flag_iso3) %>%
      summarize(fishing_KWh = unique(fishing_KWh),
                bad_subs = unique(bad_subs)) %>%
      ungroup() %>%
      group_by(eez_ter_iso3, eez_ter_name) %>%
      summarize(fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(bad_subs, na.rm = T)) %>%
      ungroup() %>%
      rename(title = eez_ter_name)
    
    }else{
      
    connect_totals <- region_dat$connect %>%
        st_drop_geometry() %>%
        mutate(fao_region = as.character(fao_region)) %>%
        dplyr::filter(fao_region == input_selected_eez) %>%
        group_by(fao_region, title, flag_iso3) %>%
        summarize(fishing_KWh = unique(fishing_KWh),
                  bad_subs = unique(bad_subs)) %>%
        ungroup() %>%
        group_by(fao_region, title) %>%
        summarize(fishing_KWh = sum(fishing_KWh, na.rm = T),
                  bad_subs = sum(bad_subs, na.rm = T)) %>%
        ungroup()

    }
    
  }else if(type == "flag"){
    
    ### Aggregate spatial totals for selected flag state
    plot_totals <- region_dat$eez_dat %>%
      dplyr::filter(flag_iso3 == input_selected_flag_state) %>%
      rename(subs = bad_subs)
    
    if(!is_hs){
      
    ### Get flag-specific totals from connectivity data (removes rounding issues from rasters)
    connect_totals <- region_dat$connect %>%
      st_drop_geometry() %>%
      dplyr::filter(eez_ter_iso3 == input_selected_eez) %>% 
      dplyr::filter(flag_iso3 == input_selected_flag_state) %>%
      group_by(eez_ter_iso3, eez_ter_name, flag_iso3) %>%
      summarize(fishing_KWh = unique(fishing_KWh, na.rm = T),
                subs = unique(bad_subs, na.rm = T)) %>%
      ungroup() %>%
      rename(title = eez_ter_name)
    
    }else{
      
      connect_totals <- region_dat$connect %>%
        st_drop_geometry() %>%
        mutate(fao_region = as.character(fao_region)) %>%
        dplyr::filter(fao_region == input_selected_eez) %>%
        dplyr::filter(flag_iso3 == input_selected_flag_state) %>%
        group_by(fao_region, title, flag_iso3) %>%
        summarize(fishing_KWh = unique(fishing_KWh),
                  bad_subs = unique(bad_subs)) %>%
        ungroup()
    }
  }
  
  # Make sure there's data
  req(nrow(plot_totals) > 0)
  
  ### Scale
  if(plot_variable == "fishing_KWh"){
    
    legend_name = paste0("Fishing effort (kWh)")
    legend_options = "plasma"
    
    # Caption
    # caption = paste0("<b>", "Total DW fishing effort (kWh): ", "</b>", 
    #                  format(round(sum(plot_totals$fishing_KWh, na.rm = T), 0), big.mark = ",", scientific = F))
    caption = paste0("<b>", "Total DW fishing effort (kWh): ", "</b>", 
                     format(round(sum(connect_totals$fishing_KWh, na.rm = T), 0), big.mark = ",", scientific = F))
    
    # caption_ggplot <- paste0("Total DW fishing effort \n(kWh): ", 
    #                          format(round(sum(plot_totals$fishing_KWh, na.rm = T), 0), big.mark = ",", scientific = F))
    caption_ggplot <- paste0("Total DW fishing effort \n(kWh): ",
                             format(round(sum(connect_totals$fishing_KWh, na.rm = T), 0), big.mark = ",", scientific = F))
    
    # Format caption
    caption <- tags$div(HTML(caption))  
    
    # Get display name
    display_name <- unique(connect_totals$title)
    
    ## Log transform variable
    plot_totals <- plot_totals %>%
      dplyr::filter(fishing_KWh > 0) %>%
      mutate(fishing_KWh = log10(fishing_KWh))
    
  }else{
    
    legend_name = paste0("Capacity-Enhancing", "<br>",  "Subsidies (2018 $US)")
    legend_options = "viridis"
    
    # Caption
    # caption = paste0("<b>", "Estimated DW subsidies (2018 $US): ", "</b>",
    #                  format(round(sum(plot_totals$subs, na.rm = T), 0), big.mark = ",", scientific = F))
    caption = paste0("<b>", "Estimated DW subsidies (2018 $US): ", "</b>",
                     format(round(sum(connect_totals$subs, na.rm = T), 0), big.mark = ",", scientific = F))
    
    # caption_ggplot = paste0("Estimated DW subsidies \n(2018 $US): ",
    #                  format(round(sum(plot_totals$subs, na.rm = T), 0), big.mark = ",", scientific = F))
    caption_ggplot = paste0("Estimated DW subsidies \n(2018 $US): ",
                            format(round(sum(connect_totals$subs, na.rm = T), 0), big.mark = ",", scientific = F))
    
    # Format caption
    caption <- tags$div(HTML(caption))  
    
    # Get display name
    display_name <- unique(connect_totals$title)
    
    ## Log transform variable
    plot_totals <- plot_totals %>%
      dplyr::filter(subs > 0 & !is.na(subs)) %>%
      mutate(subs = log10(subs))
    
  }
  
  # Make sure we still have data
  req(nrow(plot_totals) > 0)
  
  # Rasterize
  plot_totals_raster <- rasterFromXYZ(plot_totals[, c('lon_cen', 'lat_cen', plot_variable)],
                                                  res = c(0.1,0.1))
  crs(plot_totals_raster) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # Create palette
  pal <- colorBin(legend_options, domain = seq(0, 6), bins = 5, na.color = "transparent", pretty = T)
  
  return(list(r = plot_totals_raster,
         pal = pal,
         pal_title = legend_name,
         caption = caption,
         caption_ggplot = caption_ggplot,
         display_name = display_name))
    
}

### ----------------------------------------------------
### Plotting function - EEZ level maps (ggplot version with rasters)
### ----------------------------------------------------

EEZPlotRaster <- function(region_dat,
                          plot_raster = "effort_all_raster",
                          plot_variable = "fishing_KWh",
                          input_selected_eez,
                          input_selected_flag_state,
                          eez_sf,
                          land_sf,
                          map_theme){
  
  req(!is.null(region_dat[[plot_raster]]))

    ### Get limits for map area
    x_lim <- c(region_dat$eez_bb$xmin - 1, region_dat$eez_bb$xmax + 1)
    y_lim <- c(region_dat$eez_bb$ymin - 1, region_dat$eez_bb$ymax + 1)

    raster_extent <- bbox(region_dat[[plot_raster]]$r)
    raster_x_lim <- c(raster_extent[1,1] - 1, raster_extent[1,2] - 1)
    raster_y_lim <- c(raster_extent[2,1] - 1, raster_extent[2,2] - 1)
    
    if(x_lim[1] > (raster_x_lim[1] + 90)){
      
      x_lim <- x_lim - 360
    
    }
    
    ### Scale
    if(plot_variable == "fishing_KWh"){

      legend_name = "Fishing effort \n(kWh)"
      legend_options = "A"

      ### Get legend limits
      scale_breaks <- seq(0, 6)
      scale_labels <- format(round(10^scale_breaks, 0), big.mark = ",", scientific = F)

    }else{

      legend_name = "Capacity-Enhancing Subsidies\n(2018 $US)"
      legend_options = "D"
      
      ### Get legend limits
      scale_breaks <- seq(0,6)
      scale_labels <- format(round(10^scale_breaks, 0), big.mark = ",", scientific = F)

    }
    
    plot_spdf <- as(region_dat[[plot_raster]]$r, "SpatialPixelsDataFrame")
    plot_df <- as.data.frame(plot_spdf)
    colnames(plot_df) <- c("value", "x", "y")
    
    caption <- region_dat[[plot_raster]]$caption_ggplot

      # Map
      plot <- ggplot()+
        geom_tile(data = plot_df, aes(x=x, y=y, fill=value), alpha=0.8)+
        scale_fill_gradientn(colors = viridis_pal(option = legend_options)(9),
                             breaks = scale_breaks,
                             limits = c(min(scale_breaks), max(scale_breaks)),
                             labels = scale_labels,
                             name = legend_name)+
        geom_sf(data = land_sf, fill = "#fafaf8", color = "#f4ebeb", size = 0.5)+ # world countries
        geom_sf(data = eez_sf, fill = NA, color = "grey60", size = 0.5)+ # world EEZs
        guides(fill = guide_colourbar(title.position = "bottom", title.hjust = 0.5, barwidth = 20, ticks.colour = "black", frame.colour = "black"))+
        labs(x = "", y = "")+
        coord_sf(xlim = x_lim, ylim = y_lim) +
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(expand = c(0,0))+
        map_theme+
        labs(caption=caption)+
        theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

    return(list(plot = plot + theme(legend.position = "none"),
                legend = cowplot::get_legend(plot)))

}

### ----------------------------------------------------
### Wrapper - Building Static Maps for Download
### ----------------------------------------------------

EEZPlotDownloadWrapper <- function(region_dat,
                                   plot_raster_type = "effort",
                                   plot_variable = "fishing_KWh",
                                   input_selected_eez,
                                   input_selected_flag_state,
                                   eez_sf,
                                   land_sf,
                                   map_theme,
                                   is_hs = F){
  
  if(!is_hs){
    
    title <- paste0("Distant Water ", str_to_title(plot_raster_type), " | EEZ of ", region_dat[[paste0(plot_raster_type, "_all_raster")]]$display_name)
  
  }else{
    
    title <- paste0("Distant Water ", str_to_title(plot_raster_type), " | High Seas ", region_dat[[paste0(plot_raster_type, "_all_raster")]]$display_name)

  }
  
  # Subsidy Plot (all flag states)
  plot1 <- EEZPlotRaster(region_dat = region_dat,
                         plot_raster = paste0(plot_raster_type, "_all_raster"),
                         plot_variable = plot_variable,
                         input_selected_eez = input_selected_eez,
                         input_selected_flag_state = input_selected_flag_state,
                         eez_sf = eez_sf,
                         land_sf = land_sf,
                         map_theme = map_theme)
  
  #east_asia_pacific_rv$effort_legend <- plot1$legend
  
  # Make legend
  legend <- ggdraw(plot1$legend)
  
  # Subsidy Plot (selected flag state)
  if(input_selected_flag_state != "Select a flag state..."){
    
    plot2 <- EEZPlotRaster(region_dat = region_dat,
                           plot_raster = paste0(plot_raster_type, "_selected_raster"),
                           plot_variable = plot_variable,
                           input_selected_eez = input_selected_eez,
                           input_selected_flag_state = input_selected_flag_state,
                           eez_sf = eez_sf,
                           land_sf = land_sf,
                           map_theme = map_theme)
    
    # Combine into figure
    top_row <- plot_grid(
      plot1$plot + labs(title = title,
                        subtitle = "All DW Vessels"), 
      
      plot2$plot + labs(title = "",
                        subtitle = paste0("Flag state: ", input_selected_flag_state)),
      
      align = "h", axis = "bt", rel_widths = c(1, 1))
    
    plot <- plot_grid(top_row,
                      legend,
                      nrow = 2,
                      rel_heights = c(1, 0.3))
    
  }else{
    
    # Combine into figure
    plot <- plot_grid(plot1$plot + labs(title = title,
                                        subtitle = "All DW Vessels") +
                        theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)),
                      legend,
                      nrow = 2,
                      rel_heights = c(1, 0.3))
    
  }
  
  return(plot)

}
