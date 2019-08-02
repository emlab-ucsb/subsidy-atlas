### ----------------------------------------------------------------------
# ACP Subsidy Atlas Shiny App 
# This app allows users to visualize distant-water fishing effort in the EEZs of ACP countries
#
### ----------------------------------------------------------------------

### ----------
### Section 1: Initialize app
### ----------

rm(list = ls())
set.seed(123)

##install


### Load packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)

library(tidyverse)
library(sf)
library(png)

library(sf) # spatial manipulation
library(lwgeom) # needed for curved lines
library(here) # path names
library(knitr) # knit document
library(bigrquery) # access GFW data
library(DBI) # access GFW data
library(countrycode) # country names 
library(tidyverse) # data manipulation
library(ggalt) # transform to robinson projection
library(RColorBrewer) # custom color palettes
library(scales) # scales for plotting
library(ggpubr) # plot arranging
library(gridExtra)
library(grid)
library(countrycode)


### Source UI files ###

# The user interface content for each tab is stored in a separate file - Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)

### Load data ###

# Load csv of ACP EEZ and iso3 codes
ACP_codes <- read_csv("./data/ACP_eez_codes.csv") %>%
  mutate(flag = countrycode(territory_iso3, "iso3c", "country.name")) 
  #na.omit()

# Load spatial data frame with lines linking countries and EEZs
connectivity_data <- read_sf("./data/eez_results/ACP/eez_mapping_with_lines.shp") 
   
### Shapefiles ###

# Custom EEZ shapefile
eez_shp <- read_sf(dsn = "./data/shapefiles_edit/World_EEZ_v10_custom_ACP", layer = "World_EEZ_v10_custom_ACP") %>%
  st_transform(crs = 4326)
  
# Simplified EEZ shapefile (with FAO regions for high seas)
eez_map <- read_sf(dsn = "./data/shapefiles_edit/eez_v10_fao_combined_simple", layer="eez_v10_fao_combined_simple") %>%
  dplyr::filter(is.na(zone)) %>%
  st_transform(crs = 4326) 

# Simplified land shapefile 
land_map <- read_sf(dsn = "./data/shapefiles_edit/world_happy_180", layer="world_happy_180") %>%
  st_transform(crs = 4326)

# Combined land/EEZ shapefile 
land_eez_map <- read_sf(dsn = "./data/shapefiles_edit/EEZ_land_union_v2_custom_ACP", layer = "EEZ_land_union_v2_custom_ACP") %>%
  st_transform(crs = 4326)


### Widget choice values that depend on a dataset ------------------------------
# Put this here so we only have to load datasets in one place
africa_eez_choices <- ACP_codes$mrgid[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)]  
names(africa_eez_choices) <- ACP_codes$flag[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)]

caribbean_eez_choices <- ACP_codes$mrgid[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)]  
names(caribbean_eez_choices) <- ACP_codes$flag[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)]

pacific_eez_choices <- ACP_codes$mrgid[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)]
names(pacific_eez_choices) <- ACP_codes$flag[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)]

flag_state_choices <- unique(connectivity_data$flag)
names(flag_state_choices) <- countrycode(flag_state_choices, "iso3c", "country.name")
names(flag_state_choices)[is.na(names(flag_state_choices))] <- "Unknown flag"
  
#Map theme

eezmaptheme <- theme_minimal()+
  theme(plot.background = element_rect(fill = "grey27", color = NA),
        panel.background = element_rect(fill = "grey27", color = NA),
        panel.grid.major = element_line(colour = "grey27"),
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "grey27", fill = NA),
        plot.margin = margin(t = 0, r = 0.1, b = 0, l = 0, unit = "cm"),
        legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0, unit = "cm"),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.text = element_text(color = "white"))
  
# 
# ACP_codes_caribbean <- ACP_codes %>%
#      filter(region == "Caribbean")
# 
# ACP_codes_pacific <- ACP_codes %>%
#      filter(region == "Pacific")
# 
 # Africa_choices <- unique(ACP_codes_africa$territory_iso3)
 # names(Africa_choices) <- unique(ACP_codes_africa)
# 
# Caribbean_choices <- unique(ACP_codes_caribbean$territory_iso3)
# names(Caribbean_choices) <- unique(ACP_codes_caribbean)
# 
# Pacific_choices <- unique(ACP_codes_pacific$territory_iso3)
# names(Pacific_choices) <- unique(ACP_codes_pacific)

### Widget choice values that are text heavy  ------------------------------
# Put this here so it's easier to edit text throughout

### ----------
### Section 2: UI
### ----------

ui <- shinyUI(
  dashboardPage(
    
    # Header bar
    dashboardHeader(title = "Global Atlas of Distant Water Fishing",
                    titleWidth = "350px",
                    
                    # SFG logo
                    tags$li(class = "dropdown",
                            a(href = 'http://sfg.msi.ucsb.edu/',
                              img(
                                src = 'sfg-logo-white.png',
                                title = "The Sustainable Fisheries Group",
                                height = "40px"
                              ), style = "padding-top:10px; padding-bottom:10px;"
                            )
                    ),
                    # emLab logo
                    tags$li(class = "dropdown",
                            a(href = 'http://emlab.msi.ucsb.edu/',
                              img(
                                src = 'emlab-logo-white.png',
                                title = "The Environmental Market Solutions Lab",
                                height = "40px"
                              ), style = "padding-top:10px; padding-bottom:10px;"
                            )
                    )
    ), 
                    
   # Sidebar menu
   dashboardSidebar(width = "250px",
                    sidebarMenu(id = "tabs",
                                
                                # Introduction                     
                                menuItem("Introduction", 
                                         tabName = "introduction", 
                                         icon = NULL,
                                         selected = TRUE),
                                
                                # Regional Map
                                menuItem("Select a region",
                                         tabName = "selectregion",
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Africa
                                menuItem("Africa", 
                                         tabName = "africa", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Caribbean
                                menuItem("Caribbean", 
                                         tabName = "caribbean", 
                                         icon = NULL,
                                         selected = NULL),
                                
                                # Pacific Islands
                                menuItem("Pacific", 
                                         tabName = "pacific", 
                                         icon = NULL,
                                         selected = NULL)
                                
                                # #Leaflet
                                # menuItem("Leaflet EEZ",
                                #          tabName = 'leaflet_EEZ',
                                #          icon = NULL, 
                                #          selected = NULL),
                                # 
                                # 
                                # # EEZ
                                # menuItem("EEZ",
                                #          tabName = "EEZ",
                                #          icon = NULL,
                                #          selected = NULL)

                    ) # close sidebarMenu
                    
   ), # close dashboardSidebar
   
   # Main panel
   dashboardBody(
     
     # Custom stylesheet
     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ACP_custom.css")),
     
     # Tabs
     tabItems(
       
       # Introduction
       tabItem(tabName = "introduction",
               introduction()
       ),
       
       # Select a region
       tabItem(tabName = "selectregion",
               selectregion()
       ),
       
       # Africa
       tabItem(tabName = "africa",
               africa(africa_eez_choices, flag_state_choices)
       ),
       
       # Caribbean
       tabItem(tabName = "caribbean",
               caribbean(caribbean_eez_choices, flag_state_choices)
       ),
       
       # Pacific Islands
       tabItem(tabName = "pacific",
               pacific(pacific_eez_choices, flag_state_choices)
       )
       
       # #Leaflet EEZ
       # tabItem(tabName = "leaflet_EEZ",
       #         leaflet_EEZ(ACP_choices)
       # ),
       # 
       # #EEZ Connectivity
       # tabItem(tabName = "EEZ",
       #         EEZ(country_choices)
       #  )
       
     ) # close tabItems
   ) # close dashboardBody
   
  ) # close dashboardPage
) # close shinyUI
       

### ----------
### Section 3: Server
### ----------

server <- shinyServer(function(input, output, session) {
  
  ### Introduction ----------
  
  
  
  
  ### Select a region -----------
  
  ## Leaflet output: map of ACP countries aggregated by region
  output$regional_map <- renderLeaflet({
    
    regional_dat <- land_eez_map %>%
      group_by(region, rgn_spc) %>%
      summarize(geometry = st_union(geometry))
      
    
    leaflet("regional_map") %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      
      addPolygons(data = regional_dat, 
                  fillColor = "slateblue",
                  fillOpacity = 0.5,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = regional_dat$rgn_spc,
                  group = regional_dat$region) %>%
      addLayersControl(
        overlayGroups = regional_dat$rgn_spc,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(190,20, zoom = 1)
      
  })
  
  
  ### Based on where the user clicks on regional map, change tab
  observeEvent(input$regional_map_shape_click, {
    
    tab_navigation <- switch(input$regional_map_shape_click$group,
                             "Africa" = list("africa"),
                             "Caribbean" = list("caribbean"),
                             "Pacific" = list("pacific"))
    
    updateTabItems(session, "tabs", tab_navigation[[1]])
    
  })
  
  ### Africa ----------
  
  ### Map of African EEZs for which we have DW fishing effort
  output$africa_map <- renderLeaflet({
    
    # Filter data
    africa_eezs <- eez_shp %>%
      right_join(ACP_codes %>% 
                   dplyr::filter(region == "Africa"), by = c("mrgid"))
    
    # Map
    leaflet('africa_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = africa_eezs, 
                  fillColor = "slateblue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = africa_eezs$geoname,
                  layerId = africa_eezs$mrgid, #need this to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(15,-13, zoom = 2)
  
  })
  
  ### ----------------------------------------------------
  ### Africa: update EEZ selectInput based on click on map
  ### ----------------------------------------------------
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$africa_map_shape_click, {
    
    updateSelectizeInput(session, "africa_eez_select",
                         selected = input$africa_map_shape_click$id
    )
    
  })
  
  ### -----------------
  ### Africa: proxy map
  ### -----------------
  
  africa_proxy <- leafletProxy("africa_map")
  
  # When user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  observeEvent(input$africa_eez_select, {
    
    if(input$africa_eez_select == "Select an EEZ..."){
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      africa_proxy %>% setView(lng=15, lat=-13, zoom=2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_shp, eez_shp$mrgid == input$africa_eez_select)
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      africa_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = "seagreen",
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 0.3,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      #layerId = ACP_codes_filtered_africa$territory_iso3,
                                      group = "highlighted_eez") %>%
        setView(lng=selected_eez$x_1, lat=selected_eez$y_1, zoom=4)
    }
    
  }) # close observe event
  
  ### ------------------------------------------------
  ### Africa: summary statistics based on selected EEZ
  ### ------------------------------------------------
  
  output$africa_summary_text <- renderUI({
    
    req(input$africa_eez_select != "Select an EEZ...")
    
    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$africa_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3)
    
    # Total stats by EEZ
    
    total_stats_africa <- connectivity_data_filter_africa %>% 
      as.data.frame() %>% 
      select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>% 
      group_by(eez_cod, territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacty = sum(capacty, na.rm = T),
                fshng_h = sum(fshng_h, na.rm = T),
                fshn_KW = sum(fshn_KW, na.rm = T)) %>% 
      arrange(territory_iso3)
    
    
    ### Summary stats for EEZ 
    paste0("<h4>", total_stats_africa$eez_nam, "</h4>",
           "<b>", "Total # of Vessels in EEZ: ", "</b>", format(round(total_stats_africa$vessels, 0), big.mark = ","),
           "</br>",
           "<b>", "Total Fishing hours per year in EEZ: ", "</b>", format(round(total_stats_africa$fshng_h, 0), big.mark = ","), 
           "</br>",
           "<b>", "Total Fishing kwhr in EEZ: ", "</b>", format(round(total_stats_africa$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
  })
  
  ### ------------------------
  ### Africa: connectivity Map
  ### ------------------------
  
  output$africa_connection_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$africa_eez_select)
    req(input$africa_eez_select != "Select an EEZ...")
    
    selected_eez <- eez_shp %>% 
      dplyr::filter(mrgid == input$africa_eez_select)
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$africa_eez_select) %>% 
      arrange(flag)
    
    flag_states_for_selected_eez <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    #  Hover Text
    flag_state_summary <- connectivity_data_for_selected_eez %>% 
      mutate(name = countrycode(flag, "iso3c", "country.name"))
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>",  flag_state_summary$name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacty, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fshng_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    
    # Leaflet map
    leaflet('africa_connection_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = selected_eez, 
                  fillColor = "seagreen",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      
      addPolygons(data = flag_states_for_selected_eez,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_states_for_selected_eez$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      
      addPolylines(data = connectivity_data_for_selected_eez,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      
      setView(15, -13, zoom = 2)
  
  })
  
  ### --------------------------------------------------------------------------
  ### Africa: switch tab and update flag state selectInput based on click on map
  ### --------------------------------------------------------------------------
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$africa_connection_map_shape_click, {
    
    req(input$africa_connection_map_shape_click$id != input$africa_eez_select)
    
    updateTabItems(session, "africa_tabs", "Fishing effort and subsidy intensity of distant water vessels")
    
    updateSelectizeInput(session, "africa_flag_state_select",
                         selected = input$africa_connection_map_shape_click$id
    )
    
  })
  
  ### ------------------------------
  ### Africa: load eez specific data
  ### ------------------------------

  africa_eez_data <- eventReactive(input$africa_eez_select, {
    
    # Require EEZ selection
    req(input$africa_eez_select != "Select an EEZ...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$africa_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(africa_eez_choices[africa_eez_choices == input$africa_eez_select])) %>%
      arrange(name)
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -----------------------------------------------------------
  ### Africa: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$africa_eez_select, {
    
    req(input$africa_eez_select != "Select an EEZ...")
    
    flag_state_choices_africa <- unique(africa_eez_data()$flag)
    names(flag_state_choices_africa) <- unique(africa_eez_data()$name)
    
    updateSelectizeInput(session, "africa_flag_state_select",
                         choices = c("All flag states", flag_state_choices_africa)
    )
    
  })
  
  ### -------------------------
  ### Africa: subsidy heat map
  ### ------------------------
  
  output$africa_subsidy_map <- renderPlot({
    
    ### Get totals for data
    eez_totals <- africa_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$africa_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- africa_eez_data() %>%
        dplyr::filter(flag == input$africa_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$subs/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of subsidy intensity
      ggplot()+
          geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = subs/1e3))+
          scale_fill_viridis_c(na.value = NA, name = "Value of subsidies for fishing \n(2018 US$, thousands)", 
                              limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                              labels=scale_labels,
                              breaks=scale_labels,
                              oob=scales::squish)+
          geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
          geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
         labs(x = "", y = "")+
         coord_sf(xlim = x_lim, ylim = y_lim)+
         guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
         scale_x_continuous(expand = c(0,0))+
         scale_y_continuous(expand = c(0,0))+
        eezmaptheme
        
  })
  
  
  ### -------------------------
  ### Africa: effort heat map
  ### ------------------------
  
  output$africa_effort_map <- renderPlot({
    
    ### Get totals for data
    eez_totals <- africa_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$africa_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- africa_eez_data() %>%
        dplyr::filter(flag == input$africa_flag_state_select) %>%
        group_by(lon_cen, lat_cen) %>%
        summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                  fishing_KWh = sum(fishing_KWh, na.rm = T),
                  subs = sum(subs, na.rm = T)) %>%
        mutate(subsidy_intensity = subs/fishing_KWh)
      
    }
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_plot_data$fishing_KWh/1e3, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Map of fishing effort
    ggplot()+
      geom_tile(data = eez_plot_data, aes(x = lon_cen, y = lat_cen, width = 0.1, height = 0.1, fill = fishing_KWh))+
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  
  
    
  output$africa_flag_summary_text <- renderUI({
    
    req(input$africa_eez_select != "Select an EEZ...")
    
    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$africa_eez_select) 
    
   
    
    req(!is.null(input$africa_connection_map_shape_click$id))
    
    connectivity_data_filter_africa <-  connectivity_data_filter_africa %>%
      dplyr::filter(flag == input$africa_connection_map_shape_click$id)
      
   ### Summary stats for EEZ 
    
    # if (is.null(click)) return()
    paste0("<b>","EEZ: ", #connectivity_data_filter_africa$territory_iso3,
           "<br/>",
           "<b>", "Total # of Vessels in EEZ: ", format(round(connectivity_data_filter_africa$vessels, 0), big.mark = ","),
           "</br>",
           "<b>", "Total Fishing hours per year in EEZ: ", format(round(connectivity_data_filter_africa$fshng_h, 0), big.mark = ","), 
           "</br>",
           "<b>", "Total Fishing kwhr in EEZ: ", format(round(connectivity_data_filter_africa$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
  })
  
  ## Flag subsidy map
  
  output$africa_flag_subsidy_map <- renderPlot({
    
    req(input$africa_eez_select != "Select an EEZ...")
    
    req(!is.null(input$africa_connection_map_shape_click$id))
    
    
    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$africa_eez_select]
    
    eez_effort_africa <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn") 
    
    eez_effort_flag <- eez_effort_africa %>% 
      filter(flag == input$africa_connection_map_shape_click$id)
      
    
    
    
    #  ### Make Plots 
    #  # Get quantiles of data for scale limits
    eez_effort_aggregated_africa <- eez_effort_flag %>%
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    #  
    # # # # Quantiles
    intensity_quantile <- quantile(eez_effort_aggregated_africa$subs, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    #  
    # # # # Set map limits for filtering global country map and eez map appropriately
    x_lim <- c(min(eez_effort_aggregated_africa$lon_cen) - 0.5, max(eez_effort_aggregated_africa$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_africa$lat_cen) - 0.5, max(eez_effort_aggregated_africa$lat_cen) + 0.5)
    #  
    
    # ## Map of effort intensity
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_effort_aggregated_africa, aes(x = lon_cen, y = lat_cen, fill = subs))+
      scale_fill_viridis_c(na.value = NA, name = "Subsidy intensity \n(2018 $USD/KWh)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
    
    ### Summary stats for EEZ 
    
    
  })
  
  ## Flag effort Map
  
  output$africa_flag_effort_map <- renderPlot({
    
    req(input$africa_eez_select != "Select an EEZ...")
    
    req(!is.null(input$africa_connection_map_shape_click$id))
    
    
    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$africa_eez_select]
    
    eez_effort_africa <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn") 
    
    eez_effort_flag <- eez_effort_africa %>% 
      filter(flag == input$africa_connection_map_shape_click$id)
    
    
    
    # #Gemerate plot
    # 
    #  ### Make Plots 
    #  # Get quantiles of data for scale limits
    eez_effort_aggregated_africa <- eez_effort_flag %>%
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    #  
    # # # # Quantiles
    intensity_quantile <- quantile(eez_effort_aggregated_africa$subs, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    #  
    # # # # Set map limits for filtering global country map and eez map appropriately
    x_lim <- c(min(eez_effort_aggregated_africa$lon_cen) - 0.5, max(eez_effort_aggregated_africa$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_africa$lat_cen) - 0.5, max(eez_effort_aggregated_africa$lat_cen) + 0.5)
    #  
    
    # ## Map of effort intensity
    
    # Map of effort intensity
    # Labels for the log scale 
    ggplot()+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      geom_tile(data = eez_effort_aggregated_africa, aes(x = lon_cen, y = lat_cen, fill = fishing_KWh))+
      #eezmaptheme +
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
    
    ### Summary stats for EEZ 
    
    
  })
    
  
  #       # if (is.null(click)) return()
  #  paste0("<b>","State: ", input$africa_connection_map_shape_click$id,
  #          "<br/>",
  #          "<b>", "# of Vessels: ", format(round(connectivity_data_filter_africa$vessels, 0), big.mark = ","),
  #          "</br>",
  #          "<b>", "Fishing hours per year in EEZ: ", format(round(connectivity_data_filter_africa$fshng_h, 0), big.mark = ","), 
  #          "</br>",
  #          "<b>", "Fishing kwhr in EEZ: ", format(round(connectivity_data_filter_africa$fshn_KW, 0), big.mark = ",")) %>% 
  #     lapply(htmltools::HTML)
  #   
  # })
  
  
   #close render leaflet
  
  ###------------
  ### Caribbean 
  ###-----------
  ### Map of Caribbean EEZs for which we have DW fishing effort
  output$caribbean_map <- renderLeaflet({
    
    # Filter data
    caribbean_eezs <- eez_shp %>%
      right_join(ACP_codes %>% dplyr::filter(region == "Caribbean"), by = c("mrgid"))
    
    # Map
    leaflet('caribbean_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = caribbean_eezs, 
                  fillColor = "seagreen",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = caribbean_eezs$geoname,
                  layerId = caribbean_eezs$mrgid, # need this to link to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(290,20, zoom = 3)
    
  }) # Close render leaflet
  
  ###----------------------
  ### Caribbean: update EEZ selectInput based on click on map
  ###-----------------------
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$caribbean_map_shape_click, {

    updateSelectizeInput(session, "caribbean_eez_select",
                          selected = input$caribbean_map_shape_click$id
    )

  }) # Close observe event
  
  ###--------------------
  ### Caribbean: Proxy Map
  ###--------------------
  
  ### Leaflet proxy: when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  
  caribbean_proxy <- leafletProxy("caribbean_map")
  
  observeEvent(input$caribbean_eez_select, {
    
    if(input$caribbean_eez_select == "Select an EEZ..."){
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      caribbean_proxy %>% setView(lng=290, lat=20, zoom=3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_shp, eez_shp$mrgid == input$caribbean_eez_select) %>%
        mutate(x_1 = ifelse(x_1 < 0, 360 + x_1, x_1))
      
      # Remove any previously highlighted polygon
      caribbean_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      caribbean_proxy %>% addPolygons(data = selected_eez,
                                   fillColor = "darkred",
                                   fillOpacity = 1,
                                   color= "white",
                                   weight = 0.3,
                                   highlight = highlightOptions(weight = 5,
                                                                color = "#666",
                                                                fillOpacity = 1,
                                                                bringToFront = TRUE),
                                   group = "highlighted_eez") %>%
        setView(lng=selected_eez$x_1, lat=selected_eez$y_1, zoom=4)
    }
    
  }) # Close observe event
  
  
  ###-----------------
  ### Caribbean: Summary statistics based on selected EEZ
  ###-----------------
  
  output$caribbean_summary_text <- renderUI({
    
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    connectivity_data_filter_caribbean <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$caribbean_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3)
    
    # Total Stats by EEZ
    
    total_stats_caribbean <- connectivity_data_filter_caribbean %>% 
      as.data.frame() %>% 
      select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>% 
      group_by(eez_cod, territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacty = sum(capacty, na.rm = T),
                fshng_h = sum(fshng_h, na.rm = T),
                fshn_KW = sum(fshn_KW, na.rm = T)) %>% 
      arrange(territory_iso3)
    
    ### Summary stats for EEZ
    
    ### Summary stats for EEZ 
    paste0("<h4>", total_stats_caribbean$eez_nam, "</h4>",
           "<b>", "Total # of Vessels in EEZ: ", "</b>", format(round(total_stats_caribbean$vessels, 0), big.mark = ","),
           "</br>",
           "<b>", "Total Fishing hours per year in EEZ: ", "</b>", format(round(total_stats_caribbean$fshng_h, 0), big.mark = ","), 
           "</br>",
           "<b>", "Total Fishing kwhr in EEZ: ", "</b>", format(round(total_stats_caribbean$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    
  })
  
  ###---------------------
  ##Caribbean: Connectivity map
  ###---------------------
  
  output$caribbean_connection_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$caribbean_eez_select)
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    
    selected_eez <- eez_shp %>% 
      dplyr::filter(mrgid == input$caribbean_eez_select)
    
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$caribbean_eez_select) %>% 
      arrange(flag)
    
    flag_states_for_selected_eez <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    
    #  Hover Text
    
    #  Hover Text
    flag_state_summary <- connectivity_data_for_selected_eez %>% 
      mutate(name = countrycode(flag, "iso3c", "country.name"))
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>",  flag_state_summary$name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacty, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fshng_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    
    # flag_subsidy_atlas_text_caribbean <- paste0(
    #   "<b>","State: ",  country_map_filtered_caribbean$cntry_l,
    #   "<br/>",
    #   "<b>", "# of Vessels: ", connectivity_data_filter_caribbean$vessels %in% country_map_filtered_caribbean$flag, #format(round(connectivity_data_filter_africa$vessels, 0), big.mark = ","), # Not matching up
    #   "</br>",
    #   "<b>", "Capacity of fishing vessels: ",  format(round(connectivity_data_filter_caribbean$capacty, 0), big.mark = ","), # not matching up
    #   "</br>",
    #   "<b>", "Fishing hours per year in EEZ: ",  format(round(connectivity_data_filter_caribbean$fshng_h, 0), big.mark = ","), # not matching up
    #   "</br>",
    #   "<b>", "Fishing kwhr in EEZ: ", format(round(connectivity_data_filter_caribbean$fshn_KW, 0), big.mark = ","))  %>% # not matching up
    #   lapply(htmltools::HTML)
    
    #Leaflet map
    
    leaflet('caribbean_connection_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = selected_eez, 
                  fillColor = "seagreen",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
      #setView()) %>%
      
      addPolygons(data = flag_states_for_selected_eez,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_states_for_selected_eez$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_for_selected_eez,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(-75,20, zoom = 1.75)
  })
  
  ### -----------------------------------------------------------
  ### Caribbean: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$caribbean_eez_select, {
    
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    connectivity_data_for_selected_eez <- connectivity_data %>%
      dplyr::filter(eez_cod == input$caribbean_eez_select) %>% 
      arrange(flag)
    
    flag_state_choices_caribbean <- connectivity_data_for_selected_eez$flag
    names(flag_state_choices_caribbean) <- countrycode(flag_state_choices_caribbean, "iso3c", "country.name")
    
    updateSelectizeInput(session, "caribbean_flag_state_select",
                         choices = c("All flag states", flag_state_choices_caribbean)
    )
    
  })
  
  ### -------------------------------------------------
  ### caribbean: change tab with click on connectivity map
  ### -------------------------------------------------
  
  # observeEvent(input$caribbean_connection_map_shape_click, {
  #   
  #   
  #   
  #   tab_navigation <- switch(input$regional_map_shape_click$group,
  #                            "Africa" = list("africa"),
  #                            "Caribbean" = list("caribbean"),
  #                            "Pacific" = list("pacific"))
  #   
  #   updateTabItems(session, "tabs", tab_navigation[[1]])
  #   
  # })
  
  
  ### ----------------------------------------------------
  ### caribbean: switch tab and update flag state selectInput based on click on map
  ### ----------------------------------------------------
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$caribbean_connection_map_shape_click, {
    
    req(input$caribbean_connection_map_shape_click$id != input$caribbean_eez_select)
    
    updateTabItems(session, "caribbean_tabs", "Fishing effort and subsidy intensity of distant water vessels")
    
    updateSelectizeInput(session, "caribbean_flag_state_select",
                         selected = input$caribbean_connection_map_shape_click$id
    )
    
  })
  
  
  ## Heat maps
  
  output$caribbean_subsidy_map <- renderPlot({
    
    # Require EEZ selection
    req(input$caribbean_eez_select)
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    
    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$caribbean_eez_select]
    
    eez_effort_caribbean <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")  
    
    ### Generate plot
    
    # Filter data for selected flag state(s) and aggregate
    eez_effort_aggregated_caribbean <- eez_effort_caribbean %>%
      # dplyr::filter(flag == input.???) ### MATT - Add filtering option here
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_effort_aggregated_caribbean$subsidy_intensity, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Set map limits appropriately
    x_lim <- c(min(eez_effort_aggregated_caribbean$lon_cen) - 0.5, max(eez_effort_aggregated_caribbean$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_caribbean$lat_cen) - 0.5, max(eez_effort_aggregated_caribbean$lat_cen) + 0.5)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_effort_aggregated_caribbean, aes(x = lon_cen, y = lat_cen, fill = subsidy_intensity))+
      scale_fill_viridis_c(na.value = NA, name = "Subsidy intensity \n(2018 $USD/KWh)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
    
    
  })
  
  output$caribbean_effort_map <- renderPlot({
    
    #Require EEZ selection
    req(input$caribbean_eez_select)
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    
    
    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$caribbean_eez_select]
    
    eez_effort_caribbean <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")   
    
    
    # #Gemerate plot
    # 
    #  ### Make Plots 
    #  # Get quantiles of data for scale limits
    eez_effort_aggregated_caribbean <- eez_effort_caribbean %>%
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    #  
    # # # # Quantiles
    intensity_quantile <- quantile(eez_effort_aggregated_caribbean$subsidy_intensity, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    #  
    # # # # Set map limits for filtering global country map and eez map appropriately
    x_lim <- c(min(eez_effort_aggregated_caribbean$lon_cen) - 0.5, max(eez_effort_aggregated_caribbean$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_caribbean$lat_cen) - 0.5, max(eez_effort_aggregated_caribbean$lat_cen) + 0.5)
    #  
    
    # ## Map of effort intensity
    
    
    # Labels for the log scale 
    ggplot()+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      geom_tile(data = eez_effort_aggregated_caribbean, aes(x = lon_cen, y = lat_cen, fill = fishing_KWh))+
      #eezmaptheme +
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  
  
  
  
  ### Based on click on regional map, change tab
  # observeEvent(c(input$regional_map_shape_click, 
  #                #condition2
  #                ) {
  #   
  #   filter connectivity dataset <- connectivity_dat %>%
  #       dpply::filter (country = )
  # 
  #   
  # })
  
  ### Leaflet Version of EEZ Connectivity Map
  ACP_codes %>% 
    dplyr::select(mrgid)
  
  output$leaflet_map <- renderLeaflet({
    
    #Require EEZ selection
    req(input$ACP_for_profile)
    
    #filter data
    
    # ACP_codes_filtered <- connectivity_data %>% # load this in up above
    #   dplyr::filter(eez_territory_iso3 == input$ACP_for_profile)
    
     # ACP_codes <- eez_map %>% 
     #   dplyr::filter(mrgid %in% ACP_codes$mrgid)
     
     ACP_codes_filtered <- eez_shp %>% 
       dplyr::filter(mrgid == input$ACP_for_profile)
     
     
     connectivity_data_filter_leaflet <- connectivity_data %>% # load this in up above
       dplyr::filter(eez_territory_iso3 == input$ACP_for_profile)
     
     country_map_filtered <- land_map %>% 
       dplyr::filter(iso3 %in% connectivity_data_filter_leaflet$flag)
     
     
    # connectivity_data_filter <- connectivity_data %>% # load this in up above
    #   dplyr::filter(eez_territory_iso3 == input$EEZ_for_profile)
    
    leaflet('leaflet_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = ACP_codes_filtered, 
                  fillColor = "slateblue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", ACP_codes_filtered$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
                  #setView()) %>%
      
      addPolygons(data = country_map_filtered,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", country_map_filtered$cntry_l, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_filter_leaflet,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(15,-36, zoom = 1.75)

    #setView(20,-2, zoom = 3)
    #addLegend("bottomright", pal = pal_global,
    #           values = log10(tot_subs$value),
    #           labels = round(tot_subs$value, 0),
    #           title = "Est. fisheries subsidies<br>(2018 $USD)",
    #           opacity = 1,
    #           labFormat = labelFormat(prefix = "$",
    #                                   transform = function(x) 10^(x)
    #           )
    #)
    
    
  })
  
  
  ### EEZ Connectivity Maps ---------
  
  output$connectivity_plot <- renderPlot({
    
    #Require EEZ selection
    req(input$EEZ_for_profile)
    
    #Require country selection for flag highlights
    #req(input$flag_for_profile)
    
    #Data
    connectivity_data_filter <- connectivity_data %>% # load this in up above
       dplyr::filter(eez_territory_iso3 == input$EEZ_for_profile)
    
    # ACP_lines <- connectivity_data_filter %>% 
    #     st_coordinates(st_cast(network_data_sf,"MULTILINESTRING")$geometry) %>% 
    #     as_data_frame() 
    
    
    #Data
    
   # iso3 %in% ACP_eez_dat$flag
    
    #iso3 %in% connectivity_data_AGO$flag
    
    
    #map
    ggplot(connectivity_data_filter)+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.1)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.1)+ # world countries (dark grey, white border lines)
      geom_sf(data = land_map %>% dplyr::filter(iso3 %in% connectivity_data_filter$flag), fill = "darkmagenta", alpha = 0.5, color = NA, size = 0.1) + # highlighted flag states (magenta)
      geom_sf(data = eez_shp %>% dplyr::filter(mrgid == input$EEZ_for_profile), fill = "slateblue", color = "grey40", size = 0.1) + # highlighted EEZ (slateblue, grey border lines)
      geom_sf(col = "darkgoldenrod", size = 0.25) +
      #maptheme+
      coord_sf(xlim = c(-180,180), ylim = c(-90,90))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
    
    #%>% dplyr::filter(eez_codes == input$EEZ_for_profile)
    
 ####Need to make countries and lines reactive   
    
    
  })
  
  
  
  
 
  
  ### Map of Pacific Island EEZs for which we have DW fishing effort--------
  
  
    output$pacific_map <- renderLeaflet({
    
    # Filter data
    pacific_eezs <- eez_shp %>%
      right_join(ACP_codes %>% dplyr::filter(region == "Pacific"), by = c("mrgid"))
    
    # Map
    leaflet('pacific_map') %>%
      #options = leafletOptions(worldCopyJump = TRUE) %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
      addPolygons(data = pacific_eezs, 
                  fillColor = "coral",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = pacific_eezs$geoname, 
                  layerId = pacific_eezs$mrgid, # need this to link to select input below
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")
      ) %>%
      setView(lng = 180, lat = 0, zoom = 2) 
    
  }) # Close render leaflet
  
  ###--------------
  ### Pacific: Update EEZ selectInput based on click on map
  ###---------------
  
  ### Register user clicks on map - change select input from widget
  observeEvent(input$pacific_map_shape_click, {
    
    updateSelectizeInput(session, "pacific_eez_select",
                         selected = input$pacific_map_shape_click$id
    )
    
  })
  
  ###-------------
  ### Pacific: proxy map
  ###-------------
  
  ### when user selects country either from the dropdown widget or by clicking on the map, highlight EEZ and change zoom
  pacific_proxy <- leafletProxy("pacific_map")
  
  observeEvent(input$pacific_eez_select, {
    
    if(input$pacific_eez_select == "Select an EEZ..."){
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      pacific_proxy %>% setView(lng=350, lat=17, zoom=3)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_shp, eez_shp$mrgid == input$pacific_eez_select) %>%
        mutate(x_1 = ifelse(x_1 < 0, 360 + x_1, x_1))
      
      # Remove any previously highlighted polygon
      pacific_proxy %>% clearGroup("highlighted_eez")
      
      # Add a different colored polygon on top of map
      pacific_proxy %>% addPolygons(data = selected_eez,
                                      fillColor = "darkred",
                                      fillOpacity = 1,
                                      color= "white",
                                      weight = 0.3,
                                      highlight = highlightOptions(weight = 5,
                                                                   color = "#666",
                                                                   fillOpacity = 1,
                                                                   bringToFront = TRUE),
                                      group = "highlighted_eez") %>%
        setView(lng=selected_eez$x_1, lat=selected_eez$y_1, zoom=2)
    }
    
  }) # Close observe event
  
  
  ###------------
  ### Pacific: summary statistics based on selected EEZ
  ###------------
  
  output$pacific_summary_text <- renderUI({
    
    req(input$pacific_eez_select != "Select an EEZ...")
    
    connectivity_data_filter_pacific <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$pacific_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3)
    
    # Total stats by EEZ
    
    total_stats_pacific <- connectivity_data_filter_pacific %>% 
      as.data.frame() %>% 
      select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>% 
      group_by(eez_cod, territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacty = sum(capacty, na.rm = T),
                fshng_h = sum(fshng_h, na.rm = T),
                fshn_KW = sum(fshn_KW, na.rm = T)) %>% 
      arrange(territory_iso3)
    
    ### Summary stats for EEZ
    
    paste0("<h4>", total_stats_pacific$eez_nam, "</h4>",
           "<b>", "Total # of Vessels in EEZ: ", "</b>", format(round(total_stats_pacific$vessels, 0), big.mark = ","),
           "</br>",
           "<b>", "Total Fishing hours per year in EEZ: ", "</b>", format(round(total_stats_pacific$fshng_h, 0), big.mark = ","), 
           "</br>",
           "<b>", "Total Fishing kwhr in EEZ: ", "</b>", format(round(total_stats_pacific$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
  })
  
    
  ###--------------
  ### Pacific: connectivity map
  ###--------------
    
    
  output$pacific_connection_map <- renderLeaflet({
    
    crs4326 <-  leafletCRS(
      crsClass = "L.CRS.EPSG3857")
    
    #Require EEZ selection
    req(input$pacific_eez_select)
    req(input$pacific_eez_select != "Select an EEZ...")
    
    #Data filter
    # ACP_codes <- eez_map %>% 
    #   dplyr::filter(mrgid %in% ACP_codes$mrgid)
    
    selected_eez <- eez_shp %>% 
      dplyr::filter(mrgid == input$pacific_eez_select)
    
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(eez_cod == input$pacific_eez_select) %>% 
      arrange(flag)
    
    flag_states_for_selected_eez <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    #  Hover Text
    
    flag_state_summary <- connectivity_data_for_selected_eez %>% 
      mutate(name = countrycode(flag, "iso3c", "country.name"))
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>",  flag_state_summary$name,
      "<br/>",
      "<b>", "# of DW vessels: ", "</b>", flag_state_summary$vessels,
      "</br>",
      "<b>", "Total capacity of DW vessels: ", "</b>", format(round(flag_state_summary$capacty, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (hours): ", "</b>",  format(round(flag_state_summary$fshng_h, 0), big.mark = ","), 
      "</br>",
      "<b>", "DW effort in selected EEZ (KW hours): ", "</b>", format(round(flag_state_summary$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    
    #Leaflet map
    
    leaflet('pacific_connection_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = selected_eez, 
                  fillColor = "coral",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = (paste0("<b>", selected_eez$geoname, "</b>") %>%
                             lapply(htmltools::HTML)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%  #,
      #setView()) %>%
      
      addPolygons(data = flag_states_for_selected_eez,
                  fillColor = "darkmagenta",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = flag_state_summary_text,
                  layerId = flag_states_for_selected_eez$flag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>% 
      addPolylines(data = connectivity_data_for_selected_eez,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      setView(-75,20, zoom = 1.75)
      #setView(lng = input$lng, lat = input$lat, zoom = 2)
  })
  
  ### -----------------------------------------------------------
  ### pacific: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$pacific_eez_select, {
    
    req(input$pacific_eez_select != "Select an EEZ...")
    
    connectivity_data_for_selected_eez <- connectivity_data %>%
      dplyr::filter(eez_cod == input$pacific_eez_select) %>% 
      arrange(flag)
    
    flag_state_choices_pacific <- connectivity_data_for_selected_eez$flag
    names(flag_state_choices_pacific) <- countrycode(flag_state_choices_pacific, "iso3c", "country.name")
    
    updateSelectizeInput(session, "pacific_flag_state_select",
                         choices = c("All flag states", flag_state_choices_pacific)
    )
    
  })
  
  ### -------------------------------------------------
  ### pacific: change tab with click on connectivity map
  ### -------------------------------------------------
  
  # observeEvent(input$pacific_connection_map_shape_click, {
  #   
  #   
  #   
  #   tab_navigation <- switch(input$regional_map_shape_click$group,
  #                            "Africa" = list("africa"),
  #                            "Caribbean" = list("caribbean"),
  #                            "Pacific" = list("pacific"))
  #   
  #   updateTabItems(session, "tabs", tab_navigation[[1]])
  #   
  # })
  
  
  ### ----------------------------------------------------
  ### pacific: switch tab and update flag state selectInput based on click on map
  ### ----------------------------------------------------
  
  ### Register user clicks on connectivity map - change select input from widget
  observeEvent(input$pacific_connection_map_shape_click, {
    
    req(input$pacific_connection_map_shape_click$id != input$pacific_eez_select)
    
    updateTabItems(session, "pacific_tabs", "Fishing effort and subsidy intensity of distant water vessels")
    
    updateSelectizeInput(session, "pacific_flag_state_select",
                         selected = input$pacific_connection_map_shape_click$id
    )
    
  })
  
  
  
  ## Heat maps
  
  output$pacific_subsidy_map <- renderPlot({
    
    # Require EEZ selection
    req(input$pacific_eez_select)
    req(input$pacific_eez_select != "Select an EEZ...")
    
    
    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$pacific_eez_select]
    
    eez_effort_pacific <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")  
    
    ### Generate plot
    
    # Filter data for selected flag state(s) and aggregate
    eez_effort_aggregated_pacific <- eez_effort_pacific %>%
      # dplyr::filter(flag == input.???) ### MATT - Add filtering option here
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    # Get data quntiles to set fil scale limit appropriately
    intensity_quantile <- quantile(eez_effort_aggregated_pacific$subsidy_intensity, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    
    # Set map limits appropriately
    x_lim <- c(min(eez_effort_aggregated_pacific$lon_cen) - 0.5, max(eez_effort_aggregated_pacific$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_pacific$lat_cen) - 0.5, max(eez_effort_aggregated_pacific$lat_cen) + 0.5)
    
    # Map of subsidy intensity
    ggplot()+
      geom_tile(data = eez_effort_aggregated_pacific, aes(x = lon_cen, y = lat_cen, fill = subsidy_intensity))+
      scale_fill_viridis_c(na.value = NA, name = "Subsidy intensity \n(2018 $USD/KWh)", 
                           limits=c(round(intensity_quantile[1],1)-round(intensity_quantile[1],1)*0.01, round(intensity_quantile[4], 1) + round(intensity_quantile[4], 1)*0.01), 
                           labels=scale_labels,
                           breaks=scale_labels,
                           oob=scales::squish)+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim)+
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
    
    
  })
  
  output$pacific_effort_map <- renderPlot({
    
    #Require EEZ selection
    req(input$pacific_eez_select)
    req(input$pacific_eez_select != "Select an EEZ...")
    
    
    
    # Find matching data file and load data
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$pacific_eez_select]
    
    eez_effort_pacific <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")   
    
    
    # #Gemerate plot
    # 
    #  ### Make Plots 
    #  # Get quantiles of data for scale limits
    eez_effort_aggregated_pacific <- eez_effort_pacific %>%
      group_by(year, eez_code, lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    #  
    # # # # Quantiles
    intensity_quantile <- quantile(eez_effort_aggregated_pacific$subsidy_intensity, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = T)
    scale_labels <- round(seq(round(intensity_quantile[1], 1), round(intensity_quantile[4], 1), length.out = 5), 1)
    #  
    # # # # Set map limits for filtering global country map and eez map appropriately
    x_lim <- c(min(eez_effort_aggregated_pacific$lon_cen) - 0.5, max(eez_effort_aggregated_pacific$lon_cen) + 0.5)
    y_lim <- c(min(eez_effort_aggregated_pacific$lat_cen) - 0.5, max(eez_effort_aggregated_pacific$lat_cen) + 0.5)
    #  
    
    # ## Map of effort intensity
    
    
    # Labels for the log scale 
    ggplot()+
      geom_sf(data = eez_map %>% dplyr::filter(is.na(zone)), fill = NA, color = "grey60", size = 0.5)+ # world EEZs (transparent, light grey border lines)
      geom_sf(data = land_map, fill = "grey2", color = "grey40", size = 0.5)+ # world countries (dark grey, white border lines)
      geom_tile(data = eez_effort_aggregated_pacific, aes(x = lon_cen, y = lat_cen, fill = fishing_KWh))+
      #eezmaptheme +
      scale_fill_viridis_c(na.value = NA, option = "A", name = "Fishing effort \n(KWh)", trans = log10_trans(), labels = comma)+
      labs(x = "", y = "")+
      coord_sf(xlim = x_lim, ylim = y_lim) +
      guides(fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = 18))+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      eezmaptheme
    
  })
  
  
  
    
  })
  
  


### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

