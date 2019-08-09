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

# Load csv of FAO RFMO codes and links
RFMO_links <- read_csv("./data/RMFO_links.csv")

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
africa_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])
names(africa_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])

caribbean_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])  
names(caribbean_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])

pacific_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])
names(pacific_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])

flag_state_choices <- unique(connectivity_data$flag)
names(flag_state_choices) <- countrycode(flag_state_choices, "iso3c", "country.name")
names(flag_state_choices)[is.na(names(flag_state_choices))] <- "Unknown flag"
  
#Map theme
eezmaptheme <- theme_minimal()+
  theme(strip.background = element_rect(fill = "#262626", color = NA),
        plot.background = element_rect(fill = "#262626", color = NA),
        panel.background = element_rect(fill = "#262626", color = NA),
        panel.grid.major = element_line(colour = "#262626"),
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "#262626", fill = NA),
        plot.margin = margin(t = 0.2, r = 0.1, b = 0, l = 0, unit = "cm"),
        legend.margin = margin(t = 0.1, r = 0, b = 0.2, l = 0, unit = "cm"),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.text = element_text(color = "white"))
  
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
  
  ###------------
  ### Africa ----------
  ###-----------------
  
  ### Map of African EEZs for which we have DW fishing effort
  output$africa_map <- renderLeaflet({
    
    # Filter data
    africa_eezs <- eez_shp %>%
      inner_join(ACP_codes %>% dplyr::filter(region == "Africa"), by = c("mrgid")) 
    africa_eezs$geoname[africa_eezs$geoname == "South African Exclusive Economic Zone (Prince Edward Islands)"] <- "South African Exclusive Economic Zone"
    
   africa_eezs_merged <- africa_eezs %>%
      group_by(territory_iso3, geoname) %>%
      summarize(geometry = st_union(geometry))
    
    # Map
    leaflet('africa_map') %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addPolygons(data = africa_eezs_merged, 
                  fillColor = "slateblue",
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = africa_eezs_merged$geoname,
                  layerId = africa_eezs_merged$territory_iso3, #need this to select input below
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
    
    if(input$africa_eez_select == "Select a coastal state..."){
      
      # Remove any previously highlighted polygon
      africa_proxy %>% clearGroup("highlighted_eez")  
      
      # Reset view to entire region
      africa_proxy %>% setView(lng=15, lat=-13, zoom=2)
      
    }else{
      
      # Get code for selected EEZ
      selected_eez <- subset(eez_shp, eez_shp$iso_ter1 == input$africa_eez_select)
      
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
        setView(lng=mean(selected_eez$x_1), lat=mean(selected_eez$y_1), zoom=4)
    }
    
  }) # close observe event
  
  ### ------------------------------------------------
  ### Africa: summary statistics based on selected EEZ
  ### ------------------------------------------------
  
  output$africa_summary_text <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")

    connectivity_data_filter_africa <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$africa_eez_select) %>% 
      rename(territory_iso3 = ez_tr_3) %>%
      mutate(eez_nam = str_replace(eez_nam, " \\(.*\\)", ""))
    
    # Total stats by coastal state
    total_stats_africa <- connectivity_data_filter_africa %>% 
      as.data.frame() %>% 
      select(c("eez_cod", "territory_iso3", "eez_nam","vessels", "capacty", "fshng_h", "fshn_KW")) %>% 
      group_by(territory_iso3, eez_nam) %>%
      summarize(vessels = sum(vessels, na.rm = T),
                capacty = sum(capacty, na.rm = T),
                fshng_h = sum(fshng_h, na.rm = T),
                fshn_KW = sum(fshn_KW, na.rm = T)) %>% 
      arrange(territory_iso3)
    
    ### Summary stats by coastal state
    paste0("<h4>", total_stats_africa$eez_nam, "</h4>",
           "<b>", "Total # of Vessels in EEZ: ", "</b>", format(round(total_stats_africa$vessels, 0), big.mark = ","),
           "</br>",
           "<b>", "Total Fishing hours per year in EEZ: ", "</b>", format(round(total_stats_africa$fshng_h, 0), big.mark = ","), 
           "</br>",
           "<b>", "Total Fishing kwhr in EEZ: ", "</b>", format(round(total_stats_africa$fshn_KW, 0), big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
  })
  
  ###------------
  ### Africa: Links to Online references
  ###------------
  
  output$africa_online_text <- renderUI({


    req(input$africa_eez_select != "Select a coastal state...")
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$africa_eez_select)

    ACP_fao_membership <- ACP_codes_links %>% 
      separate_rows(fao_memberships, sep = ",")
      
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    #browser()
    
    EEZ_info <- paste0(
      "<b>", a("FAO Country Profile" , href = unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)])),
      "</br>",
      "<b>", "Fishery Organization: ", a(unique(ACP_codes_links$fishery_org[!is.na(ACP_codes_links$fishery_org)]), href = unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])), 
       "</br>",
        "<b>", a("Treaties and Conventions", href = unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)])),
      "</br>",
      "<b>", a("Internal Fishing Access Agreements", href = unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]))) %>% 
      # "</br>",
      # "<b>", "FAO Memberships: ",
      # "</br>",
      # "<b>", "<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$link,"</a>") %>% 
       #"<b>", (a(RFMO_links_eez$rfmo_name, href = ,RFMO_links_eez$link,))) %>%  #RFMO_links_eez$rfmo_name, href = RFMO_links_eez$link), 
      lapply(htmltools::HTML)
    
    
     
  })
  
  ###------------
  ### Africa: Links to Online references RFMOs
  ###------------
  
  output$africa_RFMO_text <- renderUI({
    
    req(input$africa_eez_select != "Select a coastal state...")
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(territory_iso3 == input$africa_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>% 
      separate_rows(fao_memberships, sep = ",")
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    EEZ_RFMO_info <- paste0(
      #"<b>", (a(href =  ,RFMO_links_eez$link,  target = "_blank" ))) %>% 
      "<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>") %>% 
      lapply(htmltools::HTML)
    
  })
    
  
  ### ------------------------
  ### Africa: connectivity Map
  ### ------------------------
  
  output$africa_connection_map <- renderLeaflet({
    
    #Require coastal state selection
    req(input$africa_eez_select != "Select a coastal state...")
    
    selected_eez <- eez_shp %>% 
      dplyr::filter(iso_ter1 == input$africa_eez_select)
    
    connectivity_data_for_selected_eez <- connectivity_data %>% # load this in up above
      dplyr::filter(ez_tr_3 == input$africa_eez_select) %>% 
      arrange(flag) 
    
    flag_states_for_selected_eez <- land_map %>% 
      dplyr::filter(iso3 %in% connectivity_data_for_selected_eez$flag) %>% 
      rename(flag = iso3) %>% 
      arrange(flag)
    
    connectivity_data_edit <- connectivity_data_for_selected_eez %>%
      dplyr::filter(flag %in% flag_states_for_selected_eez$flag)
   
  # Connectivity stats with no geometry
  no_geometry <- connectivity_data_edit %>%
    group_by(ez_tr_3, flag) %>%
    summarize(vessels = sum(vessels, na.rm = T),
              capacty = sum(capacty, na.rm = T),
              fshng_h = sum(fshng_h, na.rm = T),
              fshn_KW = sum(fshn_KW, na.rm = T))
  st_geometry(no_geometry) <- NULL
   
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(no_geometry, by = "flag") %>%
      mutate(name = countrycode(flag, "iso3c", "country.name"))
    
    flag_state_summary_text <- paste0(
      "<b>", "Flag state: ", "</b>", flag_state_summary$name,
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
      
       
      
      addPolylines(data = connectivity_data_edit,
                   fillColor = "goldenrod",
                   fillOpacity = 1,
                   weight = 1,
                   color = "darkgoldenrod") %>% 
      
      setView(15, -13, zoom = 2)
  
  })
  
  ### -----------------------------------------------------------
  ### Africa: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$africa_eez_select, {
    
    req(input$africa_eez_select != "Select a coastal state...")
    
    connectivity_data_for_selected_eez <- connectivity_data %>%
      dplyr::filter(ez_tr_3 == input$africa_eez_select) %>% 
      arrange(flag) %>%
      dplyr::filter(flag != "BES")
    
    flag_state_choices_africa <- unique(connectivity_data_for_selected_eez$flag)
    names(flag_state_choices_africa) <- unique(countrycode(flag_state_choices_africa, "iso3c", "country.name"))
    
    updateSelectizeInput(session, "africa_flag_state_select",
                         choices = c("All flag states", flag_state_choices_africa))
    
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
    req(input$africa_eez_select != "Select a coastal state...")
    
    browser()
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    coastal_state_codes <- unique(str_replace(all_data_files, "\\_.*", ""))
    matching_file <- all_data_files[coastal_state_codes == input$africa_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "nnccnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(africa_eez_choices[africa_eez_choices == input$africa_eez_select])) %>%
      arrange(name)
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -------------------------
  ### Africa: subsidy heat map
  ### ------------------------
  
  output$africa_subsidy_map <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
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
  
  output$africa_effort_map <- renderPlot(bg = "#262626", {
    
    req(input$africa_eez_select != "Select a coastal state...")
    req(nrow(africa_eez_data()) > 0)
    
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
  

  ###------------
  ### Caribbean 
  ###-----------


  ### Caribbean ----------
  

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
  
  ###------------
  ### Caribbean: Links to Online references
  ###------------
  
  output$caribbean_online_text <- renderUI({
    
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(mrgid == input$caribbean_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>% 
      separate_rows(fao_memberships, sep = ",")
    
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    #browser()
    
    fao_country_profile <- a(href = ACP_codes_links$fao_country_profile)
    country_profile <- a(href = ACP_codes_links$country_profile)
    fishery_organization <- a(href = ACP_codes_links$fishery_org)
    #fao_memberships <- a("FAO Memberships:", (RFMO_links_eez$rfmo_name), href = RFMO_links_eez$link)
    #fisheries_subsidies <- a("Fishery Subsidy Information", href = ACP_codes_links$fisheries_subsidies)
    treaties_conventions <- a(href = ACP_codes_links$treaties_conventions)
    internal_fishing_acess_agreements <- a(href = ACP_codes_links$internal_fishing_access_agreements)
    
    EEZ_RFMO_info <- paste0(
      "<b>", a("FAO Country Profile" , href = ACP_codes_links$fao_country_profile),
      "</br>",
      "<b>", a("Country Profile", href = ACP_codes_links$country_profile),
      "</br>",
      "<b>", a("Fishery Organization:", ACP_codes_links$fishery_org, href = ACP_codes_links$fishery_org_link ), 
      "</br>",
      # "<b>", a("FAO Memberships", (RFMO_links_eez_rfmo_name), href = RFMO_links_eez$link), 
      # "</br>",
      "<b>", a("Treaties and Conventions", href = ACP_codes_links$treaties_conventions),
      "</br>",
      "<b>", a("Internal Fishing Access Agreements", href = ACP_codes_links$internal_fishing_access_agreements)) %>% 
      lapply(htmltools::HTML)
    
    
    
  })
  
  ###------------
  ### Caribbean: Links to Online references RFMOs
  ###------------
  
  output$caribbean_RFMO_text <- renderUI({
    
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(mrgid == input$caribbean_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>% 
      separate_rows(fao_memberships, sep = ",")
    
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    EEZ_RFMO_info <- paste0(
      #"<b>", (a(href =  ,RFMO_links_eez$link,  target = "_blank" ))) %>% 
      "<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>") %>% 
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
    
    #browser()
    no_geometry <- connectivity_data_for_selected_eez
    st_geometry(no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(no_geometry, by = "flag") %>%
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
  
  ### ------------------------------
  ### Caribbean: load eez specific data
  ### ------------------------------
  
  caribbean_eez_data <- eventReactive(input$caribbean_eez_select, {
    
    # Require EEZ selection
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$caribbean_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(caribbean_eez_choices[caribbean_eez_choices == input$caribbean_eez_select])) %>%
      arrange(name)
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -----------------------------------------------------------
  ### caribbean: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$caribbean_eez_select, {
    
    req(input$caribbean_eez_select != "Select an EEZ...")
    
    flag_state_choices_caribbean <- unique(caribbean_eez_data()$flag)
    names(flag_state_choices_caribbean) <- unique(caribbean_eez_data()$name)
    
    updateSelectizeInput(session, "caribbean_flag_state_select",
                         choices = c("All flag states", flag_state_choices_caribbean)
    )
    
  })
  
  
  
  ### -------------------------
  ### caribbean: subsidy heat map
  ### ------------------------
  
  output$caribbean_subsidy_map <- renderPlot(bg = "#262626", {
    
    ### Get totals for data
    eez_totals <- caribbean_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$caribbean_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- caribbean_eez_data() %>%
        dplyr::filter(flag == input$caribbean_flag_state_select) %>%
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
  ### caribbean: effort heat map
  ### ------------------------
  
  output$caribbean_effort_map <- renderPlot(bg = "#262626", {
    
    ### Get totals for data
    eez_totals <- caribbean_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$caribbean_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- caribbean_eez_data() %>%
        dplyr::filter(flag == input$caribbean_flag_state_select) %>%
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
  
  ###------------
  ### Pacific: Links to Online references
  ###------------
  
  output$pacific_online_text <- renderUI({
    
    req(input$pacific_eez_select != "Select an EEZ...")
    
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(mrgid == input$pacific_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>% 
      separate_rows(fao_memberships, sep = ",")
    
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    #browser()
    
    fao_country_profile <- a(href = ACP_codes_links$fao_country_profile)
    country_profile <- a(href = ACP_codes_links$country_profile)
    fishery_organization <- a(href = ACP_codes_links$fishery_org)
    #fao_memberships <- a("FAO Memberships:", (RFMO_links_eez$rfmo_name), href = RFMO_links_eez$link)
    #fisheries_subsidies <- a("Fishery Subsidy Information", href = ACP_codes_links$fisheries_subsidies)
    treaties_conventions <- a(href = ACP_codes_links$treaties_conventions)
    internal_fishing_acess_agreements <- a(href = ACP_codes_links$internal_fishing_access_agreements)
    
    EEZ_RFMO_info <- paste0(
      "<b>", a("FAO Country Profile" , href = ACP_codes_links$fao_country_profile),
      "</br>",
      "<b>", a("Country Profile", href = ACP_codes_links$country_profile),
      "</br>",
      "<b>", a("Fishery Organization:", ACP_codes_links$fishery_org, href = ACP_codes_links$fishery_org_link ), 
      "</br>",
      # "<b>", a("FAO Memberships", (RFMO_links_eez_rfmo_name), href = RFMO_links_eez$link), 
      # "</br>",
      "<b>", a("Treaties and Conventions", href = ACP_codes_links$treaties_conventions),
      "</br>",
      "<b>", a("Internal Fishing Access Agreements", href = ACP_codes_links$internal_fishing_access_agreements)) %>% 
      lapply(htmltools::HTML)
    
    
    
  })
  
  
  ###------------
  ### pacific: Links to Online references RFMOs
  ###------------
  
  output$pacific_RFMO_text <- renderUI({
    
    req(input$pacific_eez_select != "Select an EEZ...")
    
    ACP_codes_links <- ACP_codes %>%
      dplyr::filter(mrgid == input$pacific_eez_select)
    
    ACP_fao_membership <- ACP_codes_links %>% 
      separate_rows(fao_memberships, sep = ",")
    
    
    RFMO_links_eez <- RFMO_links %>%
      dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
    
    EEZ_RFMO_info <- paste0(
      #"<b>", (a(href =  ,RFMO_links_eez$link,  target = "_blank" ))) %>% 
      "<a href='", RFMO_links_eez$link,"' target='_blank'>", RFMO_links_eez$rfmo_name,"</a>") %>% 
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
    
    #browser()
    no_geometry <- connectivity_data_for_selected_eez
    st_geometry(no_geometry) <- NULL
    
    #  Hover Text
    flag_state_summary <- flag_states_for_selected_eez %>% 
      left_join(no_geometry, by = "flag") %>%
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
  
  
  
  ### ------------------------------
  ### Pacific: load eez specific data
  ### ------------------------------
  
  pacific_eez_data <- eventReactive(input$pacific_eez_select, {
    
    # Require EEZ selection
    req(input$pacific_eez_select != "Select an EEZ...")
    
    # Find matching data file and load
    all_data_files <- list.files(path = "./data/eez_results/ACP/", pattern = "*.csv")
    eez_numbers <- str_replace(all_data_files, "\\_.*", "")
    matching_file <- all_data_files[eez_numbers == input$pacific_eez_select]
    
    out <- read_csv(paste0("./data/eez_results/ACP/", matching_file), col_types = "iccnnnnn")
    
    clean_out <- out %>%
      dplyr::filter(year == 2018) %>%
      mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
      mutate(name = countrycode(flag, "iso3c", "country.name")) %>%
      dplyr::filter(name != names(pacific_eez_choices[pacific_eez_choices == input$pacific_eez_select])) %>%
      arrange(name)
    
    clean_out$name[is.na(clean_out$name)] <- "Unknown flag"
    
    clean_out
    
  })
  
  ### -----------------------------------------------------------
  ### pacific: filter flag state selectInput choices based on selected EEZ
  ### -----------------------------------------------------------
  
  observeEvent(input$pacific_eez_select, {
    
    req(input$pacific_eez_select != "Select an EEZ...")
    
    flag_state_choices_pacific <- unique(pacific_eez_data()$flag)
    names(flag_state_choices_pacific) <- unique(pacific_eez_data()$name)
    
    updateSelectizeInput(session, "pacific_flag_state_select",
                         choices = c("All flag states", flag_state_choices_pacific)
    )
    
  })
  
  ### -------------------------
  ### pacific: subsidy heat map
  ### ------------------------
  
  output$pacific_subsidy_map <- renderPlot(bg = "#262626", {
    
    ### Get totals for data
    eez_totals <- pacific_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$pacific_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- pacific_eez_data() %>%
        dplyr::filter(flag == input$pacific_flag_state_select) %>%
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
  ### pacific: effort heat map
  ### ------------------------
  
  output$pacific_effort_map <- renderPlot(bg = "#262626", {
    
    ### Get totals for data
    eez_totals <- pacific_eez_data() %>%
      group_by(lon_cen, lat_cen) %>%
      summarize(fishing_hours = sum(fishing_hours, na.rm = T),
                fishing_KWh = sum(fishing_KWh, na.rm = T),
                subs = sum(subs, na.rm = T)) %>%
      mutate(subsidy_intensity = subs/fishing_KWh)
    
    ### Get limits for map area
    x_lim <- c(min(eez_totals$lon_cen) - 0.5, max(eez_totals$lon_cen) + 0.5)
    y_lim <- c(min(eez_totals$lat_cen) - 0.5, max(eez_totals$lat_cen) + 0.5)
    
    ### Filter data for selected flag state(s) and aggregate
    if(input$pacific_flag_state_select == "All flag states"){
      
      eez_plot_data <- eez_totals
      
    }else{
      
      eez_plot_data <- pacific_eez_data() %>%
        dplyr::filter(flag == input$pacific_flag_state_select) %>%
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
  
  
})
  
  


### ----------
### Section 4: Run application
### ----------
shinyApp(ui = ui, server = server)

