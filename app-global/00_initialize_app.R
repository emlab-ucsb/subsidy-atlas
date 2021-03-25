### -------------------------------------------------------------------
### Distant Water Fishing Atlas
### An interactive toolkit to learn more about distant water fishing activity and subsidies
### 
### Creator(s): Kat Millage and Matt Warham
### Release date (v1): July 2019
### Release date (v2): March 2021
### 
### This script loads data needed for the app and performs some final data wrangling
### --------------------------------------------------------------------

# Should the EEZs of EU countries be visualized as a unit
merge_EU <- FALSE

eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")

### Load GeoPackages 

# 1) EEZ Polygons - Merged by territory
# Note this file contains duplicate polygons for disputed and joint areas attributed to each territory individually - additionally, all non-contiguous EEZ regions have been unionized for each territory. Also contains a merged EU polygon. 
eez_ter_360 <- st_read("./data/world_eez_ter_neg360_360_subsidy_atlas.gpkg")

# 2) EEZ Polygons - Merged by region
eez_region_360 <- st_read("./data/world_eez_regions_neg360_360.gpkg")

# 3) Country Polygons
# Contains a merged EU polygon. 
land_ter_360 <- st_read("./data/ne_50m_admin_neg360_360.gpkg")

# 4) EEZ / Flag State Connectivity Lines
eez_flag_state_connectivity <- st_read("./data/eez_flag_state_connectivity_lines.gpkg")

### Deal with the EU
if(merge_EU){
  
  eez_ter_360 <- eez_ter_360 %>%
    dplyr::filter(!(eez_ter_iso3 %in% eu_countries) | pol_type != "200NM")
  
  land_ter_360 <- land_ter_360 %>%
    dplyr::filter(!(admin_iso3 %in% eu_countries))
  
  eez_flag_state_connectivity <- eez_flag_state_connectivity %>%
    dplyr::filter(!(eez_ter_iso3 %in% eu_countries) | pol_type != "200NM")

}else{
  
  eez_ter_360 <- eez_ter_360 %>%
    dplyr::filter(eez_ter_iso3 != "EU")
  
  land_ter_360 <- land_ter_360 %>%
    dplyr::filter(admin_iso3 != "EU")
  
  eez_flag_state_connectivity <- eez_flag_state_connectivity %>%
    dplyr::filter(eez_ter_iso3 != "EU")
  
}

### Widget choices that are shared across all pages -----
vessel_origins_fill_choices <- c("# of vessels" = "n_vessels", 
                                   "Total vessel capacity (kW)" = "tot_engine_power",
                                   "Total vessel tonnage (gt)" = "tot_tonnage",
                                   "Total fishing effort (hours)" = "fishing_hours",
                                   "Total fishing effort (kW hours)" = "fishing_KWh")

vessel_origins_fill_scale <- c("All distant water fishing in the region (default)" = "region",
                               "Selected coastal state only" = "selected_eez")

### Plot Themes ---------
eezmaptheme <- theme_minimal()+
  theme(#strip.background = element_rect(color = NA),
        #plot.background = element_rect(fill = "#d4dadc", color = NA),
        panel.background = element_rect(fill = "#d4dadc", color = NA),
        panel.grid.major = element_line(colour = "#d4dadc"),
        text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "#d4dadc", fill = NA),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0, unit = "cm"),
        legend.margin = margin(t = 0.1, r = 0, b = 0.2, l = 0, unit = "cm"),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.text = element_text(color = "black"))
