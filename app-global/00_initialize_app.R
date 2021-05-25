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

# region_names_long <- c("East Asia & Pacific" = "east_asia_pacific",
#                   "Europe & Central Asia" = "europe_central_asia",
#                   "Latin America & Caribbean" = "latin_america_caribbean",
#                   "Middle East & North Africa" = "middle_east_north_africa",
#                   "North America" = "north_america",
#                   "South Asia" = "south_asia",
#                   "Sub-Saharan Africa" = "sub_saharan_africa")

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

# 5) EEZ / FAO Region Lookup Table
fao_regions_by_eez <- read_csv("./data/fao_regions_by_eez_ter_id.csv")

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

# 6) High Seas Region Polygons - merged by area
fao_area_360 <- st_read("./data/fao_high_seas_areas_neg360_360_centroids.gpkg")

# 7) High Seas Region Polygon - merged by region
fao_region_360 <- st_read("./data/high_seas_region_neg360_360.gpkg")

# 8) FAO / Flag State Connectivity Lines
fao_flag_state_connectivity <- st_read("./data/fao_flag_state_connectivity_lines.gpkg") %>%
  mutate(admin = case_when(flag_iso3 == "TUV" ~ "Tuvalu",
                           TRUE ~ admin))
### --------------------

### Widget choices that are shared across all pages -----
vessel_origins_fill_choices <- c("# of vessels" = "n_vessels", 
                                   "Total vessel capacity (kW)" = "tot_engine_power",
                                   "Total vessel tonnage (gt)" = "tot_tonnage",
                                   "Total fishing effort (hours)" = "fishing_hours",
                                   "Total fishing effort (kW hours)" = "fishing_KWh")

vessel_origins_fill_scale <- c("All distant water fishing in the region (default)" = "region",
                               "Selected coastal state only" = "selected_eez")

vessel_origins_fill_scale_hs <- c("All distant water fishing on the high seas (default)" = "region",
                                  "Selected FAO area only" = "selected_eez")

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

hs_pal <- list(dark_col = "#1783C3",
               light_col = "#67C6FF")

### Manual zoom/extent corrections -----------

manual_eez <- list("CAN" = c("lat" = NA, "lon" = NA, "zoom" = 1),
                   "FRA" = c("lat" = 24, "lon" = -58, "zoom" = 1),
                   "GBR" = c("lat" = NA, "lon" = NA, "zoom" = 1),
                   "GRL" = c("lat" = NA, "lon" = NA, "zoom" = 1),
                   "NOR" = c("lat" = NA, "lon" = NA, "zoom" = 2),
                   "RUS" = c("lat" = 71, "lon" = 70, "zoom" = 1),
                   "SJM" = c("lat" = NA, "lon" = NA, "zoom" = 2),
                   "USA" = c("lat" = 40, "lon" = -112, "zoom" = 1))

manual_region <- list("61" = c("lat" = NA, "lon" = 175, "zoom" = 1),
                      "71" = c("lat" = NA, "lon" = 180, "zoom" = 1),
                      "81" = c("lat" = NA, "lon" = 195, "zoom" = 1),
                      "88" = c("lat" = NA, "lon" = 200, "zoom" = 1))

