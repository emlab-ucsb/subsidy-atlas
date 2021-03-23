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
eez_ter_360 <- st_read("./data/world_eez_ter_neg360_360.gpkg")

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

}else{
  
  eez_ter_360 <- eez_ter_360 %>%
    dplyr::filter(eez_ter_iso3 != "EU")
  
}

### Widget choices that are shared across all pages -----
vessel_origins_fill_choices <- c("# of different vessels" = "n_vessels", 
                                   "Avg. engine capacity (kW)" = "mean_engine_power",
                                   "Avg. tonnage (gt)" = "mean_tonnage",
                                   "Avg. length (m)" = "mean_length",
                                   "Total fishing effort (hours)" = "fishing_hours",
                                   "Total fishing effort (kW hours)" = "fishing_KWh")

vessel_origins_fill_scale <- c("All distant water fishing in the region (default)" = "region",
                               "Selected coastal state only" = "selected_eez")

### Plot Themes ---------
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

### Data -----

# 1) CSV of ACP EEZ and ISO3 codes
ACP_codes <- read_csv("./data/ACP_eez_codes.csv") %>%
  mutate(flag = countrycode(territory_iso3, "iso3c", "country.name")) 

eez_regions <- ACP_codes %>%
  distinct(mrgid, region)

flag_regions <- ACP_codes %>%
  distinct(territory_iso3, region)

# 2) CSV of FAO memberships, RFMO memberships, and more information links
RFMO_links <- read_csv("./data/RMFO_links.csv")

# 3) Spatial data frame with connectivity lines linking countries and the EEZs in which their fleets fish
connectivity_data <- read_sf("./data/eez_results/ACP/eez_mapping_with_lines.shp") %>%
  rename(eez_code = eez_cod,
         eez_territory_iso3 = ez_tr_3,
         capacity = capacty,
         fishing_h = fshng_h,
         fishing_KWh = fshn_KW)

### Shapefiles -----

### 1) Simplified EEZ shapefile (-360 to 360 degrees: crop appropriately for each region)
eez_map <- read_sf(dsn = "./data/shapefiles_edit/World_EEZ_v10_SubsidyAtlasACP", layer = "eez_v10") %>%
  setNames(tolower(names(.))) %>%
  st_transform(crs = 4326) %>%
  left_join(eez_regions, by = "mrgid")

# Africa
africa_eez_map <- eez_map %>%
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) 

# Caribbean
caribbean_eez_map <- eez_map %>%
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE)

# Pacific
pacific_eez_map <- eez_map %>%
  st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) 

### 2) Simplified land shapefile 
land_map <- read_sf(dsn = "./data/shapefiles_edit/ne_50m_admin_SubsidyAtlasACP", layer="land_50m") %>%
  st_transform(crs = 4326) %>%
  group_by(admin_iso3) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(display_name = countrycode(admin_iso3, "iso3c", "country.name"))

# Africa
africa_land_map <- land_map %>%
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) 

# Caribbean
caribbean_land_map <- land_map %>%
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE)

# Pacific
pacific_land_map <- land_map %>%
  st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90), warn = FALSE) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) 

### 3) Simplified combined land/EEZ shapefile (-360 to 360 degrees: crop appropriately for each region)
land_eez_map <- read_sf(dsn = "./data/shapefiles_edit/EEZ_land_v2_201410_-360_360", layer = "EEZ_land_v2_201410_-360_360") %>%
  setNames(tolower(names(.))) %>%
  st_transform(crs = 4326) %>% 
  dplyr::select(iso3 = iso_3digit,
                country,
                geometry) %>%
  dplyr::filter(!is.na(iso3)) %>%
  mutate(display_name = countrycode(iso3, "iso3c", "country.name")) %>%
  left_join(flag_regions, by = c("iso3" = "territory_iso3"))

# Africa
africa_land_eez_map <- land_eez_map %>% 
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
  st_collection_extract(type = c("POLYGON"))

# Caribbean
caribbean_land_eez_map <- land_eez_map %>% 
  st_crop(c(xmin=-180, xmax=180, ymin=-90, ymax=90)) %>%
  st_collection_extract(type = c("POLYGON"))

# Pacific
pacific_land_eez_map <- land_eez_map %>% 
  st_crop(c(xmin=0, xmax=360, ymin=-90, ymax=90)) %>%
  st_collection_extract(type = c("POLYGON"))

flag_state_choices <- unique(connectivity_data$flag)
names(flag_state_choices) <- countrycode(flag_state_choices, "iso3c", "country.name")
names(flag_state_choices)[is.na(names(flag_state_choices))] <- "Unknown flag"

### Themes -----

