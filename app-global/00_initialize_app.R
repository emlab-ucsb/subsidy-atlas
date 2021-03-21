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

### Load GeoPackages 
# Note the first contains duplicate polygons for disputed and joint areas attributed to each territory individually - additionally, all non-contiguous EEZ regions have been unionized for each territory. 
eez_ter_360 <- st_read("./data/world_eez_ter_neg360_360.gpkg")

eez_region_360 <- st_read("./data/world_eez_regions_neg360_360.gpkg")

### MOVE TO DATA PROCESSING SCRIPT EVENTUALLY
eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
  
eez_eu_360 <- eez_ter_360 %>%
  dplyr::filter((iso_ter %in% eu_countries) & pol_type == "200NM") %>%
  mutate(iso_sov = "EU") %>%
  group_by(iso_sov, pol_type) %>%
  summarize(geom = st_union(geom)) %>%
  ungroup() %>%
  mutate(region = "Europe & Central Asia",
         name_ter = "European Union",
         iso_ter = "EU",
         name_sov = "European Union",
         geoname_new = "Exclusive Economic Zone: European Union")

eez_ter_360 <- eez_ter_360 %>%
  dplyr::filter(!(iso_ter %in% eu_countries) | pol_type != "200NM") %>%
  rbind(eez_eu_360)

### REPLACE WITH ACTUAL DATA
ter_flag_connectivity_data <- eez_ter_360 %>%
  st_drop_geometry() %>%
  dplyr::filter(!(iso_ter %in% eu_countries)) %>%
  distinct(region, name_ter, iso_ter, name_sov, iso_sov) %>%
  arrange(region, name_ter)

### EEZ choices for each region
# East Asia & Pacific
east_asia_pacific_eezs <- ter_flag_connectivity_data$iso_ter[ter_flag_connectivity_data$region == "East Asia & Pacific"]
names(east_asia_pacific_eezs) <- ter_flag_connectivity_data$name_ter[ter_flag_connectivity_data$region == "East Asia & Pacific"]

# Europe & Central Asia
europe_central_asia_eezs <- ter_flag_connectivity_data$iso_ter[ter_flag_connectivity_data$region == "Europe & Central Asia"]
names(europe_central_asia_eezs) <- ter_flag_connectivity_data$name_ter[ter_flag_connectivity_data$region == "Europe & Central Asia"]

# Latin America & Caribbean
latin_america_caribbean_eezs <- ter_flag_connectivity_data$iso_ter[ter_flag_connectivity_data$region == "Latin America & Caribbean"]
names(latin_america_caribbean_eezs) <- ter_flag_connectivity_data$name_ter[ter_flag_connectivity_data$region == "Latin America & Caribbean"]

# Middle East & North Africa
middle_east_north_africa_eezs <- ter_flag_connectivity_data$iso_ter[ter_flag_connectivity_data$region == "Middle East & North Africa"]
names(middle_east_north_africa_eezs) <- ter_flag_connectivity_data$name_ter[ter_flag_connectivity_data$region == "Middle East & North Africa"]

# North America
north_america_eezs <- ter_flag_connectivity_data$iso_ter[ter_flag_connectivity_data$region == "North America"]
names(north_america_eezs) <- ter_flag_connectivity_data$name_ter[ter_flag_connectivity_data$region == "North America"]

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

### Widget choice values that depend on a dataset -----

# Define EEZ and flag state choices
africa_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])
names(africa_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Africa" & !is.na(ACP_codes$mrgid)])

caribbean_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])  
names(caribbean_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Caribbean" & !is.na(ACP_codes$mrgid)])

pacific_eez_choices <- unique(ACP_codes$territory_iso3[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])
names(pacific_eez_choices) <- unique(ACP_codes$flag[ACP_codes$region == "Pacific" & !is.na(ACP_codes$mrgid)])

flag_state_choices <- unique(connectivity_data$flag)
names(flag_state_choices) <- countrycode(flag_state_choices, "iso3c", "country.name")
names(flag_state_choices)[is.na(names(flag_state_choices))] <- "Unknown flag"

### Themes -----

# Standard map ggplot theme
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