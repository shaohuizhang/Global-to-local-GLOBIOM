#'========================================================================================================================================
#' Project:  ISWEL
#' Subject:  Data to process CEEPA and extract relevant data for target countries
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "haven")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### DOWNLOAD MAPS AND PROCESS MAPS
# Adm conversion table
adm_names <- read_csv(file.path(dataPath, "Data/ZWE/Raw/Household_surveys/CEEPA/district_match_Jan2017.csv")) %>%
  dplyr::select(country, adm2 = hs_original, GADM_original)

# Download maps
ZWE_adm1 <- readRDS(file.path(dataPath, "Data/ZWE/Processed/Maps/GADM_2.8_ZWE_adm1.rds"))
ZWE_adm2 <- readRDS(file.path(dataPath, "Data/ZWE/Processed/Maps/GADM_2.8_ZWE_adm2.rds"))
ZMB_adm1 <- readRDS(file.path(dataPath, "Data/ZMB/Processed/Maps/GADM_2.8_ZMB_adm1.rds"))
ZMB_adm2 <- readRDS(file.path(dataPath, "Data/ZMB/Processed/Maps/GADM_2.8_ZMB_adm2.rds"))

# Add id and extract adm data
ZWE_adm1_df <- ZWE_adm1@data
ZWE_adm2@data$id <- rownames(ZWE_adm2@data)
ZWE_adm2_df <- ZWE_adm2@data %>%
  transmute(id, iso3c = ISO, adm1 = NAME_1, GADM_original = NAME_2) %>%
  left_join(., adm_names)

ZMB_adm1_df <- ZMB_adm1@data
ZMB_adm2_df <- ZMB_adm2@data %>%
  transmute(OBJECTID, iso3c = ISO, adm1 = tolower(NAME_1), adm2 = tolower(NAME_2))

### LOAD DATA

# yld_ZWE
yld_ZWE <- readRDS(file.path(dataPath, "Data/ZWE/Processed/Household_surveys/yld_ag_ZWE.rds")) %>%
  ungroup() %>%
  dplyr::select(-adm1) %>%
  spread(crop_name, yld)

### CREATE MAPS
# link data to maps
check <- left_join(ZWE_adm2_df, yld_ZWE)

# Fortify polygon
ZWE_adm2_fo <- fortify(ZWE_adm2)

# Link data
ZWE_adm2_fo <- left_join(ZWE_adm2_fo, check)

# Plot data
ggplot(ZWE_adm2_fo, aes(long, lat, group = group, fill = maize)) +
  geom_polygon() + 
  geom_path(colour = "black") +
  coord_equal() +
  labs(x="", y="") +
  theme_classic() +
  theme(legend.position="none",
        line = element_blank(),
        axis.text = element_blank())
