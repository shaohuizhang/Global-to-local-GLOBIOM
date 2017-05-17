#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process FAOSTAT data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode")



### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### DOWNLOAD AND PROCESS FAOSTAT DATA
# Load CROP_LVST_MAP
crop_lvst_map <- read_csv(file.path(dataPath, "Data\\Mappings\\crop_lvst_mapping.csv")) %>%
  dplyr::select(short_name, FCL_item_code) %>%
  na.omit()

# Download
FAOSTAT_prod_raw <- read_csv(file.path(FAOSTATPath, "Production_Crops_E_All_Data_(Normalized).csv"))
FAOSTAT_lvst_raw <- read_csv(file.path(FAOSTATPath, "Production_Livestock_E_All_Data_(Normalized).csv"))

# Process 
FAOSTAT_crop <- FAOSTAT_prod_raw %>%
  mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
         iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
  dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
  filter(iso3c == "MWI") %>%
  left_join(., crop_lvst_map) %>%
  filter(!is.na(value))

# FAOSTAT_lvst <- FAOSTAT_lvst_raw %>%
#   mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
#          iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
#   dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
#   filter(iso3c == "MWI") %>%
#   left_join(., FCL) %>%
#   mutate(value = ifelse(FCL_title %in% c("Poultry Birds", "Chickens"), value*1000, value),
#          unit = "Head") %>%
#   filter(!is.na(value), !(class == "total")) %>%
#   dplyr::select(-class)


### CREATE FAOSTAT DATABASE
# Create files for relevant variables
area_FAOSTAT <- FAOSTAT_crop %>%
  filter(unit == "ha", year >1990, variable == "area") %>%
  na.omit() %>%# remove rows with na values for value
  group_by(short_name, unit, year, variable) %>%
  summarize(value = sum(value, na.rm = T))

prod_FAOSTAT <- FAOSTAT_crop %>%
  filter(unit == "tonnes", year >1990, variable == "production") %>%
  mutate(unit = replace(unit, unit=="tonnes", "tons")) %>%
  na.omit() %>%# remove rows with na values for value
  group_by(short_name, unit, year, variable) %>%
  summarize(value = sum(value, na.rm = T))
  

# Note that for some crops area or prodution is not available resulting in NA for yield
yld_FAOSTAT <- bind_rows(area_FAOSTAT, prod_FAOSTAT) %>%
  ungroup() %>%
  dplyr::select(-unit) %>%
  spread(variable, value) %>%
  mutate(value = production/area,
         unit = "tons/ha",
         variable = "yield") %>%
  dplyr::select(-production, -area)
  
# number_FAOSTAT <- FAOSTAT_lvst %>%
#   filter(year >1990) 

# Bind area, production and yield
FAOSTAT_comb <- bind_rows(area_FAOSTAT, prod_FAOSTAT, yld_FAOSTAT) %>%
  ungroup()

# save files
write_csv(FAOSTAT_comb, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/FAOSTAT_MWI.csv"))
