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


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### DOWNLOAD AND PROCESS FAOSTAT DATA
# Load crop_lvst2FCL
crop_lvst2FCL <- read_excel(file.path(dataPath, "Data\\Mappings\\Mappings.xlsx"), sheet = "crop_lvst2FCL") %>%
  dplyr::select(short_name, FCL_item_code) %>%
  na.omit()

# Download
faostat_prod_raw <- read_csv(file.path(FAOSTATPath, "Production_Crops_E_All_Data_(Normalized).csv"))
faostat_lvst_raw <- read_csv(file.path(FAOSTATPath, "Production_Livestock_E_All_Data_(Normalized).csv"))

# Process 
faostat_crop <- faostat_prod_raw %>%
  mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
         iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
  dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
  filter(iso3c == iso3c_sel) %>%
  left_join(., crop_lvst2FCL) %>%
  filter(!is.na(value))

# faostat_lvst <- faostat_lvst_raw %>%
#   mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
#          iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
#   dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
#   filter(iso3c == "ZMB") %>%
#   left_join(., FCL) %>%
#   mutate(value = ifelse(FCL_title %in% c("Poultry Birds", "Chickens"), value*1000, value),
#          unit = "Head") %>%
#   filter(!is.na(value), !(class == "total")) %>%
#   dplyr::select(-class)


### CREATE FAOSTAT DATABASE
# Create files for relevant variables
area_faostat <- faostat_crop %>%
  filter(unit == "ha", variable == "area") %>%
  na.omit() %>%# remove rows with na values for value
  group_by(short_name, unit, year, variable) %>%
  summarize(value = sum(value, na.rm = T))

prod_faostat <- faostat_crop %>%
  filter(unit == "tonnes", 1990, variable == "production") %>%
  mutate(unit = replace(unit, unit=="tonnes", "tons")) %>%
  na.omit() %>%# remove rows with na values for value
  group_by(short_name, unit, year, variable) %>%
  summarize(value = sum(value, na.rm = T))
  

# Note that for some crops area or prodution is not available resulting in NA for yield
yld_faostat <- bind_rows(area_faostat, prod_faostat) %>%
  ungroup() %>%
  dplyr::select(-unit) %>%
  spread(variable, value) %>%
  mutate(value = production/area,
         unit = "tons/ha",
         variable = "yield") %>%
  dplyr::select(-production, -area)
  
# number_faostat <- faostat_lvst %>%
#   filter(year >1990) 

# Bind area, production and yield
faostat_comb <- bind_rows(area_faostat, prod_faostat, yld_faostat) %>%
  ungroup() %>%
  mutate(source = "FAOSTAT",
         adm_level = 0,
         adm = iso3c_sel) %>%
  na.omit()
summary(faostat_comb)
str(faostat_comb)

# save files
write_csv(faostat_comb, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/faostat_", iso3c_sel, ".csv")))
