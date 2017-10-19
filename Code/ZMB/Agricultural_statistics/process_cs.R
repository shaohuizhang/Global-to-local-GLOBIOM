#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process CountrySTAT data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("ghit", "tabulizer")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD crop_lvst2FCL MAPPING
crop_lvst2FCL <- read_excel(file.path(dataPath, "Data/Mappings/Mappings.xlsx"), sheet = "crop_lvst2FCL")


### LOAD DATA
# load data
area <- read_csv(file.path(dataPath, "Data/ZMB/Raw/Agricultural_statistics/CountrySTAT/area_harvested.csv")) %>%
  mutate(variable = "area")
crop_prod <- read_csv(file.path(dataPath, "Data/ZMB/Raw/Agricultural_statistics/CountrySTAT/Production.csv")) %>%
  mutate(variable = "production")

# combine
cs_raw <- bind_rows(area, crop_prod) %>%
  set_names(c("year", "adm1_code", "adm1", "FCL_item_code", "crop_lvst_cs", "value", "flag", "variable")) %>%
  mutate(adm1 = toupper(adm1))

# OBTAIN ADM AND CROP_LVST LIST
# Save adm2 list
cs_adm1_list <- cs_raw %>%
  dplyr::select(adm1_cs = adm1) %>%
  unique %>%
  arrange(adm1_cs)
summary(cs_adm1_list)

write_csv(cs_adm1_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/cs_ZMB_adm1_list.csv"))

# Save crop_lvst list
cs_crop_lvst_list <- cs_raw %>%
  dplyr::transmute(crop_lvst_cs, FCL_item_code) %>%
  left_join(crop_lvst2FCL) %>%
  unique
summary(cs_crop_lvst_list)

write_csv(cs_crop_lvst_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/cs_ZMB_crop_lvst_list.csv"))


### PROCESS COUNTRYSTAT
# Review and copy the cs_crop_lvst_list and adm_list to the Mappings_ZMB.xlsx file before!
# Read crop_lvs mapping
crop_lvst_map <- read_excel(file.path(dataPath, "Data\\ZMB\\Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB_cs2crop_lvst")

# Read adm mapping
adm1_map <- read_excel(file.path(dataPath, "Data\\ZMB\\Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_cs) %>%
  na.omit %>%
  unique()

cs <- cs_raw %>%
  left_join(adm1_map) %>%
  left_join(.,crop_lvst_map) %>%
  group_by(adm1, short_name, year, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 1,
         source = "cs",
         unit = dplyr::recode(variable, "production" = "tons", "area" = "ha")) %>%
  ungroup() %>%
  rename(adm = adm1) 
summary(cs)

# Write file
write_csv(cs, file.path(dataPath , "Data/ZMB/Processed/Agricultural_statistics/cs_ZMB.csv"))

