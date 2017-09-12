#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to analyse and process agro-maps data
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


# OBTAIN ADM AND CROP_LVST LIST
# Load data
am_raw <- read_csv(file.path(dataPath, "Data/ZMB/Raw/Agricultural_statistics/Agro-maps/ZMB_all_data.csv")) 

# Save adm1 list
am_ZMB_adm1_list <- am_raw %>%
  filter(ADMIN_LEVEL == 1) %>%
  dplyr::transmute(adm1_am = toupper(AREA_NAME)) %>%
  unique %>%
  arrange(adm1_am)

write_csv(am_ZMB_adm1_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/am_ZMB_adm1_list.csv"))

# Save adm2 list
am_ZMB_adm2_list <- am_raw %>%
  filter(ADMIN_LEVEL == 2) %>%
  dplyr::transmute(adm2_am = toupper(AREA_NAME)) %>%
  unique %>%
  arrange(adm2_am)
  
write_csv(am_ZMB_adm2_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/am_ZMB_adm2_list.csv"))

# Save crop_lvst list
am_ZMB_crop_lvst_list <- am_raw %>%
  dplyr::transmute(crop_lvst_am = ITEM_NAME, FCL_item_code = ITEM_CODE) %>%
  unique %>%
  arrange(crop_lvst_am)

write_csv(am_ZMB_crop_lvst_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/am_ZMB_crop_lvst_list.csv"))


### PROCESS AGRO-MAPS
# Read adm1 mappping
ZMB2adm1 <- read_excel(file.path(dataPath, "Data\\ZMB\\Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_am) %>%
  na.omit %>%
  unique()

# Read adm2 mappping
ZMB2adm2 <- read_excel(file.path(dataPath, "Data\\ZMB\\Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm2, adm2_am) %>%
  na.omit %>%
  unique()

# Read crop and livestock mapping
crop_lvst_map <- read_excel(file.path(dataPath, "Data\\Global\\SPAM\\SPAM_mappings.xlsx"), sheet = "SPAM2FAOSTAT_crop") %>%
  dplyr::select(short_name = SPAM_short_name, FCL_item_code) %>%
  na.omit()


# Process data: change names, units and put in long format
am <- am_raw %>%
  dplyr::select(ADMIN_LEVEL, AREA_NAME, FCL_item_code = ITEM_CODE, year = YEAR, area = AEREA_HARVESTED, yield = YIELD, production = PRODUCTION) %>%
  gather(variable, value, -AREA_NAME, -ADMIN_LEVEL, -FCL_item_code, -year) %>%
  mutate(unit = dplyr::recode(variable, "area" = "ha", "yield" = "tons/ha", "production" = "tons"),
         AREA_NAME = toupper(AREA_NAME)) %>%
  filter(year != "Most recent") %>%
  left_join(., crop_lvst_map) %>%
  na.omit() 

# Agregate to adm1 level and core crops and lvst
am_adm1 <- am %>%
  filter(ADMIN_LEVEL == 1) %>%
  dplyr::select(-ADMIN_LEVEL) %>%
  rename(adm1_am = AREA_NAME) %>%
  left_join(ZMB2adm1) %>%
  group_by(year, adm1, short_name, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 1,
         source = "am") %>%
  rename(adm = adm1)

# Agregate to adm1 level and core crops and lvst
am_adm2 <- am %>%
  filter(ADMIN_LEVEL == 2) %>%
  dplyr::select(-ADMIN_LEVEL) %>%
  rename(adm2_am = AREA_NAME) %>%
  left_join(ZMB2adm2) %>%
  group_by(year, adm2, short_name, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 2,
         source = "am") %>%
  rename(adm = adm2)

# Combine
am <- bind_rows(am_adm1, am_adm2)

# save file
write_csv(am, file.path(dataPath, "Data\\ZMB\\Processed\\agricultural_statistics\\am_ZMB.csv"))

