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


### LOAD crop_lvst2FCL MAPPING
crop_lvst2FCL <- read_excel(file.path(dataPath, "Data/Mappings/Mappings.xlsx"), sheet = "crop_lvst2FCL")


### OBTAIN ADM AND CROP_LVST LIST
# Load data
am_raw <- read_csv(file.path(dataPath, "Data/ZMB/Raw/Agricultural_statistics/Agro-maps/ZMB_all_data.csv")) %>%
  transmute(adm_level = ADMIN_LEVEL, adm = toupper(AREA_NAME),  crop_lvst_am = ITEM_NAME, FCL_item_code = ITEM_CODE, year = YEAR, 
                area = AEREA_HARVESTED, yield = YIELD, production = PRODUCTION) 

# Save adm1 list
am_adm1_list <- am_raw %>%
  filter(adm_level == 1) %>%
  dplyr::transmute(adm1_am = adm) %>%
  unique %>%
  arrange(adm1_am)
summary(am_adm1_list)

write_csv(am_adm1_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/am_ZMB_adm1_list.csv"))

# Save crop_lvst list
am_crop_lvst_list <- am_raw %>%
  dplyr::transmute(crop_lvst_am, FCL_item_code) %>%
  left_join(crop_lvst2FCL) %>%
  unique %>%
  arrange(crop_lvst_am)
summary(am_crop_lvst_list)

write_csv(am_crop_lvst_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/am_ZMB_crop_lvst_list.csv"))


### PROCESS AGRO-MAPS
# Review and copy the am_crop_lvst_list and adm_list to the Mappings_ZMB.xlsx file before!
# Read crop_lvs mapping
crop_lvst_map <- read_excel(file.path(dataPath, "Data\\ZMB\\Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB_am2crop_lvst")

# Read adm1 mappping
adm1_map <- read_excel(file.path(dataPath, "Data\\ZMB\\Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_am) %>%
  na.omit %>%
  unique()

# Process data: change names, units and put in long format
am <- am_raw %>% 
  gather(variable, value, -adm, -adm_level, -crop_lvst_am, -FCL_item_code, -year) %>%
  filter(year != "Most recent") %>%
  mutate(unit = dplyr::recode(variable, "area" = "ha", "yield" = "tons/ha", "production" = "tons"),
         year = as.integer(year)) %>%
  na.omit() 

# Agregate to adm1 level and core crops and lvst
am <- am %>%
  filter(adm_level == 1) %>%
  rename(adm1_am = adm) %>%
  left_join(adm1_map) %>%
  left_join(crop_lvst_map) %>%
  group_by(year, adm1, short_name, unit, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 1,
         source = "am") %>%
  rename(adm = adm1)
summary(am)

# save file
write_csv(am, file.path(dataPath, "Data\\ZMB\\Processed\\agricultural_statistics\\am_ZMB.csv"))

