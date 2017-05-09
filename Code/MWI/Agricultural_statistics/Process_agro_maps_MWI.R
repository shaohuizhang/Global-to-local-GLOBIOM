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


### PROCESS AGRO-MAPS DATA
# Load adm mapping


# Load FLC
FCL <- read_excel(file.path(dataPath, "Data\\Global\\FAOSTAT\\FCL\\FCL.xlsx"), sheet = "FCL_short")

# Load data
agro_maps_raw <- read_csv(file.path(dataPath, "Data/MWI/Raw/Agricultural_statistics/Agro-maps/mwi_all_data.csv")) 
  
# Process data: change names, units and put in long format
agro_maps <- agro_maps_raw %>%
  dplyr::select(ADMIN_LEVEL, AREA_NAME, FCL_item_code = ITEM_CODE, year = YEAR, area = AEREA_HARVESTED, yield = YIELD, production = PRODUCTION) %>%
  gather(variable, value, -AREA_NAME, -ADMIN_LEVEL, -FCL_item_code, -year) %>%
  mutate(unit = dplyr::recode(variable, "area" = "ha", "yield" = "tons/ha", "production" = "tons")) %>%
  filter(year != "Most recent") %>%
  left_join(., FCL) %>% 
  na.omit() 

agro_maps_adm1 <- agro_maps %>%
  filter(ADMIN_LEVEL == 1) %>%
  dplyr::select(-ADMIN_LEVEL)

agro_maps_adm2 <- agro_maps %>%
  filter(ADMIN_LEVEL == 2) %>%
  dplyr::select(-ADMIN_LEVEL)

# save files
write_csv(agro_maps_adm1, file.path(dataPath, "Data\\MWI\\Processed\\agricultural_statistics\\agro_maps_adm1.csv"))
write_csv(agro_maps_adm2, file.path(dataPath, "Data\\MWI\\Processed\\agricultural_statistics\\agro_maps_adm2.csv"))
