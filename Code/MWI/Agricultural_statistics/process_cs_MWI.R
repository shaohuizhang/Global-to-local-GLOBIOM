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
p_load("ghit", "tabulizer")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


# OBTAIN ADM LIST
# Load data
cs_raw <- read_csv(file.path(dataPath, "Data/MWI/Raw/Agricultural_statistics/CountrySTAT/Total_area_under_crop.csv")) 

# Save adm2 list
cs_MWI_adm2_list <- cs_raw %>%
  dplyr::transmute(adm2_cs = toupper(District)) %>%
  unique %>%
  arrange(adm2_cs)

write_csv(cs_MWI_adm2_list, file.path(dataPath, "Data/MWI/Processed/Mappings/cs_MWI_adm2_list.csv"))

### PROCESS COUNTRYSTAT
# Read adm mappping
MWI2adm <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm2_GAUL, adm2_cs) %>%
  na.omit

cs <- cs_raw %>%
  transmute(adm2_cs = toupper(District), value = Value, variable = "area", unit = "ha", year = 2007) %>%
  left_join(MWI2adm) %>%
  group_by(adm2_GAUL, year, variable, unit) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 2)

# Write file
write_csv(cs, file.path(dataPath , "Data/MWI/Processed/Agricultural_statistics/cs_MWI.csv"))

