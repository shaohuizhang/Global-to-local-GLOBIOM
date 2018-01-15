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

### LOAD DATA
# load data
area <- read_csv(file.path(dataPath, "Data/MWI/Raw/Agricultural_statistics/CountrySTAT/area_harvested.csv")) %>%
  mutate(variable = "area")
crop_prod <- read_csv(file.path(dataPath, "Data/MWI/Raw/Agricultural_statistics/CountrySTAT/prod_prim_crops.csv")) %>%
  mutate(variable = "production")

# combine
cs_raw <- bind_rows(area, crop_prod) %>%
  set_names(c("year", "ADD_code", "ADD", "crop_lvst_cs_code", "crop_lvst_cs", "value", "flag", "variable")) %>%
  mutate(ADD = toupper(ADD))

# OBTAIN ADM AND CROP_LVST LIST
# Note that data is presented at the level of 8 so-called Agricultural Development Divisions (ADD).
# We can map these to the adm1_GAUL level

cs_raw <- cs_raw %>%
  mutate(adm1_GAUL = ifelse(ADD %in% c("BLANTYRE", "SHIRE VALLEY", "MACHINGA"), "SOUTHERN REGION", 
                        ifelse(ADD %in% c("KASUNGU", "SALIMA", "LILONGWE"), "CENTRAL REGION", 
                          ifelse(ADD %in% c("KARONGA", "MZUZU"), "NORTHERN REGION", NA))))
                                       
                                                        
# Save adm2 list
cs_MWI_adm1_list <- cs_raw %>%
  dplyr::select(adm1_GAUL) %>%
  unique %>%
  arrange(adm1_GAUL)

write_csv(cs_MWI_adm1_list, file.path(dataPath, "Data/MWI/Processed/Mappings/cs_MWI_adm1_list.csv"))

# Save crop_lvst list
cs_MWI_crop_lvst_list <- cs_raw %>%
  dplyr::transmute(crop_lvst_cs) %>%
  unique

write_csv(cs_MWI_crop_lvst_list, file.path(dataPath, "Data/MWI/Processed/Mappings/cs_MWI_crop_lvst_list.csv"))

# ### PROCESS COUNTRYSTAT
# Read crop_lvs mapping
crop_lvst_map <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_cs2crop_lvst")

# Read adm mapping
MWI2adm <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

cs <- cs_raw %>%
  left_join(.,crop_lvst_map) %>%
  group_by(adm1_GAUL, short_name, year, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm_level = 1,
         source = "cs",
         unit = dplyr::recode(variable, "production" = "tons", "area" = "ha")) %>%
  left_join(MWI2adm) %>%
  ungroup() %>%
  rename(adm = adm1) %>%
  dplyr::select(-adm1_GAUL)

# Write file
write_csv(cs, file.path(dataPath , "Data/MWI/Processed/Agricultural_statistics/cs_MWI.csv"))

