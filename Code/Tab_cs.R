#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create tables
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "projroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "stargazer")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### PREPARE MWI land cover classes
land_cover_map_raw <- raster(file.path(dataPath, "Raw\\MWI\\Land_cover_maps\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 1\\malawi 2000 classification scheme1.img"))

# Percentage of clouds and shadow
Tab_MWI_lc_classes <- bind_rows(
  as.data.frame(levels(land_cover_map_raw)) %>%
    dplyr::filter(ID != 0) %>%
    mutate(share = Area_Ha/sum(Area_Ha)*100) %>%
    dplyr::select(`Land_cover class` = Land_Cover, `Area (ha)` = Area_Ha, `Share (%)` = share),
  as.data.frame(levels(land_cover_map_raw)) %>%
    dplyr::filter(ID != 0) %>%
    mutate(share = Area_Ha/sum(Area_Ha)*100) %>%
    summarize(Land_Cover = "Total",
              Area_Ha = sum(Area_Ha),
              share = sum(share)) %>%
    dplyr::select(`Land_cover class` = Land_Cover, `Area (ha)` = Area_Ha, `Share (%)` = share))



