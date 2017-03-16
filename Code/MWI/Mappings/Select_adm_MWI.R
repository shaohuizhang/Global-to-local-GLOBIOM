#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Select adm regions ZMB
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
#p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### MWI
# Download relevant maps
mapPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Processed\\MWI\\GADM_maps"
MWI_adm0 <- GADM_f("MWI", lev = 0, dataPath = mapPath)
MWI_adm1 <- GADM_f("MWI", lev = 1, dataPath = mapPath)
MWI_adm2 <- GADM_f("MWI", lev = 2, dataPath = mapPath)
MWI_adm3 <- GADM_f("MWI", lev = 3, dataPath = mapPath)


# Save list of adm level 1 districts that will be used as regional identifiers
adm_list_MWI <- MWI_adm1@data %>%
  dplyr::select(iso3c = ISO, adm_list = NAME_1)
write_csv(adm_list_MWI, file.path(dataPath, "Processed/MWI/GADM_maps/adm_list_MWI.csv"))

