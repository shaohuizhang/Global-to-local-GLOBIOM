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
options("stringsAsFactors"=FALSE) # ensures that character data that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### MWI
# Download relevant maps
MWI_adm1 <- readRDS(file.path(dataPath, "Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm1.rds"))

# Save list of adm level 1 districts that will be used as regional identifiers
adm_list_MWI <- MWI_adm1@data %>%
  transmute(iso3c = ISO, adm1 = toupper(NAME_1))
write_csv(adm_list_MWI, file.path(dataPath, "Processed/MWI/Mappings/adm_list_MWI.csv"))



