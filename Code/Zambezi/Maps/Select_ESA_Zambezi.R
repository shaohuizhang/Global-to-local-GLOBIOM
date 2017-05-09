#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select ESA land cover map per country
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
p_load("WDI", "countrycode", "plotKML", "sf")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD GAUL MAPS
adm2_map <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000.rds"))
plot(adm2_map)

### LOAD LAND COVER MAP 
# Load global ESA map
land_cover_map_ESA <- raster(file.path(dataPath, "Data\\Global\\ESA\\Annual_maps\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))
land_cover_map <- crop(land_cover_map_ESA, adm2_map)
land_cover_map <- mask(land_cover_map, adm2_map)
plot(land_cover_map)

# Save map
saveRDS(land_cover_map, file.path(dataPath, "Data/MWI/Processed/Maps/ESA_MWI_2000.rds"))

