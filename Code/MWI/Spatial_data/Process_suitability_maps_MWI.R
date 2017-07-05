#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Code to select urban mask
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "R.utils", "plotKML", "ggthemes")


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


### SUITABILITY MAPS
# Load global mask
suitability_raw <- readOGR(file.path(dataPath, "Data/MWI/raw/Spatial_data/suitability_maps/land_units/land_units_1st_code_fnl_ADD_specific.shp"))
plot(suitability_raw)
suitability_df <- suitability_raw@data
