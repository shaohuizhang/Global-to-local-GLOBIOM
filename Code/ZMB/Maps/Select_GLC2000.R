#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select GLC2000 land cover map per country
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


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD Data
# adm
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_",iso3c_sel, ".rds")))
plot(adm)

# GLC2000
glc_raw <- raster(file.path(dataPath, "Data/Global/GLC2000/glc2000_v1_1.tif"))
crs(glc_raw) <- " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
glc <- crop(glc_raw, adm)
glc <- mask(glc, adm)
plot(glc)

# Save map
writeRaster(glc, file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/GLC2000/GLC2000_raw_2000_", iso3c_sel, ".tif")), overwrite = T)

