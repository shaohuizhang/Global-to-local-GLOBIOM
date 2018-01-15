#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid SPAM 2005 land cover data to 1 x 1km/30 arc-sec
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview")
# Additional packages
p_load("countrycode", "plotKML")

### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### SET COUNTRY
source("Code/MWI/Set_country.R")

### DATA
# Adm
adm0 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm0_2000_adj.rds")))

# SPAM
maize_raw <- raster(file.path(dataPath, "Data/Global/SPAM/SPAM v3.1/spam2005V3r1_global_phys_area.geotiff/SPAM2005V3r1_global_A_TA_MAIZ_A.tif"))
rice_raw <- raster(file.path(dataPath, "Data/Global/SPAM/SPAM v3.1/spam2005V3r1_global_phys_area.geotiff/SPAM2005V3r1_global_A_TA_RICE_A.tif"))

# Grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"

### CREATE COUNTRY MAP
# Crop and mask global map
maize <- crop(maize_raw, adm0)
maize <- mask(maize, adm0)
plot(maize)

# Save map
writeRaster(maize, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/spam/spam_maize_", iso3c_sel, "_2005.tif")))

