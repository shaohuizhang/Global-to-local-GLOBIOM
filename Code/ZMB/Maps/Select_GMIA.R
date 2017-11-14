#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select GMIA information
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
p_load("WDI", "countrycode", "plotKML")


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


### LOAD GMIA MAPS
GMIA_r_raw <- raster(file.path(dataPath, "Data/Global/GMIA/gmia_v5_aei_ha_asc/gmia_v5_aei_ha.asc")) # hectares per cell
crs(GMIA_r_raw) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

### LLOAD ADM
adm0 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/GAUL_", iso3c_sel, "_adm0_2000.rds")))

### SELECT COUNTRY GMIA RASTER MAP
GMIA <- crop(GMIA_r_raw, adm0)
GMIA <- mask(GMIA, adm0)
plot(GMIA)
hist(GMIA, breaks = 50)
cellStats(GMIA,sum)

# Save map
saveRDS(GMIA, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GMIA_", iso3c_sel, ".rds")))

