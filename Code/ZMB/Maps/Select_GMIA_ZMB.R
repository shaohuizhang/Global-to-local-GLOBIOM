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


### SET COUNTRY CODE
iso3c <- "ZMB"


### LOAD SIMU MAPS
GMIA_r_raw <- raster(file.path(dataPath, "Data/Global/GMIA/gmia_v5_aei_ha_asc/gmia_v5_aei_ha.asc"))
GMIA_poly_raw <- readOGR(file.path(dataPath, "Data/Global/GMIA/gmia_v5_shp/gmia_v5_aai_pct_aei.shp"))
crs(GMIA_r_raw) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

### LLOAD ADM
adm1 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c, "\\Processed\\Maps/GAUL_", iso3c, "_adm2_2000.rds")))
adm0 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c, "\\Processed\\Maps/GAUL_", iso3c, "_adm0_2000.rds")))

### SELECT COUNTRY GMIA RASTER MAP
GMIA <- crop(GMIA_r_raw, adm2)
GMIA <- mask(GMIA, adm2)
plot(GMIA)
hist(GMIA, breaks = 50)
cellStats(GMIA,sum)

# Save map
saveRDS(GMIA, file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/GMIA_", iso3c, ".rds")))

