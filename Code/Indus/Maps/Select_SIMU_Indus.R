#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select simu map per country
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


### LOAD SIMU MAPS
ogrListLayers(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"))
simu_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")

### LOAD BASIN SHAPE FILES
indus_map <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\indus\\indus_hybas_lev3.shp"))

### CLIP SIMU MAP TO BASIN
# Create polygon
simu_indus <- gIntersection(indus_map, simu_5min_poly, byid = TRUE, drop_lower_td = TRUE)
plot(simu_indus)

# Save map
saveRDS(simu_indus, file.path(dataPath, "Data/indus/Processed/Maps/simu_indus.rds"))

