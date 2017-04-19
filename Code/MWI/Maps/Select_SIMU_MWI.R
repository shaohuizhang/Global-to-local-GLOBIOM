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
p_load("WDI", "countrycode", "plotKML")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD SIMU MAPS
ogrListLayers(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"))
SIMU_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")

### SELECT COUNTRY SIMU MAP USING ISO3 NUMBERING
# set country code
# 454 = Malawi
iso3n <- 454
simu2country_poly <- simu_5min_poly[simu_5min_poly$COUNTRY == iso3n,]
plot(simu2country_poly)

# Save map
saveRDS(simu2country_poly, file.path(dataPath, "Data/MWI/Processed/Maps/simu_MWI.rds"))
