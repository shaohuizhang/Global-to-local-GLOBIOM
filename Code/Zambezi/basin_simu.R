#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare overlay simu with basin map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "plotKML")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"
ISWELPath <- "P:\\is-wel"


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD SIMU MAPS
SIMU_LU <- read_csv(file.path(dataPath, "Data/GLOBIOM/simu_lu/SimUIDLUID.csv"))
ogrListLayers(file.path(dataPath, "Data/GLOBIOM/simu_poly/SimU_all.shp"))
simu_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/simu_poly/SimU_all.shp"), layer = "SimU_all")
simu_raster <- raster(file.path(dataPath, "Data/GLOBIOM/simu_raster/w001001.adf"))

### DOWNLOAD BASIN SHAPE FILES
zambezi <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\Zambezi\\Zambezi_hybas_lev3.shp"))
indus <- readOGR(file.path(ISWELPath, "shared_data_sources\\processed_data\\Indus\\Indus_hybas_lev3.shp"))
plotKML(zambezi)
plotKML(indus)

### USE ZAMBEZI MAP TO CLIP SIMU MAP
# Create polygon
zambezi_simu_poly <- gIntersection(zambezi, simu_5min_poly, byid = TRUE, drop_lower_td = TRUE)
plot(zambezi_simu_poly, col = "lightblue")

# Extract SIMU IDS
zambezi_simu <- crop(simu_raster, zambezi)
zambezi_simu <- mask(zambezi_simu, zambezi)
plot(zambezi_simu)
plot(zambezi, add = T)
zambezi_simu_df <- as.data.frame(rasterToPoints(zambezi_simu)) %>%
  setNames(c("x", "y", "SimUID")) %>%
  filter(SimUID != 0)

# Check selection
check <- SIMU_5min_poly[SIMU_5min_poly$SimUID %in% zambezi_simu_df$SimUID,]
plot(zambezi, add = T)
rm(check)

### USE INDUS MAP TO CLIP SIMU MAP
# Create polygon
indus_simu_poly <- gIntersection(indus, simu_5min_poly, byid = TRUE, drop_lower_td = TRUE)
plot(indus_simu_poly, col = "lightblue")
plot(indus_simu_poly)

# Extract SIMU IDS
indus_simu <- crop(simu_raster, indus)
indus_simu <- mask(indus_simu, indus)
plot(indus_simu)
plot(indus, add = T)
indus_simu_df <- as.data.frame(rasterToPoints(indus_simu)) %>%
  setNames(c("x", "y", "SimUID")) %>%
  filter(SimUID != 0)

# Check selection
check <- simu_5min_poly[simu_5min_poly$SimUID %in% indus_simu_df$SimUID,]
plot(check, add = T)
rm(check)
