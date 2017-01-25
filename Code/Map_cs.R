#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
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
#p_load("WDI", "countrycode")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### ADMINISTRATIVE REGIONS MAP
# MWI GADM
MWI_adm1 <- readRDS(file.path(dataPath, "Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm1.rds"))
Fig_MWI_adm <- spplot(MWI_adm1, "OBJECTID", colorkey=FALSE)

### LAND COVER MAP
# Load SIMU Map
SIMU_5min_poly <- readOGR(file.path(dataPath, "GLOBIOM/simu_poly/SimU_all.shp"), layer = "SimU_all")

# Obtain country poly (using iso3c numbering: 454 = Malawi)
SIMU2country_poly <- SIMU_5min_poly[SIMU_5min_poly$COUNTRY==454,]

# Load land cover map
land_cover_map_raw <- raster(file.path(dataPath, "Raw\\MWI\\Land_cover_maps\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 1\\malawi 2000 classification scheme1.img"))

# Load GADM country borders
country_map_raw <- readRDS(file.path(dataPath, "Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm0.rds"))

# Reproject GADM and SIMU maps
country_crs <- crs(land_cover_map_raw)
SIMU2country_poly_rp <- spTransform(SIMU2country_poly, country_crs)
country_map_rp <-  spTransform(country_map_raw, country_crs)

# Create map
Fig_MWI_lc_SIMU <- levelplot(land_cover_map_raw, att='Land_Cover', par.settings = RdBuTheme) +
  layer(sp.polygons(SIMU2country_poly_rp, col = "black")) +
  layer(sp.polygons(country_map_rp, col = "black"))

