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
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY CODE
iso3c <- "ZMB"


### LOAD SIMU MAPS
ogrListLayers(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"))
simu_5min_poly <- readOGR(file.path(dataPath, "Data/GLOBIOM/Simu/Simu_poly/SimU_all.shp"), layer = "SimU_all")


### SELECT COUNTRY SIMU MAP USING ISO3c
iso3n <- countrycode(iso3c, "iso3c", "iso3n")
simu <- simu_5min_poly[simu_5min_poly$COUNTRY == iso3n,]
plot(simu)

# Load GAUL ADM map
adm1 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c, "\\Processed\\Maps/GAUL_", iso3c, "_adm1_2000.rds")))

# Compare and inspect maps
plot(adm1)
plot(simu, add = T, border = "red")
plotKML(simu)

# Save map
saveRDS(simu, file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/simu_", iso3c, ".rds")))

