#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to grid adm information
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if (!require(pacman)) install.packages("pacman")
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


### SET COUNTRY CODE
iso3c <- "ZMB"


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()

### PREPARE 5 ARC MIN GLOBAL RASTER IN WSG84
r <- raster() # 1 degree raster
r <- disaggregate(r, fact=12) # 5 arcmin raster
values(r) <- 1:ncell(r) # Add ID numbers
names(r) <- "gridID" 
r


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/GAUL_", iso3c, "_adm1_2000.rds")))
adm2 <- readRDS(file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/GAUL_", iso3c, "_adm2_2000.rds")))
plot(adm1)
plot(adm2)


### CREATE COUNTRY GRID
# crop and mask
# Mask and crop raster
grid <- crop(r, adm1)
grid <- mask(grid, adm1)
plot(grid)
names(grid) <- "gridID"

# Write raster
saveRDS(grid, file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/grid_r_", iso3c, ".rds")))

# Create polygon
grid_py <- rasterToPolygons(grid)
plot(grid_py)
plot(adm1, add = T, border = "red")

# Write polygon
saveRDS(grid_py, file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/grid_", iso3c, ".rds")))

