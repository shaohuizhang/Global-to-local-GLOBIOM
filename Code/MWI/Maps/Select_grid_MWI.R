#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to grid adm information
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
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))
plot(adm1)
plot(adm2)

### CREATE COUNTRY GRID
# crop and mask
# Mask and crop raster
grid <- mask(r, adm1)
grid <- crop(grid, adm1)
plot(grid)
names(grid) <- "gridID"

# Create polygon
grid_py <- rasterToPolygons(grid)
plot(grid_py)

# Write polygon
saveRDS(grid_py, file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))


