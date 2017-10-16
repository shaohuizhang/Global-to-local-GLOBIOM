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

### PREPARE 30 ARCSEC GLOBAL RASTER IN WSG84
r <- raster() # 1 degree raster
r <- disaggregate(r, fact=120) # 30 arcsec raster


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/GAUL_", iso3c, "_adm1_2000.rds")))
adm2 <- readRDS(file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/GAUL_", iso3c, "_adm2_2000.rds")))
plot(adm1)
plot(adm2)


### CREATE 5 ARCMIN COUNTRY GRID
# crop and mask
# Mask and crop raster
# NB method to first assign gridID numbers to global raster and then crop does not result in unique gridID?
grid <- crop(r, adm1)
values(grid) <- 1:ncell(grid) # Add ID numbers
names(grid) <- "gridID" 
grid <- mask(grid, adm1)
plot(grid)

# Write raster
saveRDS(grid, file.path(dataPath, paste0("Data/", iso3c, "/Processed/Maps/grid_30sec_r_", iso3c, ".rds")))


# Create polygon
grid_py <- rasterToPolygons(grid)
#plot(grid_py)
#plot(adm1, add = T, border = "red")

# Write polygon
saveRDS(grid_py, file.path(dataPath, "Data/MWI/Processed/Maps/grid_30sec_MWI.rds"))


