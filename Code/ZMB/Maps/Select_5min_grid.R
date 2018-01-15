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
p_load("quickPlot")


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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm1_2000.rds")))
plot(adm1)


### CREATE COUNTRY GRID
# NB method to first assign gridID numbers to global raster and then crop does not result in unique gridID?
r <- raster() # 1 degree raster
r <- disaggregate(r, fact=12) # 5 arcmin raster
grid <- crop(r, adm1)
values(grid) <- 1:ncell(grid) # Add ID numbers
names(grid) <- "gridID" 
grid <- mask(grid, adm1)
grid
plot(grid)

# Write raster
writeRaster(grid, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_5min_r_", iso3c_sel, ".tif")), overwrite = T)

# Create polygon
grid_p <- rasterToPolygons(grid)
library(quickPlot)
grid_p

#plot(grid_p) # Might take a long time in case of high resolution!
plot(grid_p) # Plot (with capital) from quickPlot package works faster but might still take time!

# Write polygon
saveRDS(grid_p, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_5min_p_", iso3c_sel, ".rds")))

