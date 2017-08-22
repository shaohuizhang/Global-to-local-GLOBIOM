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
grid <- crop(r, adm1)
grid <- mask(grid, adm1)
plot(grid)
names(grid) <- "gridID"

# Write raster
saveRDS(grid, file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# Create polygon
grid_py <- rasterToPolygons(grid)
plot(grid_py)
plot(adm1, add = T, border = "red")

# Write polygon
saveRDS(grid_py, file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))


### APPROACH TO ADD BORDER CELLS #1
# Note that:
# 1. We need another version of adm0 that excludes lake MWI => union adm1
# 2. Any next steps that include the (total crop) area of the grid cell requires special treatment of the 
# border cells => potential approach is to polygize the raster, crop to adm0 and calculate area of border cells
# However all this might be a lot of extra work for very limited returns.

# https://stackoverflow.com/questions/44023272/r-crop-raster-using-polygon-keeping-cells-along-the-border
adm0 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm0_2000.rds"))
cls <- cellFromPolygon(r, adm0, weights = TRUE)[[1]][, "cell"]

r2 <- r
r2[][-cls] <- NA
r2 <- trim(r2) 
plot(r2)
plot(adm1, add = TRUE)
x <- rasterToPoints(r2)

### APPROACH TO ADD BORDER CELLS #2
# Still have to check if it works
# Note that:
# 1. We need another version of adm0 that excludes lake MWI => union adm1
# 2. Any next steps that include the (total crop) area of the grid cell requires special treatment of the 
# border cells => potential approach is to polygize the raster, crop to adm0 and calculate area of border cells
# However all this might be a lot of extra work for very limited returns.

adm0 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm0_2000.rds"))
plot(adm0)
weights <- raster::extract(r, adm0, df = T, weights = T, normalizeWeights = F) %>%
  setNames(c("polyID", "gridID", "weight"))

gridID_df <- as.data.frame(rasterToPoints(grid))
check <- left_join(weights, gridID_df) 

check_r <- r
check_r[!(check_r %in% check$gridID)] <- NA
plot(check_r)
plot(adm0)
plot(check_r, add = T)
plot(grid_py, add = T, border = "red")
plot(adm0, add = T, border = "red")

