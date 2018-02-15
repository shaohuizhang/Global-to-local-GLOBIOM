#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select GMIA information
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "plotKML")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### DATA
# gmia
gmia_r_raw <- raster(file.path(dataPath, "Data/Global/gmia/gmia_v5_aei_ha_asc/gmia_v5_aei_ha.asc")) # hectares per cell
#gmia_r_raw2 <- raster(file.path(dataPath, "Data/Global/gmia/gmia_v5_aeigw_pct_aei_asc/gmia_v5_aeigw_pct_aei.asc")) # hectares per cell
crs(gmia_r_raw) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Grid
grid_5min <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_5min_r_", iso3c_sel, ".tif")))
names(grid_5min) <- "gridID"
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"

# adm
adm <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/GAUL/adm_2000_", iso3c_sel, ".rds")))


### SELECT COUNTRY GMIA MAP
gmia_5min <- crop(gmia_r_raw, adm)
gmia_5min <- mask(gmia_5min, adm)
names(gmia_5min) <- "gmia"
levelplot(gmia_5min)
hist(gmia_5min, breaks = 50)
cellStats(gmia_5min,sum)

# Save map
writeRaster(gmia_5min, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gmia/gmia_5min_", iso3c_sel, ".tif")), overwrite = T)


### RESAMPLE GMIA MAP TO 30 ARC-SEC GRID
# As values are in ha and relative to grid cell, we cannot simply warp to a higher resolution.,
# We calculate the share of irrigated area first and then warp
area_5min <- area(grid_5min)
gmia_5min_share <- gmia_5min/(area_5min*100)
writeRaster(gmia_5min_share, file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia/gmia_5min_share_", iso3c_sel, ".tif")), overwrite = T)

# Specify input and output files
gmia_5min_share_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia/gmia_5min_share_", iso3c_sel, ".tif"))
gmia_30sec_share_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia/gmia_30sec_share_", iso3c_sel, ".tif"))
grid_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif"))

# Resample
gmia_30sec_share <- align_raster_f(gmia_5min_share_file, grid_file, gmia_30sec_share_file, nThreads = "ALL_CPUS", verbose = T,
                                   output_Raster = T, overwrite = TRUE, r = "bilinear", border = adm)
names(gmia_30sec_share) <- "gmia"

# Calculate area
area_30sec <- area(grid_30sec)
names(area_30sec) <- "grid_size"
gmia_30sec <- gmia_30sec_share*area_30sec*100
names(gmia_30sec) <- "gmia"

plot(gmia_30sec)
cellStats(gmia_30sec, sum)
cellStats(gmia_5min, sum)

# ### RESAMPLE TO 30 ARCSEC
# # As the resolution of the gmia is at 5 arcmin and the lc map is 30 arcsec we need to downscale.
# # Given the detailed lc map, we create a 30 sec GMIA map with has 1 for equiped for irrigation, 0 otherwise.
# # Using the data on ha in the lc map we can obtain the ha irrigated.
# 
# # Create gmia 1/0 map
# gmia <- gmia_5min
# gmia[gmia > 0] <- 1
# gmiax <- disaggregate(gmia, fact=10) # 30 arcsec raster
# # # Specify input and output files
# gmia_5min_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia/gmia_5min_", iso3c_sel, ".tif"))
# gmia_30sec_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia/gmia_30sec_", iso3c_sel, ".tif"))
# grid_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif"))

# # # Resample
# gmia_30sec <- align_raster_f(gmia_5min_file, grid_file, gmia_30sec_file, nThreads = "ALL_CPUS", verbose = T, 
#                                   output_Raster = T, overwrite = TRUE, r = "near", border = adm)
# names(gmia_30sec_share) <- "gmia"

# Save
writeRaster(gmia_30sec, file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia/gmia_30sec_", iso3c_sel, ".tif")), overwrite = T)


