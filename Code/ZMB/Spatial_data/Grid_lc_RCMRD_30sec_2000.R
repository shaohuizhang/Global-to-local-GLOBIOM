#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid RCMRD 2000 land cover data to 1 x 1km/30 arc-sec
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview")
# Additional packages
p_load("countrycode", "plotKML")

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
options(max.print=1000000) # more is printed on screen


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### SET COUNTRY
source("Code/ZMB/Set_country.R")

### DATA
# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II.tif")))


### PROCESS LAND COVER MAP
# Obtain land cover class
lc_class <- levels(lc_raw)[[1]]
write_csv(lc_class, file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/land_cover_classes_2000_Scheme_II.csv")))

# Compare Projections
crs(grid)
crs(lc_raw)
plot(lc_raw) # might take a long time in case of high resolution!
plot(adm1, add = T)


### RESAMPLE MAP TO 30 ARC-SEC GRID
# Specify input and output files
lc_raw_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II.tif"))
lc_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/lc_RCMRD_30sec_2000_", iso3c_sel, ".tif"))
grid_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/30sec_grid_r_", iso3c_sel, ".tif"))

# Resample
lc <- align_raster_f(lc_raw_file, grid_file, lc_file, nThreads = "ALL_CPUS", verbose = T, 
                         output_Raster = T, overwrite = TRUE, r = "near", border = adm1)
names(lc) <- "lc"
  
plot(lc)
plot(adm1, add = T)

# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/lc_RCMRD_30sec_2000_", iso3c_sel, ".rds")))


