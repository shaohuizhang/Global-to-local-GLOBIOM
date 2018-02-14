#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid GLC 2000 land cover data to 30 arc-sec and calculate shares per land cover class
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
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/GLC2000/GLC2000_raw_2000_", iso3c_sel, ".tif")))

# Load land cover classes
lc_class <- read_csv(file.path(dataPath, "Data/Global/GLC2000/glc2000_legend.csv")) %>%
   dplyr::select(lc_code, lc)

# Grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"


### RESAMPLE MAP TO 30 ARCSEC GRID
# Specify input and output files
lc_raw_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/GLC2000/GLC2000_raw_2000_", iso3c_sel, ".tif"))
lc_30sec_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/lc/lc_GLC2000_30sec_2000_", iso3c_sel, ".tif"))
grid_30sec_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif"))

# Resample
lc_30sec <- align_raster2_f(lc_raw_file, grid_30sec_file, lc_30sec_file, nThreads = "ALL_CPUS", verbose = T, 
                           output_Raster = T, overwrite = TRUE, r = "near")
names(lc_30sec) <- "lc_code"
plot(lc_30sec)
plot(adm, add = T)


### COMBINE LC AND GRID 
# No need to calculate shares as resolution is 1kmx1km
# Combine and calculate shares
lc_stack <- stack(grid_30sec, lc_30sec)
lc <- as.data.frame(rasterToPoints(lc_stack)) %>%
  set_names(c("x", "y", "gridID", "lc_code")) %>%
  na.omit() %>% # remove approx 1000 border cells where gridID and/or lc is NA
  unique()
summary(lc)

# Calculate area
grid_size <- area(grid_30sec)
names(grid_size) <- "grid_size"
grid_size <- stack(grid_30sec, grid_size)
grid_size <- as.data.frame(rasterToPoints(grid_size)) %>%
  na.omit

lc <- left_join(lc, grid_size) %>%
  mutate(value = grid_size*100) %>% # in ha
  dplyr::select(-grid_size)

# Filter out crop grid cells
crop_class <- lc_class %>%
  filter(lc == "crops")

lc <- lc %>%
  filter(lc_code %in% crop_class$lc_code)

# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_GLC2000_2000_", iso3c_sel, ".rds")))
