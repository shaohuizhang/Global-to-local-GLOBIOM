#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid ESA 2000 land cover data to 1 x 1km/30 arc-sec
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
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm1_2000.rds")))

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/ESA/ESA_", iso3c_sel, "_raw_2000.tif")))

# Load land cover classes
lc_class <- read_csv(file.path(dataPath, "Data/Global/ESA/ESACCI-LC-Legend.csv")) %>%
   dplyr::select(lc_code, lc_class)

# Grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"


### PROCESS LAND COVER MAP
# Compare Projections
crs(grid_30sec)
crs(lc_raw)
plot(lc_raw) # might take a long time in case of high resolution!
plot(adm1, add = T)


### RESAMPLE MAP TO 30 ARC-SEC GRID
# Specify input and output files
lc_raw_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/ESA/ESA_", iso3c_sel, "_raw_2000.tif"))
lc_30sec_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/lc/lc_ESA_30sec_2000_", iso3c_sel, ".tif"))
grid_30sec_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif"))

# Resample
lc_30sec <- align_raster_f(lc_raw_file, grid_30sec_file, lc_30sec_file, nThreads = "ALL_CPUS", verbose = T, 
                         output_Raster = T, overwrite = TRUE, r = "near", border = adm1)
names(lc_30sec) <- "lc_code"
plot(lc_30sec)
plot(adm1, add = T)


### COMPUTE AREA BY LAND COVER CLASS
# area size
area <- area(grid_30sec)
names(area) <- "lc_area"

# Rasterize adm
adm_r <- rasterize(adm1, grid_30sec)
names(adm_r) <- "ID"

# Stack 
lc_stack <- stack(lc_30sec, grid_30sec, area, adm_r)
plot(lc_stack)

# Get adm info
adm1_df <- levels(adm_r)[[1]] %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME), ID) %>%
  left_join(.,adm1_map) %>%
  dplyr::select(-adm1_GAUL) 


# Create data.frame, remove cells outside border and add adm names
lc_df <- as.data.frame(rasterToPoints(lc_stack)) %>%
  left_join(adm1_df) %>%
  left_join(lc_class) %>%
  na.omit %>%
  mutate(lc_area = lc_area *100) %>% # km3 to ha
  dplyr::select(-ID, -lc_code)

# Save
saveRDS(lc_df, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_ESA_2000_", iso3c_sel, ".rds")))
