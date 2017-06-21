#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to grid adm information to 5 arc min
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


### SOURCE
source("Code/process_large_raster_f.R")


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### PREPARE 5 ARC MIN GLOBAL RASTER IN WSG84
r <- raster() # 1 degree raster
r <- disaggregate(r, fact=12) # 5 arcmin raster
values(r) <- 1:ncell(r) # Add ID numbers


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))
plot(adm1)
plot(adm2)

# check projection, should be same as r
# REPROJECT, IF NEEDED
crs(r)
crs(adm1)
crs(adm2)

### CREATE COUNTRY GRID
# Mask and crop raster
grid <- mask(r, adm1)
grid <- crop(grid, adm1)
plot(grid)
names(grid) <- "gridID"

# Add area information
area <- area(grid)
names(area) <- "grid_size"

# Stack grid id and area
grid <- stack(grid, area)

### OVERLAY ADM MAPS TO ALLOCATE CELLS TO ADM REGIONS
# Read adm mappping
MWI2adm1 <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

MWI2adm2 <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm2, adm2_GAUL) %>%
  na.omit %>%
  unique()

# Get adm info
adm2_df <- adm2@data %>%
  transmute(adm2_GAUL = toupper(ADM2_NAME), ID = 1:length(adm2_GAUL))

adm1_df <- adm1@data %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME)) %>%
  unique() %>%
  mutate(ID = 1:length(adm1_GAUL))

# Identify cells
adm1_grid <- raster::extract(grid, adm1, df = T) %>%
  left_join(.,adm1_df) %>%
  left_join(.,MWI2adm1) %>%
  dplyr::select(-ID, -adm1_GAUL)

adm2_grid <- raster::extract(grid, adm2, df = T) %>%
  left_join(.,adm2_df) %>%
  left_join(.,MWI2adm2) %>%
  dplyr::select(-ID, -adm2_GAUL)

# Create adm1 and adm2 file
adm_grid <- left_join(adm1_grid, adm2_grid) %>%
  mutate(adm0 = "MWI")

# Save files
write_csv(adm_grid, file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv"))
  

