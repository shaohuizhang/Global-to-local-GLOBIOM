#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview", "sf")
# Additional packages
p_load("XLConnect", "measurements", "nominatim")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD MAPS AND DATA
# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
grid_r <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# Irrigation
GMIA <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GMIA_MWI.rds"))
ir_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2000")


### TRANSFORM MAPS
crs <- "+init=EPSG:4326" # WSG84
crs_ir <- "+init=EPSG:21036" 
grid <- spTransform(grid, CRS(crs)) 
grid_sf <- st_as_sf(grid)


### PREPARE 2000 MAP
# Calculate radius so that circle has same area as stated irrigated area
ir <- ir_raw %>%
  gather(crop, value, -site:-type) %>%
  na.omit() %>%
  mutate(radius = sqrt(value*10000/pi),
         ID = 1:nrow(.)) 

ir_geo <- ir
coordinates(ir_geo) <- c("lon", "lat")
proj4string(ir_geo) <- crs
ir_geo <- spTransform(ir_geo, CRS(crs_ir)) 
ir_geo <- gBuffer(ir_geo, width=ir_geo$radius, byid=T)
ir_geo <- spTransform(ir_geo, CRS(crs)) 

# Compare grid with irrigation points
# Note that one irrigation point got lost because it is located in a grid cell that is not part of the MWI grid
ir_in_MWI <- as.data.frame(ir_geo[grid, ])
ir_out_MWI <- filter(ir, !(ID %in% ir_in_MWI$ID))

# Plot
mapview(ir_geo)
mapview(grid) + mapview(ir_geo[ir_geo$ID == ir_out_MWI$ID,])


### CALCULATE SIZE OF IRRIGATION POINTS IN GRID
# We use package sf and need to convert all polygons to sf class

ir_sf <- st_as_sf(ir_geo) %>%
  dplyr::select(ID, geometry, value) %>%
  mutate(area = as.numeric(st_area(.)/10000))
  
# Determine intersection of simu polygons with adm2 to calculate the share of simu that lies within an adm.
# This is to check if simus are fragmented over multiple adms.
grid_ir <- st_intersection(grid_sf, ir_sf)  

# Add polygion size in areas in ha
area_grid_ir <- grid_ir %>% 
  mutate(area_prox = as.numeric(st_area(.)/10000))
  
# For irrigated point, get area per adm
# We multiply this with original size of irrigated area as calculate of polygon area is slightly smaller than real area.
# This is probably caused by the unprojected CRS (in lat, lon instead of meters)
# Sum over irrigated points to get total irrigated area by gridID
area_ir_in_grid <- area_grid_ir %>% 
  as_tibble() %>% 
  group_by(ID) %>% 
  mutate(ir_area_prox = sum(area_prox),
         share = area_prox/ir_area_prox,
         ir_area = share*value) %>%
  ungroup() %>%
  group_by(gridID) %>%
  summarize(ir_area = sum(ir_area))
  
# Create raster
ir_map <- as.data.frame(rasterToPoints(grid_r)) %>%
  left_join(area_ir_in_grid) %>%
  dplyr::select(-gridID) %>%
  replace(is.na(.), 0) 
ir_map <- rasterFromXYZ(ir_map)
crs(ir_map) <- crs

# Plot
mapview(ir_map, legend = T, alpha.regions = 0.6)
# Note one point is missing
sum(ir$value)
cellStats(ir_map, sum)

# Save
saveRDS(ir_map, file.path(dataPath, "Data/MWI/Processed/Maps/irrigation_map_MWI_2000.rds"))

