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


### SOURCE
source(file.path(root, "Code/Support/distribute_irrigation.R"))


### LOAD MAPS AND DATA
# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))

# Irrigation
ir_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2000")

# land_cover
lc_raw <- readRDS(file.path(dataPath, "Data/MWI/processed/Spatial_data/land_cover_FAO_2000_MWI.rds"))


### TRANSFORM MAPS
crs <- "+init=EPSG:4326" # WSG84
crs_ir <- "+init=EPSG:21036" 
grid <- spTransform(grid, CRS(crs_ir)) 


### PREPARE IRRIGATED MAP
ir <- ir_raw %>%
  gather(short_name, value, -site:-type) %>%
  na.omit() %>%
  mutate(lc = ifelse(short_name %in% c("vege", "maize"), "crops", short_name),
         lc = ifelse(lc %in% c("coff"), "teas", lc),
         ID = c(1:nrow(.)))

ir_geo <- ir
coordinates(ir_geo) <- c("lon", "lat")
proj4string(ir_geo) <- crs
ir_geo <- spTransform(ir_geo, CRS(crs_ir)) 

# Compare grid with irrigation points
# Note that several irrigation points are located in a grid cell that is not part of the MWI grid
# Irrigated area of these points wil be located at closest grid cell. 
# Potentially, the available crop land can be too small for the allocation process to work.
# The chance for this is very small.
ir_in_MWI <- as.data.frame(ir_geo[grid, ])
ir_out_MWI <- filter(ir, !(ID %in% ir_in_MWI$ID))

# Plot
mapview(grid, col.regions = NA) + mapview(ir_geo[ir_geo$ID %in% ir_out_MWI$ID,]) 


### ALLOCATE IRRIGATED AREA TO GRID CELLS
# Determine how much crop specific cover is available in/near the location of the irrigated areas
# Prepare lc
lc <- lc_raw %>%
  group_by(gridID, lc) %>%
  summarize(area = sum(area, na.rm = T))

# Tea and coff grid
tea_grid <- prep_f(c("teas", "coff"))

# Distribute
teas_ir_area <- distribute_f(tea_grid, c("teas", "coff"))

# Sugc grid
# Prepare input data for distribution
sugc_grid <- prep_f(c("sugc"))

# Distribute
sugc_ir_area <- distribute_f(sugc_grid, c("sugc"))


# rice grid
# Prepare input data for distribution
rice_grid <- prep_f(c("rice"))

# Distribute
rice_ir_area <- distribute_f(rice_grid, c("rice"))


# Crops grid
crops_grid <- prep_f(c("crops"))

# Distribute
crops_ir_area <- distribute_f(crops_grid, c("crops"))

# Combine
ir_grid <- bind_rows(teas_ir_area, sugc_ir_area, rice_ir_area, crops_ir_area) %>%
  filter(ir_area >0)

# Save
saveRDS(ir_grid, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ir_grid_MWI_2000.rds"))





# Irrigated
teas_gridID <- lc$gridID[lc$lc == "teas" & lc$area >0]
teas_grid <- grid[grid$gridID %in% teas_gridID,]









# Check location
grid_r_p <- rasterToPoints(grid_r) %>%
  as.data.frame()

teas_map <- grid_r_p %>%
  left_join(.,teas_ir_area) %>%
  filter(ir_area >0) %>%
  ungroup() %>%
  dplyr::select(x, y, ir_area) %>%
  as.data.frame() %>%
  rasterFromXYZ(.)
crs(teas_map) <- crs

mapview(teas_map, legend = T, alpha.regions = 0.6) +
  mapview(ir_geo[ir_geo$short_name %in% c("teas","coff"),], cex = "value", alpha.region = 0.2) +
  mapview(teas_grid, col.regions = NA)



# Irrigated
sugc_gridID <- lc$gridID[lc$lc == "sugc" & lc$area >0]
sugc_grid <- grid[grid$gridID %in% sugc_gridID,]

# Check location
grid_r_p <- rasterToPoints(grid_r) %>%
  as.data.frame()

sugc_map <- grid_r_p %>%
  left_join(.,sugc_ir_area) %>%
  filter(ir_area >0) %>%
  ungroup() %>%
  dplyr::select(x, y, ir_area) %>%
  as.data.frame() %>%
  rasterFromXYZ(.)
crs(sugc_map) <- crs

mapview(sugc_map, legend = T, alpha.regions = 0.6) +
  mapview(ir_geo[ir_geo$short_name %in% c("sugc"),], cex = "value", alpha.region = 0.2) +
  mapview(sugc_grid, col.regions = NA)








