#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# CHECK
# Code contains a minor bug. Loop should not be over crops but separately over maize and vege (irrigated crops)
# Now last allocation of maiz is allocated to vege. Only relatively very small bias but can be corrected.

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


### SET COUNTRY
source("Code/MWI/Set_country.R")

### SOURCE
source(file.path(root, "Code/Support/distribute_irrigation.R"))


### LOAD MAPS AND DATA
# adm2
adm2 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm2_2000_adj.rds")))

# Adm_map
adm2_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm2, adm2_GAUL) %>%
  na.omit %>%
  unique()

# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid/grid_30sec_p_MWI.rds"))
grid_r <- raster(file.path(dataPath, "Data/MWI/Processed/Maps/grid/grid_30sec_r_MWI.tif"))
names(grid_r) <- "gridID"

# Irrigation
ir_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2000")

# land_cover
lc_raw <- readRDS(file.path(dataPath, "Data/MWI/processed/Agricultural_statistics/lc_FAO_2000_MWI.rds"))


### TRANSFORM MAPS
# crs <- "+init=EPSG:4326" # WSG84
crs_ir <- "+init=EPSG:21036" 
# grid <- spTransform(grid, CRS(crs_ir)) 


### PREPARE IRRIGATED MAP
ir <- ir_raw %>%
  gather(short_name, value, -site:-type) %>%
  na.omit() %>%
  mutate(short_name = ifelse(short_name %in% c("coff", "teas"), "teas_coff", short_name),
         lc = ifelse(short_name %in% c("vege", "maiz"), "crops", short_name),
         ID = c(1:nrow(.)))

ir_geo <- ir
coordinates(ir_geo) <- c("lon", "lat")
proj4string(ir_geo) <- crs(grid)
#ir_geo <- spTransform(ir_geo, CRS(crs_ir)) 
plot(ir_geo)

# Compare grid with irrigation points
# Note that several irrigation points are located in a grid cell that is not part of the MWI grid
# Irrigated area of these points wil be located at closest grid cell. 
# Potentially, the available crop land can be too small for the allocation process to work.
# The chance for this is very small.
ir_in_MWI <- as.data.frame(ir_geo[grid, ])
ir_out_MWI <- filter(ir, !(ID %in% ir_in_MWI$ID))

# Plot
#mapview(grid, col.regions = NA) + mapview(ir_geo[ir_geo$ID %in% ir_out_MWI$ID,]) 


### ALLOCATE IRRIGATED AREA TO GRID CELLS
# Determine how much crop specific cover is available in/near the location of the irrigated areas
# Prepare lc
lc <- lc_raw %>%
  filter(suitability_level == 1) %>% # We only include pure agric land cover classes
  group_by(gridID, lc) %>%
  summarize(area = sum(lc_area, na.rm = T))

# Tea and coff grid
teas_coff_grid <- prep_f(c("teas_coff"))

# Distribute
teas_coff_ir_area <- distribute_f(teas_coff_grid, c("teas_coff"))

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

# crops grid
crops_grid <- prep_f(c("crops"))

# Distribute
crops_ir_area <- distribute_f(crops_grid, c("crops"))

### COMBINE DATA, ADD ADM INFO
# Rasterize adm
adm_r <- rasterize(adm2, grid_r)
names(adm_r) <- "ID"

# Get adm info
adm2_df <- levels(adm_r)[[1]] %>%
  transmute(adm2_GAUL = toupper(ADM2_NAME), ID) %>%
  left_join(.,adm2_map) %>%
  dplyr::select(-adm2_GAUL) 

# Add gridID
adm_r <- stack(grid_r, adm_r)

# Create data.frame, remove cells outside border and add adm names
adm_grid <- as.data.frame(rasterToPoints(adm_r)) %>%
  left_join(.,adm2_df) %>%
  na.omit %>%
  dplyr::select(gridID, adm2)

# Combine, add adm info and remove checking variables
ir_grid <- bind_rows(teas_coff_ir_area, sugc_ir_area, rice_ir_area, crops_ir_area) %>%
  filter(ir_area >0) %>%
  mutate(system = "I") %>%
  left_join(., adm_grid) %>%
  dplyr::select(-area, -total_area, -ID)

# Clean up
rm(adm_grid, crops_grid, crops_ir_area, rice_grid, rice_ir_area, sugc_grid, sugc_ir_area, teas_coff_grid, 
   teas_coff_ir_area, lc, lc_raw, crs, crs_ir, ir, ir_raw, ir_geo, grid)

# Save
saveRDS(ir_grid, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ir_grid_MWI_2000.rds"))


