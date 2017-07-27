#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to validate irrigation data
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
# Adm
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))

# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
grid_r <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# Irrigation
GMIA <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GMIA_MWI.rds"))
ir_2000_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2000")
ir_2010_raw <- read_excel(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI_combined.xlsx"), sheet = "ir_2010")

# land_cover
lc_raw <- readRDS(file.path(dataPath, "Data/MWI/processed/Spatial_data/land_cover_FAO_2000_MWI.rds"))

### COMPARE LAND COVER with IRRIGATION POINTS
# There is a good overlap between sugar, tea and rice estates and related land cover. 
# Almost all tea and sugar land cover seems to be irrigated. Only a part of rice cultivation is irrigated.
# It appears that the Ngapani coffee estate is identified as sugar cane land cover => aggregate sugar and coffee statistics.

# prepare ir data
ir_2000 <- ir_2000_raw %>%
  gather(crop, value, -site:-type) %>%
  na.omit()

ir_2010 <- ir_2010_raw %>%
  gather(crop, value, -site:-type) %>%
  na.omit()

# Prepare ir geo
crs <- CRS("+init=EPSG:4326") # WSG84
ir_2000_geo <- ir_2000
coordinates(ir_2000_geo) <- c("lon", "lat")
proj4string(ir_2000_geo) <- crs

ir_2010_geo <- ir_2010
coordinates(ir_2010_geo) <- c("lon", "lat")
proj4string(ir_2010_geo) <- crs

# Prepare lc
lc <- lc_raw %>%
  group_by(gridID, lc) %>%
  summarize(area = sum(area, na.rm = T))
  
# Prepare maps with irrigated crops
grid_r_p <- rasterToPoints(grid_r) %>%
  as.data.frame()

### TEA AND COFF
# land cover teas map
# Tea and coff are combined because large coffee estate is considered as tea land cover
# Kavuzi tea estate, which geocodes were not exact (river instead of estate) is located too far North.
lc_teas_map <- grid_r_p %>%
  left_join(lc,.) %>%
  filter(lc == "teas", area >0) %>%
  ungroup() %>%
  dplyr::select(x, y, area) %>%
  as.data.frame()

coordinates(lc_teas_map) <- ~ x + y
gridded(lc_teas_map) <- TRUE
lc_teas_map <- raster(lc_teas_map)
crs(lc_teas_map) <- crs

# Irrigation map
ir_teas_map <- ir_grid %>%
  filter(lc %in% "teas") %>%
  left_join(grid_r_p,.) %>%
  dplyr::select(x, y, ir_area)

ir_teas_map <- rasterFromXYZ(ir_teas_map)
crs(ir_teas_map) <- crs

mapview(ir_teas_map, legend = T) +
  mapview(ir_geo, col.regions = "red", alpha.region = 0.2)
+ 
slideview(ir_teas_map, lc_teas_map)
mapview(grid, col.regions = NA, alpha.region = 0.2) +
mapview(lc_teas_map, alpha.regions = 0.4)
+
mapview(ir_2010_geo[ir_2010_geo$crop %in% c("teas","coff"),], color = "red", col.regions = "red", cex = "value", alpha.region = 0.2) +
mapview(ir_2000_geo[ir_2000_geo$crop%in% c("teas","coff"),], cex = "value", alpha.region = 0.2) 


# Sugar
lc_sugc_map <- grid_r_p %>%
  left_join(lc,.) %>%
  filter(lc == "sugc", area >0) %>%
  ungroup() %>%
  dplyr::select(x, y, area) %>%
  as.data.frame()

coordinates(lc_sugc_map) <- ~ x + y
gridded(lc_sugc_map) <- TRUE
lc_sugc_map <- raster(lc_sugc_map)
crs(lc_sugc_map) <- crs

mapview(lc_sugc_map, alpha.regions = 0.4) +
  mapview(ir_2010_geo[ir_2010_geo$crop %in% c("sugc"),], color = "red", col.regions = "red", cex = "value", alpha.region = 0.2) +
  mapview(ir_2000_geo[ir_2000_geo$crop%in% c("sugc"),], cex = "value", alpha.region = 0.2)

# rice
lc_rice_map <- grid_r_p %>%
  left_join(lc,.) %>%
  filter(lc == "rice", area >0) %>%
  ungroup() %>%
  dplyr::select(x, y, area) %>%
  as.data.frame()

coordinates(lc_rice_map) <- ~ x + y
gridded(lc_rice_map) <- TRUE
lc_rice_map <- raster(lc_rice_map)
crs(lc_rice_map) <- crs

mapview(lc_rice_map, alpha.regions = 0.4) +
  mapview(ir_2010_geo[ir_2010_geo$crop %in% c("rice"),], color = "red", col.regions = "red", cex = "value", alpha.region = 0.2) +
  mapview(ir_2000_geo[ir_2000_geo$crop%in% c("rice"),], cex = "value", alpha.region = 0.2)


### COMPARE IRRIGATED AREAS AND CROPCOVER AT GRID LEVEL TO SEE IF THERE IS SUFFICIENT CROP COVER
# lc at grid
lc_grid <- lc_raw %>%
  filter(suitability_level %in% c(1)) %>%
  group_by(gridID, lc, type) %>%
  summarize(value = sum(area, na.rm = T))

ir_grid <- ir_grid %>%
  left_join(lc2crop_lvst) %>%
  mutate(type = "irrigated") %>%
  group_by(gridID, lc, type) %>%
  summarize(value = sum(ir_area, na.rm = T)) %>%
  na.omit()

# Combine and plot
grid_check <- bind_rows(lc_grid, ir_grid) %>%
  filter(gridID %in% ir_grid$gridID) %>%
  spread(type, value) %>%
  na.omit() %>%
  mutate(check = land_cover > irrigated)

# Have a closes look at problematic grid cells
grid_problem <- filter(grid_check, !check)

mapview(adm2) +
  mapview(grid[grid$gridID %in% grid_problem$gridID,])

mapview(adm2) +
  mapview(grid[grid$gridID %in% c(5393945),])

# Prepare maps with irrigated crops
grid_r_p <- rasterToPoints(grid_r) %>%
  as.data.frame()
