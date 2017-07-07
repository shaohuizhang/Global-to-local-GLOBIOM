#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare land use and land cover data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
# Additional packages
p_load("countrycode")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen

# LOAD DATA
# Crop cover data
lc_raw <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_FAO_2000_MWI.rds")) %>%
  mutate(type = "land_cover")

# Agricultural statistics
ag_stat <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv")) 

# land cover crop_lvst mapping
lc2crop_lvst <- read_excel(file.path(dataPath, "Data/MWI/Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_lc2crop_lvst")

# Irrigation
ir_stat <- readRDS(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ir_stat_MWI_2000.rds")) 
ir_grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ir_grid_MWI_2000.rds")) 

# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
grid_r <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# Adm
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))

# COMPARE ADM2
# Aggregate crop cover at adm 2 level
lc_adm2 <- lc_raw %>%
  filter(suitability_level %in% c(1)) %>%
  group_by(adm2, lc, type) %>%
  summarize(value = sum(area, na.rm = T)) %>%
  rename(adm = adm2)

# Land use at adm 2 level
lu_adm2 <- ag_stat %>%
  left_join(lc2crop_lvst) %>%
  filter(variable == "area", adm_level == 2) %>%
  mutate(type = "land_use") %>%
  group_by(adm, lc, type) %>%
  summarize(value = sum(value, na.rm = T)) 

# Irrigation at adm level
ir_adm2 <- ir_stat %>%
  left_join(lc2crop_lvst) %>%
  mutate(type = "irrigated") %>%
  group_by(adm2, lc, type) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  rename(adm = adm2)

# Combine and plot
adm2 <- bind_rows(lu_adm2, lc_adm2, ir_adm2) %>%
  filter(lc %in% c("crops", "rice"))

ggplot(data = adm2, aes(x = adm, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~lc, scales = "free") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


# COMPARE ADM0
# Aggregate crop cover at adm 0 level
lc_adm0 <- lc_raw %>%
  filter(suitability_level %in% c(1)) %>%
  group_by(adm0, lc, type) %>%
  summarize(value = sum(area, na.rm = T))

# land use at adm 0 level
lu_adm0 <- ag_stat %>%
  filter(variable == "area" & adm_level == 0) %>%
  left_join(lc2crop_lvst) %>%
  rename(adm0 = adm) %>%
  mutate(type = "land_use") %>%
  group_by(adm0, type, lc) %>%
  summarize(value = sum(value, na.rm = T)) 

# Irrigation at adm level
ir_adm0 <- ir_stat %>%
  left_join(lc2crop_lvst) %>%
  mutate(type = "irrigated") %>%
  group_by(lc, type) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(adm0 = "MWI")

# Combine and plot
adm0 <- bind_rows(lu_adm0, lc_adm0, ir_adm0)

ggplot(data = adm0, aes(x = adm0, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~lc, scales = "free")


### COMPARE IRRIGATED AREAS AND CROPCOVER AT GRID LEVEL
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

# sugc
lc_sugc_map <- grid_r_p %>%
  left_join(lc_grid,.) %>%
  filter(lc == "teas", value >0) %>%
  ungroup() %>%
  dplyr::select(x, y, value) %>%
  as.data.frame()

coordinates(lc_sugc_map) <- ~ x + y
gridded(lc_sugc_map) <- TRUE
lc_sugc_map <- raster(lc_sugc_map)
crs <- CRS("+init=EPSG:4326") # WSG84
crs(lc_sugc_map) <- crs

grid_problem_sugc <- filter(grid_problem, lc == "sugc")

mapview(lc_sugc_map) +
  mapview(grid[grid$gridID %in% grid_problem_sugc$gridID,])

# teas
lc_teas_map <- grid_r_p %>%
  left_join(lc_grid,.) %>%
  filter(lc == "teas", value >0) %>%
  ungroup() %>%
  dplyr::select(x, y, value) %>%
  as.data.frame()

coordinates(lc_teas_map) <- ~ x + y
gridded(lc_teas_map) <- TRUE
lc_teas_map <- raster(lc_teas_map)
crs <- CRS("+init=EPSG:4326") # WSG84
crs(lc_teas_map) <- crs

grid_problem_teas <- filter(grid_problem, lc == "teas")

mapview(lc_teas_map) +
  mapview(grid[grid$gridID %in% grid_problem_teas$gridID,])



### CORRECT DISCREPANCY LAND COVER AND LAND USE
# For some adm2 crop area is larger than land cover area from maps. 
# This is impossible unless, there is double cropping, fallowed land or data errors.
# As irrigated areas are not part of the model yet, we create a test dataset that simply shrinks the land use with a certain factor

# Calculate correction factor
adm2_corr <- adm2 %>%
  spread(type, value) %>%
  mutate(factor = land_use/land_cover) %>%
  filter(factor > 1) %>%
  dplyr::select(adm, factor)

# Correct area
adm2_maiz_corr <- ag_stat %>%
  filter(adm %in% adm2_corr$adm) %>%
  left_join(adm2_corr) %>%
  mutate(value = value/factor) %>%
  dplyr::select(-factor)

# replace values
ag_stat[ag_stat$adm %in% adm2_corr$adm, ] <- adm2_maiz_corr


# Save
write_csv(ag_stat_2000, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))
