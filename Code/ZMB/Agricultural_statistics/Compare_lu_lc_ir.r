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


### SET COUNTRY
source("Code/ZMB/Set_country.R")


# LOAD DATA
# Crop cover data
lc_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/land_cover/lc_RCMRD_2000_5min_", iso3c_sel, ".rds"))) %>%
  mutate(type = "land_cover") %>%
  rename(value = area)

# Agricultural statistics
lu_adm_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ag_stat_2000_", iso3c_sel, ".csv"))) 

# Irrigation
ir_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/irrigation_", iso3c_sel, ".csv"))) 

# Suitability
su_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/gaez2iso_", iso3c_sel, ".rds")))
                 
# Grid
grid <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_", iso3c_sel, ".rds")))
grid_r <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_r_", iso3c_sel, ".rds")))

# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))

### COMPARE ADM0
# Aggregate crop cover at adm 0 level
lc_adm0 <- lc_raw %>%
  filter(ID == 8) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(type = "land_cover",
         adm0 = iso3c_sel)

# land use at adm 0 level
lu_adm0 <- lu_adm_raw %>%
  filter(variable == "area" & adm_level == 0) %>%
  rename(adm0 = adm) %>%
  group_by(adm0) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(type = "land_use")

# Irrigation at adm level
ir_adm0 <- ir_raw %>%
  summarize(value = sum(ir_area, na.rm = T)) %>%
  mutate(type = "irrigated",
         adm0 = iso3c_sel)

# Combine and plot
adm0_comp <- bind_rows(lu_adm0, lc_adm0, ir_adm0)

ggplot(data = adm0_comp, aes(x = type, y = value, fill = type)) +
  geom_col() +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
rm(lu_adm0, lc_adm0, ir_adm0, adm0_comp)


### COMPARE ADM0, CROP AND SYSTEM
# land use at adm 0, crop level and system
lu_adm0_crop <- lu_adm_raw %>%
  filter(variable == "area" & adm_level == 0) %>%
  rename(adm0 = adm) %>%
  group_by(adm0, short_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(type = "land_use")

# suitable area at adm 0, crop and system
su_adm0_crop <- su_raw %>%
  filter(variable == "area" & adm_level == 0) %>%
  rename(adm0 = adm) %>%
  group_by(adm0, short_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(type = "land_use")


### COMPARE ADM2
# Aggregate crop cover at adm 2 level
lc_adm2 <- lc_raw %>%
  filter(ID == 8) %>%
  group_by(adm1, type) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  rename(adm = adm1)

# Land use at adm 2 level
lu_adm2 <- lu_adm_raw %>%
  filter(variable == "area", adm_level == 1) %>%
  group_by(adm) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(type = "land_use")

# Combine and plot
adm2_comp <- bind_rows(lu_adm2, lc_adm2)

ggplot(data = adm2_comp, aes(x = adm, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# Identify adms with not enough crop cover
check_adm2 <- adm2 %>%
  spread(type, value) %>%
  mutate(check_lu = land_cover - land_use) %>%
  filter(lc == "crops", check_lu <0)
rm(lu_adm2, lc_adm2, ir_adm2, adm2)


### CORRECT DISCREPANCY LAND COVER AND LAND USE AND PREPARE GRID FILES
# Problems:
# (1) For some adm2 crop area is larger than land cover area from maps.
# (2) For some adm2 rice area is substantially larger than land cover area from maps. Problems for rice are caused by problematic statistics, mainly Nhkata Bay, which we imputed.
# (3) National rice area is larger than national rice land cover. 
# (4) Note that there are no ag statistics and no crop cover for Likoma. Hence, no data will be allocated.

# We applied the following corrections:

# (1) We discard adm rice statistics and allocate national statistics to rice land cover and add lc classification
lu_adm <- lu_adm_raw %>%
  filter(!(adm != "MWI" & short_name == "rice"))

# (2) We add all non pure agri grid cells to adms where lc < lu, add and select relevant variables
# This gives total available land cover per grid cell
lc_av <- bind_rows(
  filter(lc_raw, suitability_level == 1),
  filter(lc_raw, suitability_level == 2, adm2 %in% check_adm2$adm)) %>%
  group_by(gridID, lc, adm2, adm0) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  filter(value > 0)
rm(check_adm2)

# (3) We remove irrigated area to get area available for other systems 
lc_non_ir <- lc_av %>%
  left_join(., ir_grid) %>%
  mutate(value = ifelse(!is.na(ir_area), value-ir_area, value)) %>%
  dplyr::select(gridID, lc, value, adm0, adm2) %>%
  ungroup()

# (4) We allocate 5% of crop cover cells to rice cover if they occur in the same grid cell
shift = 0.05
lc_non_ir <- lc_non_ir %>%
  spread(lc, value) %>%
  mutate(crop_switch = ifelse(!is.na(rice) & !is.na(crops), shift*crops, 0),
         crops = crops - crop_switch,
         rice = rice + crop_switch) %>%
  dplyr::select(-crop_switch) %>%
  gather(lc, value, -gridID, -adm0, -adm2) %>%
  na.omit()

# (5) We add irrigated area back in to get total available area for each crop class.
lc_av <- lc_non_ir %>%
  left_join(., ir_grid) %>%
  mutate(value = ifelse(!is.na(ir_area), value + ir_area, value)) %>%
  dplyr::select(gridID, lc, value, adm0, adm2) %>%
  ungroup()
rm(lc_non_ir)

# (6) We prepare irrigated lc 
lc_ir <- ir_grid %>%
  dplyr::select(gridID, short_name, adm0, adm2, value = ir_area) %>%
  mutate(system = paste(short_name, "I", sep = "_"))


### CHECK AGAIN
# COMPARE ADM0
# Aggregate crop cover at adm 0 level
lc_adm0 <- lc_av %>%
  group_by(adm0, lc) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(type = "land_cover")

# land use at adm 0 level
lu_adm0 <- lu_adm %>%
  filter(variable == "area" & adm_level == 0) %>%
  rename(adm0 = adm) %>%
  mutate(type = "land_use") %>%
  group_by(adm0, type, lc) %>%
  summarize(value = sum(value, na.rm = T)) 

# Irrigation at adm level
ir_adm0 <- ir_grid %>%
  group_by(lc) %>%
  summarize(value = sum(ir_area, na.rm = T)) %>%
  mutate(type = "irrigated",
         adm0 = "MWI")

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
rm(lu_adm0, lc_adm0, ir_adm0, adm0)


# COMPARE ADM2
lc_adm2 <- lc_av %>%
  group_by(adm2, lc) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  rename(adm = adm2) %>%
  mutate(type = "land_cover")

# Land use at adm 2 level
lu_adm2 <- lu_adm %>%
  filter(variable == "area", adm_level == 2) %>%
  mutate(type = "land_use") %>%
  group_by(adm, lc, type) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()

# Combine and plot
adm2 <- bind_rows(lu_adm2, lc_adm2) %>%
  filter(lc %in% c("crops"))

ggplot(data = adm2, aes(x = adm, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~lc, scales = "free") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# Identify adms with not enough crop cover
check_adm2 <- adm2 %>%
  spread(type, value) %>%
  mutate(check_lu = land_cover - land_use) %>%
  filter(lc == "crops", check_lu <0)
rm(lu_adm2, lc_adm2, adm2, check_adm2)


### ADD PRODUCTION SYSTEM INFO TO AG_STAT
# (1) Irrigated crops
I <- lc_ir %>%
  group_by(short_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(system = "I")
  
# (2) All non-irrigated sugc, teas_coff, predominantly produced in estates/plantations are rainfed-high input (H)
H <- lu_adm %>%
  filter(short_name %in% c("sugc", "teas_coff"), adm == "MWI") %>%
  left_join(.,dplyr::rename(I, I = value)) %>%
  mutate(value= value - I,
         system = "H") %>%
  dplyr::select(value, short_name, system)

# (3) All non-irrigated other crops are subsistence (S)
S <- lu_adm %>%
  filter(!short_name %in% c("sugc", "teas_coff"), adm == "MWI") %>%
  left_join(.,dplyr::rename(I, I = value)) %>%
  mutate(value = ifelse(!is.na(I), value - I, value),
         system = "S") %>%
  dplyr::select(value, short_name, system)

# Combine
# keep short_name and lc to make mapping for GAMS later
lu_system <- bind_rows(I, H, S) %>%
  mutate(system = paste(short_name, system, sep = "_"))

# Compare with ag_stat_upd total => should be the same
sum(lu_system$value)
sum(lu_adm$value[lu_adm$adm == "MWI"])


### COMBINE MODEL INPUT DATA FILES
model_data <- list()

# Land use per agricultural system
model_data[["lu_system"]] <- lu_system

# Land use per adm
model_data[["lu_adm"]] <- lu_adm

# Irrigated land cover per grid cell
model_data[["lc_ir"]] <- lc_ir

# Available land cover per crop group
model_data[["lc_av"]] <- lc_av

# save
saveRDS(model_data, file.path(dataPath, "Data/MWI/Processed/GAMS/model_data.rds"))

