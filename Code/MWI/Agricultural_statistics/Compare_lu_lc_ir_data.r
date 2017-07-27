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
ir_grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ir_grid_MWI_2000.rds")) 

# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
grid_r <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# Adm
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))


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
ir_adm2 <- ir_grid %>%
  group_by(adm2, lc) %>%
  summarize(value = sum(ir_area, na.rm = T)) %>%
  rename(adm = adm2) %>%
  mutate(type = "irrigated")

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

# Identify adms with not enough crop cover
check_adm2 <- adm2 %>%
  spread(type, value) %>%
  mutate(check_lu = land_cover - land_use) %>%
  filter(lc == "crops", check_lu <0)


### CORRECT DISCREPANCY LAND COVER AND LAND USE
# For some adm2 crop and rice area is larger than land cover area from maps.
# Problems for rice are caused by problematic statistics, mainly Nhkata Bay, which we imputed.
# We discard adm statistics and allocate national statistics to rice land cover. 
# Small remainder is allocated to crop cover.

# We used the following corrections:

# (1) We set land use to 0 for Likoma
ag_stat_upd <- ag_stat %>%
  mutate(value = ifelse(adm == "LIKOMA", 0, value))

# (2) We remove adm level rice statistics
ag_stat_upd <- ag_stat_upd %>%
  filter(!(adm != "MWI" & short_name == "rice"))

# (3) We add all non pure agri grid cells to adms where lc < lu
lc_upd <- bind_rows(
  filter(lc_raw, suitability_level == 1),
  filter(lc_raw, suitability_level == 2, adm2 %in% check_adm2$adm))

lc_adm2 <- lc_upd %>%
  group_by(adm2, lc, type) %>%
  summarize(value = sum(area, na.rm = T)) %>%
  rename(adm = adm2)

# Land use at adm 2 level
lu_adm2 <- ag_stat_upd %>%
  left_join(lc2crop_lvst) %>%
  filter(variable == "area", adm_level == 2) %>%
  mutate(type = "land_use") %>%
  group_by(adm, lc, type) %>%
  summarize(value = sum(value, na.rm = T)) 

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

# Identify adms with not enough crop cover
check_adm2 <- adm2 %>%
  spread(type, value) %>%
  mutate(check_lu = land_cover - land_use) %>%
  filter(lc == "crops", check_lu <0)

### SAVE
# ag_stat_upd
saveRDS(ag_stat_upd, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_upd_MWI.rds"))

# lc_upd
saveRDS(lc_upd, file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/lc_2000_upd_MWI.rds"))
