#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare and harmonise land use, land cover and irrigation data
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
lc_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 

# Agricultural statistics
lu_adm_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 

# Irrigation
ir_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/gmia/gmia_30sec_", iso3c_sel, ".tif")))
names(ir_raw) <- "ir_area"
ir_raw2 <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/gia/gia_", iso3c_sel, ".tif"))) 
names(ir_raw2) <- "ir_area"

# lu_sy 
lu_sy_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# Grid
grid <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid) <- "gridID"



### COMPARE LC and LU ADM
# Combine
adm0_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 0) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(type = "lu"),
  lc_raw %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(type = "lc"))

ggplot(data = adm0_comp, aes(x = type, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw()


### COMPARE ADM2
# Combine
adm_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1) %>%
    group_by(adm) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(type = "lu"),
  lc_raw %>%
    group_by(adm) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(type = "lc"))

ggplot(data = adm_comp, aes(x = adm, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# Identify adms with not enough crop cover
check_adm <- adm_comp %>%
  spread(type, value) %>%
  mutate(check_lu = lc - lu) %>%
  filter(check_lu <0)


### COMPARE IRRIGATION
# Link gmia with gridid
ir_grid <- stack(grid, ir_raw2)
ir_grid_df <- as.data.frame(rasterToPoints(ir_grid)) %>%
  dplyr::select(-x, -y) %>%
  filter(ir_area >0)
sum(ir_grid_df$ir_area, na.rm = T)

# Link to lc
ir_grid <- left_join(lc_raw, ir_grid_df) 
sum(ir_grid$value[!is.na(ir_grid$ir_area)])
sum(lu_sy_raw$value[lu_sy_raw$system == "I"])
summary(ir_grid)

ir_grid_30sec_df <- dplyr::select(ir_grid, gridID, value = ir_area) %>%
  filter(value >0)

# save
saveRDS(ir_grid_30sec_df, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/ir_2000_", iso3c_sel, ".rds")))



