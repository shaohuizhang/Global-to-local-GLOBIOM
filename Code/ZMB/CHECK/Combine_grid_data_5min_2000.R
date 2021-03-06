#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to combine gridded data
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


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD DATA
# Grid
grid <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_r_", iso3c_sel, ".rds")))

# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))
plot(adm1)

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Land cover
lc <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/lc_RCMRD_30sec_2000_", iso3c_sel, ".rds")))

# Land cover shares
lc_sh_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/land_cover/lc_sh_raw_RCMRD_2000_5min_", iso3c_sel, ".rds")))


### ADD AREA AND ADM DATA
# area size
area <- area(grid)
names(area) <- "grid_size"

# Rasterize adm
adm_r <- rasterize(adm1, grid)
names(adm_r) <- "ID"

# Stack 
grid_all <- stack(grid, area, adm_r)
plot(grid_all)

# Get adm info
adm1_df <- levels(adm_r)[[1]] %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME), ID) %>%
  left_join(.,adm1_map) %>%
  dplyr::select(-adm1_GAUL)

# Create data.frame, remove cells outside border and add adm names
grid_all <- as.data.frame(rasterToPoints(grid_all)) %>%
  left_join(adm1_df) %>%
  na.omit %>%
  dplyr::select(-ID)


### ADD LAND COVER DATA
# Load land cover classes
lc_class <- read_csv(file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/land_cover_classes_2000_Scheme_II.csv"))) %>%
  mutate(ID = as.character(ID)) %>%
  dplyr::select(ID, Land_Cover)

lc <- lc_sh_raw %>%
  gather(ID, share, -gridID) %>%
  group_by(gridID, ID) %>%
  summarize(share = sum(share, na.rm = T)) %>%
  left_join(., lc_class)

# Combine data and express area in ha
grid_all <- lc %>%
  left_join(., grid_all) %>%
  mutate(grid_size = grid_size,
         area = share*grid_size) %>%
  ungroup()

# Save
saveRDS(grid_all, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/Grid_all_2000_", iso3c_sel, ".rds")))
