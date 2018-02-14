#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid RCMRD 2000 land cover data to 10 x 10km/5 arc-min
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview")
# Additional packages
p_load("countrycode", "plotKML")

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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### SET COUNTRY
source("Code/ZMB/Set_country.R")

### DATA
# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Grid polygon
grid <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_", iso3c_sel, ".rds")))

# grid raster
grid_r <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_r_", iso3c_sel, ".rds")))

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II.tif")))
levels(lc_raw)


### PROCESS LAND COVER MAP
# Obtain land cover class
lc_class <- levels(lc_raw)[[1]] %>%
  rename(class = `Land_Cover`)
write_csv(lc_class, file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/land_cover_classes_2000_Scheme_II.csv")))

# Compare Projections
crs(grid)
crs(lc_raw)
plot(lc_raw) # might take a long time in case of high resolution!
plot(adm1, add = T)
#plot(grid, add = T) # might take a long time in case of high resolution!


# If not the same, reproject country polygon, conduct analysis and reproject back - faster than reprojecting high resolution maps
# Set crs
#crs <- crs(lc_raw)

# Reproject grid
#grid <- spTransform(grid, crs)

# Reproject adm
#adm1 <-  spTransform(adm1, crs)


### GRID CROP LAND DATA 2000 MAP
# Create grid list
grid_list <- unique(grid@data$gridID)

# Overlay land cover map and grid polygon
# function to extract values by gridID
extract_grid_f <- function(polyID, cover){
  print(polyID)
  poly_grid <- grid[grid$gridID==polyID,]
  df_poly <- raster::extract(cover, poly_grid, df = T) %>%
    setNames(c("ID", "class")) %>%
    na.omit() %>%
    group_by(class) %>%
    summarize(n = n()) %>%
    mutate(freq = n / sum(n, na.rm = T)) %>%
    dplyr::select(-n) %>%
    spread(class, freq) %>%
    mutate(gridID = polyID)
}


# Run function and combine
#lc_sh_raw <- bind_rows(lapply(grid_list, extract_grid_f, lc_raw)) 
#saveRDS(lc_sh_raw, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/land_cover/lc_sh_raw_RCMRD_2000_5min_", iso3c_sel, ".rds")))
lc_sh_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/land_cover/lc_sh_raw_RCMRD_2000_5min_", iso3c_sel, ".rds")))


### COMPUTE AREA BY LAND COVER CLASS
# area size
area <- area(grid_r)
names(area) <- "grid_size"

# Rasterize adm
adm_r <- rasterize(adm1, grid_r)
names(adm_r) <- "ID"

# Stack 
grid_all <- stack(grid_r, area, adm_r)
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

# Load land cover classes
lc_class <- read_csv(file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/land_cover_classes_2000_Scheme_II.csv"))) %>%
  mutate(ID = as.character(ID)) %>%
  dplyr::select(ID, class)

lc <- lc_sh_raw %>%
  gather(ID, share, -gridID) %>%
  group_by(gridID, ID) %>%
  summarize(share = sum(share, na.rm = T)) %>%
  left_join(., lc_class)

# Calculate area size in ha (area is in m3)
lc <- lc %>%
  left_join(., grid_all) %>%
  mutate(grid_size = grid_size,
         grid_size = grid_size*100,
         area = share*grid_size) %>%
  ungroup() %>%
  dplyr::select(-share)

# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/land_cover/lc_RCMRD_2000_5min_", iso3c_sel, ".rds")))





