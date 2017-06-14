#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to grid land cover map 
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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))
plot(adm1)
plot(adm2)


### LOAD COUNTRY GRID
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
plot(grid)


### LOAD ADM GRID INFO
adm_grid <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/adm_grid_2000_MWI.csv"))


### LOAD LAND COVER MAP 
# Load map
land_cover_map_raw <- raster(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 2\\malawi 2000 classification scheme2.img"))
land_cover_map_raw

# Save land class
land_class <- levels(land_cover_map_raw)[[1]]
#write_csv(land_class, file.path(dataPath, "Data/MWI/processed/Spatial_data/land_class_MWI.csv"))

# check projection, should be same as r
# REPROJECT, IF NEEDED
crs(grid)
crs(land_cover_map_raw)

# Set crs
crs <- crs(land_cover_map_raw)

# Reproject grid
grid <- spTransform(grid, crs)

# Reproject adm2
adm2 <-  spTransform(adm2, crs)

### PREPARE COUNTRY LAND COVER MAP
# Mask (takes a long time) and crop are not needed
land_cover_map <- land_cover_map_raw

# Add colours based on RGB information
map_colour <- levels(land_cover_map)[[1]] %>%
  mutate(colour = rgb(Red, Green, Blue, max = 255),
         colour = ifelse(colour == "#000000", "#FFFFFF", colour)) %>% # replace black with white for shadows and no data
  unique()
map_colour <- map_colour[order(map_colour$Land_Cover, decreasing = F),]

# Plot
levelplot(land_cover_map_raw, att='Land_Cover', col.regions = map_colour$colour, margin = F) +
  layer(sp.polygons(adm2, col = "black", lwd = 2)) +
  layer(sp.polygons(grid, col = "black", lwd = 2))

### GRID CROP LAND DATA
# Create SimuID list
# Add unique as several SimuS consist of multiple polygons!
grid_list <- unique(grid@data$gridID)

# Overlay land cover map and grid polygon
# function to extract values by gridID
extract_grid_f <- function(polyID, cover){
  print(polyID)
  poly_grid <- grid[grid$gridID==polyID,]
  df_poly <- raster::extract(land_cover_map, poly_grid, df = T) %>%
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
#land_cover_shares_raw <- bind_rows(lapply(grid_list, extract_grid_f)) 
#saveRDS(land_cover_shares_raw, file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_raw_2000_MWI.rds"))
land_cover_shares_raw <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_raw_2000_MWI.rds"))

# Select annual cropland and perrenial cropland and aggregate (later we will try to split)
# Check total of shares
check_total <- land_cover_shares_raw %>%
  gather(variable, value, -gridID) %>%
  group_by(gridID) %>%
  summarize(total = sum(value, na.rm=T))

crop_cover <- land_cover_shares_raw %>%
  gather(variable, value, -gridID) %>%
  filter(variable %in% c(9, 10)) %>%
  group_by(gridID) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  filter(value >0) 

### COMBINE WITH ADM DATA
# Load gridded adm data
adm_grid <- read_csv(file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv")) 

# Combine data and express area in ha
crop_cover <- crop_cover %>%
  left_join(., adm_grid) %>%
  mutate(grid_size = grid_size * 100,
         area = value*grid_size) %>%
  dplyr::select(-value)

# Save
saveRDS(crop_cover, file.path(dataPath, "Data/MWI/processed/Spatial_data/crop_cover_2000_MWI.rds"))
