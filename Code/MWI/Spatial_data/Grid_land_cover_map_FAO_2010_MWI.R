#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid FAO land cover data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
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


### LOAD FAO MAP
ogrListLayers(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\FAO_Land_Cover_Data\\DATA\\NATIONAL_LC/Malawi_lc.shp"))
lcm_FAO_raw <- readOGR(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\FAO_Land_Cover_Data\\DATA\\NATIONAL_LC/Malawi_lc.shp"))
 

### RASTERIZE FAO MAP
# Create factors needed for rasterize
lcm_FAO <- lcm_FAO_raw
lcm_FAO_df <- lcm_FAO@data %>% 
  mutate(LCCSUSLB = factor(LCCSUSLB),
         E2000USLB = factor(E2000USLB),
         E1990USLB = factor(E1990USLB))
lcm_FAO@data <- lcm_FAO_df

# Land cover classes
land_cover_class_FAO <- lcm_FAO_df %>%
  dplyr::select(LCCSUSLB, CLASS_ELEM) %>%
  unique

land_cover_class_FAO <- as.data.frame(levels(lcm_FAO_df$E2000USLB))
write_csv(land_cover_class_FAO, file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\FAO_Land_Cover_Data\\DATA\\NATIONAL_LC/land_cover_class_FAO_raw.csv"))

# Create raster frame with 30 m resolution
r_ext <- raster(extent(lcm_FAO), resolution = 30)
crs_fao <- crs(lcm_FAO)
projection(r_ext) <- crs_fao

# Rasterize 2010 map
lcm_FAO_r_2010 <- rasterize(lcm_FAO, r_ext, field = lcm_FAO@data[,"LCCSUSLB"])
writeRaster(lcm_FAO_r_2010, file.path(dataPath, "Data/MWI/Raw/Spatial_data/FAO_Land_Cover_Data/DATA/NATIONAL_LC/land_cover_map_FAO_2010.grd"))
lcm_FAO_r_2010 <- raster(file.path(dataPath, "Data/MWI/Raw/Spatial_data/FAO_Land_Cover_Data/DATA/NATIONAL_LC/land_cover_map_FAO_2010.grd"))


### CHECK PROJECTION, REPROJECT, IF NEEDED
crs(grid)
crs(lcm_FAO_raw)

# Set crs
crs <- crs(lcm_FAO_raw)

# Reproject grid
grid <- spTransform(grid, crs)

# Reproject adm2
adm2 <-  spTransform(adm2, crs)


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
lc_sh_raw <- bind_rows(lapply(grid_list, extract_grid_f, lcm_FAO_r_2010)) 
saveRDS(lc_sh_raw, file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_raw_FAO_2010_MWI.rds"))
lc_sh_raw <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_raw_FAO_2010_MWI.rds"))


### PROCESS LAND COVER SHARES
# Load land cover classes
lc_class <- read_csv(file.path(dataPath, "Data/MWI/raw/Spatial_data/FAO_Land_Cover_Data/DATA/NATIONAL_LC/land_cover_class_FAO.csv")) %>%
  mutate(ID = as.character(ID))

lc <- lc_sh_raw %>%
  gather(ID, share, -gridID) %>%
  #filter(variable %in% c(9, 10)) %>%
  group_by(gridID, ID) %>%
  summarize(share = sum(share, na.rm = T)) %>%
  left_join(., lc_class)


### COMBINE WITH ADM DATA
# Combine data and express area in ha
lc <- lc %>%
  left_join(., adm_grid) %>%
  mutate(grid_size = grid_size * 100,
         area = share*grid_size) %>%
  ungroup()


### CHECKS
# Check total of shares
check_total <- lc_sh_raw %>%
  gather(variable, share, -gridID) %>%
  group_by(gridID) %>%
  summarize(total = sum(share, na.rm=T))

# Check subtotals => similar to statistics in FAO Atlas of Malawi land cover
# Differences probably due to gridding. 
check_total_agg <- lc %>%
  group_by(agg) %>%
  summarize(area = sum(area))

check_total_agg2 <- lc %>%
  group_by(agg2) %>%
  summarize(area = sum(area))

check_total_ir <- lc %>%
  group_by(irrigation, agg2) %>%
  summarize(area = sum(area))

# Save
saveRDS(lc, file.path(dataPath, "Data/MWI/processed/Spatial_data/land_cover_FAO_2000_MWI.rds"))


