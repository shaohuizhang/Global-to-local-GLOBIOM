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

# Grid
grid <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/5min_grid_", iso3c_sel, ".rds")))

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data\\", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II.tif")))


### PROCESS LAND COVER MAP
# Obtain land cover class
lc_class <- levels(lc_raw)[[1]]
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
lc_sh_raw <- bind_rows(lapply(grid_list, extract_grid_f, lc_raw)) 
saveRDS(lc_sh_raw, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/land_cover/lc_sh_raw_RCMRD_2000_5min_", iso3c_sel, ".rds")))
lc_sh_raw <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_shares_raw_FAO_2000_MWI.rds"))


### PROCESS LAND COVER SHARES
# Load land cover classes
lc_class <- read_csv(file.path(dataPath, "Data/MWI/raw/Spatial_data/FAO_Land_Cover_Data/DATA/NATIONAL_LC/land_cover_class_FAO.csv")) %>%
  mutate(ID = as.character(ID))

lc <- lc_sh_raw %>%
  gather(ID, share, -gridID) %>%
  group_by(gridID, ID) %>%
  summarize(share = sum(share, na.rm = T)) %>%
  left_join(., lc_class)


### COMBINE WITH ADM DATA
# Combine data and express area in ha
lc <- lc %>%
  left_join(., adm_grid) %>%
  mutate(grid_size = grid_size,
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

### ASSESS CROP CATEGORIES
# Non-pure agri grid cells (with suitability level 2)
lc_non_pure <- lc %>%
  filter(suitability_level %in% c(2)) %>%
  group_by(class_short, adm2) %>%
  summarize(area = sum(area, na.rm = T))

ggplot(data = lc_non_pure, aes(x = factor(class_short), y = area, fill = factor(class_short))) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "suitability comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~adm2, scales = "free") +
  theme(axis.text.x=element_blank())


### TO UPDATE
# Compare results with land cover map
waterbody <- land_cover_shares$SimUID[land_cover_shares$"210" > 0.1]
urban_areas <- land_cover_shares$SimUID[land_cover_shares$"190" > 0.03]
cropland <- land_cover_shares$SimUID[land_cover_shares$"10" > 0.2]
missing <- land_cover_shares$SimUID[land_cover_shares$"0" > 0]

# Create mask for specific class: 10 annual cropland
check_r <- keep_value_r_f(land_cover_map, 10, filename= "Cache/check.grd")
plot(check_r, col = "black")
plot(country_map, add = T, border = "blue")
plot(simu2country_poly[simu2country_poly$SimUID %in% cropland,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/check.gri")

# Save
saveRDS(lc, file.path(dataPath, "Data/MWI/processed/Spatial_data/land_cover_FAO_2000_MWI.rds"))


