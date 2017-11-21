#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to combine irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "imputeTS")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# Aquastat
ir_crop_raw <- read_csv(file.path(dataPath, "Data/Global/AQUASTAT/Irrigated_area_by_crop.csv")) %>%
  filter(iso3c == iso3c_sel)

# GMIA
gmia_5min <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GMIA_", iso3c_sel, ".rds")))

# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/GAUL_", iso3c_sel, "_adm1_2000.rds")))

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Grid
grid_5min <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid_5min_r_", iso3c_sel, ".tif")))
names(grid_5min) <- "gridID"
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"

### COMPARE TOTALS BETWEEN SOURCES
# GMIA area equipped for irrigation
cellStats(gmia_5min, sum)

# Aquastat area equipped for irrigation
ir_crop_raw[ir_crop_raw$variable == "Area equipped for irrigation",]

# Aquastat total harvested area irrigated
ir_crop_raw[ir_crop_raw$variable == "Total harvested irrigated crop area (full control irrigation)",]


### SELECT DATA
# NB: Depending on country select tropical (trof) or temperate fruits for "Other fruits" category if available
# NB: fodder and Grass and fodder are not mapped

# We select 2002 data to represent 2000
ir_crop_2000 <- filter(ir_crop_raw, short_name != "total", year == 2002) %>%
  dplyr::select(-variable) %>%
  dplyr::rename(adm = iso3c) %>%
    mutate(system = "I", 
           ir_area = ir_area*1000,
           adm_level = 0)
sum(ir_crop_2000$ir_area)

# Save
write_csv(ir_crop_2000, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ir_2000_", iso3c_sel, ".csv")))


### RESAMPLE GMIA MAP TO 30 ARC-SEC GRID
# As values are in ha and relative to grid cell, we cannot simply warp to a higher resolution.,
# We calculate the share of irrigated area first and then warp
area_5min <- area(grid_5min)
gmia_5min_share <- gmia_5min/(area_5min*100)
writeRaster(gmia_5min_share, file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia_5min_share_", iso3c_sel, ".tif")), overwrite = T)


# Specify input and output files
gmia_5min_share_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia_5min_share_", iso3c_sel, ".tif"))
gmia_30sec_share_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia_30sec_share_", iso3c_sel, ".tif"))
grid_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid_30sec_r_", iso3c_sel, ".tif"))

# Resample
gmia_30sec_share <- align_raster_f(gmia_5min_share_file, grid_file, gmia_30sec_share_file, nThreads = "ALL_CPUS", verbose = T, 
                     output_Raster = T, overwrite = TRUE, r = "bilinear", border = adm1)
names(gmia_30sec_share) <- "gmia"

# Calculate area
area_30sec <- area(grid_30sec)
names(area_30sec) <- "grid_size"
gmia_30sec <- gmia_30sec_share*area_30sec*100
names(gmia_30sec) <- "gmia"

plot(gmia_30sec)
cellStats(gmia_30sec, sum)
cellStats(gmia_5min, sum)

# Save
writeRaster(gmia_30sec, file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/gmia_30sec_", iso3c_sel, ".tif")), overwrite = T)


### COMBINE WITH AREA AND ADM DATA
# Rasterize adm
adm_r <- rasterize(adm1, grid_30sec)
names(adm_r) <- "ID"

# Stack 
ir_grid_30sec <- stack(grid_30sec, adm_r, area_30sec, gmia_30sec)
plot(ir_grid_30sec)

# Get adm info
adm1_df <- levels(adm_r)[[1]] %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME), ID) %>%
  left_join(.,adm1_map) %>%
  dplyr::select(-adm1_GAUL)

# Create data.frame
ir_grid_30sec_df <- as.data.frame(rasterToPoints(ir_grid_30sec)) %>%
  left_join(adm1_df) %>%
  filter(gmia > 1) %>% # filter out all cells with < 1 ha of irrigated area
  dplyr::select(gridID, value = gmia)
summary(ir_grid_30sec_df)

# save
saveRDS(ir_grid_30sec_df, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/ir_2000_", iso3c_sel, ".rds")))
