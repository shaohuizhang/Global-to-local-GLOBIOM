#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid ESA 2000 land cover data to 30 arc-sec and calculate shares per land cover class
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


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


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
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm1_2000.rds")))

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II.tif")))

# Load land cover classes
lc_class <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_landcover_2000_Scheme_II/Zambia_landcover_2000_Scheme_II_legend.csv"))) %>%
  dplyr::select(lc_code, lc)

# Grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"

# grid_ESA
grid_ESA <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_ESA_raw_2000_", iso3c_sel, ".tif")))


### RESAMPLE 30 SEC GRID TO LC RESOLUTION
# We first resample the map to 300x300m resolution of ESA because it is otherwise too large to process - still to solve this
#https://gist.github.com/alfcrisci/0fb27d9a46d3ee2b600f

# Specify input and output files
lc_raw_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II.tif"))
lc_lr_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/Zambia_LandCover_2000_Scheme_II/Zambia_Landcover2_2000_Scheme_II_lr.tif"))
lc_esa_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/ESA/ESA_raw_2000_", iso3c_sel, ".tif"))

# Resample
# No need to mask grid (slow) as the grid_30sec is already masked => use align_raster2_f
lc_lr <- align_raster2_f(lc_raw_file, lc_esa_file, lc_lr_file, nThreads = "ALL_CPUS", verbose = T, 
                         output_Raster = T, overwrite = TRUE, r = "near")
lc_lr
levelplot(lc_lr)


### COMBINE LC AND GRID AND CALCULATE SHARES
# Combine and calculate shares
lc_stack <- stack(grid_ESA, lc_lr)
lc <- as.data.frame(rasterToPoints(lc_stack)) %>%
  set_names(c("x", "y", "gridID", "lc_code")) %>%
  filter(!is.na(gridID)) %>%
  unique() %>%
  group_by(gridID, lc_code) %>%
  summarize(n = n())  %>%
  mutate(share = n / sum(n, na.rm = T)) %>%
  dplyr::select(-n) 
summary(lc)

# Calculate area
grid_size <- area(grid_30sec)
names(grid_size) <- "grid_size"
grid_size <- stack(grid_30sec, grid_size)
grid_size <- as.data.frame(rasterToPoints(grid_size)) %>%
  na.omit

lc <- left_join(lc, grid_size) %>%
  mutate(value = grid_size*share*100) # in ha

# Filter out crop grid cells
crop_class <- lc_class %>%
  filter(lc == "crops")

lc <- lc %>%
  filter(lc_code %in% crop_class$lc_code)

# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_RCMRD_sh_2000_", iso3c_sel, ".rds")))


## Possible approach to process large raster using SpaDES => split raster and then process
grid_lc_res <- as.raster(grid_lc_res)
hist(grid_lc_res)
lc_raw
# SPLIT RASTER INTO 4
?splitRaster
if (interactive()) {
  n <- pmin(parallel::detectCores(), 8) # use up to 4 cores
  beginCluster(n)
  y3 <- splitRaster(lc_raw, 4, 4, path = file.path("c:/tmp"))
  endCluster()
}

lc <- as.data.frame(rasterToPoints(lc_raw))
grid_lc_res_x <- as.data.frame(rasterToPoints(grid_lc_res))
summary(grid_lc_res_x)
summary(lc)
check <- left_join(lc, grid_lc_res_x)
summary(check)
