#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select GAEZ suitability maps per country
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

### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/MWI/Set_country.R")


### LOAD DATA
# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm1_2000_adj.rds")))

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Grid
grid_5min <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_5min_", iso3c_sel, ".tif")))

# gaez2lvst_crop_map
gaez2lvst_crop <- read_excel(file.path(dataPath, "Data/Mappings/Mappings.xlsx"), sheet = "gaez2crop_lvst") %>%
  mutate(sy = paste(short_name, system, sep = "_"))

# Sy
lu_sy <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))


### PREPARE GAEZ MAPS FOR SUBSISTENCE
# Select crops
S_crops <- filter(gaez2lvst_crop, system == "S")

# Create file lookup table
S_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/L"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
S_files <- left_join(S_crops, S_files) %>%
  mutate(sy = ifelse(sy == "teas_S", "teas_coff_S", sy)) # Assume teas applies to teas_coff



### PREPARE GAEZ MAPS FOR LOW-INPUT
# Select crops
L_crops <- filter(gaez2lvst_crop, system == "L")

# Create file lookup table
L_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/L"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
L_files <- left_join(L_crops, L_files) %>%
  mutate(sy = ifelse(sy == "teas_L", "teas_coff_L", sy)) # Assume teas applies to teas_coff



### PREPARE GAEZ MAPS FOR HIGH_INPUT
# Select crops
H_crops <- filter(gaez2lvst_crop, system == "H")

# Create file lookup table
H_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/H"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
H_files <- left_join(H_crops, H_files) %>%
  mutate(sy = ifelse(sy == "teas_H", "teas_coff_H", sy)) # Assume teas applies to teas_coff



### PREPARE GAEZ MAPS FOR IRRIGATED
# Select crops
I_crops <- filter(gaez2lvst_crop, system == "I")

# Create file lookup table
# NB REPLACE FILES WITH IRRIGATED FILES WHEN AVAILABLE!!!!!!!!!!!!!!!!!!!!
I_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/irrigated/H"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
I_files <- left_join(I_crops, I_files) %>%
  mutate(sy = ifelse(sy == "teas_I", "teas_coff_I", sy)) # Assume teas applies to teas_coff



### FILTER OUT RELEVANT SYSTEMS AND SELECT ISO
# Filter out relevant systems
sy_files <- bind_rows(S_files, L_files, H_files, I_files) %>%
  filter(sy %in% lu_sy$sy) 

# Stack
gaez_stack <- stack(file.path(dataPath, "Data/Global/GAEZv3", sy_files$gaez_system, sy_files$gaez_input, sy_files$files))

# Replace gaez crop names with sy
names(gaez_stack) <- sy_files$sy

# Crop and mask to iso
gaez2iso_stack <- crop(gaez_stack, adm1)
gaez2iso_stack <- mask(gaez2iso_stack, adm1)
plot(gaez2iso_stack)


### SAVE TIF FILES 
gaezPath5min <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gaez\\5min"))
dir.create(gaezPath5min, recursive = T)

# Function to save tif files
iso2tif_f <- function(i){
    r <- gaez2iso_stack[[i]]
    name <- names(r)
    print(name)
    writeRaster(r, 
                file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaez/5min/", name, "_5min_", iso3c_sel, ".tif")), overwrite = T)
}

# Save
lapply(c(1:nlayers(gaez2iso_stack)), iso2tif_f)


### RESAMPLE MAP TO 30 ARC-SEC GRID
# function to resample
resample_f <- function(i){
  input_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaez/5min/", files_5min[i]))
  output_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaez/30sec/", files_30sec[i]))
  grid_30sec_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif"))
  name <- names(raster(input_file))
  print(name)
  align_raster2_f(input_file, grid_30sec_file, output_file, nThreads = "ALL_CPUS", verbose = F, 
                             output_Raster = F, overwrite = TRUE, r = "bilinear")
}

# Resample and save
gaezPath30sec <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gaez\\30sec"))
dir.create(gaezPath30sec, recursive = T)

files_5min <- list.files(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaez/5min")))
files_30sec <- gsub("5min", "30sec", files_5min)
lapply(c(1:length(files_5min)), resample_f)

