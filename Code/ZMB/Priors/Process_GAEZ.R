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

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# Adm
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))

# Grid
grid <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_5min_r_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# gaez2lvst_crop_map
gaez2lvst_crop <- read_excel(file.path(dataPath, "Data/Mappings/Mappings.xlsx"), sheet = "gaez2crop_lvst") %>%
  mutate(id = paste(short_name, system, sep = "_"))


### PREPARE GAEZ MAPS FOR SUBSISTENCE
# Select crops
S_crops <- filter(gaez2lvst_crop, system == "S")

# Create file lookup table
S_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/L"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
S_files <- left_join(S_crops, S_files)

# Create raster stack
S_stack <- stack(file.path(dataPath, paste0("Data/Global/GAEZv3/rainfed/L/", S_files$files)))

# Replace gaez crop names with id
names(S_stack) <- S_crops$id
S_stack


### PREPARE GAEZ MAPS FOR LOW-INPUT
# Select crops
L_crops <- filter(gaez2lvst_crop, system == "L")

# Create file lookup table
L_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/L"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
L_files <- left_join(L_crops, L_files)

# Create raster stack
L_stack <- stack(file.path(dataPath, paste0("Data/Global/GAEZv3/rainfed/L/", L_files$files)))

# Replace gaez crop names with id
names(L_stack) <- L_crops$id
L_stack


### PREPARE GAEZ MAPS FOR HIGH_INPUT
# Select crops
H_crops <- filter(gaez2lvst_crop, system == "H")

# Create file lookup table
H_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/H"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
H_files <- left_join(H_crops, H_files)

# Create raster stack
H_stack <- stack(file.path(dataPath, paste0("Data/Global/GAEZv3/rainfed/H/", H_files$files)))

# Replace gaez crop names with id
names(H_stack) <- H_crops$id
H_stack


### PREPARE GAEZ MAPS FOR IRRIGATED
# Select crops
I_crops <- filter(gaez2lvst_crop, system == "I")

# Create file lookup table
# NB REPLACE FILES WITH IRRIGATED FILES WHEN AVAILABLE!!!!!!!!!!!!!!!!!!!!
I_files <- data.frame(files = list.files(file.path(dataPath, "Data/Global/GAEZv3/rainfed/H"), pattern = ".rst$")) %>%
  mutate(gaez_code = gsub("\\..*","", files),
         gaez_code = gsub("^.*\\_","", gaez_code))
I_files <- left_join(H_crops, I_files)

# Create raster stack
# NB REPLACE FILES WITH IRRIGATED FILES WHEN AVAILABLE!!!!!!!!!!!!!!!!!!!!
I_stack <- stack(file.path(dataPath, paste0("Data/Global/GAEZv3/rainfed/H/", I_files$files)))

# Replace gaez crop names with id
names(I_stack) <- I_crops$id
I_stack


### COMBINE, CROP AND MASK TO ISO
# Combine
suit_stack <- stack(S_stack, L_stack, H_stack, I_stack)

# Crop and mask to iso
gaez2iso_stack <- crop(suit_stack, adm)
gaez2iso_stack <- mask(gaez2iso_stack, adm)
plot(gaez2iso_stack)
plot(gaez2iso_stack$maiz_S)
plot(gaez2iso_stack$cass_S)


### COMBINE WITH AREA AND ADM DATA
# area size
area <- area(grid_r)
names(area) <- "grid_size"

# Rasterize adm
adm_r <- rasterize(adm, grid_r)
names(adm_r) <- "ID"

# Stack 
gaez <- stack(grid_30sec, gaez)
plot(gaez)

# Get adm info
adm1_df <- levels(adm_r)[[1]] %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME), ID) %>%
  left_join(.,adm1_map) %>%
  dplyr::select(-adm1_GAUL)

# Create data.frame, remove cells outside border, add adm names and convert to probability
gaez <- as.data.frame(rasterToPoints(gaez)) %>%
  left_join(adm1_df) %>%
  gather(id, value, -x, -y, -gridID, -ID, -grid_size, -adm1) %>%
  na.omit() %>%
  separate(id, c("short_name", "system"), sep = "_") %>%
  mutate(suit = as.numeric(value)/10000) %>%
  dplyr::select(-ID, -value)


# Save data
saveRDS(gaez, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Spatial_data/gaez2iso_", iso3c_sel, ".rds")))



