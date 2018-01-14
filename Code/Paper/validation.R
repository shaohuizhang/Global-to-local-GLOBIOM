#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
# Additional packages
p_load("WDI", "countrycode", "gdxrrw", "viridis")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### LOAD DATA
# Load results from land use allocation model
file <- file.path(dataPath, "Model/Results/entropy.gdx")
land_use_raw <- rgdx.param(file, "Palloc", names = c("gridID", "system", "value"),  compress = T) %>%
  mutate(system = as.character(system),
         gridID = as.numeric(as.character(gridID)))

# Load country grid
grid_r <- readRDS(file.path(dataPath, "Data/MWI  - old/Processed/Maps/grid_r_MWI.rds"))

# Adm map
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/gaul/GAUL_MWI_adm2_2000_adj.rds"))

# City information
data(world.cities)
cities <- filter(world.cities, country.etc == "Malawi", capital == 1)

# Household survey data
suppressMessages(source(file.path(root, "Code/MWI/Household_surveys/combine_data_MWI_2010.R")))

# Mapping
hs2crop_lvst <- read_excel(file.path(dataPath, "Data/MWI/Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_hs2crop_lvst") %>%
  dplyr::select(crop_code, short_name)


### LAND USE MAPS
# Add short_name
land_use <- land_use_raw %>%
  mutate(short_name = substr(system,0,4),
         short_name = ifelse(system %in% c("teas_coff_H", "teas_coff_I"), "teas_coff", short_name)) %>%
  na.omit


### PREPARE VALIDATION DATA
# prepare data
MWI2010 <- MWI2010 %>%
  left_join(.,hs2crop_lvst) %>%
  dplyr::select(case_id, plotnum, crop_code, short_name, lat, lon) %>%
  group_by(lat, lon, short_name) %>%
  summarize(hh = n())

lsms_coord <- dplyr::select(MWI2010, lat, lon)
coordinates(lsms_coord) <-  ~ lon + lat

# Extract land use values
lsms_sel <- extract(grid_r, lsms_coord, df = T)

# Combine
# CHECK WHY THERE ARE NA GRID CELLS? BORDERS?
lsms_df <- bind_cols(MWI2010, lsms_sel) %>%
  ungroup() %>%
  dplyr::select(gridID, short_name) %>%
  mutate(source = "lsms") %>%
  na.omit %>%
  unique # filter out ea located in one grid cell
rm(MWI2010)

length(unique(lsms_df$gridID))
gridID_df <- lsms_df %>% 
  group_by(gridID) %>%
  summarize(n_crops = length(short_name))

# Select comparable land use data
# IGNORE SYSTEMS FOR NOW
# WHY ARE THERE DUPLICATES????
lu_val_df <- filter(land_use, gridID %in% lsms_df$gridID) %>%
  dplyr::select(gridID, short_name) %>%
  mutate(source = "model") %>%
  unique
rm(land_use, land_use_raw)

# Combine data
val <- bind_rows(lsms_df, lu_val_df) %>%
  mutate(value = 1) %>%
  spread(source, value) %>%
  filter(!is.na(lsms))

tab_val <- bind_rows(
  val %>%
    group_by(short_name) %>%
    summarize(n_model = sum(!is.na(model)),
               n_lsms = n(),
              "Misclassified (%)" = round((1-(n_model/n_lsms))*100)) %>%
    arrange(desc(n_model)),
  val %>%
    summarize(short_name = "total",
              n_model = sum(!is.na(model)),
              n_lsms = n(),
              "Misclassified (%)" = round((1-(n_model/n_lsms))*100))) %>%
      rename(`Modelled land use (Number of cells)` = n_model,
           `LSMS land use (Number of cells)` = n_lsms)
    
              


  
