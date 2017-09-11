#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Prepare validation for country SPAM
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("haven", "gdxrrw", "viridis")


### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### LOAD DATA
# Household survey data
suppressMessages(source("Code/MWI/Household_surveys/combine_data_MWI_2010.R"))

# Mapping
hs2crop_lvst <- read_excel(file.path(dataPath, "Data/MWI/Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_hs2crop_lvst") %>%
  dplyr::select(crop_code, short_name)

# Adm map
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))

# Load country grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
grid_r <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_r_MWI.rds"))

# urban mask
urban_mask <- readRDS(file.path(dataPath, "Data/MWI/Processed/Spatial_data/urban_mask_MWI.rds"))

# Model results
file <- file.path(dataPath, "Model/Results/land_use.gdx")
land_use_raw <- rgdx.param(file, "Palloc", names = c("gridID", "system", "value"),  compress = T) %>%
  mutate(system = as.character(system),
         gridID = as.numeric(as.character(gridID)))

# City information
data(world.cities)
cities <- filter(world.cities, country.etc == "Malawi", capital == 1)

### VALIDATION DATA
# prepare data
val_df <- MWI2010 %>%
  left_join(.,hs2crop_lvst) %>%
  dplyr::select(case_id, plotnum, crop_code, short_name, lat, lon) %>%
  na.omit() %>%
  group_by(lat, lon, short_name) %>%
  summarize(hh = n())


### SPAM RESULTS
# Add NA to missing values to show where there is no crop cover
land_use <- land_use_raw %>%
  spread(system, value, fill = NA) 

# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid_r))

land_use <- land_use %>%
  left_join(grid_df,.) %>%
  gather(system, value, -gridID, -x, -y)

# Add short_name
land_use <- land_use %>%
  mutate(short_name = str_sub(system,1,-3), # cut of last two characters
         short_name = ifelse(system %in% c("teas_coff_H", "teas_coff_I"), "teas_coff", short_name))


### PLOTS
# Function to plot validation and land use data
# Plot function
plot_crop_val_f <- function(crop){
  df_val <- filter(val_df, short_name %in% crop)
  df_lu <-  filter(land_use, short_name %in% crop)
  p = ggplot() +
    geom_raster(data = df_lu, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(na.value = "light grey", direction = -1, labels = comma) +
    geom_path(data = adm2, aes (x = long, y = lat, group = group), colour = "black") +
    geom_point(data = df_val, aes(x = lon, y = lat, size = hh), colour = "red", alpha = 0.3) +
    facet_wrap(~short_name) +
    coord_quickmap() +
    labs(x="", y="") +
    theme_classic() +
    theme(line = element_blank(),
          axis.text = element_blank(),
          strip.background = element_rect(colour = NA, fill = NA)) +
    geom_point(data = cities, aes(x = long, y = lat), col = "black") +
    geom_text(data = cities, aes(x = long, y = lat, label = name), size = 4)
  
  p
}

plot_crop_val_f(c("trof", "rice"))






### SHARE OF CORRECT CLASSIFICATION
# Create spatial points object
val_geo <-val_df
coordinates(val_geo) <- c("lon", "lat")
crs(val_geo) <- crs(adm2)

# Map validation data to gridID and link to land use data
val2gridID <- extract(grid_r, val_geo, df = T) %>%
  bind_cols(val_df) %>%
  dplyr::select(gridID, short_name) %>%
  na.omit %>%
  unique() %>%
  mutate(validation = 1)

# Link to land
lu2gridID <- land_use %>%
  na.omit() %>%
  dplyr::select(gridID, short_name) %>%
  unique() %>%
  mutate(allocation = 1) 

# Overlap
# NB ADD confindence intervals usingh package binom!
overlap <- bind_rows(
  left_join(val2gridID, lu2gridID) %>%
  group_by(short_name) %>%
  dplyr::summarize(match_n = sum(allocation, na.rm = T),
            match_sh = sum(allocation, na.rm = T)/sum(validation, na.rm = T),
            non_match_n = n()- match_n,
            non_match_sh = (1-match_sh),
            n = n(),
            share = match_sh  + non_match_sh),
  left_join(val2gridID, lu2gridID) %>%
  dplyr::summarize(match_n = sum(allocation, na.rm = T),
              match_sh = sum(allocation, na.rm = T)/sum(validation, na.rm = T),
              non_match_n = n()- match_n,
              non_match_sh = (1-match_sh),
              n = n(),
              share = match_sh  + non_match_sh) %>%
    mutate(short_name = "Total")
  )


  bind_rows(val2gridID,lu2gridID) %>%
  group_by(short_name, gridID)

  library(tables)
  