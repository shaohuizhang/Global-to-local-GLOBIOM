#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare land use and land cover data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
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


### SET COUNTRY
source("Code/ZMB/Set_country.R")


# LOAD DATA
# Agricultural statistics
lu_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ag_stat_2000_", iso3c_sel, ".csv")))
  
# Irrigation
ir_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ir_2000_", iso3c_sel, ".csv"))) %>%
  dplyr::select(-year)

# System
sy_raw <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/sy_2000_", iso3c_sel, ".xlsx")), sheet = "system") %>%
  dplyr::select(-note, -source, -total)


### SELECT NATIONAL DATA
lu <- lu_raw %>%
  filter(adm_level == 0) 

### IRRIGATED AREA
# Combine and plot
adm0_comp <- bind_rows(
  ir_raw %>%
    rename(value = ir_area),
  lu %>%
    filter(short_name %in% ir_raw$short_name) %>%
    mutate(system = "Total") %>%
    rename(value = area))

ggplot(data = adm0_comp, aes(x = system, y = value, fill = system)) +
  facet_wrap(~short_name, scales = "free") +
  geom_col() +
  labs(title = "Crop cover and irrigated area comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() 
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Correct
ir <- left_join(ir_raw, lu) %>%
  mutate(value = ifelse(ir_area > area, area, ir_area)) %>%
  dplyr::select(-area, -ir_area)


### OTHER SYSTEMS
# System
sy <- sy_raw %>%
  gather(system, share, - short_name)

# Adm0 minus ir_area
lu_nir <- left_join(lu,ir) %>%
  mutate(area = ifelse(is.na(value), area, area-value)) %>%
  dplyr::select(-system, -value)

lu_sy <- left_join(sy, lu_nir) %>%
  mutate(value = share/100 * area) %>%
  dplyr::select(-share, -area) %>%
  filter(value != 0) %>%
  bind_rows(ir) %>%
  mutate(sy = paste(short_name, system, sep = "_")) %>%
  dplyr::select(-adm, -adm_level)

# Compare total with lu => should be the same
sum(lu_sy$value)
sum(lu$area)

# Save 
saveRDS(lu_sy, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

