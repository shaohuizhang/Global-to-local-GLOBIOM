#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare agricultural statistics data and land cover data
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

# LOAD DATA
# Crop cover data
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_ESA_2000_MWI.rds")) %>%
  mutate(type = "land_cover")

# Land use data
ag_stat <- read_csv(file.path(dataPath, "Data/MWI/Processed\\Agricultural_statistics/ag_stat_2000_MWI.csv")) %>%
  mutate(type = "land_use")

# COMPARE ADM0


# COMPARE ADM2
# Aggregate crop cover at adm 2 level
cc_adm2 <- crop_cover %>%
  group_by(adm2_GAUL, type) %>%
  summarize(value = sum(area, na.rm = T))

# land use at adm 2 level
lu_adm2 <- ag_stat %>%
  filter(variable == "area", adm_level == 2) %>%
  rename(adm2_GAUL = adm) %>%
  group_by(adm2_GAUL, type) %>%
  summarize(value = sum(value, na.rm = T))

# Combine and plot
adm2 <- bind_rows(lu_adm2, cc_adm2)

fig_adm2 <- ggplot(data = adm2, aes(x = adm2_GAUL, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_adm2

# COMPARE ADM0
# Aggregate crop cover at adm 2 level
cc_adm0 <- crop_cover %>%
  group_by(adm0_GAUL, type) %>%
  summarize(value = sum(area, na.rm = T))

# land use at adm 2 level
lu_adm0 <- ag_stat %>%
  filter(variable == "area", adm_level == 0) %>%
  rename(adm0_GAUL = adm) %>%
  group_by(adm0_GAUL, type) %>%
  summarize(value = sum(value, na.rm = T))

# Combine and plot
adm0 <- bind_rows(lu_adm0, cc_adm0)

fig_adm0 <- ggplot(data = adm0, aes(x = adm0_GAUL, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_adm0

