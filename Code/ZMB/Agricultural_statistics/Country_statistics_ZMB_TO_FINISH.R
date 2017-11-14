#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to prepare agriculural statistics
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


### LOAD MAPPINGS
# Regional mapping
adm_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_", iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000)

crop_lvst <- read_excel(file.path(dataPath, "Data\\Mappings\\Mappings.xlsx"), sheet = "crop_lvst") %>%
  dplyr::select(short_name) %>%
  na.omit()


### LOAD DATA
# FAOSTAT
faostat_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/FAOSTAT_", iso3c_sel, ".csv")))


### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL
# Rank area of crops using FAOSTAT
ey <- max(unique(faostat_raw$year))

area_share_faostat <- faostat_raw %>%
  filter(year %in% c(1980, 1990, 2000, 2010, ey), variable == "area") %>%
  group_by(year) %>%
  mutate(value = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(value))
%>%
  dplyr::select(short_name, share, year) %>%
  spread(year, share) %>%
  arrange(desc(`2010`))


ggplot(data = area_share_faostat, aes(x = year, y = value)) +
  geom_bar()

# aggregate adm1 tot adm0
ag_stat_crop_adm0 <- ag_stat %>%
  filter(variable %in% c("area", "production")) %>%
  group_by(id, year, adm_level, short_name, unit, variable, source) %>%
  summarize( value = sum(value, na.rm = T))

# Rank area of crops using all sources in base year
tab_area_rank_adm0 <- ag_stat_crop_adm0 %>%
  filter(variable == "area") %>%
  group_by(year, id) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, id, share, year) %>%
  filter(year %in% c(2000)) %>%
  spread(id, share) %>%
  arrange(desc(`FAOSTAT_0`))

### COMPARE DATA AT ADM1 LEVEL
# Area data
ag_stat_area_adm1 <- ag_stat %>%
  filter(variable %in% c("area"), adm_level == 1)

# Comparison of adm2 regions
fig_area_crop_adm1 <- ggplot(data = ag_stat_area_adm1, aes(x = year, y = value, colour = short_name, shape = id)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~adm, scales = "free") +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm1

# Rank area of crops using all sources in base year
tab_area_rank_adm1 <- ag_stat_area_adm1 %>%
  group_by(year, adm, id) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, id, share, year, adm) %>%
  filter(year %in% c(2000, 2007)) %>%
  spread(adm, share)

# Maps of adm2 with largest share of crop
# TO ADD

# Area comparison between crops in key years
fig_area_crop_adm0_2 <- ggplot(data = filter(ag_stat_crop_adm0, id == "FAOSTAT_0", variable == "area", 
                                             year %in% c(2000, 2010)), aes(x = factor(year), y = value, fill = short_name)) +
  geom_col() +
  facet_wrap(~ short_name, scales = "free") +
  labs(title = "Area comparison between crops FAOSTAT",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm0_2


# Crop area over time
ggplot(data = filter(faostat_raw, variable == "area"), 
                                             aes(x = year, y = value, fill = short_name)) +
  geom_col() +
  facet_wrap(~ short_name, nrow = 6, scales = "free") +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  labs(y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10)) +
  guides(fill = F) +
  theme(axis.text.x = element_text(size=5))

