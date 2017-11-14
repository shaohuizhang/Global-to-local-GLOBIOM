#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to combine information on agricultural systems
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
# Agricultural statistics
ag_stat_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ag_stat_2000_", iso3c_sel, ".csv"))) 

# Irrigation
ir_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/irrigation_", iso3c_sel, ".csv"))) %>%
  dplyr::select(-Crop)


### COMPARE IRRIGATED DATA
# Aggregate irrigated data at adm 0 and crop level
ir_adm0_crop <- ir_raw %>%
  group_by(short_name) %>%
  summarize(value = sum(ir_area, na.rm = T)) %>%
  mutate(type = "irrigated",
         adm0 = iso3c_sel)

# Select ag_stat for irrigated crops
lu_adm0_crop <- ag_stat_raw %>%
  filter(short_name %in% ir_adm0_crop$short_name, adm_level == 0) %>%
  mutate(type = "total area")

# Combine and plot
adm0_comp <- bind_rows(lu_adm0_crop, ir_adm0_crop)

ggplot(data = adm0_comp, aes(x = type, y = value, fill = type)) +
  facet_wrap(~short_name, scales = "free") +
  geom_col() +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

rm(lu_adm0, lc_adm0, ir_adm0, adm0_comp)


# Rainfed high input (H)
# All non-irrigated sugc, tea, coff are considered high-input (H)
H <- ag_stat %>%
  filter(short_name %in% c("sugc", "teas", "coff"), adm_level == 0) %>%
  left_join(.,I) %>%
  mutate(value= value - ir_area,
         system = "H") %>%
  dplyr::select(value, short_name, system)

# (3) All non-irrigated other crops are subsistence (S)
S <- lu_adm %>%
  filter(!short_name %in% c("sugc", "teas_coff"), adm == "MWI") %>%
  left_join(.,dplyr::rename(I, I = value)) %>%
  mutate(value = ifelse(!is.na(I), value - I, value),
         system = "S") %>%
  dplyr::select(value, short_name, system)

# Combine
# keep short_name and lc to make mapping for GAMS later
lu_system <- bind_rows(I, H, S) %>%
  mutate(system = paste(short_name, system, sep = "_"))

# Compare with ag_stat_upd total => should be the same
sum(lu_system$value)
sum(lu_adm$value[lu_adm$adm == "MWI"])






### ADD INFORMATION ON AGRICULTURAL SYSTEMS
# Irrigated area



### SAVE
write_csv(ag_stat_2000, file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/ag_stat_2000_ZMB.csv"))
