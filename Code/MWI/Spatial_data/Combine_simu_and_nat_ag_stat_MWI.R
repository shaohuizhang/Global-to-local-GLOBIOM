#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to combine simu data and national agricultural statistics data
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

### SET YIELD AT SIMU LEVEL
# Load area_sh_simu_adm
area_sh_simu_in_adm <- read_csv(file.path(dataPath, "Data\\MWI\\Processed\\Spatial_data\\area_sh_simu_in_adm_MWI.csv"))

# Load yld_adm_2000
yld_adm_2000 <- read_csv(file.path(dataPath, "Data\\MWI\\Processed\\Agricultural_statistics\\yld_adm_2000.csv")) 

# Calculate yield at simu level. Use weighted average if simu is located in multiple adms.
# If yield data is only available for one adm but simu is split, average yield will be artificially low. 
# Shares are recalculated where yield data is available
yld_simu_2000 <- yld_adm_2000 %>%
  left_join(area_sh_simu_in_adm,.) %>%
  ungroup() %>%
  group_by(SimUID, ALLPRODUCT) %>%
  mutate(share = share/sum(share, na.rm = T)) %>%
  summarize(yld = sum(value*share)) 


### COMPARE LAND COVER DATA WITH NATIONAL AGRICULTURAL STATISTICS
# Read land cover mapping
ESA2GLOBIOM_lc <- read_excel(file.path(dataPath, "Data/GLOBIOM/Concordance/GLOBIOM_mappings.xlsx"), sheet = "ESA2GLOBIOM_lc") %>%
  dplyr::select(land_cover_code, LC_AGG)

# Read national agricultural statistics data
area_adm <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/area_adm_2000.csv")) %>%
  group_by(adm2_GAUL) %>%
  summarize(area = sum(area)) %>%
  mutate(source = "ag_stat",
         class = "agric lu")

# Read land cover adm area data
land_cover_adm <- read_csv(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/land_cover_adm_2000_MWI.csv")) %>%
  left_join(., ESA2GLOBIOM_lc) %>%
  group_by(adm2_GAUL, LC_AGG) %>%
  summarize(area = sum(area)) %>%
  rename(class = LC_AGG) %>%
  mutate(source = "ESA")

# Comparison between land cover and land use
lc_lu_adm <-   bind_rows(area_adm, land_cover_adm)

fig_lc_lu_adm <- ggplot(data = lc_lu_adm, aes(x = class, y=area)) + 
  geom_bar(stat="identity", colour = "black", aes(fill = class)) +
  facet_wrap(~adm2_GAUL, scales = "free") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10)) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x = "",
       y = "ha",
       title = "Land cover by admininistrative area")

### COMPARE LAND COVER AREA CALCULATED USING DIRECT OVERLAY WITH ADM AND WITH OVERLAY OF SIMU
# Read land cover area data at simu level
land_cover_area_simu <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/land_cover_area_ESA_2000_MWI.csv"))

# Multiply shares of simu in adm with land cover area to distribute simu over adms
# And combine with land_cover_adm
# Data is similar. Differences are likely to be caused by edge problem as simus have a rectangular shape
land_cover_compare <- land_cover_area_simu %>%
  left_join(., ESA2GLOBIOM_lc) %>%
  left_join(., area_sh_simu_in_adm) %>%
  mutate(area = area *share) %>%
  na.omit() %>%
  group_by(adm2_GAUL, LC_AGG) %>%
  summarize(area_simu = sum(area)) %>%
  rename(class = LC_AGG) %>%
  left_join(.,land_cover_adm)
  

ggplot(data = land_cover_compare) +
  geom_point(aes(x = area, y = area_simu))  +
  coord_fixed(ratio = 1) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_abline(intercept = 0, slope =1) +
  theme_bw()
  
