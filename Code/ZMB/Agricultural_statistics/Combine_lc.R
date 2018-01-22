#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to create synergistic land cover map
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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD DATA
# lc data
esa_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_ESA_sh_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "esa")
rcmrd_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_RCMRD_sh_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "rcmrd")

# Grid
grid <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/30sec_grid_r_", iso3c_sel, ".rds")))

# ag_stat


### COMBINE LC DATA
esa <- esa_raw %>%
  group_by(gridID, source, x, y) %>%
  summarize(value = sum(value, na.rm = T))

test <- bind_rows(esa, rcmrd_raw) %>%
  dplyr::select(gridID, x, y, source, value) %>%
  spread(source, value)

both <- test %>%
  na.omit %>%
  gather(source, value, -gridID, -x, -y)

summary(both)
both %>%
  group_by(source) %>%
summarize(value = sum(value))

x <- filter(test, rcmrd >50)
sum(x$rcmrd)
summary(x)

ggplot() +
  geom_raster(data = both, aes(x = x, y = y, fill = value)) +
  coord_equal() +
  facet_wrap(~source)

test <- test %>%
  na.omit()
ggplot() +
  geom_point(data = test, aes(x = esa, y = rcmrd))

### MAP DIFFERENCES



### SELECT 



# area size
area <- area(grid)
names(area) <- "grid_size"

# Rasterize adm
adm_r <- rasterize(adm1, grid)
names(adm_r) <- "ID"

# Stack 
grid_all <- stack(grid, area, adm_r, lc)
plot(grid_all)

# Get adm info
adm1_df <- levels(adm_r)[[1]] %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME), ID) %>%
  left_join(.,adm1_map) %>%
  dplyr::select(-adm1_GAUL)

# Create data.frame, remove cells outside border and add adm names
grid_all <- as.data.frame(rasterToPoints(grid_all)) %>%
  left_join(adm1_df) %>%
  na.omit %>%
  dplyr::select(-ID)

# Save
saveRDS(grid_all, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/Grid_all_2000_", iso3c_sel, ".rds")))
