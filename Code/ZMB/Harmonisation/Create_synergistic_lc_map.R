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
# Adm
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))

# Adm_r
adm_r <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_r_2000_", iso3c_sel, ".rds"))) %>%
  dplyr::select(-x, -y)

# gia
gia_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/gia/gia_", iso3c_sel, ".tif"))) 
names(gia_raw) <- "value"

# lc data
esa_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/lc_ESA_sh_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "esa")
rcmrd_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/lc_RCMRD_sh_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "rcmrd")
glc_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Spatial_data/lc_glc2000_2000_", iso3c_sel, ".rds"))) %>%
  mutate(source = "glc2000")

# Spatial detail
lu_detail_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_detail_2000_", iso3c_sel, ".rds"))) 

# System
lu_sy_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# Grid
grid <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Agricultural statistics
lu_adm_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 

# Population map
pop <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/pop/pop_30sec_", iso3c_sel, ".tif")))
names(pop) <- "pop"

# Travel time map
tt_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/travel_time/travel_time_", iso3c_sel, ".tif")))
names(tt_raw) <- "travel_time"

# Roads
roads <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/roads/roads_", iso3c_sel, ".rds")))


### COMBINE DIFFERENT CLASSES OF LC DATA
# Here also crop specific land cover maps should be added and be given the lowest rank so they are always included
# in the final land cover map.

esa <- esa_raw %>%
  group_by(gridID, source) %>%
  summarize(value = sum(value, na.rm = T))
sum(esa$value)

rcmrd <- rcmrd_raw %>%
  dplyr::select(gridID, value, source)
sum(rcmrd$value)

glc <- glc_raw %>%
  dplyr::select(gridID, value, source)
sum(glc$value)

lc_comb <- bind_rows(esa, rcmrd, glc)


### ADD SPATIAL DETAIL
lu_detail <- lu_detail_raw %>%
  dplyr::select(gridID, value) %>%
  mutate(source = "detail")

lc_comb <- bind_rows(lc_comb, lu_detail)


### ADD IRRIGATION DATA
# Prepare irrigation data
gia <- stack(gia_raw, grid)
gia <- as.data.frame(rasterToPoints(gia)) %>%
  dplyr::select(-x, -y) %>%
  filter(value >0) %>%
  mutate(source = "gia")

# Add
lc_comb <- bind_rows(lc_comb, gia)


### RANK GRIDID
# CODE NEED TO be replaced by a piece of code where a ranking table is loaded to make it more general. 
# Data for different land cover maps are combined to create a synergistic land cover map (Fritz et al. 2011)
# We use the following ranking:
# 1. Grid with crop cover according to all sources
# 2. Grid with crop cover from rcmrd + esa
# 3. Grid with crop cover from rcmrd + glc2000
# 4. Grid with crop cover from esa + glc2000
# rcmrd has our preference because it has the highest resolution followed by esa and glc 2000

# Function to rank cells
rank_f <- function(df){
  cov = sum(!is.na(df$value))  
  #if(cov == 5){
  #  rnk <- 1
  if (all(c("detail") %in% df$source)){
    rnk <- 1  
  } else if (all(c("gia", "rcmrd") %in% df$source)){
    rnk <- 2    
  } else if (all(c("rcmrd", "esa", "glc2000") %in% df$source)){
    rnk <- 3
  } else if (all(c("rcmrd", "esa") %in% df$source)){
    rnk <- 4
  } else if (all(c("rcmrd", "glc2000") %in% df$source)){
    rnk <- 5
  } else if (all(c("rcmrd") %in% df$source)){
    rnk <- 6
  } else 
    rnk <- 7
  ranking <- data.frame(coverage = cov, rank = rnk)
  return(ranking)
}

# Rank
# ranking <- lc_comb %>%
#   group_by(gridID) %>%
#   do(rank_f(.))
#saveRDS(ranking, "Cache/ranking.rds")
ranking <- readRDS("Cache/ranking.rds")


# Rank statistics
lc_rank <- lc_comb %>%
  spread(source, value) %>%
  left_join(ranking)
table(lc_rank$rank)


# Plot ranking
grid_df <- as.data.frame(rasterToPoints(grid))

left_join(lc_rank, grid_df) %>%
  #filter(rank %in% c(1,2,3,4)) %>%
  ggplot(.) +
    geom_raster(aes(x = x, y = y, fill = factor(rank))) +
    geom_path(data = adm, aes(x = long, y = lat, group = group), colour = "black") +
    #geom_path(data = roads, aes(x = long, y = lat, group = group), colour = "grey50") +
    scale_fill_brewer(palette="Set1",
                      name = "Source",
                      breaks=c("1", "2", "3", "4", "5", "6", "7"),
                      labels=c("Detail", "gmia, rcmrd", "rcmrd, esa, glc2000",
                               "rcmrd, esa", "rcmrd, glc2000", "rcmrd", "rest")) +
    coord_equal() +
    labs(x = "", y = "") +
    theme_classic() +
    theme(line = element_blank(),
        axis.text = element_blank())

# Area by rank and source
lc_comb %>%
  left_join(.,ranking) %>%
  group_by(source, rank) %>%
  summarize(area = sum(value, na.rm = T))


### ADD GIA AND DETAILED DATA
# Compare irrigated area using gia, preferred source and system
# => Use gia data
gia_check <- lc_rank[lc_rank$rank %in% c(2),]
sum(gia_check$gia, na.rm = T)
sum(gia_check$rcmrd)
sum(lu_sy_raw$value[lu_sy_raw$system == "I"])

# Combine preferred source, gia and detailed data
lc_rank <- lc_rank %>%
  mutate(value = ifelse(rank == 1, detail, 
                        ifelse(rank == 2, gia, 
                               rcmrd))) %>%
  dplyr::select(gridID, value, rank) %>%
  na.omit

sum(lc_rank$value[lc_rank$rank == 1], na.rm = T)
sum(lc_rank$value[lc_rank$rank == 2], na.rm = T)


### ADD DETAILED DATA TO LU
# adm crops
adm_crops <- unique(lu_adm_raw$short_name[lu_adm_raw$adm_level == 1])

# For crops that are both in lu_detail and lu_adm, check if lu_detail is fully included
# Normally this should be fine
lu_adm_detail_both <- lu_detail_raw %>%
  dplyr::select(gridID, value, short_name) %>%
  left_join(adm_r) %>%
  group_by(adm, short_name) %>%
  summarize(lu_detail = sum(value)) %>%
  mutate(adm_level = 1) %>%
  filter(short_name %in% adm_crops)

lu_adm_detail_check <- lu_adm_raw %>%
  filter(adm_level == 1) %>%
  left_join(lu_adm_detail_both) %>%
  na.omit %>%
  mutate(short = value - lu_detail) %>%
  filter(short < 0)

# Additional lu_adm from detailed data
# Only select crops that are not already covered by adm
lu_detail_adm <- lu_detail_raw %>%
  dplyr::select(gridID, value, short_name) %>%
  left_join(adm_r) %>%
  group_by(adm, short_name) %>%
  summarize(value = sum(value)) %>%
  mutate(adm_level = 1) %>%
  filter(!short_name %in% adm_crops)

# Combine with lu_adm
lu_adm <- lu_adm_raw %>%
  bind_rows(lu_detail_adm) %>%
  group_by(adm, short_name, adm_level) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()


### COMPARE LC AND LU ADM0
# Combine
adm0_comp <- bind_rows(
  lu_adm %>%
    filter(adm_level == 0) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(rank = 0,
           type = "lu"),
  lc_rank %>%
    filter(!is.na(value)) %>%
    group_by(rank) %>%
    summarize(value = sum(value)) %>%
    mutate(type = "lc")) %>%
  mutate(rank = factor(rank,levels = c(8,7,6,5,4,3,2,1,0)))

ggplot(data = adm0_comp, aes(x = type, y = value, fill = rank)) +
  geom_col(colour = "black") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw()


### COMPARE ADM2
# Combine
adm_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1) %>%
    group_by(adm) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(rank = 0,
           type = "lu"),
  left_join(lc_rank, adm_r)%>%
    filter(!is.na(value)) %>%
    group_by(adm, rank) %>%
    summarize(value = sum(value)) %>%
    mutate(type = "lc")) %>%
  mutate(rank = factor(rank,levels = c(8,7,6,5,4,3,2,1,0)))

ggplot(data = adm_comp, aes(x = type, y = value, fill = rank)) +
  geom_col(colour = "black") +
  facet_wrap(~adm) +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() 
  

### PREPARE FINAL LC MAP BY RANKING CELLS PER ADM
# Prepare travel time
tt <- stack(grid, tt_raw)
tt <- as.data.frame(rasterToPoints(tt)) %>%
  dplyr::select(-x, -y)


# We distribute lu not covered by adm stat using relative lu area
area_not_adm <- sum(lu_adm$value[lu_adm$adm == iso3c_sel])-sum(lu_adm$value[lu_adm$adm != iso3c_sel])

target_lu <- lu_adm %>%
  filter(adm_level == 1) %>%
  group_by(adm) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(share = value/sum(value),
         target_lu = value + (share * area_not_adm)) %>%
  dplyr::select(adm, target_lu)

# Add ranking variables, sort on rank and tt and select up to target
# To make sure there is enough lc, we add slack
lc_slack <- 1.01

lc_raw <- lc_rank %>%
  left_join(tt) %>% 
  left_join(adm_r) %>%
  ungroup() %>%
  arrange(adm, rank, travel_time) %>%
  group_by(adm) %>%
  mutate(tot_area = cumsum(value)) %>%
  left_join(target_lu) %>%
  filter(tot_area <= target_lu*lc_slack) %>%
  left_join(grid_df) %>%
  ungroup()


### CREATE IRRIGATION LC MAP BY SELECTING GIA AND DETAILED DATA
# Note that we probably need another rank if we have detailed data on other than irrigated schemes
lc_ir <- filter(lc_raw, rank %in% c(1,2)) %>%
  dplyr::select(gridID, value, x, y) %>%
  mutate(sy = "I")

# Plot lc_ir
ggplot() +
  geom_raster(data = lc_ir, aes(x = x, y = y, fill = value)) +
  geom_path(data = adm, aes(x = long, y = lat, group = group), colour = "black") +
  geom_path(data = roads, aes(x = long, y = lat, group = group), colour = "grey50") +
  coord_equal() +
  theme_bw()

### CREATE SYNERGISTIC LC MAP
lc <- dplyr::select(lc_raw, value, adm, gridID, x, y)

# Plot synergistic lc map
ggplot() +
  #geom_raster(data = lc, aes(x = x, y = y, fill = value)) +
  geom_raster(data = lc, aes(x = x, y = y), fill = "dark green") +
  geom_path(data = adm, aes(x = long, y = lat, group = group), colour = "black") +
  #geom_path(data = roads, aes(x = long, y = lat, group = group), colour = "grey50") +
  coord_equal() +
  coord_equal() +
  labs(x = "", y = "") +
  theme_classic() +
  theme(line = element_blank(),
        axis.text = element_blank())

### SAVE
# lc_ir
saveRDS(lc_ir, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lc_ir_2000_", iso3c_sel, ".rds"))) 
lc_ir_r <- dplyr::select(lc_ir, x, y, value)
lc_ir_r <- rasterFromXYZ(lc_ir_r)
crs(lc_ir_r) <- crs(adm)
writeRaster(lc_ir_r, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/maps/lc/lc_ir_30_sec_2000_", iso3c_sel, ".tif")), overwrite = T)

# lc
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 
lc_r <- dplyr::select(lc, x, y, value)
lc_r <- rasterFromXYZ(lc_r)
crs(lc_r) <- crs(adm)
writeRaster(lc_r, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/maps/lc/lc_syn_30_sec_2000_", iso3c_sel, ".tif")), overwrite = T)



