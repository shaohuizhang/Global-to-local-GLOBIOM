#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to analyse and national agricultural statistics data
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


### DOWNLOAD FLc LIST
# Load FLC
FCL <- read_excel(file.path(dataPath, "Data\\Global\\FAOSTAT\\FCL\\FCL.xlsx"), sheet = "FCL_short")


### DOWNLOAD AND PROCESS FAOSTAT DATA
# Download
FAOSTAT_prod_raw <- read_csv(file.path(FAOSTATPath, "Production_Crops_E_All_Data_(Normalized).csv"))
FAOSTAT_lvst_raw <- read_csv(file.path(FAOSTATPath, "Production_Livestock_E_All_Data_(Normalized).csv"))

# Process 
FAOSTAT_prod <- FAOSTAT_prod_raw %>%
  mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
         iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
  dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
  filter(iso3c == "MWI") %>%
  left_join(., FCL) %>%
  filter(!is.na(value), !(class == "total")) %>%
  dplyr::select(-class)

FAOSTAT_lvst <- FAOSTAT_lvst_raw %>%
  mutate(variable = dplyr::recode(Element, "Area harvested" = "area", "Yield" = "yield", "Production" = "production"),
         iso3c = countrycode(`Area Code`, "fao", "iso3c")) %>%
  dplyr::select(iso3c, FCL_item_code = `Item Code`, variable, year = Year, unit = Unit, value = Value) %>%
  filter(iso3c == "MWI") %>%
  left_join(., FCL) %>%
  mutate(value = ifelse(FCL_title %in% c("Poultry Birds", "Chickens"), value*1000, value),
         unit = "Head") %>%
  filter(!is.na(value), !(class == "total")) %>%
  dplyr::select(-class)

### DOWNLOAD REGIONAL MAPPING
MWI2ADM_2000 <- read_excel(file.path(dataPath, "Data/MWI/Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2ADM_2000") %>%
  dplyr::select(adm1_hs, adm2_GAUL) %>%
  na.omit() %>%
  unique()

# Create files for relevant variables
area_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "ha", year >1990, variable == "area") %>%
  na.omit() # remove rows with na values for value
  
prod_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "tonnes", year >1990, variable == "production")

yld_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "hg/ha", year >1990, variable == "yield") %>%
  mutate(value = value/10000) # convert to tons/ha

number_FAOSTAT <- FAOSTAT_lvst %>%
  filter(year >1990) 


### PROCESS AGRO-MAPS DATA
# Load data
am_adm1 <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Agro_maps_adm1.csv"))
am_adm2 <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Agro_maps_adm2.csv"))

# Aggregate area and production to country level
area_am_adm2 <- am_adm2 %>%
  mutate(variable == "area") %>%
  group_by(FCL_title, FCL_item_code, year) %>%
  summarize(value = sum(value, na.rm=T))

prod_am_adm2 <- am_adm2 %>%
  mutate(variable == "production") %>%
  group_by(FCL_title, FCL_item_code, year) %>%
  summarize(value = sum(value, na.rm=T))

yld_am_adm2 <- am_adm2 %>%
  filter(variable %in% c("area", "production")) %>%
  group_by(FCL_title, FCL_item_code, variable, year) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  spread(variable, value) %>%
  mutate(value = production/area)



### PROCESS AGRICULTURAL STATISTICS
# Read data
as <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_MWI.csv"))
tot_pop_adm <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/tot_pop_adm_2000_MWI.csv"))

# Aggregate area and production to country level
area_as <- as %>%
  filter(unit == "ha") %>%
  group_by(FCL_title, FCL_item_code, year) %>%
  summarize(value = sum(value, na.rm=T))

prod_as <- as %>%
  filter(unit == "tons") %>%
  group_by(FCL_title, FCL_item_code, year) %>%
  summarize(value = sum(value, na.rm=T))

yld_as <- as %>%
  filter(unit %in% c("ha","tons")) %>%
  group_by(FCL_title, FCL_item_code, unit, year) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  spread(unit, value) %>%
  mutate(value = tons/ha)

number_as <- as %>%
  filter(unit == "number") %>%
  group_by(FCL_title, FCL_item_code, year) %>%
  summarize(value = sum(value, na.rm=T))


### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL
# Rank area of crops using FAOSTAT in base year
tab_area_rank_FAOSTAT <- area_FAOSTAT %>%
  filter(year == 2000) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(FCL_title, FCL_item_code, `area (ha)` = value, share)

# Rank area of crops using ag stat
tab_area_rank_as <- area_as %>%
  ungroup() %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  rename(`area (ha)` = value)

# Rank area of crops using ag stat
tab_area_rank_am <- area_am_adm2 %>%
  ungroup() %>%
  filter(year == 2000) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  rename(`area (ha)` = value)


### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS PER ADM
# Area share of crops within regions
tab_area_share_within_reg_as <- as %>%
  filter(unit == "ha") %>%
  ungroup() %>%
  group_by(year, adm2_GAUL) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm2_GAUL, year, share, FCL_title) %>%
  arrange(desc(share)) %>%
  spread(FCL_title, share)


### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL, COMPARING ADMs
# Area share of crops within country by crop
area_share_within_country_as <- as %>%
  filter(unit == "ha") %>%
  ungroup() %>%
  group_by(year, FCL_title) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm2_GAUL, year, share, FCL_title) %>%
  arrange(desc(share)) %>%
  spread(FCL_title, share)

# Maps of total area per region
MWI_adm2 <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\maps\\GAUL_MWI_adm2_2000_adj.rds"))
MWI_adm2_df <- MWI_adm2@data %>%
  rename(adm2_GAUL = ADM2_NAME) %>%
  mutate(adm2_GAUL = toupper(adm2_GAUL)) %>%
  left_join(., MWI2ADM_2000) %>%
  left_join(., area_share_within_country_as) %>%
  dplyr::select(ADM2_CODE, adm2_GAUL, `Beans, dry`:`Tobacco leaves`)
names(MWI_adm2_df) <- gsub(" ", "", names(MWI_adm2_df))
names(MWI_adm2_df) <- gsub(",", "", names(MWI_adm2_df))
MWI_adm2@data <- MWI_adm2_df

plot_length <- length(unique(area_as$FCL_title)) + 2
fig_area_share <- spplot(MWI_adm2, c(3:plot_length), main = "Share of crop area within country",
       colorkey=list(space="bottom"))


### COMPARE FAOSTAT, AS and AM
# Area
fig_area_compare <- ggplot() + 
  geom_line(data = area_FAOSTAT, aes(x = year, y = value)) +
  geom_line(data = area_am_adm2, aes(x = year, y = value), colour = "green") +
  facet_wrap(~FCL_title, scales = "free") +
  geom_point(data = area_as, aes(x = year, y = value), colour = "red") +
  labs(title = "Area comparison between FAOSTAT, agricultural statistics and Agro-maps",
       caption = "Source: FAOSTAT, Malawi National Census of Agriculture and Livestock 2006/07) and Agro-maps",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

# Prod
fig_prod_compare <- ggplot() + 
  geom_line(data = prod_FAOSTAT, aes(x = year, y = value)) +
  geom_line(data = prod_am_adm2, aes(x = year, y = value), colour = "green") +
  facet_wrap(~FCL_title, scales = "free") +
  geom_point(data = prod_as, aes(x = year, y = value), colour = "red") +
  labs(title = "Production comparison FAOSTAT, agricultural statistics and Aro-maps",
       caption = "Source: FAOSTAT, Malawi National Census of Agriculture and Livestock 2006/07) and Agro-maps",
       y = "tons",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

# yield
fig_yld_compare <- ggplot() + 
  geom_line(data = yld_FAOSTAT, aes(x = year, y = value)) +
  geom_line(data = yld_am_adm2, aes(x = year, y = value), colour = "green") +
  facet_wrap(~FCL_title, scales = "free") +
  geom_point(data = yld_as, aes(x = year, y = value), colour = "red") +
  labs(title = "Yield comparison FAOSTAT, agricultural statistics and Aro-maps",
       caption = "Source: FAOSTAT, Malawi National Census of Agriculture and Livestock 2006/07) and Agro-maps",
       y = "tons/ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

# Heads
fig_heads_compare <- ggplot() + 
  geom_line(data = number_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~FCL_title, scales = "free") +
  geom_point(data = number_as, aes(x = year, y = value), colour = "red") +
  labs(title = "Heads comparison between national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
       y = "heads",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

### CHECK YIELD FOR OUTLIERS
# Functions
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

winsor <- function (x){
  # deviation
  IQRhigh = quantile(x, 0.75) + 1.5 * IQR(x)
  IQRlow = quantile(x, 0.25) - 1.5 * IQR(x)
  x[x > IQRhigh] <- IQRhigh
  x[x < IQRlow] <- IQRlow
  return(x)
}

# Yield per adm
yld_as_adm <- as %>%
  filter(unit %in% c("ha","tons")) %>%
  spread(unit, value) %>%
  mutate(value = tons/ha) %>%
  na.omit %>%
  filter(value >0, !is.infinite(value)) %>% # remove zero values that decrease average and inf
  group_by(FCL_title) %>%
  mutate(outlier = ifelse(is_outlier(value), adm2_GAUL, NA))

yld_am_adm2 <- am_adm2 %>%
  filter(variable %in% c("production","area")) %>%
  spread(variable, value) %>%
  mutate(value = production/area) %>%
  na.omit %>%
  filter(value >0, !is.infinite(value)) %>% # remove zero values that decrease average and inf
  group_by(FCL_title) %>%
  mutate(outlier = ifelse(is_outlier(value), adm2_GAUL, NA))


# Distribution of yield across regions and outliers
fig_yld_outlier <- ggplot(data = yld_as_adm, aes(y = value, x = FCL_title)) +
  geom_boxplot(fill = "light grey") +
  stat_boxplot(geom ='errorbar') +
  #geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  labs(title = "Yield comparison between national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
       y = "tons/ha",
       x ="",
       colour = "Year (FAOSTAT)") +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=10)) +
  coord_flip() 


# Winsor values
yld_as_adm_winsor <- yld_as_adm %>%
  mutate(value = winsor(value))

# Plot new values and compare with FAO
fig_yld_winsor <- ggplot(data = yld_as_adm_winsor, aes(y = value, x = FCL_title)) +
  geom_boxplot(fill = "light grey") +
  stat_boxplot(geom ='errorbar') +
  geom_point(data = filter(yld_FAOSTAT, year %in% c(2005:2008), FCL_title %in% yld_as_adm$FCL_title), 
             aes(y = value, x = FCL_title, colour = factor(year)), size = 2.5) +
  labs(title = "Yield comparison between winsorised national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
      y = "tons/ha",
      x ="",
      colour = "Year (FAOSTAT)") +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=10)) +
  coord_flip() 
  
# 
# ### PROJECT YIELD FROM AGRICULTURAL STATISTICS TO 2000 USING FAOSTAT TRENDS
# # Calculate FAOSTAT growth in yields between 2000 and 2007 (base year nat ag stat)
# 
# # Yld comparison shows data for Soya in 2000. We assume yield has been the same as last year available for historical period.
# soya_2003 <- yld_FAOSTAT %>%
#   filter(ALLPRODUCT == "Soya", year == 2003)
# 
# soya_fix <- soya_2003[rep(1,6),]
# soya_fix$year <- c(1997:2002)
# 
# 
# # There is no data on SwPo, we substitute by yield for potatoes.
# swpo_fix <- yld_FAOSTAT %>%
#   filter(ALLPRODUCT == "Pota") %>%
#   mutate(ALLPRODUCT = "SwPo")
# 
# # Add fixes
# yld_FAOSTAT <- bind_rows(yld_FAOSTAT, soya_fix, swpo_fix)
# 
# # Clean up
# rm(soya_fix, soya_2003, swpo_fix)
# 
# # We first create a moving average over five years to smooth the data
# ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
# yld_FAOSTAT <- yld_FAOSTAT %>%
#   ungroup() %>%
#   group_by(ALLPRODUCT) %>%
#   mutate(value = ma(value))
# 
# yld_FAOSTAT_gr <- yld_FAOSTAT %>%
#   ungroup() %>%
#   group_by(ALLPRODUCT) %>%
#   filter(year %in% c(2000, 2007)) %>%
#   spread(year, value) %>%
#   mutate(yld_gr = unique(`2007`/`2000`)) %>%
#   dplyr::select(ALLPRODUCT, yld_gr)
# 
# # Combine with nat ag stat and rebase
# yld_adm_winsor_2000 <- yld_adm_winsor %>%
#   dplyr::select(-ha, -tons, -outlier) %>%
#   left_join(., yld_FAOSTAT_gr) %>%
#   mutate(value = value/yld_gr,
#          year = 2000) %>%
#   dplyr::select(-yld_gr)
# 
# # Boxplots
# fig_yld_winsor_2000 <- ggplot(data = yld_adm_winsor_2000, aes(y = value, x = ALLPRODUCT)) +
#   geom_boxplot(fill = "light grey") +
#   stat_boxplot(geom ='errorbar') +
#   geom_point(data = filter(yld_FAOSTAT, year %in% c(1999:2001), ALLPRODUCT %in% yld_adm$ALLPRODUCT), 
#              aes(y = value, x = ALLPRODUCT, colour = factor(year)), size = 2.5) +
#   labs(title = "Yield comparison between winsorised national agricultural statistics and FAOSTAT for 2000",
#        caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
#        y = "tons/ha",
#        x ="",
#        colour = "Year (FAOSTAT)") +
#   theme_bw() +
#   theme(legend.position="bottom",
#         plot.title = element_text(hjust = 0.5),
#         text = element_text(size=10)) +
#   coord_flip() 
# 
# 
# # Save file
# write_csv(yld_adm_winsor_2000, file.path(dataPath, "Data\\MWI\\Processed\\agricultural_statistics\\yld_adm_2000.csv"))
# 
# 
# ### PROJECT AREA FROM AGRICULTURAL STATISTICS TO 2000 USING FAOSTAT TRENDS
# # Calculate FAOSTAT growth in area between 2000 and 2007 (base year nat ag stat)
# 
# # Area comparison shows data for Soya in 2000. We assume area has been the same as last year available for historical period.
# soya_2003 <- area_FAOSTAT %>%
#   filter(ALLPRODUCT == "Soya", year == 2003)
# 
# soya_fix <- soya_2003[rep(1,6),]
# soya_fix$year <- c(1997:2002)
# 
# 
# # There is no data on SwPo, we substitute by yield for potatoes.
# swpo_fix <- area_FAOSTAT %>%
#   filter(ALLPRODUCT == "Pota") %>%
#   mutate(ALLPRODUCT = "SwPo")
# 
# # Add fixes
# area_FAOSTAT <- bind_rows(area_FAOSTAT, soya_fix, swpo_fix)
# 
# # Clean up
# rm(soya_fix, soya_2003, swpo_fix)
# 
# # We first create a moving average over five years to smooth the data
# ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
# area_FAOSTAT <- area_FAOSTAT %>%
#   ungroup() %>%
#   group_by(ALLPRODUCT) %>%
#   mutate(value = ma(value))
# 
# area_FAOSTAT_gr <- area_FAOSTAT %>%
#   ungroup() %>%
#   group_by(ALLPRODUCT) %>%
#   filter(year %in% c(2000, 2007)) %>%
#   spread(year, value) %>%
#   mutate(area_gr = unique(`2007`/`2000`)) %>%
#   dplyr::select(ALLPRODUCT, area_gr)
# 
# # Area dataset
# area_adm <- ag_stat %>%
#   filter(unit == "ha")
# 
# # Combine with nat ag stat and rebase
# area_adm_2000 <- area_adm %>%
#   left_join(., area_FAOSTAT_gr) %>%
#   mutate(area_gr = ifelse(is.na(area_gr), 1, area_gr), 
#          area = value/area_gr,
#          year = 2000) %>%
#   dplyr::filter(area > 0) %>%
#   dplyr::select(-area_gr, -value)
#   
# 
# # save file
# write_csv(area_adm_2000, file.path(dataPath, "Data\\MWI\\Processed\\agricultural_statistics\\area_adm_2000.csv"))
