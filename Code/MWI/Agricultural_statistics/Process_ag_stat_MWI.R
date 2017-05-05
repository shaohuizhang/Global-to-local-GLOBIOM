#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to analyse and process National agricultural statistics data
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


### DOWNLOAD MAPPINGS AND GLOBIOM CROP LIST
GLOBIOM_cropanim <- read_excel(file.path(dataPath, "Data/GLOBIOM/Concordance/GLOBIOM_mappings.xlsx"), sheet = "GLOBIOM_cropanim")
GLOBIOM_itemCode <- read_excel(file.path(dataPath, "Data/GLOBIOM/Concordance/GLOBIOM_mappings.xlsx"), sheet = "GLOBIOM_itemCode") %>%
  dplyr::select(ALLPRODUCT, itemCode) 
GLOBIOM_itemCode <- read_excel(file.path(dataPath, "Data/GLOBIOM/Concordance/GLOBIOM_mappings.xlsx"), sheet = "GLOBIOM_itemCode") %>%
  dplyr::select(ALLPRODUCT, itemCode) 


### DOWNLOAD AND PROCESS FAOSTAT DATA
# Download
FAOSTAT_prod_raw <- read_csv(file.path(FAOSTATPath, "Production_Crops_E_All_Data_(Normalized).csv"))
FAOSTAT_lvst_raw <- read_csv(file.path(FAOSTATPath, "Production_Livestock_E_All_Data_(Normalized).csv"))


### DOWNLOAD REGIONAL MAPPING
MWI2ADM_2000 <- read_excel(file.path(dataPath, "Data/MWI/Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2ADM_2000") %>%
  dplyr::select(adm1_hs, adm2_GAUL) %>%
  na.omit() %>%
  unique()

# process production stat
FAOSTAT_prod <- FAOSTAT_prod_raw %>%
  dplyr::select(fao = `Area Code`, country = Area, item = Item, itemCode = `Item Code`, element = Element, elementCode = `Element Code`, year = Year, unit = Unit, value = Value) %>%
  mutate(iso3c = countrycode(fao, "fao", "iso3c")) %>%
  filter(iso3c == "MWI") %>%
  left_join(., GLOBIOM_itemCode) %>%
  filter(!is.na(ALLPRODUCT))

area_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "ha", year >1990) %>%
  na.omit() # remove rows with na values for value
  
prod_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "tonnes", year >1990, element == "Production") 

yld_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "hg/ha", year >1990, element == "Yield") %>%
  mutate(value = value/10000) # convert to tons/ha

# process livestock stat
FAOSTAT_lvst <- FAOSTAT_lvst_raw %>%
  dplyr::select(fao = `Area Code`, country = Area, item = Item, itemCode = `Item Code`, element = Element, elementCode = `Element Code`, year = Year, unit = Unit, value = Value) %>%
  mutate(iso3c = countrycode(fao, "fao", "iso3c")) %>%
  filter(iso3c == "MWI") %>%
  left_join(., GLOBIOM_itemCode) %>%
  filter(!is.na(ALLPRODUCT)) %>%
  mutate(value = ifelse(ALLPRODUCT == "PTRY", value*1000, value))

number_FAOSTAT <- FAOSTAT_lvst %>%
  filter(year >1990) 


### AGGREGATE AGRICULTURAL STATISTICS
# Read data
ag_stat <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_MWI.csv"))
tot_pop_adm <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/tot_pop_adm_2000_MWI.csv"))

# Aggregate area and production to country level
area_nat <- ag_stat %>%
  filter(unit == "ha") %>%
  group_by(ALLPRODUCT, year) %>%
  summarize(value = sum(value, na.rm=T))

prod_nat <- ag_stat %>%
  filter(unit == "tons") %>%
  group_by(ALLPRODUCT, year) %>%
  summarize(value = sum(value, na.rm=T))

yld_nat <- ag_stat %>%
  filter(unit %in% c("ha","tons")) %>%
  group_by(ALLPRODUCT, year, unit) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  spread(unit, value) %>%
  mutate(value = tons/ha)

number_nat <- ag_stat %>%
  filter(unit == "number") %>%
  group_by(ALLPRODUCT, year) %>%
  summarize(value = sum(value, na.rm=T))


### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL
# Rank area of crops using FAOSTAT in base year
tab_area_rank_FAOSTAT <- area_FAOSTAT %>%
  filter(year == 2000) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(item, ALLPRODUCT, `area (ha)` = value, share)

# Rank area of crops using ag stat
tab_area_rank_nat <- area_nat %>%
  ungroup() %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  rename(`area (ha)` = value)

### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS PER ADM
# Area share of crops within regions
tab_area_share_within_reg <- ag_stat %>%
  filter(unit == "ha") %>%
  ungroup() %>%
  group_by(year, adm2_GAUL) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm2_GAUL, year, share, ALLPRODUCT) %>%
  arrange(desc(share)) %>%
  spread(ALLPRODUCT, share)

### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL, COMPARING ADMs
# Area share of crops within country by crop
area_share_within_country <- ag_stat %>%
  filter(unit == "ha") %>%
  ungroup() %>%
  group_by(year, ALLPRODUCT) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm2_GAUL, year, share, ALLPRODUCT) %>%
  arrange(desc(share)) %>%
  spread(ALLPRODUCT, share)

# Maps of total area per region
MWI_adm2 <- readRDS(file.path(dataPath, "Data\\MWI\\Processed\\maps\\GAUL_MWI_adm2_2000.rds"))
MWI_adm2_df <- MWI_adm2@data %>%
  rename(adm2_GAUL = ADM2_NAME) %>%
  mutate(adm2_GAUL = toupper(adm2_GAUL)) %>%
  left_join(., MWI2ADM_2000) %>%
  left_join(., area_share_within_country) %>%
  dplyr::select(ADM2_CODE, adm2_GAUL, BeaD:Toba)
MWI_adm2@data <- MWI_adm2_df

plot_length <- length(unique(area_nat$ALLPRODUCT)) + 2
fig_area_share <- spplot(MWI_adm2, c(3:plot_length), main = "Share of crop area within country",
       colorkey=list(space="bottom"))


### COMPARE FAOSTAT AND AG STAT MWI
# Area
fig_area_compare <- ggplot() + 
  geom_line(data = area_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = area_nat, aes(x = year, y = value), colour = "red") +
  labs(title = "Area comparison between national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

# Prod
fig_prod_compare <- ggplot() + 
  geom_line(data = prod_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = prod_nat, aes(x = year, y = value), colour = "red") +
  labs(title = "Production comparison between national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
       y = "tons",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

# yield
fig_yld_compare <- ggplot() + 
  geom_line(data = yld_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = yld_nat, aes(x = year, y = value), colour = "red") +
  labs(title = "Yield comparison between national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
       y = "tons/ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

# Heads
fig_heads_compare <- ggplot() + 
  geom_line(data = number_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = number_nat, aes(x = year, y = value), colour = "red") +
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
yld_adm <- ag_stat %>%
  filter(unit %in% c("ha","tons")) %>%
  spread(unit, value) %>%
  mutate(value = tons/ha) %>%
  na.omit %>%
  filter(value >0, !is.infinite(value)) %>% # remove zero values that decrease average and inf
  group_by(ALLPRODUCT) %>%
  mutate(outlier = ifelse(is_outlier(value), adm2_GAUL, NA))

# Distribution of yield across regions and outliers
fig_yld_outlier <- ggplot(data = yld_adm, aes(y = value, x = ALLPRODUCT)) +
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
yld_adm_winsor <- yld_adm %>%
  mutate(value = winsor(value))

# Plot new values and compare with FAO
fig_yld_winsor <- ggplot(data = yld_adm_winsor, aes(y = value, x = ALLPRODUCT)) +
  geom_boxplot(fill = "light grey") +
  stat_boxplot(geom ='errorbar') +
  geom_point(data = filter(yld_FAOSTAT, year %in% c(2005:2008), ALLPRODUCT %in% yld_adm$ALLPRODUCT), 
             aes(y = value, x = ALLPRODUCT, colour = factor(year)), size = 2.5) +
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
  

### PROJECT YIELD FROM AGRICULTURAL STATISTICS TO 2000 USING FAOSTAT TRENDS
# Calculate FAOSTAT growth in yields between 2000 and 2007 (base year nat ag stat)

# Yld comparison shows data for Soya in 2000. We assume yield has been the same as last year available for historical period.
soya_2003 <- yld_FAOSTAT %>%
  filter(ALLPRODUCT == "Soya", year == 2003)

soya_fix <- soya_2003[rep(1,6),]
soya_fix$year <- c(1997:2002)


# There is no data on SwPo, we substitute by yield for potatoes.
swpo_fix <- yld_FAOSTAT %>%
  filter(ALLPRODUCT == "Pota") %>%
  mutate(ALLPRODUCT = "SwPo")

# Add fixes
yld_FAOSTAT <- bind_rows(yld_FAOSTAT, soya_fix, swpo_fix)

# Clean up
rm(soya_fix, soya_2003, swpo_fix)

# We first create a moving average over five years to smooth the data
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
yld_FAOSTAT <- yld_FAOSTAT %>%
  ungroup() %>%
  group_by(ALLPRODUCT) %>%
  mutate(value = ma(value))

yld_FAOSTAT_gr <- yld_FAOSTAT %>%
  ungroup() %>%
  group_by(ALLPRODUCT) %>%
  filter(year %in% c(2000, 2007)) %>%
  spread(year, value) %>%
  mutate(yld_gr = unique(`2007`/`2000`)) %>%
  dplyr::select(ALLPRODUCT, yld_gr)

# Combine with nat ag stat and rebase
yld_adm_winsor_2000 <- yld_adm_winsor %>%
  dplyr::select(-ha, -tons, -outlier) %>%
  left_join(., yld_FAOSTAT_gr) %>%
  mutate(value = value/yld_gr,
         year = 2000) %>%
  dplyr::select(-yld_gr)

# Boxplots
fig_yld_winsor_2000 <- ggplot(data = yld_adm_winsor_2000, aes(y = value, x = ALLPRODUCT)) +
  geom_boxplot(fill = "light grey") +
  stat_boxplot(geom ='errorbar') +
  geom_point(data = filter(yld_FAOSTAT, year %in% c(1999:2001), ALLPRODUCT %in% yld_adm$ALLPRODUCT), 
             aes(y = value, x = ALLPRODUCT, colour = factor(year)), size = 2.5) +
  labs(title = "Yield comparison between winsorised national agricultural statistics and FAOSTAT for 2000",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
       y = "tons/ha",
       x ="",
       colour = "Year (FAOSTAT)") +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=10)) +
  coord_flip() 


# Save file
write_csv(yld_adm_winsor_2000, file.path(dataPath, "Data\\MWI\\Processed\\agricultural_statistics\\yld_adm_2000.csv"))


### PROJECT AREA FROM AGRICULTURAL STATISTICS TO 2000 USING FAOSTAT TRENDS
# Calculate FAOSTAT growth in area between 2000 and 2007 (base year nat ag stat)

# Area comparison shows data for Soya in 2000. We assume area has been the same as last year available for historical period.
soya_2003 <- area_FAOSTAT %>%
  filter(ALLPRODUCT == "Soya", year == 2003)

soya_fix <- soya_2003[rep(1,6),]
soya_fix$year <- c(1997:2002)


# There is no data on SwPo, we substitute by yield for potatoes.
swpo_fix <- area_FAOSTAT %>%
  filter(ALLPRODUCT == "Pota") %>%
  mutate(ALLPRODUCT = "SwPo")

# Add fixes
area_FAOSTAT <- bind_rows(area_FAOSTAT, soya_fix, swpo_fix)

# Clean up
rm(soya_fix, soya_2003, swpo_fix)

# We first create a moving average over five years to smooth the data
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
area_FAOSTAT <- area_FAOSTAT %>%
  ungroup() %>%
  group_by(ALLPRODUCT) %>%
  mutate(value = ma(value))

area_FAOSTAT_gr <- area_FAOSTAT %>%
  ungroup() %>%
  group_by(ALLPRODUCT) %>%
  filter(year %in% c(2000, 2007)) %>%
  spread(year, value) %>%
  mutate(area_gr = unique(`2007`/`2000`)) %>%
  dplyr::select(ALLPRODUCT, area_gr)

# Area dataset
area_adm <- ag_stat %>%
  filter(unit == "ha")

# Combine with nat ag stat and rebase
area_adm_2000 <- area_adm %>%
  left_join(., area_FAOSTAT_gr) %>%
  mutate(area_gr = ifelse(is.na(area_gr), 1, area_gr), 
         area = value/area_gr,
         year = 2000) %>%
  dplyr::filter(area > 0) %>%
  dplyr::select(-area_gr, -value)
  

# save file
write_csv(area_adm_2000, file.path(dataPath, "Data\\MWI\\Processed\\agricultural_statistics\\area_adm_2000.csv"))
