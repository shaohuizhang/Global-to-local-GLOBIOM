#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare survey data with FAOSTAT
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
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"
FAOSTATPath <- "C:\\Users\\vandijkm\\DATA\\FAOSTAT_20170117"


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
  filter(unit == "ha", year >1990) 
  
prod_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "tonnes", year >1990, element == "Production") 

yield_FAOSTAT <- FAOSTAT_prod %>%
  filter(unit == "hg/ha", year >1990, element == "Yield") %>%
  mutate(value = value/10000)

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

yield_nat <- ag_stat %>%
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
area_rank_FAOSTAT <- area_FAOSTAT %>%
  filter(year == 2000) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(item, ALLPRODUCT, value, share)

# Rank area of crops using ag stat
area_rank_nat <- area_nat %>%
  ungroup() %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) 

### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS PER ADM
# Area share of crops within regions
area_share_within_reg <- ag_stat %>%
  filter(unit == "ha") %>%
  ungroup() %>%
  group_by(year, adm2_GAUL) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm2_GAUL, year, share, ALLPRODUCT) %>%
  arrange(desc(share)) %>%
  spread(adm2_GAUL, share)

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
Fig_area_share <- spplot(MWI_adm2, c(3:plot_length), main = "Share of crop area within country",
       colorkey=list(space="bottom"))


### COMPARE FAOSTAT AND AG STAT MWI
# Area
area_compare <- ggplot() + 
  geom_line(data = area_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = area_nat, aes(x = year, y = value), colour = "red")

# Prod
prod_compare <- ggplot() + 
  geom_line(data = prod_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = prod_nat, aes(x = year, y = value), colour = "red")

# yield
yld_compare <- ggplot() + 
  geom_line(data = yield_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = yield_nat, aes(x = year, y = value), colour = "red")

# Heads
heads_compare <- ggplot() + 
  geom_line(data = number_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = number_nat, aes(x = year, y = value), colour = "red")

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
yld_outlier <- ggplot(data = yld_adm, aes(y = value, x = ALLPRODUCT)) +
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
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() 


# Winsor values
yld_adm_winsor <- yld_adm %>%
  mutate(value = winsor(value))

# Plot new values and compare with FAO
yld_winsor <- ggplot(data = yld_adm_winsor, aes(y = value, x = ALLPRODUCT)) +
  geom_boxplot(fill = "light grey") +
  stat_boxplot(geom ='errorbar') +
  geom_point(data = filter(yield_FAOSTAT, year %in% c(2005:2008), ALLPRODUCT %in% yld_adm$ALLPRODUCT), 
             aes(y = value, x = ALLPRODUCT, colour = factor(year)), size = 2.5) +
  labs(title = "Yield comparison between winsorised national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
      y = "tons/ha",
      x ="",
      colour = "Year (FAOSTAT)") +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() 
  


