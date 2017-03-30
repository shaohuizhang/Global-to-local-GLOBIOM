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
GLOBIOM_cropanim <- read_excel(file.path(dataPath, "GLOBIOM/Concordance/GLOBIOM_mappings.xlsx"), sheet = "GLOBIOM_cropanim")
GLOBIOM_itemCode <- read_excel(file.path(dataPath, "GLOBIOM/Concordance/GLOBIOM_mappings.xlsx"), sheet = "GLOBIOM_itemCode") %>%
  dplyr::select(ALLPRODUCT, itemCode) 


### DOWNLOAD AND PROCESS FAOSTAT DATA
# Download
FAOSTAT_prod_raw <- read_csv(file.path(FAOSTATPath, "Production_Crops_E_All_Data_(Normalized).csv"))
FAOSTAT_lvst_raw <- read_csv(file.path(FAOSTATPath, "Production_Livestock_E_All_Data_(Normalized).csv"))

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
ag_stat <- read_csv(file.path(dataPath, "Data/Processed/MWI/Agricultural_statistics/ag_stat_MWI.csv"))
tot_pop_adm <- read_csv(file.path(dataPath, "Data/Processed/MWI/Spatial_data/tot_pop_adm_2000_MWI.csv"))

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
  group_by(year, adm1) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm1, year, share, ALLPRODUCT) %>%
  arrange(desc(share)) %>%
  spread(adm1, share)

### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL, COMPARING ADMs
# Area share of crops within country by crop
area_share_within_country <- ag_stat %>%
  filter(unit == "ha") %>%
  ungroup() %>%
  group_by(year, ALLPRODUCT) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  dplyr::select(adm1, year, share, ALLPRODUCT) %>%
  arrange(desc(share)) %>%
  spread(ALLPRODUCT, share)

# Maps of total area per region
MWI_adm1 <- readRDS(file.path(dataPath, "Data\\Processed\\MWI\\GADM_maps\\GADM_2.8_MWI_adm1.rds"))
MWI_adm1_df <- MWI_adm1@data %>%
  rename(adm1 = NAME_1) %>%
  mutate(adm1 = toupper(adm1)) %>%
  left_join(., area_share_within_country) %>%
  dplyr::select(OBJECTID, adm1, BeaD:Toba)
MWI_adm1@data <- MWI_adm1_df


plot_length <- length(unique(area_nat$ALLPRODUCT)) + 2
spplot(MWI_adm1, c(3:plot_length), main = "Share of crop area within country",
       colorkey=list(space="bottom"))


### COMPARE FAOSTAT AND AG STAT MWI
# Area
ggplot() + 
  geom_line(data = area_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = area_nat, aes(x = year, y = value), colour = "red")

# Prod
ggplot() + 
  geom_line(data = prod_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = prod_nat, aes(x = year, y = value), colour = "red")

# yield
ggplot() + 
  geom_line(data = yield_FAOSTAT, aes(x = year, y = value)) +
  facet_wrap(~ALLPRODUCT, scales = "free") +
  geom_point(data = yield_nat, aes(x = year, y = value), colour = "red")

# Number
ggplot() + 
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
yield_adm <- ag_stat %>%
  filter(unit %in% c("ha","tons")) %>%
  spread(unit, value) %>%
  mutate(value = tons/ha) %>%
  na.omit %>%
  filter(value >0, !is.infinite(value)) %>% # remove zero values that decrease average and inf
  group_by(ALLPRODUCT) %>%
  mutate(outlier = ifelse(is_outlier(value), adm1, NA))

# Distribution of yield across regions and outliers
ggplot(data = yield_adm, aes(y = value, x = ALLPRODUCT)) +
  geom_boxplot()+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

# Winsor values
yield_adm <- yield_adm %>%
  mutate(value = winsor(value))

# Plot new values and compare with FAO
ggplot(data = yield_adm, aes(y = value, x = ALLPRODUCT)) +
  geom_boxplot(fill = "light grey") +
  geom_point(data = filter(yield_FAOSTAT, year %in% c(2005:2008), ALLPRODUCT %in% yield_adm$ALLPRODUCT), 
             aes(y = value, x = ALLPRODUCT, colour = factor(year)), size = 2.5) +
  labs(title = "Yield comparison between national agricultural statistics and FAOSTAT",
       caption = "Source: FAOSTAT and Malawi National Census of Agriculture and Livestock 2006/07)",
      y = "tons/ha",
      x ="",
      colour = "Year (FAOSTAT)") +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() 
  


