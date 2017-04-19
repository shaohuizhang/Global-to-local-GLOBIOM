#'========================================================================================================================================
#' Project:  ISWEL
#' Subject:  Data to process CEEPA and extract relevant data for target countries
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "haven")


### SET WORKING DIRECTORY
wdPath<-"~/ISWEL"
setwd(wdPath)

### SET DATAPATH
dataPath <- "P:\\globiom\\Projects\\ISWEL\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### FUNCTIONS
# Function to strip attributes and add classes
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}

### DOWNLOAD MAPS AND PROCESS MAPS
# Download maps
ZWE_adm1 <- readRDS(file.path("Data/Maps/GADM_2.8_ZWE_adm1.rds"))
ZWE_adm2 <- readRDS(file.path("Data/Maps/GADM_2.8_ZWE_adm2.rds"))
ZMB_adm1 <- readRDS(file.path("Data/Maps/GADM_2.8_ZMB_adm1.rds"))
ZMB_adm2 <- readRDS(file.path("Data/Maps/GADM_2.8_ZMB_adm2.rds"))

# Extract adm data
ZWE_adm1_df <- ZWE_adm1@data
ZWE_adm2_df <- ZWE_adm2@data %>%
  transmute(OBJECTID, iso3c = ISO, adm1 = tolower(NAME_1), adm2 = tolower(NAME_2))
  
ZMB_adm1_df <- ZMB_adm1@data
ZMB_adm2_df <- ZMB_adm2@data %>%
  transmute(OBJECTID, iso3c = ISO, adm1 = tolower(NAME_1), adm2 = tolower(NAME_2))

### DOWNLOAD DATA
CEEPA_raw <- read_dta(file.path(dataPath, "Raw/CEEPA/CEEPASurvey.dta")) 
adm_names <- read_csv(file.path(dataPath, "Raw/CEEPA/district_match_Jan2017.csv"))

# Tolower gives error: Wait for answer of Katharina on maps
CEEPA_loc <- CEEPA_raw %>% 
  mutate(adm0 = as_factor(adm0)) %>%
  filter(adm0 %in% c("zambia","zimbabwe")) %>%
  transmute(country = adm0,
         adm1 = as_factor(adm1),
         adm2 = as_factor(adm2),
         vname = as_factor(vname),
         iso3c = countrycode(adm0, "country.name", "iso3c"),
         lon = lon,
         lat = lat) %>%
  stripAttributes()

str(CEEPA_loc)

loc <- CEEPA_loc %>%
  group_by(country, adm1, adm2) %>%
  summarize(n = n())

check <- left_join(loc, adm_names, by = c("adm2" = "hs_original", "country" = "country"))

### Section 4
# Check number of plots => max 6 and only 7 in lvs
names(CEEPA_raw)[grep("p7" , names(CEEPA_raw))]
season <- c("s1", "s2", "s3")

sect4_raw <- dplyr::select(CEEPA_raw, hhcode, s1p1c1:s1p3c6sval) %>%
  gather(variable, value, -hhcode) %>%
  separate(variable, c("season", "plotid", "cropnumber", "indicator"), sep = c(2, 4, 6), remove = F)

sect4 <- sect4_raw %>%
  mutate(indicator = ifelse(indicator == "", "crop", indicator)) %>%
  select(-variable) %>%
  spread(indicator, value) %>%
  mutate_at(vars(area:sval), funs(replace(., is.nan(.), NA))) # replace all nan by NA

check <- sect4 %>%
  mutate_at(vars(area:sval), funs(replace(., is.nan(.), NA)))

check2 <- check %>% select(-hhcode:-cropnumber) %>%
  rowSums(mutate_at(.,vars(area:sval), funs(is.na(.)))) == 13
%>%
  rowSums(.)

c
rowSums(mutate_at(.,vars(area:sval), funs(is.na(.)) == 13))



check <- sect4 %>%
  mutate_at(vars(area:sval), funs(replace(., . > 5, NA)))

check <- sect4 %>%



sect4[is.nan(sect4)] <- NA
inf.nan.na.clean.f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  x<-x[complete.cases(x),]
  return(x)
}
# To remove rows where all valuesa are NA
checkRows <- c("GE_NABS_GOV_ag",  "GE_NABS_HES_ag",  "GE_NABS_PNP_ag",
               "GE_FOS_GOV_ag", "GE_FOS_HES_ag",  "GE_FOS_PNP_ag", 
               "GB_NABS_NA_ag", 
               "hr_FOS_GOV_ag", "hr_FOS_HES_ag",  "hr_FOS_PNP_ag",
               "GE_NABS_GOV_tot", "GE_NABS_HES_tot", "GE_NABS_PNP_tot", 
               "hr_FOS_GOV_tot",  "hr_FOS_HES_tot", "hr_FOS_PNP_tot")
no_inf <- sect4 %>%
  filter(rowSums(mutate_each(total_imp[,checkRows], funs(is.na(.)))) == length(checkRows))
