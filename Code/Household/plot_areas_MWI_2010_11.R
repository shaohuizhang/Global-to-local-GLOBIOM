# -------------------------------------
#' Plot areas Malawi wave 1 (2010 - 11)
# ------------------------------------- 

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Other/Plot_size"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\"
}

# Areas for wave 1 have been imputed by the world bank lsms-isa team
# as some plots did not have a gps measurement. Measurements appear
# to be in hectacres

areas_2010_11 <- read_dta(file.path(dataPath, "Other\\Plot_size\\areas_mwi_y1_imputed.dta")) %>%  
  select(case_id, plotnum,
         area_farmer=area_sr, area_gps=area_gps_mi_50)

areas_2010_11$area_gps <- ifelse(areas_2010_11$area_gps %in% 0, NA, areas_2010_11$area_gps)

rm(dataPath)

