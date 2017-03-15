#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to create database with farm inputs
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot", "haven")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode", "plotKML")


### SET WORKING DIRECTORY
wdPath <- "~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### SOURCE
source("code/Household_survey/location_MWI_2010.R")
source("code/winsor.R")

### FUNCTIONS
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}


### DEFINE ADM
adm <- location %>% 
  dplyr::select(region, district) %>%
  unique() %>%
  na.omit


### FERTILIZER, PESTICIDES AND HERBICIDES
# Note that the question is about buying inputs without coupons.
# Hence unit values should reflect unsubsidised prices.

inputs1 <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_F.dta")) %>%
  transmute(case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f16a, qty_unit = as_factor(ag_f16b), valu=ag_f19) %>%
  do(filter(., complete.cases(.)))
levels(inputs1$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96, 97, 98, 99, NaN)
inputs1$unit2kg <- as.numeric(as.character(inputs1$qty_unit)); inputs1$qty_unit <- NULL
inputs1$unit2kg <- ifelse(inputs1$unit2kg > 90, NA, inputs1$unit2kg)
inputs1$qty <- inputs1$qty * inputs1$unit2kg

inputs2 <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_F.dta")) %>%
  transmute(case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f26a, qty_unit = as_factor(ag_f26b), valu=ag_f29) %>%
  do(filter(., complete.cases(.)))
levels(inputs2$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99, NaN)
inputs2$unit2kg <- as.numeric(as.character(inputs2$qty_unit)); inputs2$qty_unit <- NULL
inputs2$unit2kg <- ifelse(inputs2$unit2kg > 90, NA, inputs2$unit2kg)
inputs2$qty <- inputs2$qty * inputs2$unit2kg


inputs <- bind_rows(inputs1, inputs2) %>%
  mutate(typ = ifelse(typ %in% c("23:21:0+4S/CHITOWE"),
                                  "NPK (MWI)", typ)) %>%
  left_join(.,location)

# Number of observations => only for fertilizer there are sufficient observations to calculate regional prices 
table(inputs$typ)

### HERBICIDES
herb <- inputs %>%
  filter(typ == "HERBICIDE")

### INSECTICIDES
insec <- inputs %>%
  filter(typ == "INSECTICIDE")

### FUMIGANT
fumig <- inputs %>%
  filter(typ == "FUMIGANT")


### FERTILIZER
# read in external conversion file
conv <- read.csv(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Conversion/Fert_comp.csv")) %>%
  transmute(typ=toupper(Fert_type2), n=N_share/100, p=P_share/100)

# Calculate fertilizer nitrogen price.
# Values are winsored aggregates are presented for at least 5 values

fert <- inputs %>%
  filter(typ %in% c("CAN", "DAP", "D COMPOUND", "NPK (MWI)", "UREA")) %>%
  left_join(., conv) %>%
  mutate(Vfert=valu/qty,
         Qn=qty*n,
         Qp=qty*p,
         price = Vfert/n) %>%
  na.omit %>%
  mutate(price = winsor2(price),
         Vfert = winsor(Vfert))

# Check average fertilizer prices by type and region
# prices are close to around 100 nat cur which is also given by MWI statistics on the internet.
check_fert <- fert %>%
  group_by(district, typ) %>%
  summarize(Vfert = mean(Vfert, na.rm=T),
            price = mean(price, na.rm=T),
            n = n())


medianPrice_f <- function(df, level, group, type){
  prices <- df %>% 
    group_by_(.dots = c(group)) %>%
    dplyr::summarize(
      number = sum(!is.na(price)),
      price = median(price, na.rm=T)) %>%
    filter(number>=5) %>%
    mutate(level = level) %>%
    select(-number) 
  #prices <- setNames(prices, c(group, "price", "level")) 
  out <- left_join(adm, prices) %>%
    mutate(type = type)
  return(out)
}

fpCountry <- fert %>% 
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(adm, price = fpCountry$price,
                    level = "country",
                    type = "Pn")

fpRegion <- medianPrice_f(fert, "region", c("region"), "Pn")
fpDistrict <- medianPrice_f(fert, "district", c("region", "district"), "Pn")
names(fpRegion)[1:2] <- c("REGNAME", "DISNAME")
names(fpDistrict)[1:2] <- c("REGNAME", "DISNAME")
names(fpCountry)[1:2] <- c("REGNAME", "DISNAME")


fert_price <- bind_rows(fpCountry, fpDistrict, fpRegion) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                           ifelse(!is.na(region), region, country)),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region", "country")), 
         product = "fertilizer") %>%
  select(-country, -region, -district)

rm(fpCountry, fpRegion, fpDistrict, fertmar)


