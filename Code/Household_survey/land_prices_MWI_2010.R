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
#source("code/winsor.R")

### FUNCTIONS
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}

# Make script to determine last rainy and dry season that relates to the questionnaire on the basis of MOD.

### LAND
# Plot area
plot_areaRS <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_C.dta")) %>%
  transmute(case_id, ea_id, plotnum=ag_c00,
            plot_area_qty_f=ag_c04a, plot_area_unit_f=as_factor(ag_c04b), plot_area_qty_gps=ag_c04c) %>%
  stripAttributes

# Plot rent
plot_rentRS <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  transmute(case_id, ea_id, plotnum=ag_d00,
          pot_price=ag_d05, pot_rent=ag_d06, 
          rent_paid_cash=ag_d11a, rent_paid_kind=ag_d11b, rent_topay_cash=ag_d11c, rent_topay_kind=ag_d11d, period=ag_d12) %>%
  stripAttributes


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
inputs1$unit2kg <- NULL

inputs2 <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_F.dta")) %>%
  transmute(case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f26a, qty_unit = as_factor(ag_f26b), valu=ag_f29) %>%
  do(filter(., complete.cases(.)))
levels(inputs2$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99, NaN)
inputs2$unit2kg <- as.numeric(as.character(inputs2$qty_unit)); inputs2$qty_unit <- NULL
inputs2$unit2kg <- ifelse(inputs2$unit2kg > 90, NA, inputs2$unit2kg)
inputs2$qty <- inputs2$qty * inputs2$unit2kg
inputs2$unit2kg <- NULL

input_prices <- bind_rows(inputs1, inputs2) %>%
  mutate(typ = ifelse(typ %in% c("23:21:0+4S/CHITOWE"),
                                  "NPK (MWI)", typ))




# read in external conversion file
conv <- read.csv(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Conversion/Fert_comp.csv")) %>%
  transmute(typ=toupper(Fert_type2), n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fertRS1$typ))

# join conversion information with 
# fertilizer information
fertRS1 <- left_join(fertRS1, conv)
fertRS2 <- left_join(fertRS2, conv)

# bind the type 1 and type 2 fertilizer
fertRS <- rbind(fertRS1, fertRS2)

# calcualte the quantity of nitrogen 
# in fertilizer
fertRS <- mutate(fertRS,
                 Qn=qty*n,
                 Qp=qty*p)

















plotDS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_k0a,
         crop1 = ag_k21a, crop2 = ag_k21b, crop3 = ag_k21c,
         crop4 = ag_k21d, crop5 = ag_k21e, soil = ag_k22,
         slope_farmer = ag_k27, irrig = ag_k29a,
         fallow = ag_k34, manure = ag_k37,
         pest = ag_k41, pest_type1 = ag_k42a, pest_q1 = ag_k42b,
         pest_unit1 = ag_k42c, pest_type2 = ag_k42d,
         pest_q2 = ag_k42e, pest_unit2 = ag_k42f)

# re-factor soil and slope_farmer variables
plotDS$soil <- factor(plotDS$soil, levels=c(1,2,3,4),
                      labels=c("Sandy", "Loam", "Clay", "Other"))
plotDS$slope_farmer <- factor(plotDS$slope_farmer, levels=c(1,2,3,4),
                              labels=c("Flat", "Slightly sloped",
                                       "Moderately sloped", "Very steep"))

# remove labels from certain variables
plotDS$crop1 <- as.integer(plotDS$crop1)
plotDS$crop2 <- as.integer(plotDS$crop2)
plotDS$crop3 <- as.integer(plotDS$crop3)
plotDS$crop4 <- as.integer(plotDS$crop4)
plotDS$crop5 <- as.integer(plotDS$crop5)
plotDS$irrig <- ifelse(plotDS$irrig %in% 7, 0,
                       ifelse(plotDS$irrig %in% c(1:6), 1, NA))
plotDS$manure <- ifelse(plotDS$manure %in% 1, 1,
                        ifelse(plotDS$manure %in% 2, 0, NA))
plotDS$pest <- ifelse(plotDS$pest %in% 1, 1,
                      ifelse(plotDS$pest %in% 2, 0, NA))

# convert units for pesticide to kg/litres
plotDS$pest_q1 <- ifelse(plotDS$pest_unit1 %in% c(2, 8), plotDS$pest_q1,
                         ifelse(plotDS$pest_unit1 %in% c(1, 9), plotDS$pest_q1*0.001, NA))
plotDS$pest_q2 <- ifelse(plotDS$pest_unit2 %in% c(2, 8), plotDS$pest_q2,
                         ifelse(plotDS$pest_unit2 %in% c(1, 9), plotDS$pest_q2*0.001, NA))

# sum pesticide quantity and replace 
# the missing values.
plotDS$pest_q <- with(plotDS,
                      rowSums(cbind(pest_q1, pest_q2),
                              na.rm=TRUE))
miss <- with(plotDS,
             is.na(pest_q1) & is.na(pest_q2))
plotDS$pest_q[miss] <- NA

# remove unecessary vars
plotDS$pest_unit1 <- plotDS$pest_unit2 <-
  plotDS$pest_type1 <- plotDS$pest_type2 <- 
  plotDS$pest_q1 <- plotDS$pest_q2 <-  NULL

# inorganic fertilizer: respondents may
# have used more than one type of
# inorganic fertilizer.
fertDS1 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_k0a, typ = ag_k40a,
         qty = ag_k40d, month = ag_k40e)
fertDS2 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_k0a, typ=ag_k40f,
         qty=ag_k40i, month = ag_k40j )

# get the type of fertilizer
fertDS1$typ <- as_factor(fertDS1$typ)
fertDS2$typ <- as_factor(fertDS2$typ)

# make levels consistent with external
# conversion to nitrogen file
levels(fertDS1$typ) <- levels(fertDS2$typ) <-
  c("NPK (MWI)", "DAP", "CAN", "UREA", "D compound", "OTHER", "NaN")

# read in external conversion file
conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fertDS1$typ))

# join conversion information with 
# fertilizer information
fertDS1 <- left_join(fertDS1, conv)
fertDS2 <- left_join(fertDS2, conv)

# bind the type 1 and type 2 fertilizer
fertDS <- rbind(fertDS1, fertDS2)

# calcualte the quantity of nitrogen 
# in fertilizer
fertDS <- mutate(fertDS,
                 Qn=qty*n,
                 Qp=qty*p)

# summarise to the plot level
fertDS <- group_by(fertDS, case_id, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE))

# -------------------------------------
# join all the plot details information
# together and return a list for the 
# raing season and dry season.
# -------------------------------------

plotRS_2010_11 <- left_join(plotRS, fertRS) %>% mutate(season="RS")
plotDS_2010_11 <- left_join(plotDS, fertDS) %>% mutate(season="DS")
plot_2010_11 <- rbind(plotRS, plotDS)

# take out the trash
rm(dataPath, conv, fertDS, fertDS1, fertDS2,
   fertRS, fertRS1, fertRS2, miss, plotDS, plotRS)

