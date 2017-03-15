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

### FARM LABOUR

# labour is recorded in the rainy season and the dry
# season for both household and hired labour. In addition
# labour is broken down depending on whether it was used for:
# 1. land preperation and harvesting
# 2. Weeding/fertilizing
# 3. Harvesting

# household labour in the rainy season - land preparation and harvesting
labHH_RS_prep <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d42a:ag_d42p ) %>%
  transmute(case_id, plotnum,
            pid1=ag_d42a, lab1_hrs=ag_d42b*ag_d42c*ag_d42d,
            pid2=ag_d42e, lab2_hrs=ag_d42f*ag_d42g*ag_d42h,
            pid3=ag_d42i, lab3_hrs=ag_d42j*ag_d42k*ag_d42l,
            pid4=ag_d42m, lab4_hrs=ag_d42n*ag_d42o*ag_d42p,
            lab1_days=ag_d42b*ag_d42c,
            lab2_days=ag_d42f*ag_d42g,
            lab3_days=ag_d42j*ag_d42k,
            lab4_days=ag_d42n*ag_d42o)

labHH_RS_prep$labHH_prep_hrs <- with(labHH_RS_prep,
                       rowSums(cbind(lab1_hrs, lab2_hrs, lab3_hrs, lab4_hrs),
                               na.rm=TRUE))
labHH_RS_prep$labHH_prep_days <- with(labHH_RS_prep,
                                     rowSums(cbind(lab1_days, lab2_days, lab3_days, lab4_days),
                                             na.rm=TRUE))

# household labHHour in the rainy season - Weeding/fertilizing
labHH_RS_WF <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d43a:ag_d43p ) %>%
  transmute(case_id, plotnum,
            pid1=ag_d43a, lab1_hrs=ag_d43b*ag_d43c*ag_d43d,
            pid2=ag_d43e, lab2_hrs=ag_d43f*ag_d43g*ag_d43h,
            pid3=ag_d43i, lab3_hrs=ag_d43j*ag_d43k*ag_d43l,
            pid4=ag_d43m, lab4_hrs=ag_d43n*ag_d43o*ag_d43p,
            lab1_days=ag_d43b*ag_d43c,
            lab2_days=ag_d43f*ag_d43g,
            lab3_days=ag_d43j*ag_d43k,
            lab4_days=ag_d43n*ag_d43o)

labHH_RS_WF$labHH_WF_hrs <- with(labHH_RS_WF,
                               rowSums(cbind(lab1_hrs, lab2_hrs, lab3_hrs, lab4_hrs),
                                       na.rm=TRUE))
labHH_RS_WF$labHH_WF_days <- with(labHH_RS_WF,
                                 rowSums(cbind(lab1_days, lab2_days, lab3_days, lab4_days),
                                         na.rm=TRUE))

# household labHHour in the rainy season - Harvesting
labHH_RS_harv <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d44a:ag_d44p ) %>%
  transmute(case_id, plotnum,
            pid1=ag_d44a, lab1_hrs=ag_d44b*ag_d44c*ag_d44d,
            pid2=ag_d44e, lab2_hrs=ag_d44f*ag_d44g*ag_d44h,
            pid3=ag_d44i, lab3_hrs=ag_d44j*ag_d44k*ag_d44l,
            pid4=ag_d44m, lab4_hrs=ag_d44n*ag_d44o*ag_d44p,
            lab1_days=ag_d44b*ag_d44c*ag_d44d,
            lab2_days=ag_d44f*ag_d44g*ag_d44h,
            lab3_days=ag_d44j*ag_d44k*ag_d44l,
            lab4_days=ag_d44n*ag_d44o*ag_d44p)

labHH_RS_harv$labHH_harv_hrs <- with(labHH_RS_harv,
                           rowSums(cbind(lab1_hrs, lab2_hrs, lab3_hrs, lab4_hrs),
                                   na.rm=TRUE))
labHH_RS_harv$labHH_harv_days <- with(labHH_RS_harv,
                                     rowSums(cbind(lab1_days, lab2_days, lab3_days, lab4_days),
                                             na.rm=TRUE))

# Instead consider total household labHHour in each of the three
# activities, and also their sum

labHH_RS_prep <- select(labHH_RS_prep, case_id, plotnum,
                        labHH_prep_hrs, labHH_prep_days)
labHH_RS_WF <- select(labHH_RS_WF, case_id, plotnum,
                      labHH_WF_hrs, labHH_WF_days)
labHH_RS_harv <- select(labHH_RS_harv, case_id, plotnum,
                        labHH_harv_hrs, labHH_harv_days)


### HIRED LABOUR

# three options for hired labour
# 1. All activities
# 2. All non-harvest activities
# 3. harvest activities

# Hired labour is offered in days, not hours, aggregation
# of household and hired labour can, therefore, only be aggregated by day.
# A small share of hired labour is payed in kind (and cash). As converting kind payments into value is cumbersome
# and this only happens on small scale, we calculate the average wage only for workers that are paid in cash.
# As we are interested in a weighted average wage, create tables for three all labour, harvest and non-harvest activities per class
# and remove all rows where data is missing. Finally, we calculate the average wage per adm.

# All activities
labHire_RS_all_men <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d46a:ag_d46h) %>%
  filter(is.na(ag_d46h) | ag_d46h == 0) %>%
  transmute(case_id, plotnum,
          lab_days = ag_d46a,
          wage_days = ag_d46b,
          class = "men", 
          activity ="all")

labHire_RS_all_women <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d46a:ag_d46h) %>%
  filter(is.na(ag_d46h) | ag_d46h == 0) %>%
  transmute(case_id, plotnum,
          lab_days = ag_d46c,
          wage_days = ag_d46d,
          class = "women",
          activity ="all")

labHire_RS_all_child <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d46a:ag_d46h) %>%
  filter(is.na(ag_d46h) | ag_d46h == 0) %>%
  transmute(case_id, plotnum,
          lab_days = ag_d46e,
          wage_days = ag_d46f,
          class = "child",
          activity ="all")


# Non-harvest
labHire_RS_non_harv_men <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d47a:ag_d47h) %>%
  filter(is.na(ag_d47h) | ag_d47h == 0) %>%
  transmute(case_id, plotnum,
            lab_days = ag_d47a,
            wage_days = ag_d47b,
            class = "men",
            activity = "non-harv")

labHire_RS_non_harv_women <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d47a:ag_d47h) %>%
  filter(is.na(ag_d47h) | ag_d47h == 0) %>%
  transmute(case_id, plotnum,
            lab_days = ag_d47c,
            wage_days = ag_d47d,
            class = "women",
            activity = "non-harv")

labHire_RS_non_harv_child <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d47a:ag_d47h) %>%
  filter(is.na(ag_d47h) | ag_d47h == 0) %>%
  transmute(case_id, plotnum,
            lab_days = ag_d47e,
            wage_days = ag_d47f,
            class = "child",
            activity = "non-harv")


# Harvest
labHire_RS_harv_men <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d48a:ag_d48h) %>%
  filter(is.na(ag_d48h) | ag_d48h == 0) %>%
  transmute(case_id, plotnum,
            lab_days = ag_d48a,
            wage_days = ag_d48b,
            class = "men",
            activity = "harv")

labHire_RS_harv_women <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d48a:ag_d48h) %>%
  filter(is.na(ag_d48h) | ag_d48h == 0) %>%
  transmute(case_id, plotnum,
            lab_days = ag_d48c,
            wage_days = ag_d48d,
            class = "women",
            activity = "harv")

labHire_RS_harv_child <- read_dta(file.path(dataPath, "Raw/MWI/Household_survey/2010/IHS3/Agriculture/AG_MOD_D.dta")) %>%
  select(case_id, plotnum=ag_d00, ag_d48a:ag_d48h) %>%
  filter(is.na(ag_d48h) | ag_d48h == 0) %>%
  transmute(case_id, plotnum,
            lab_days = ag_d48e,
            wage_days = ag_d48f,
            class = "child",
            activity = "harv")

# Combine all activities and clases
labHire_RS <- bind_rows(labHire_RS_all_men, labHire_RS_all_women, labHire_RS_all_child,
                        labHire_RS_non_harv_men, labHire_RS_non_harv_women, labHire_RS_non_harv_child,
                        labHire_RS_harv_men, labHire_RS_harv_women, labHire_RS_harv_child)%>%
  na.omit()



### COMBINE ALL LABOUR VARIABLES

lab <- full_join(labHH_RS_prep, labHH_RS_WF) %>%
  full_join(labHH_RS_harv) %>%
  full_join(labHire_RS_non_harv) %>%
  full_join(labHire_RS_harv)

# finally create aggregate variables
# 1. all household labour in days
# 2. all household labour in hrs
# 3. all hired labour in days
# 4. the sum of household and hired labour

lab_2010_11 <- transmute(lab, case_id, plotnum,
                 labHH_hrs = labHH_prep_hrs + labHH_WF_hrs + labHH_harv_hrs,
                 labHH_days = labHH_prep_days + labHH_WF_days + labHH_harv_days,
                 labHire_days = labHire_non_harv + labHire_harv,
                 lab_total = labHH_days + labHire_days)

# take out trash
rm(dataPath, labHH_RS_harv, labHH_RS_prep,
   labHH_RS_WF, labHire_RS_harv, labHire_RS_non_harv, lab)

