# -------------------------------------
#' Household labHHour wave 1 (2010 - 11)
#' Output: dataframe containing all labour
#' variables
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010/Data"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2010\\Data"
}

# load packages
library(haven)
library(dplyr)

# labour is recorded in the rainy season and the dry
# season for both household and hired labour. In addition
# labour is broken down depending on whether it was used for:
# 1. land preperation and harvesting
# 2. Weeding/fertilizing
# 3. Harvesting

# household labour in the rainy season - land preparation and harvesting
labHH_RS_prep <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select( HHID, case_id, plotnum=ag_d00, ag_d42a:ag_d42p ) %>%
  transmute(HHID, case_id, plotnum,
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
labHH_RS_WF <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select( HHID, case_id, plotnum=ag_d00, ag_d43a:ag_d43p ) %>%
  transmute(HHID, case_id, plotnum,
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
labHH_RS_harv <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select( HHID, case_id, plotnum=ag_d00, ag_d44a:ag_d44p ) %>%
  transmute(HHID, case_id, plotnum,
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

# for now we do not need the prsonal identifications, although it may
# be interesting to know the age and gender of those working on each
# plot. Instead consider total household labHHour in each of the three
# activities, and also their sum

labHH_RS_prep <- select(labHH_RS_prep, HHID, case_id, plotnum,
                        labHH_prep_hrs, labHH_prep_days)
labHH_RS_WF <- select(labHH_RS_WF, HHID, case_id, plotnum,
                      labHH_WF_hrs, labHH_WF_days)
labHH_RS_harv <- select(labHH_RS_harv, HHID, case_id, plotnum,
                        labHH_harv_hrs, labHH_harv_days)

# -------------------------------------
# Hired labour
# -------------------------------------

# three options for hired labour
# 1. All activities
# 2. All non-harvest activities
# 3. harvest activities

# payment was recorded for hired labour and
# can take the form of crops. Currently this
# is not of interest, therefore we focus on 
# the quantity of labour supplied. Hired labour
# is offered in days, not hours, aggregation
# of household and hired labour can, therefore,
# only be aggregated by day

labHire_RS_non_harv <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select( HHID, case_id, plotnum=ag_d00, ag_d47a:ag_d47e ) %>%
  transmute(HHID, case_id, plotnum,
            lab_men_days = ag_d47a,
            lab_women_days = ag_d47c,
            lab_child_days = ag_d47e)

labHire_RS_non_harv$labHire_non_harv <- with(labHire_RS_non_harv,
                                 rowSums(cbind(lab_men_days, lab_women_days, lab_child_days),
                                         na.rm=TRUE))


labHire_RS_harv <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select( HHID, case_id, plotnum=ag_d00, ag_d48a:ag_d48e ) %>%
  transmute(HHID, case_id, plotnum,
            lab_men_days = ag_d48a,
            lab_women_days = ag_d48c,
            lab_child_days = ag_d48e)

labHire_RS_harv$labHire_harv <- with(labHire_RS_harv,
                                             rowSums(cbind(lab_men_days, lab_women_days, lab_child_days),
                                                     na.rm=TRUE))

# combine the hired labour
labHire_RS_non_harv <- select(labHire_RS_non_harv, HHID, case_id, plotnum, labHire_non_harv)
labHire_RS_harv <- select(labHire_RS_harv, HHID, case_id, plotnum, labHire_harv)

# -------------------------------------
# combine all labour variables together
# -------------------------------------

lab <- full_join(labHH_RS_prep, labHH_RS_WF) %>%
  full_join(labHH_RS_harv) %>%
  full_join(labHire_RS_non_harv) %>%
  full_join(labHire_RS_harv)

# finally create aggregate variables
# 1. all household labour in days
# 2. all household labour in hrs
# 3. all hired labour in days
# 4. the sum of household and hired labour

lab_2010_11 <- transmute(lab, HHID, case_id, plotnum,
                 labHH_hrs = labHH_prep_hrs + labHH_WF_hrs + labHH_harv_hrs,
                 labHH_days = labHH_prep_days + labHH_WF_days + labHH_harv_days,
                 labHire_days = labHire_non_harv + labHire_harv,
                 lab_total = labHH_days + labHire_days)

# take out trash
rm(dataPath, labHH_RS_harv, labHH_RS_prep,
   labHH_RS_WF, labHire_RS_harv, labHire_RS_non_harv, lab)