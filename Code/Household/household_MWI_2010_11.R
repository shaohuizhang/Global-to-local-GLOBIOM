# -------------------------------------
# Household Information MWI 2010 - 11
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010/Data"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2010\\Data"
}


household_2010_11 <- read_dta(file.path(dataPath, "Household/HH_MOD_B.dta")) %>%
  select(HHID, case_id, ea_id, indidy1 = PID, status=hh_b04, sex=hh_b03,
         yob=hh_b06b, age=hh_b05a, hh_b11, years=hh_b12)

household_2010_11$years <- as.numeric(household_2010_11$years)
household_2010_11$years <- ifelse(household_2010_11$hh_b11 %in% 1, household_2010_11$age, household_2010_11$years)
household_2010_11$status <- as_factor(household_2010_11$status)
household_2010_11$sex <- toupper(as_factor(household_2010_11$sex))
household_2010_11$yob <- as.integer(household_2010_11$yob)
household_2010_11$hh_b11 <- NULL

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

household_2010_11$cage <- cut(household_2010_11$age, breaks = c(0, 15, 55, max(household_2010_11$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

education <- read_dta(file.path(dataPath, "Household/HH_MOD_C.dta")) %>%
  select(HHID, case_id, ea_id, indidy1=PID, education_any=hh_c06, start=hh_c10,
         end=hh_c15, atSchool=hh_c13)

education$education_any <- as_factor(education$education_any) # ever went to school
education$end <- as.integer(as.character(education$end))
education$end <- ifelse(education$atSchool == 1, 2010, education$end)

# join with household_2010_11 dataframe
household_2010_11 <- left_join(household_2010_11, education)
household_2010_11$education <- household_2010_11$end - (household_2010_11$yob + household_2010_11$start)
household_2010_11$education <- ifelse(household_2010_11$education_any %in% "No", 0, household_2010_11$education)
household_2010_11 <- select(household_2010_11, -start, -end, -yob, -atSchool)

# remove negative years of education (56 obs)
household_2010_11$education <- ifelse(household_2010_11$education < 0, NA, household_2010_11$education)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

household_2010_11_x <- group_by(household_2010_11, HHID, case_id, ea_id) %>%
  summarise(education1555=sum(education[cage %in% "16-55"], na.rm=T),
            N1555=sum(cage %in% "16-55"))
household_2010_11 <- left_join(household_2010_11, household_2010_11_x); rm(household_2010_11_x)

# select only the head of the household
household_2010_11 <- filter(household_2010_11, status == "Head") %>% select(-status, -cage)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "Household/HH_MOD_W.dta")) %>%
  select(HHID, case_id, ea_id, hh_w01) %>% group_by(HHID, case_id, ea_id) %>%
  summarise(death = ifelse(any(hh_w01 %in% 1), 1, 0))

household_2010_11 <- left_join(household_2010_11, death)

# take out trash
rm(dataPath, education, death)
