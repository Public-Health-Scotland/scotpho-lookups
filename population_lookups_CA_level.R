# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script creates population lookups using council area (CA) population estimates
# NRS typically release CA estimates first, and then small area population estimates (SAPE) a few months later
# However, there has been a significant delay in the release of 2022 SAPEs
# This script can therefore be run to create population estimates at CA, HB, HSCP, ADP and Scotland level whilst waiting on SAPEs to be released

# This allows a number of indicators to be updated where population estimates are used as the denominator,
# and where they are not released at intermediate zone or locality level (i.e. dependent on SAPEs)
# This is because we have different types of population lookups 
# (i.e. datazone level lookups for calculating all areatypes including HSC localities and IZs
# and higher geography level lookups containing only CA/HB/ADP/HSCP/Scotland)

# there are therefore a number of indicators where the population lookup passed to the analysis functions are those higher geo level lookups:


# Indicators which can be updated prior to release of SAPEs:-
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (these ones we can extract the data ourselves)
# COPD incidence
# COPD deaths
# Alcohol-related hospital admissions , aged 11-25 years
# Children hospitalised due to asthma, aged 0-15 years
# Deaths from suicide, female
# Deaths from suicide, male
# Deaths from suicide, aged 11-25 years
# Deaths in children, aged 1-15
# drug related hospital admissions
# Drug-related hospital admissions
# Lung cancer deaths
# Lung cancer registrations
# Unintentional unjuries inunder 5s
# Young people admitted to hospital due to assault

# (these ones require data requests)
# Drug-related deaths
# Drug-related deaths, females
# Drug-related deaths, males
# Smoking Attributable admissions
# Smoking Attrbutable deaths
# Availability of smoking cessation products
# Personal licences in force
# Premise licences in force - Off trade
# Premise licences in force - On trade
# Premise licences in force - Total



# (these ones we save ourselves from publications)
# Child protection with parental alcohol misuse
# Child protection with parental drug misuse
# Child protection with parental drug or alcohol misuse
# Children looked after by local authority
# Children on the child protection register
# Children referred to the Children's Reporter for care and protection
# Children referred to the Children's Reporter for offences
# Primary school children
# Secondary school children




# Lookups which need updated to update these indicators:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# note: versions ending in _SR contain age and sex splits for calculating standardised rates

# CA_pop_allages_SR 
# CA_pop_16+_SR
# CA_pop_11to25_SR
# CA_pop_11to25
# CA_pop_12+
# CA_pop_under18 
# CA_pop_1to15
# CA_pop_under16_SR
# CA_pop_15to25_SR
# CA_pop_under5_SR


# ~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Housekeeping ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

## packages ----
library(dplyr)
library(tidyr)

## filepaths ----
cl_out <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/" #cl-out folderwith new 2023 estimates
scotpho_lookups_folder <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" # scotpho folder where geography and population lookups are saved


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Lookups ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

## geography lookup 
geo_lookup<- readRDS(paste0(scotpho_lookups_folder, "Geography/DataZone11_All_Geographies_Lookup.rds")) |>
  select(ca2019, hscp2019, hb2019, adp, pd) |>
  unique()

## new council area population estimates
CA_estimates_raw<- readRDS(paste0(cl_out, "CA2019_pop_est_1981_2023.rds")) |>
  filter(year >= 2002)



# ~~~~~~~~~~~~~~~~~~~~~~
### 4. Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# function to create population lookups for different age groups
create_population_lookups <- function(lower_age, upper_age, basefile = CA_estimates_raw, geography_lookup = geo_lookup, name){
  
  # step 1: filter by age
  # note this needs to be done first before creating age group column 
  # so that for example, if trying to create a population 16+ file, the age 15 is removed from the '15-19' grouping
  CA_estimates <- basefile |>
    filter(age >= lower_age & age <= upper_age)
  
  
  # step 2: create age group column required for calculating standardised rates
  CA_estimates <- CA_estimates |>
    mutate(age_grp = case_when(
      age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
      age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7,
      age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
      age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13,
      age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
      age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19,
      TRUE ~ as.numeric(age)
    ))
  
  
  # step 3: attach other geography levels (HB, ADP, HSCP, Scotland)
  CA_estimates <- CA_estimates |>
    left_join(geography_lookup, by = "ca2019") |>
    mutate(Scotland = "S00000001")
  
  
  # step 4: pivot the data longer to create a geography code column
  CA_estimates <- CA_estimates |>
    pivot_longer(cols = c(ca2019, hscp2019, hb2019, adp, Scotland),
                 names_to = "area_type",
                 values_to = "code")
  
  # step 5: sum the population estimates for each geography
  
  # a. population estimates by geography and year only
  # for calculating crude rates
  CR_CA_estimates <- CA_estimates |>
    group_by(year, code) |>
    summarize(denominator = sum(pop), .groups = 'drop')
  
  # b. population estimates by geography, year, age and sex
  # for calculating standardised rates
  SR_CA_estimates <- CA_estimates |>
    rename(sex_grp = sex) |>
    group_by(year, code, age_grp, sex_grp) |>
    summarize(denominator = sum(pop), .groups = 'drop')
  
  
  # step 6: save the lookups in a temporary sub-folder
  saveRDS(CR_CA_estimates, file=paste0(scotpho_lookups_folder, 'Population/Temporary/CA_pop_', name,'.rds'))
  saveRDS(SR_CA_estimates, file=paste0(scotpho_lookups_folder, 'Population/Temporary/CA_pop_', name,'_SR.rds'))
}




# function to check the historic data in the newly created lookup against the old lookup saved in the scotpho folder
dq_check <- function(filename){
  
  
  old_file <- readRDS(paste0(scotpho_lookups_folder, "Population/", filename, ".rds")) # read in old file 
  new_file <- readRDS(paste0(scotpho_lookups_folder, "Population/Temporary/", filename, ".rds")) # read in new file 
  
  # columns to join new and old datasets on 
  join_cols = c("year", "code")
  
  # also join on age and sex columns if it's an SR file 
  if(grepl("SR", filename)){
    join_cols <- c(join_cols, "sex_grp", "age_grp")
  }
  
  # join files and check 
  check <- left_join(old_file, new_file, by = join_cols) |>
    mutate(number_diff = denominator.x - denominator.y,
           percent_diff = (number_diff/denominator.x) * 100)
  
  
  rm(old_file, new_file)
  
  return(check)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5.  Create population lookups ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

create_population_lookups(lower_age = 0, upper_age = 90, name = "allages")
create_population_lookups(lower_age = 16, upper_age = 90, name = "16+")
create_population_lookups(lower_age = 19, upper_age = 90, name = "19+")
create_population_lookups(lower_age = 11, upper_age = 25, name = "11to25")
create_population_lookups(lower_age = 0, upper_age = 15, name = "under16")
create_population_lookups(lower_age = 0, upper_age = 4, name = "under5")
create_population_lookups(lower_age = 15, upper_age = 25, name = "15to25")
create_population_lookups(lower_age = 0, upper_age = 17, name = "under18")
create_population_lookups(lower_age = 0, upper_age = 17, name = "1to15")
create_population_lookups(lower_age = 12, upper_age = 90, name = "12+")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Check historic data against old lookups  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# choosing a few new lookups and checking against the old lookup before archiving them 
# some historic changes between 2012 - 2021
# everything pre 2012 an exact matc

#check <- dq_check("CA_pop_allages_SR")
#check <- dq_check("CA_pop_allages")
#check <- dq_check("CA_pop_16+_SR")
#check <- dq_check("CA_pop_16+")
#check <- dq_check("CA_pop_16+")
#check <- dq_check("CA_pop_11to25_SR")
#check <- dq_check("CA_pop_11to25")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. create backups of old files ---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# note: save the old files from the Population lookup folder into the 'backups' subfolder, then move these files
# across to the population folder after checking they are ok so they are ready to be used within the indicator analysis functions.



