##################################
# Analyst notes ---
##################################

# This script creates population lookups using council area (CA) population estimates
# NRS typically release CA estimates first, and then small area population estimates (SAPE) a few months later
# However, there has been a significant delay in the release of 2022 SAPEs
# This script can therefore be run to create population estimates at CA, HB, HSCP, ADP and Scotland level whilst waiting on SAPEs to be released

# This allows a number of indicators to be updated where population estimates are used as the denominator,
# and where they are not released at intermediate zone or locality level (i.e. dependent on SAPEs)

# Indicators which can be updated (with name of lookup file required to carry out update):-

# COPD incidence - CA_pop_16_SR+
# COPD deaths - CA_pop_16+_SR
# Alcohol-related hospital admissions , aged 11-25 years - CA_pop_11to25_SR
# Children hospitalised due to asthma, aged 11-25 years - CA_pop_under16_SR
# Deaths from suicide, female - CA_pop_allages_SR
# Deaths from suicide, male - CA_pop_allages_SR
# Deaths from suicide, aged 11-25 years - CA_pop_11to25
# drug related hospital admissions - CA_pop_11to25_SR
# Drug-related hospital admissions - CA_pop_allages_SR
# Lung cancer deaths - CA_pop_16+_SR
# Lung cancer registrations - CA_pop_16+_SR
# Unintentional unjuries inunder 5s – CA_pop_under5_SR
# Young people admitted to hospital due to assault - CA_pop_15to25_SR
# Drug-related deaths, all - CA_pop_all_ages_R
# Drug-related deaths, females - CA_pop_allages_SR
# Drug-related deaths, males - CA_pop_allages_SR
# Smoking Attributable admissions – CA_pop_all_ages_SR
# Smoking Attrbutable deaths – CA_pop_all_ages_SR


########################################################
# Lookups which need updated to update these indicators:
########################################################

# CA_pop_allages_SR 
# CA_pop_16+_SR
# CA_pop_11to25_SR
# CA_pop_11to25 (version used for crude rates)
# CA_pop_under16_SR
# CA_pop_15to25_SR
# CA_pop_under5_SR


########################
# packages
#########################
library(dplyr)
library(tidyr)


########################
# filepaths
########################
cl_out <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/" #cl-out folderwith new 2022 estimates
scotpho_lookups_folder <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" # scotpho folder where geography and population lookups are saved


##########################
# Lookups 
###########################

# geography lookup 
geo_lookup<- readRDS(paste0(scotpho_lookups_folder, "Geography/DataZone11_All_Geographies_Lookup.rds")) |>
  select(ca2019, hscp2019, hb2019, adp) |>
  unique()

# new council area population estimates
CA_estimates_raw<- readRDS(paste0(cl_out, "CA2019_pop_est_1981_2022.rds")) |>
  filter(year >= 2002)



###################################################################
# Functions
###################################################################


# function to create SR population lookups for different age groups
create_population_lookup <- function(lower_age, upper_age, basefile = CA_estimates_raw, geography_lookup = geo_lookup){
  
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
  
  
  # step 3: select required columns
  CA_estimates <- CA_estimates |>
    select(year, ca2019, sex, age_grp, pop)
  
  
  # step 4: attach other geography levels (HB, ADP, HSCP, Scotland)
  CA_estimates <- CA_estimates |>
    left_join(geography_lookup, by = "ca2019") |>
    mutate(Scotland = "S00000001")
  
  
  # step 5: pivot the data longer to create a geography code column
  # and sum the population estimates for each geography code
  CA_estimates <- CA_estimates |>
    pivot_longer(cols = c(ca2019, hscp2019, hb2019, adp, Scotland),
                 names_to = "area_type",
                 values_to = "code") |>
    group_by(age_grp, sex, year, code) |>
    summarize(pop = sum(pop), .groups = 'drop')
  
  # step 6: rename columns as required to be used in scotpho analysis functions
  CA_estimates <- CA_estimates |>
    rename(denominator = pop,
           sex_grp = sex)
  
  return(CA_estimates)
  
}



# function to check the historic data in the newly createed lookup against the old lookup saved in the scotpho folder
dq_check <- function(old_file, new_data){
  
  old_estimates <- readRDS(paste0(scotpho_lookups_folder, "Population/", old_file, ".rds"))
  
  check <- left_join(old_estimates, new_data, by = c("year", "sex_grp", "age_grp", "code")) |>
    mutate(number_diff = denominator.x - denominator.y,
           percent_diff = (number_diff/denominator.x) * 100)
}


####################################
# Create population lookups 
###################################

pop_all_ages <- create_population_lookup(lower_age = 0, upper_age = 90)
pop_16_plus <- create_population_lookup(lower_age = 16, upper_age = 90)
pop_11_to_25 <- create_population_lookup(lower_age = 11, upper_age = 25)
pop_under_16 <- create_population_lookup(lower_age = 0, upper_age = 15)
pop_under_5 <- create_population_lookup(lower_age = 0, upper_age = 4)
pop_15_to_25 <- create_population_lookup(lower_age = 15, upper_age = 25)


########################################################
# Check historic data against old lookups before saving 
########################################################

pop_all_ages_check <- dq_check(new_data = pop_all_ages, old_file = "CA_pop_allages_SR")
pop_16_plus_check <- dq_check(new_data = pop_16_plus, old_file = "CA_pop_16+_SR")
pop_11_to_25_check <- dq_check(new_data = pop_11_to_25, old_file = "CA_pop_11to25_SR")
pop_under_16_check <-  dq_check(new_data = pop_under_16, old_file = "CA_pop_under16_SR")
pop_under_5_check <-  dq_check(new_data = pop_under_5, old_file = "CA_pop_under5_SR")
pop_15_to_25_check <- dq_check(new_data = pop_15_to_25, old_file = "CA_pop_15to25_SR")


# remove checks from global env
rm(pop_all_ages_check, pop_16_plus_check, pop_11_to_25_check, pop_under_16_check, pop_under_5_check, pop_15_to_25_check)


###############################################
# save new lookups in temporary folder 
##############################################
saveRDS(pop_all_ages, paste0(scotpho_lookups_folder, "Population_workaround/CA_pop_allages_SR.rds"))
saveRDS(pop_16_plus, paste0(scotpho_lookups_folder, "Population_workaround/CA_pop_16+_SR.rds"))
saveRDS(pop_11_to_25, paste0(scotpho_lookups_folder, "Population_workaround/CA_pop_11to25_SR.rds"))
saveRDS(pop_under_16, paste0(scotpho_lookups_folder, "Population_workaround/CA_pop_under16_SR.rds"))
saveRDS(pop_under_5, paste0(scotpho_lookups_folder, "Population_workaround/CA_pop_under5_SR.rds"))
saveRDS(pop_15_to_25, paste0(scotpho_lookups_folder, "Population_workaround/CA_pop_15to25_SR.rds"))

###########################
# create backups of old files 
###########################


# note: save the old files from the Population lookup folder into the 'backups' subfolder, then move these files
# across to the population folder so they are ready to be used within the indicator analysis functions.



