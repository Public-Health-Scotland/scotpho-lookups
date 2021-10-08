# Code to create a population lookup files for the Profiles. 
# The file first creates some basefiles to then run a series of macros to get the lookups with the desired population

# Part 1 - Functions.
# Part 2 - Basefiles based on DZ01 and DZ11
# Part 3 - Basefiles by deprivation quintile
# Part 4 - Create population files 

###############################################.
## Packages/Filepaths ----
###############################################.
library(dplyr)
library(tidyr) # long to wide format
library(httr) # api connection
library(jsonlite)  # transforming JSON files into dataframes
library(readr)

# Varies filepaths depending on if using server or not and what organisation uses it.
if (exists("organisation") == TRUE) { #Health Scotland
  if (organisation == "HS") { 
    pop_lookup <- "X:/ScotPHO Profiles/Data/Lookups/Population/"
    geo_lookup <- "X:/ScotPHO Profiles/Data/Lookups/Geography/"
  }
} else  { #ISD, first server then desktop
  if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
    pop_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"
    geo_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/"
  } else {
    pop_lookup <- "//stats/ScotPHO/Profiles/Data/Lookups/Population/"
    geo_lookup <- "//stats/ScotPHO/Profiles/Data/Lookups/Geography/"
  }
}

# Setting file permissions to anyone to allow writing/overwriting of project files
Sys.umask("006")

###############################################.
## Part 1 - Functions ----
###############################################.
#Function to create age groups needed for standardization
create_agegroups <- function(df) {
  df <- df %>% mutate(age_grp = case_when( 
    age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
    age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
    age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
    age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
    age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
    age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
    TRUE ~ as.numeric(age)
  ))
}

###############################################.
# This function extract the data for each geographical level from the NHS data platform
# and then formats it in the way is neeeded. It uses two parameters, one for the 
# resource-id used in the plaform and one for the name of the geography variable for that data
extract_open_data <- function(resource_id, geography) {
  
  # URL used for data extraction
  open_data_url <- "https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id="
  
  # After finding out the length of the file we can extract the whole thing
  # Limit is set to 1 million, check nothing is bigger than that
  data_extraction <-  GET(paste0(open_data_url, resource_id, "&limit=1000000"))
  # Checking if request has worked
  print(paste0("A value of 200 means the server has received the request:", data_extraction$status_code))
  # It's a JSON file, so transforming into something more usable in R
  data_extraction <- rawToChar(data_extraction$content) %>% fromJSON()
  data_extraction <- data_extraction$result$records %>% #extracting data  
    setNames(tolower(names(.)))   #variables to lower case
  
  # Formatting the data in the way needed
  data_extraction <- data_extraction %>%
    filter(year > 2001 & substr({{geography}}, 1, 3) != "S92" & #years and no Scotland
             sex %in% c("Male", "Female", "f", "m")) %>% #dropping "all" rows
    mutate(sex = recode(sex, "Male" = 1, "Female" = 2, "f" = 2, "m" = 1)) %>%
    select(year, sex, {{geography}}, age0:age90plus) %>%
    gather(age, pop, -c(year, sex, {{geography}})) %>% #wide to long format
    mutate(age = as.numeric(gsub("age|plus", "", age))) %>% #recoding age variable
    rename(code = {{geography}}, sex_grp = sex)
  
}

###############################################.
# This functions takes a basefile (DZ01 or DZ11), selects the age of interest and
# produces files for % and crude rates and for standard rates. It also allows you
# to create files only with council and higher geographies.
# Lower and upper are the age limits, name is the name of the output file,
# dx is what datazone you are using, council if you want council files and
# stdrate if you want pops used  for standarized rates
create_pop <- function(lower, upper, name, dz) {
  
  ###############################################.
  # Creating files for percentages crude rates 
  #Reading file, aggregating and saving file for % and crude rates
  data_dz <- readRDS(file=paste0(pop_lookup, "basefile_", dz, ".rds")) %>% 
    subset(age >= lower & age <= upper) %>% #selecting age of interest
    group_by(year, code) %>% summarise(denominator=sum(denominator)) %>% ungroup()
  
  saveRDS(data_dz, file=paste0(pop_lookup, dz, '_', name,'.rds'))
  
   # Creating file for indicators that need council as base
    data_ca <- data_dz %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12', "S11", "S37"))
    
    saveRDS(data_ca, file=paste0(pop_lookup, 'CA_', name,'.rds'))
  
  ###############################################.
  # Creating files for standardized rates
    data_dz_sr <- readRDS(file=paste0(pop_lookup, "basefile_", dz, ".rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, sex_grp, age_grp) %>% #aggregating
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_dz_sr, file=paste0(pop_lookup, dz, '_', name,'_SR.rds'))
    
    # Creating file for indicators that need council as base
    data_ca_sr <- data_dz_sr %>% subset(substr(code,1,3) %in%  c('S00', 'S08', 'S12', "S11", "S37"))
      
      saveRDS(data_ca_sr, file=paste0(pop_lookup, 'CA_', name,'_SR.rds'))
  
  ###############################################.
  # Creating files for deprivation cases
    data_depr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, quintile, quint_type) %>% 
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_depr, file=paste0(pop_lookup, 'depr_', name,'.rds'))
    
    # Creating files for standardized rates
      data_depr_sr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
        subset(age >= lower & age <= upper) %>% #selecting age of interest
        group_by(year, code, sex_grp, age_grp, quintile, quint_type) %>% 
        summarise(denominator=sum(denominator)) %>% ungroup()
      
      saveRDS(data_depr_sr, file=paste0(pop_lookup, 'depr_', name,'_SR.rds'))
      
}

###############################################.
#This function groups the data for the variables selected and then aggregates it
#It works for the different types of quintiles and for all measures
create_quintile_data <- function(group_vars, geo, quint) {
  
  depr_pop_base %>% group_by_at(c("age_grp", "sex_grp", "age", "year", geo, quint)) %>% 
    summarise(denominator = sum(denominator, na.rm =T)) %>% 
    rename_(code = geo, quintile = quint) %>% ungroup() %>% 
    mutate(quint_type = quint)
}

###############################################.
## Part 2 - Basefiles based on DZ01 and DZ11 ----
###############################################.
# They are used for custom geography requests and for creating the rest of lookups
###############################################.
# Datazone 2001.
# Resource ids can be accessed in the page for the datazone 2001 pop here:
# https://www.opendata.nhs.scot/dataset/population-estimates
dz01_base <- extract_open_data("bf086aee-130d-4487-b854-808db0e29dc4", datazone) %>% 
  create_agegroups()  %>% rename(denominator = pop, datazone2001 =code)

saveRDS(dz01_base, file=paste0(pop_lookup, "DZ01_pop_basefile.rds"))

rm(dz01_base) #freeing up memory
# dz01_base <- readRDS(paste0(pop_lookup, "DZ01_pop_basefile.rds"))

###############################################.
# Datazone 2011.
# # Resource ids can be accessed in the page for the datazone 2011 pop here:
# https://www.opendata.nhs.scot/dataset/population-estimates
# This is slower and fails more than the second method
dz11_base <- extract_open_data("c505f490-c201-44bd-abd1-1bd7a64285ee", datazone) %>% 
  create_agegroups()  %>% rename(denominator = pop, datazone2011 =code)
# If the previous one times out/fails try this, but the link might change every year
dz11_base <- read_csv("https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/c505f490-c201-44bd-abd1-1bd7a64285ee/download/dz2011-pop-est_07092021.csv") %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  filter(year > 2001 & substr(datazone, 1, 3) != "S92" & sex != "All") %>% #years and no Scotland
  mutate(sex = recode(sex, "Male" = 1, "Female" = 2, "f" = 2, "m" = 1)) %>%
  select(year, sex, datazone, age0:age90plus) %>%
  gather(age, pop, -c(year, sex, datazone)) %>% #wide to long format
  mutate(age = as.numeric(gsub("age|plus", "", age))) %>% #recoding age variable
  create_agegroups()  %>%
  rename(sex_grp = sex, denominator = pop, datazone2011 = datazone)

saveRDS(dz11_base, file=paste0(pop_lookup, "DZ11_pop_basefile.rds"))
dz11_base <- readRDS(paste0(pop_lookup, "DZ11_pop_basefile.rds"))

###############################################.
# Intermediate zones, councils, health boards and HSC partnerships
# Resource ids can be accessed in the page for the estimate pops here:
# https://www.opendata.nhs.scot/dataset/population-estimates
hscp_pop <- extract_open_data("c3a393ce-253b-4c75-82dc-06b1bb5638a3", hscp) 
hb_pop <- extract_open_data("27a72cc8-d6d8-430c-8b4f-3109a9ceadb1", hb) 
ca_pop <- extract_open_data("09ebfefb-33f4-4f6a-8312-2d14e2b02ace", ca) 
iz11_pop <- extract_open_data("93df4c88-f74b-4630-abd8-459a19b12f47", intzone) 
iz01_pop <- extract_open_data("0bb11b73-27ad-45ed-9a35-df688d69b12b", intzone)

###############################################.
#Scotland population
scot_pop <- hb_pop %>% group_by(year, sex_grp, age) %>% 
  summarise(pop = sum(pop)) %>% ungroup() %>% mutate(code = "S00000001")

###############################################.
#HSC locality population
loc_lookup <- readRDS(paste0(geo_lookup, "DataZone11_All_Geographies_Lookup.rds")) %>% 
  select(datazone2011, hscp_locality) %>% rename(code = hscp_locality) 

#Merging with locality lookup
local_pop <- left_join(dz11_base, loc_lookup, c("datazone2011")) %>% 
  select(-datazone2011, age_grp) %>% group_by(year, sex_grp, age, code) %>% 
  summarise(pop = sum(denominator)) %>% ungroup()

###############################################.
# ADP population
adp_pop <- ca_pop %>% 
  mutate(code =  #recoding from LA to ADP
           recode(code, 'S12000005' = 'S11000005', 'S12000006' = 'S11000006',  
                  'S12000008' = 'S11000008', 'S12000010' = 'S11000051' ,  'S12000011' ='S11000011' , 
                  'S12000013'= 'S11000032' , 'S12000014' = 'S11000013', 'S12000047' = 'S11000014', 
                  'S12000017'= 'S11000016',  'S12000018'= 'S11000017',  'S12000019'= 'S11000051' , 
                  'S12000020' = 'S11000019', 'S12000021'=  'S11000020',  'S12000023'=  'S11000022',  
                  'S12000048' = 'S11000023' , 'S12000026'= 'S11000025' ,  'S12000027' = 'S11000026', 
                  'S12000028'=  'S11000027',  'S12000029' =  'S11000052', 'S12000030' =  'S11000029', 
                  'S12000033'=  'S11000001',  'S12000034'= 'S11000002',  'S12000035' =  'S11000004',  
                  'S12000036' =  'S11000012', 'S12000038'=  'S11000024',  'S12000039'=  'S11000030' ,  
                  'S12000040' = 'S11000031', 'S12000041'=  'S11000003',  'S12000042'= 'S11000007',  
                  'S12000050' =  'S11000052' , 'S12000045' = 'S11000009', 'S12000049'= 'S11000015')) %>% 
  group_by(year, sex_grp, age, code) %>% 
  summarise(pop = sum(pop)) %>% ungroup()

###############################################.
#merging all geographical levels
all_pop01 <- rbind(iz01_pop, ca_pop, adp_pop, hb_pop, hscp_pop, scot_pop, local_pop) %>% 
  rename(denominator=pop) %>% create_agegroups() %>% #recoding age
  mutate(year = as.numeric(year))

saveRDS(all_pop01, file=paste0(pop_lookup, "basefile_DZ01.rds"))

all_pop11 <- rbind(iz11_pop, ca_pop, adp_pop, hb_pop, hscp_pop, scot_pop, local_pop) %>% 
  rename(denominator=pop) %>% create_agegroups() %>%  #recoding age
  mutate(year = as.numeric(year))

saveRDS(all_pop11, file=paste0(pop_lookup, "basefile_DZ11.rds"))

###############################################.
## Part 3 - Population by deprivation quintile basefile ----
###############################################.
#This is better to be run in R server.
dz01_base <- readRDS(paste0(pop_lookup, "DZ01_pop_basefile.rds")) %>% 
  filter(year<2014) %>% # 2014 uses simd2016 based on dz2011
  rename(datazone = datazone2001)

dz11_base <- readRDS(paste0(pop_lookup, "DZ11_pop_basefile.rds")) %>% 
  subset(year>2013) %>% # 2014 onwards uses simd based on dz2011
  rename(datazone = datazone2011)

depr_pop_base <- rbind(dz01_base, dz11_base)
rm(dz01_base, dz11_base)

depr_lookup <- readRDS(paste0(geo_lookup, 'deprivation_geography.rds')) %>% 
  mutate(scotland="S00000001")

depr_pop_base <- left_join(depr_pop_base, depr_lookup, by = c("datazone", "year"))

depr_pop_base <- rbind( 
  create_quintile_data(geo = "scotland", quint = "sc_quin"),   #Scotland 
  #Health boards using national quintiles
  create_quintile_data(geo = "hb", quint = "sc_quin"),
  #Health boards using health board quintiles
  create_quintile_data(geo = "hb", quint = "hb_quin"),
  #Council area using national quintiles
  create_quintile_data(geo = "ca", quint = "sc_quin"),
  #Council area using concil quintiles
  create_quintile_data(geo = "ca", quint = "ca_quin"))

depr_totals <- depr_pop_base %>% group_by(year, sex_grp, age_grp, age, code, quint_type) %>% 
  summarise(denominator = sum(denominator, na.rm=T)) %>% ungroup() %>% 
  mutate(quintile = "Total")

depr_pop_base <- rbind(depr_pop_base, depr_totals)

saveRDS(depr_pop_base, paste0(pop_lookup, "basefile_deprivation.rds"))
rm(depr_pop_base)

###############################################.
## Part 4 - Create population files  ----
###############################################.
# DZ11, LA and deprivation
create_pop(dz = "DZ11", lower = 0, upper = 200, name = "pop_allages")
create_pop(dz = "DZ11", lower = 60, upper = 200, name = "pop_60+")
create_pop(dz = "DZ11", lower = 16, upper = 200, name = "pop_16+")
create_pop(dz = "DZ11", lower = 18, upper = 200, name = "pop_18+")
create_pop(dz = "DZ11", lower = 65, upper = 200, name = "pop_65+")
create_pop(dz = "DZ11", lower = 75, upper = 200, name = "pop_75+")
create_pop(dz = "DZ11", lower = 85, upper = 200, name = "pop_85+")
create_pop(dz = "DZ11", lower = 12, upper = 200, name = "pop_12+")
create_pop(dz = "DZ11", lower = 0, upper = 4, name = "pop_under5")
create_pop(dz = "DZ11", lower = 0, upper = 17, name = "pop_under18")
create_pop(dz = "DZ11", lower = 0, upper = 18, name = "pop_under19")
create_pop(dz = "DZ11", lower = 0, upper = 15, name = "pop_under16")
create_pop(dz = "DZ11", lower = 0, upper = 25, name = "pop_under26")
create_pop(dz = "DZ11", lower = 0, upper = 74, name = "pop_under75")
create_pop(dz = "DZ11", lower = 0, upper = 0, name = "pop_under1")
create_pop(dz = "DZ11", lower = 5, upper = 5, name = "pop_5")
create_pop(dz = "DZ11", lower = 11, upper = 11, name = "pop_11")
create_pop(dz = "DZ11", lower = 8, upper = 15, name = "pop_8to15")
create_pop(dz = "DZ11", lower = 11, upper = 25, name = "pop_11to25")
create_pop(dz = "DZ11", lower = 16, upper = 39, name = "pop_16to39")
create_pop(dz = "DZ11", lower = 16, upper = 64, name = "pop_16to64")
create_pop(dz = "DZ11", lower = 40, upper = 64, name = "pop_40to64")
create_pop(dz = "DZ11", lower = 65, upper = 74, name = "pop_65to74")
create_pop(dz = "DZ11", lower = 1, upper = 4, name = "pop_1to4")
create_pop(dz = "DZ11", lower = 1, upper = 15, name = "pop_1to15")
create_pop(dz = "DZ11", lower = 5, upper = 15, name = "pop_5to15")
create_pop(dz = "DZ11", lower = 16, upper = 25, name = "pop_16to25")
create_pop(dz = "DZ11", lower = 15, upper = 25, name = "pop_15to25")
create_pop(dz = "DZ11", lower = 15, upper = 44, name = "pop_15to44")

###############################################.
# Working age population
working_pop <- readRDS(file=paste0(pop_lookup, "basefile_DZ11.rds")) %>% 
  subset(age > 15 & age < 65 & sex_grp ==1 | #selecting age of interest
         sex_grp==2 & age>15 & age<65 & year>2009 | 
           sex_grp==2 & age>15 & age<60 & year<2010) %>% 
  group_by(year, code) %>% summarise(denominator=sum(denominator)) %>% ungroup()

saveRDS(working_pop, file=paste0(pop_lookup, 'DZ11_working_pop.rds'))

# For CA
working_pop <- working_pop %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12', 'S37'))
saveRDS(working_pop, file=paste0(pop_lookup, 'CA_working_pop.rds'))

#For deprivation cases
working_pop_depr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
  subset(age > 15 & age < 65 & sex_grp ==1 | #selecting age of interest
           sex_grp==2 & age>15 & age<65 & year>2009 | 
           sex_grp==2 & age>15 & age<60 & year<2010) %>% 
  group_by(year, code, quintile, quint_type) %>% 
  summarise(denominator=sum(denominator)) %>% ungroup()

saveRDS(working_pop_depr, file=paste0(pop_lookup, 'depr_working_pop.rds'))

###############################################.
# Teenage pregnancy  
teenpreg_pop <- readRDS(file=paste0(pop_lookup, "basefile_DZ11.rds")) %>% 
  subset(age > 14 & age < 20 & sex_grp ==2) %>%  #selecting age of interest
  group_by(year, code) %>% summarise(denominator=sum(denominator)) %>% ungroup()

saveRDS(teenpreg_pop, file=paste0(pop_lookup, 'DZ11_pop_fem15to19.rds'))

teenpreg_pop <- teenpreg_pop %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12', 'S37'))
saveRDS(teenpreg_pop, file=paste0(pop_lookup, 'CA_pop_fem15to19.rds'))

#For deprivation cases
teenpreg_pop_depr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
  subset(age > 14 & age < 20 & sex_grp ==2) %>%  #selecting age of interest
  group_by(year, code, quintile, quint_type) %>% 
  summarise(denominator=sum(denominator)) %>% ungroup()

saveRDS(teenpreg_pop_depr, file=paste0(pop_lookup, 'depr_pop_fem15to19.rds'))

###############################################.
# Live births (used for infant deaths under 1)
live_births <- readxl::read_excel(paste0("/PHI_conf/ScotPHO/Profiles/Data/Received Data/",
                                 "Births 2002-2019 datazone_2011.xlsx")) %>%
  janitor::clean_names() %>% 
  rename(datazone = datazone_2011, year = registration_year) %>% 
  group_by(year, datazone) %>% 
  summarise(denominator = sum(count, na.rm = T)) %>% ungroup
  
live_lookup <- readRDS(paste0(geo_lookup, "DataZone11_All_Geographies_Lookup.rds")) %>% 
  select(datazone2011, ca2019, hb2019, hscp2019) %>% 
  mutate(scot = "S00000001")

live_births <- left_join(live_births, live_lookup, by = c("datazone" = "datazone2011"))

live_births %<>% select(-datazone) %>% 
  pivot_longer(ca2019:scot, values_to = "code") %>% select(-name) 

live_births %<>% group_by(code, year) %>% 
  summarise(denominator = sum(denominator, na.rm = T)) %>%  ungroup

saveRDS(live_births, file=paste0(pop_lookup, 'live_births.rds'))

##END
