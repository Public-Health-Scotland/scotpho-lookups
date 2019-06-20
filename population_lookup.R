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
library(foreign) 
library(reshape2)

# filepaths vary depending on if using server or desktop
if (sessionInfo()$platform == "x86_64-redhat-linux-gnu (64-bit)") {
  pop_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"
  geo_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/"
  cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  
} else {
  pop_lookup <- "//stats/ScotPHO/Profiles/Data/Lookups/Population/"
  geo_lookup <- "//stats/ScotPHO/Profiles/Data/Lookups/Geography/"
  cl_out_pop <- "//stats/linkage/output/lookups/Unicode/Populations/Estimates/"
}

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
# This functions takes a basefile (DZ01 or DZ11), selects the age of interest and
# produces files for % and crude rates and for standard rates. It also allows you
# to create files only with council and higher geographies.
# Lower and upper are the age limits, name is the name of the output file,
# dx is what datazone you are using, council if you want council files and
# stdrate if you want pops used  for standarized rates
create_pop <- function(lower, upper, name, dz, council = F, adp = F, stdrate = F,
                       deprivation = F) {
  
  ###############################################.
  # Creating files for percentages crude rates 
  #Reading file, aggregating and saving file for % and crude rates
  data_dz <- readRDS(file=paste0(pop_lookup, "basefile_", dz, ".rds")) %>% 
    subset(age >= lower & age <= upper) %>% #selecting age of interest
    group_by(year, code) %>% summarise(denominator=sum(denominator)) %>% ungroup()
  
  saveRDS(data_dz, file=paste0(pop_lookup, dz, '_', name,'.rds'))
  
  if (council == TRUE) { # select only local authorites and above.
    data_ca <- data_dz %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12'))
    
    saveRDS(data_ca, file=paste0(pop_lookup, 'CA_', name,'.rds'))
  }
  
  if (adp == TRUE) { # select only adp, ca and above.
    data_adp <- data_dz %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12', 'S11'))
    
    saveRDS(data_adp, file=paste0(pop_lookup, 'ADP_', name,'.rds'))
  }
  
  ###############################################.
  # Creating files for standardized rates
  if (stdrate == TRUE) { # creating files for standard rates
    data_dz_sr <- readRDS(file=paste0(pop_lookup, "basefile_", dz, ".rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, sex_grp, age_grp) %>% #aggregating
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_dz_sr, file=paste0(pop_lookup, dz, '_', name,'_SR.rds'))
    
    if (council == TRUE) { # select only local authorites and above. 
      data_ca_sr <- data_dz_sr %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12'))
      
      saveRDS(data_ca_sr, file=paste0(pop_lookup, 'CA_', name,'_SR.rds'))
    }
    
    if (adp == TRUE) { # select only adp, ca and above.
      data_adp_sr <- data_dz_sr %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12', 'S11'))
      
      saveRDS(data_adp_sr, file=paste0(pop_lookup, 'ADP_', name,'_SR.rds'))
    }
  }
  
  ###############################################.
  # Creating files for deprivation cases
  if (deprivation == TRUE) {
    
    data_depr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, quintile, quint_type) %>% 
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_depr, file=paste0(pop_lookup, 'depr_', name,'.rds'))
    
    if (stdrate == TRUE) {
      
      data_depr_sr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
        subset(age >= lower & age <= upper) %>% #selecting age of interest
        group_by(year, code, sex_grp, age_grp, quintile, quint_type) %>% 
        summarise(denominator=sum(denominator)) %>% ungroup()
      
      saveRDS(data_depr_sr, file=paste0(pop_lookup, 'depr_', name,'_SR.rds'))
      
    }
  }
}

###############################################.
## Part 2 - Basefiles based on DZ01 and DZ11 ----
###############################################.
# They are used for custom geography requests and for creating the rest of lookups
###############################################.
# Datazone 2001.
dz01_base <- read.spss(paste0(cl_out_pop, "DataZone2001_pop_est_2001_2014.sav"),
                       to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  select(-c(total_pop)) %>% 
  rename(code = intzone2001, sex_grp = sex) %>% 
  subset(year>2001) %>%  #select only 2002+
  mutate(sex_grp = recode(sex_grp, "M" = "1", 'F' = "2")) #Recoding sex

dz01_base <- dz01_base[1:95] #selecting variables of interest

#Converting from wide to long format
dz01_base <- dz01_base %>% melt(id.vars = c("year", "datazone2001", "sex_grp", "code"),
  variable.name = "age", value.name = "pop") %>% 
  #Converting age in numeric and recoding age
  mutate(age = as.numeric(gsub("age|plus", "", age))) %>% 
  create_agegroups()

iz01 <- dz01_base #iz basefile

dz01_base <- dz01_base %>% select(-c(code)) %>% rename(denominator = pop)
saveRDS(dz01_base, file=paste0(pop_lookup, "DZ01_pop_basefile.rds"))
rm(dz01_base) #freeing up memory
# dz01_base <- readRDS(paste0(pop_lookup, "DZ01_pop_basefile.rds"))

###############################################.
#Creating basefile for IZ2001
iz01 <- iz01 %>% select(-datazone2001, -age_grp) %>% 
  group_by(year, sex_grp, age, code) %>% #aggregating
  summarise(pop = sum(pop)) %>% ungroup()

###############################################.
# Datazone 2011.
dz11_base <- readRDS(paste0(cl_out_pop, "DataZone2011_pop_est_2011_2017.rds")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  select(-c(total_pop)) %>% 
  rename(code = intzone2011, sex_grp = sex) %>% 
  mutate(sex_grp = recode(sex_grp, "M" = "1", 'F' = "2")) #Recoding sex

dz11_base <- dz11_base[1:95] #selecting variables of interest


#Converting from wide to long format
dz11_base <- dz11_base %>% melt(id.vars = c("year", "datazone2011", "sex_grp", "code"),
                                variable.name = "age", value.name = "pop") %>% 
  #Converting age in numeric and recoding age
  mutate(age = as.numeric(gsub("age|plus", "", age))) %>% 
  create_agegroups()

iz11 <- dz11_base #iz basefile

dz11_base <- dz11_base %>% select(-c(code)) %>% rename(denominator = pop)
saveRDS(dz11_base, file=paste0(pop_lookup, "DZ11_pop_basefile.rds"))
dz11_base <- readRDS(paste0(pop_lookup, "DZ11_pop_basefile.rds"))

###############################################.
#Creating basefile for IZ2011
iz11 <- iz11 %>% select(-datazone2011, -age_grp) %>% 
  group_by(year, sex_grp, age, code) %>% 
  summarise(pop = sum(pop)) %>% ungroup()

###############################################.
#HSC partnership population
hscp <- readRDS(paste0(cl_out_pop, "HSCP2019_pop_est_1981_2018.rds")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>2001) %>%  #select only 2002+
  rename(code = hscp2016, sex_grp = sex) %>% 
  select(code, age, sex_grp, year, pop)
  
###############################################.
#HSC locality population
loc_lookup <- readRDS(paste0(geo_lookup, "DataZone11_All_Geographies_Lookup.rds")) %>% 
  select(datazone2011, hscp_locality) %>% rename(code = hscp_locality) 

#Merging with locality lookup
locality <- left_join(dz11_base, loc_lookup, c("datazone2011")) %>% 
  select(-datazone2011, age_grp) %>% group_by(year, sex_grp, age, code) %>% 
  summarise(pop = sum(denominator)) %>% ungroup()

###############################################.
#Council area population
ca <- readRDS(paste0(cl_out_pop, "CA2019_pop_est_1981_2018.rds")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>2001) %>%  #select only 2002+
  rename(code = ca2011, sex_grp = sex) %>% 
  select(code, age, sex_grp, year, pop)

###############################################.
# ADP population
adp <- ca %>% 
  mutate(code =  #recoding from LA to ADP
           recode(code, 'S12000005' = 'S11000005', 'S12000006' = 'S11000006',  
                  'S12000008' = 'S11000008', 'S12000010' = 'S11000051' ,  'S12000011' ='S11000011' , 
                  'S12000013'= 'S11000032' , 'S12000014' = 'S11000013', 'S12000015' = 'S11000014', 
                  'S12000017'= 'S11000016',  'S12000018'= 'S11000017',  'S12000019'= 'S11000051' , 
                  'S12000020' = 'S11000019', 'S12000021'=  'S11000020',  'S12000023'=  'S11000022',  
                  'S12000024' = 'S11000023' , 'S12000026'= 'S11000025' ,  'S12000027' = 'S11000026', 
                  'S12000028'=  'S11000027',  'S12000029' =  'S11000052', 'S12000030' =  'S11000029', 
                  'S12000033'=  'S11000001',  'S12000034'= 'S11000002',  'S12000035' =  'S11000004',  
                  'S12000036' =  'S11000012', 'S12000038'=  'S11000024',  'S12000039'=  'S11000030' ,  
                  'S12000040' = 'S11000031', 'S12000041'=  'S11000003',  'S12000042'= 'S11000007',  
                  'S12000044' =  'S11000052' , 'S12000045' = 'S11000009', 'S12000046'= 'S11000015')) %>% 
  group_by(year, sex_grp, age, code) %>% 
  summarise(pop = sum(pop)) %>% ungroup()

###############################################.
#Health board population
hb <- readRDS(paste0(cl_out_pop, "HB2019_pop_est_1981_2018.rds")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>2001) %>%  #select only 2002+
  rename(code = hb2014, sex_grp = sex) %>% 
  select(code, age, sex_grp, year, pop)

###############################################.
#Scotland population
scotland <- hb %>% group_by(year, sex_grp, age) %>% 
  summarise(pop = sum(pop)) %>% ungroup() %>% mutate(code = "S00000001")

###############################################.
#merging all geographical levels
all_pop01 <- rbind(iz01, ca, adp, hb, hscp, scotland, locality) %>% 
  rename(denominator=pop) %>% create_agegroups() %>% #recoding age
  mutate(year = as.numeric(year))

saveRDS(all_pop01, file=paste0(pop_lookup, "basefile_DZ01.rds"))

all_pop11 <- rbind(iz11, ca, adp, hb, hscp, scotland, locality) %>% 
  rename(denominator=pop) %>% create_agegroups() %>%  #recoding age
  mutate(year = as.numeric(year))

all_pop11 <- all_pop11 %>% filter(year<2018) # TEMPORARY UNTIL DZ pop for 2018 ready

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

#This function groups the data for the variables selected and then aggregates it
#It works for the different types of quintiles and for all measures
create_quintile_data <- function(group_vars, geo, quint) {

  depr_pop_base %>% group_by_at(c("age_grp", "sex_grp", "age", "year", geo, quint)) %>% 
      summarise(denominator = sum(denominator, na.rm =T)) %>% 
      rename_(code = geo, quintile = quint) %>% ungroup() %>% 
      mutate(quint_type = quint)
}

depr_pop_base <- rbind( 
  create_quintile_data(geo = "scotland", quint = "sc_quin"),   #Scotland 
  #Health boards using national quintiles
  create_quintile_data(geo = "hb2014", quint = "sc_quin"),
  #Health boards using health board quintiles
  create_quintile_data(geo = "hb2014", quint = "hb_quin"),
  #Council area using national quintiles
  create_quintile_data(geo = "ca2011", quint = "sc_quin"),
  #Council area using concil quintiles
  create_quintile_data(geo = "ca2011", quint = "ca_quin"))

depr_totals <- depr_pop_base %>% group_by(year, sex_grp, age_grp, age, code, quint_type) %>% 
  summarise(denominator = sum(denominator, na.rm=T)) %>% ungroup() %>% 
  mutate(quintile = "Total")

depr_pop_base <- rbind(depr_pop_base, depr_totals)

saveRDS(depr_pop_base, paste0(pop_lookup, "basefile_deprivation.rds"))

###############################################.
## Part 4 - Create population files  ----
###############################################.
# DZ11, LA and deprivation
create_pop(dz = "DZ11", lower = 0, upper = 200, name = "pop_allages",  
           council = T, adp = T, stdrate = T, deprivation = T)
create_pop(dz = "DZ11", lower = 60, upper = 200, name = "pop_60+", council = T)
create_pop(dz = "DZ11", lower = 16, upper = 200, name = "pop_16+", council = T, stdrate = T)
create_pop(dz = "DZ11", lower = 18, upper = 200, name = "pop_18+", 
           council = T, adp = T, stdrate = T)
create_pop(dz = "DZ11", lower = 65, upper = 200, name = "pop_65+", council = T,
           stdrate = T, deprivation = T)
create_pop(dz = "DZ11", lower = 75, upper = 200, name = "pop_75+")
create_pop(dz = "DZ11", lower = 85, upper = 200, name = "pop_85+")
create_pop(dz = "DZ11", lower = 12, upper = 200, name = "pop_12+", council = T)
create_pop(dz = "DZ11", lower = 0, upper = 4, name = "pop_under5", council = T, stdrate = T)
create_pop(dz = "DZ11", lower = 0, upper = 17, name = "pop_under18", council = T, adp = T)
create_pop(dz = "DZ11", lower = 0, upper = 15, name = "pop_under16", council = T, stdrate = T)
create_pop(dz = "DZ11", lower = 0, upper = 25, name = "pop_under26", council = T)
create_pop(dz = "DZ11", lower = 0, upper = 74, name = "pop_under75", council = T, 
           stdrate = T, deprivation = T)
create_pop(dz = "DZ11", lower = 0, upper = 0, name = "pop_under1")
create_pop(dz = "DZ11", lower = 5, upper = 5, name = "pop_5", deprivation = T)
create_pop(dz = "DZ11", lower = 11, upper = 11, name = "pop_11", deprivation = T)
create_pop(dz = "DZ11", lower = 8, upper = 15, name = "pop_8to15", council = T)
create_pop(dz = "DZ11", lower = 11, upper = 25, name = "pop_11to25", council = T, stdrate = T)
create_pop(dz = "DZ11", lower = 16, upper = 39, name = "pop_16to39", council = T)
create_pop(dz = "DZ11", lower = 16, upper = 64, name = "pop_16to64")
create_pop(dz = "DZ11", lower = 40, upper = 64, name = "pop_40to64", council = T)
create_pop(dz = "DZ11", lower = 65, upper = 74, name = "pop_65to74")
create_pop(dz = "DZ11", lower = 1, upper = 4, name = "pop_1to4")
create_pop(dz = "DZ11", lower = 5, upper = 15, name = "pop_5to15")
create_pop(dz = "DZ11", lower = 16, upper = 25, name = "pop_16to25")
create_pop(dz = "DZ11", lower = 15, upper = 25, name = "pop_15to25", council = T, stdrate = T)
create_pop(dz = "DZ11", lower = 15, upper = 44, name = "pop_15to44", council = T, 
           stdrate = T, deprivation = T)

###############################################.
# DZ01
create_pop(dz = "DZ01", lower = 0, upper = 200, name = "pop_allages", stdrate = T)
create_pop(dz = "DZ01", lower = 60, upper = 200, name = "pop_60+")
create_pop(dz = "DZ01", lower = 16, upper = 200, name = "pop_16+", stdrate = T)
create_pop(dz = "DZ01", lower = 18, upper = 200, name = "pop_18+", stdrate = T)
create_pop(dz = "DZ01", lower = 0, upper = 74, name = "pop_under75", stdrate = T)
create_pop(dz = "DZ01", lower = 15, upper = 44, name = "pop_15to44", stdrate = T)

###############################################.
# Working age population
working_pop <- readRDS(file=paste0(pop_lookup, "basefile_DZ11.rds")) %>% 
  subset(age > 15 & age < 65 & sex_grp ==1 | #selecting age of interest
         sex_grp==2 & age>15 & age<65 & year>2009 | 
           sex_grp==2 & age>15 & age<60 & year<2010) %>% 
  group_by(year, code) %>% summarise(denominator=sum(denominator)) %>% ungroup()

saveRDS(working_pop, file=paste0(pop_lookup, 'DZ11_working_pop.rds'))

# For CA
working_pop <- working_pop %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12'))
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

teenpreg_pop <- teenpreg_pop %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12'))
saveRDS(teenpreg_pop, file=paste0(pop_lookup, 'CA_pop_fem15to19.rds'))

#For deprivation cases
teenpreg_pop_depr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
  subset(age > 14 & age < 20 & sex_grp ==2) %>%  #selecting age of interest
  group_by(year, code, quintile, quint_type) %>% 
  summarise(denominator=sum(denominator)) %>% ungroup()

saveRDS(teenpreg_pop_depr, file=paste0(pop_lookup, 'depr_pop_fem15to19.rds'))

##END
