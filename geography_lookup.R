##This script creates  geography lookup files, one for SIMD-deprivation and one
#that contains all datazone 2011 and which HSCP Locality they map to.
##This is a manual process carried out by ScotPHO until H&SCP localities have standard 
# geographic codes and are added to ISD GPD lookup files.
##The source of the locality lookup was Ana Rodriguez from ISD LIST team.

#Part 1 - HSC locality lookup.
#Part 2 - Parent geography for IZ and locality
#Part 3 - Create dictionary  to have the names and not codes.
#Part 4 -Deprivation (SIMD) geographies
###############################################.
## Packages and filepaths ----
###############################################.
lapply(c("dplyr", "readr", "foreign"), library, character.only = TRUE)

geo_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/"
cl_out_geo <- "/conf/linkage/output/lookups/geography/"
cl_out_depr <- "/conf/linkage/output/lookups/Unicode/Deprivation/"
  
###############################################.
## Functions ----
###############################################.
#Function to create deprivation rank data set for different years 
#and different simd versions
create_simd <- function(file_name, simd_version, year) {
  
  #Creating quintile variables to keep
  simd_sc <- paste0("simd", simd_version, "_sc_quintile")
  simd_hb <- paste0("simd", simd_version, "_hb2014_quintile")
  simd_ca <-   if (file_name == "DataZone2001") {
    paste0("simd", simd_version, "_ca_quintile")
  } else if (file_name == "DataZone2011") {
    paste0("simd", simd_version, "_ca2011_quintile")
  }
  
  if (file_name == "DataZone2001") {
    data_simd <- read.spss(paste0(cl_out_depr, 'DataZone2001_all_simd.sav'),
                      to.data.frame=TRUE, use.value.labels=FALSE) 
  } else if (file_name == "DataZone2011") {
    data_simd <- read.spss(paste0(cl_out_depr, 'DataZone2011_simd2016.sav'),
                      to.data.frame=TRUE, use.value.labels=FALSE) 
  }
  
  data_simd <- data_simd %>% setNames(tolower(names(.))) %>%    #variables to lower case
    select(eval(tolower(file_name)), hb2014, ca2011, eval(simd_sc), eval(simd_hb), eval(simd_ca)) %>% 
    rename_(datazone = tolower(file_name), sc_quin = eval(simd_sc), 
            hb_quin = eval(simd_hb), ca_quin = eval(simd_ca)) %>% 
    mutate(year = year) 
  
  # recode simd 2004 and 2006, as they follow an inverse scale.
  if (simd_version %in% c(2004, 2006)) {
    data_simd <- data_simd %>%
      mutate_at(vars(sc_quin, ca_quin, hb_quin),
                funs(recode(., "1" = 5, "2" = 4, "4" = 2, "5" = 1)))
  } else {
    data_simd <- data_simd
  }
}

###############################################.
## Part 1 - HSC locality lookup ----
###############################################.
hscp_loc <- read.spss(paste0(geo_lookup, "HSCP Localities_DZ11_Lookup_20180903.sav"), 
          to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  rename(datazone2011 = datazone) %>% 
  select(datazone2011, locality, partnership) %>% arrange(partnership, locality) 

##Create artificial standard 9 digit code to identify unique localityfor use 
#in matching files to generate indicator data.
##ScotPHO programs are set up to expect a field called "code" which contains the 
#standard geography code for an area but these technically haven't been created for HSCPs yet.
##Beware some localities might have common names to those used by other HSCPs 
#(e.g. 'East'/'West' are commonly used as locality names by more than one partnership ).
loc_seq <- seq_len(length(unique(hscp_loc$locality)))
loc_zeros <- ifelse(nchar(loc_seq) == 1, "00000",
                    ifelse(nchar(loc_seq) == 2, "0000",
                           ifelse(nchar(loc_seq) == 3, "000", "ERROR")))
loc_code <- paste0("S99", loc_zeros, loc_seq)

loc_code <- data.frame(hscp_locality = loc_code, locality = unique(hscp_loc$locality))

hscp_loc <- left_join(x = hscp_loc, y = loc_code, by = c("locality"))

saveRDS(hscp_loc, paste0(geo_lookup, 'DataZone11_HSCLocality_Lookup.rds'))

###############################################.
# Joining with rest of geographies
dz11_lookup <- read.spss(paste0(cl_out_geo, "DataZone2011/DataZone2011.sav"), 
                  to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  rename(hscp_partnership = hscp2016) %>% 
  select(datazone2011, intzone2011, hb2014, ca2011, hscp_partnership)  

dz11_lookup <- left_join(dz11_lookup, hscp_loc, c("datazone2011")) %>% 
  select(-locality, -partnership)

saveRDS(dz11_lookup, paste0(geo_lookup, 'DataZone11_All_Geographies_Lookup.rds'))

###############################################.
## Part 2 - Parent geography for IZ and locality ----
###############################################.
dz11_lookup <- readRDS(paste0(geo_lookup, 'DataZone11_All_Geographies_Lookup.rds'))
parent_lookup <- dz11_lookup %>% 
  select(intzone2011, hscp_locality, hscp_partnership) %>% unique

saveRDS(parent_lookup, paste0(geo_lookup, 'IZtoPartnership_parent_lookup.rds'))

###############################################.
## Part 3 - Create dictionary  to have the names and not codes ----
###############################################.
# Create HSC locality dictionary.
local_dictio <- hscp_loc %>% rename(areaname = locality, code = hscp_locality) %>%
  select(areaname, code) %>% unique
saveRDS(local_dictio, paste0(geo_lookup, 'HSClocalitydictionary.rds'))

# Create ADP dictionary.
adp_dictio <- read.spss("/conf/phip/Projects/Profiles/Data/Scotland Localities/Geo Data for Shiny.sav",
                        to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  rename(areaname = geography_name, code = geography_code) %>%
  subset(substr(code, 1,3) == "S11") %>% 
  mutate(areaname = gsub(" ADP", "", areaname))

saveRDS(adp_dictio, paste0(geo_lookup, 'ADPdictionary.rds'))

# Create IZ dictionary.
iz_dictio <- read_csv(paste0(cl_out_geo, "/Codes_and_Names/Intermediate Zone 2011 Lookup.csv")) %>% 
  rename(areaname = IntermediateZone2011Name, code = IntermediateZone2011Code) 
  
saveRDS(iz_dictio, paste0(geo_lookup, 'IZ11dictionary.rds'))

# Create CA dictionary.
ca_dictio <- read_csv(paste0(cl_out_geo, "/Codes_and_Names/Council Area 2011 Lookup.csv")) %>% 
  rename(areaname = CouncilArea2011Name, code = CouncilArea2011Code) 
  
saveRDS(ca_dictio, paste0(geo_lookup, 'CAdictionary.rds'))

# create HB dictionary.
hb_dictio <- read_csv(paste0(cl_out_geo, "/Codes_and_Names/Health Board Area 2014 Lookup.csv")) %>% 
  rename(areaname = NRSHealthBoardAreaName , code = HealthBoardArea2014Code ) %>% 
  select(-HealthBoardArea2014Name)

saveRDS(hb_dictio, paste0(geo_lookup, 'HBdictionary.rds'))

# create HSC partnership dictionary.
part_dictio <- read_csv(paste0(cl_out_geo, "/Codes_and_Names/Integration Authority 2016 Lookup.csv")) %>% 
  rename(areaname = IntegrationAuthority2016Name , code = IntegrationAuthority2016Code ) 

saveRDS(part_dictio, paste0(geo_lookup, 'HSCPdictionary.rds'))

# create scotland dictionary.
scot_dictio <- data.frame(code = 'S00000001', areaname = "Scotland")
saveRDS(scot_dictio, paste0(geo_lookup, 'Scotlandictionary.rds'))

# Merge files together. 
code_dictio <- rbind(scot_dictio, hb_dictio, ca_dictio, adp_dictio, part_dictio, 
                     local_dictio, iz_dictio)
saveRDS(code_dictio, paste0(geo_lookup, 'codedictionary.rds'))

###############################################.
## Part 4 -Deprivation (SIMD) geographies ----
###############################################.
# Creating a data frame with all ranks for each year
data_simd <- as.data.frame(rbind(
  #simd2004
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 1996),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 1997),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 1998),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 1999),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 2000),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 2001),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 2002),
  create_simd(file_name = "DataZone2001", simd_version =2004, year = 2003),
  #simd2006
  create_simd(file_name = "DataZone2001", simd_version =2006, year = 2004),
  create_simd(file_name = "DataZone2001", simd_version =2006, year = 2005),
  create_simd(file_name = "DataZone2001", simd_version =2006, year = 2006),
  #simd2009
  create_simd(file_name = "DataZone2001", simd_version ="2009v2", year = 2007),
  create_simd(file_name = "DataZone2001", simd_version ="2009v2", year = 2008),
  create_simd(file_name = "DataZone2001", simd_version ="2009v2", year = 2009),
  #simd2012
  create_simd(file_name = "DataZone2001", simd_version =2012, year = 2010),
  create_simd(file_name = "DataZone2001", simd_version =2012, year = 2011),
  create_simd(file_name = "DataZone2001", simd_version =2012, year = 2012),
  create_simd(file_name = "DataZone2001", simd_version =2012, year = 2013),
  #simd2016
  create_simd(file_name = "DataZone2011", simd_version =2016, year = 2014),
  create_simd(file_name = "DataZone2011", simd_version =2016, year = 2015),
  create_simd(file_name = "DataZone2011", simd_version =2016, year = 2016),
  create_simd(file_name = "DataZone2011", simd_version =2016, year = 2017)))

saveRDS(data_simd, paste0(geo_lookup, 'deprivation_geography.rds'))

##END
