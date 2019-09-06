##This script creates  geography lookup files, one for SIMD-deprivation and one
#that contains all datazone 2011 and which HSCP Locality they map to.
##This is a manual process carried out by ScotPHO until H&SCP localities have standard 
# geographic codes and are added to ISD GPD lookup files.
##The source of the locality lookup was Ana Rodriguez from ISD LIST team.

#Part 1 - HSC locality lookup.
#Part 2 - ADP lookup
#Part 3 - Joining all geographies 
#Part 4 - Parent geography for IZ and locality
#Part 5 - Create dictionary  to have the names and not codes.
#Part 6 - Deprivation (SIMD) geographies
###############################################.
## Packages and filepaths ----
###############################################.
lapply(c("dplyr", "readr"), library, character.only = TRUE)
library(httr) # api connection
library(jsonlite)  # transforming JSON files into dataframes


# filepaths vary depending on if using server or desktop
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  geo_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/"
  cl_out_geo <- "/conf/linkage/output/lookups/Unicode/Geography/"
  cl_out_depr <- "/conf/linkage/output/lookups/Unicode/Deprivation/"
} else  {
  geo_lookup <- "//stats/ScotPHO/Profiles/Data/Lookups/Geography/"
  cl_out_geo <- "//stats/linkage/output/lookups/Unicode/Geography/"
  cl_out_depr <- "//stats/linkage/output/lookups/Unicode/Deprivation/"

}
  
###############################################.
## Functions ----
###############################################.
#Function to create deprivation rank data set for different years 
#and different simd versions
create_simd <- function(file_name, simd_version, year, list_pos) {
  
  #Creating quintile variables to keep
  simd_sc <- paste0("simd", simd_version, "_sc_quintile")
  simd_hb <- paste0("simd", simd_version, "_hb2014_quintile")
  simd_ca <-   if (file_name == "DataZone2001") {
    paste0("simd", simd_version, "_ca_quintile")
  } else if (file_name == "DataZone2011") {
    paste0("simd", simd_version, "_ca2011_quintile")
  }
  
  if (file_name == "DataZone2001") {
    data_simd <- readRDS(paste0(cl_out_depr, 'DataZone2001_all_simd.rds')) 
  } else if (file_name == "DataZone2011") {
    data_simd <- readRDS(paste0(cl_out_depr, 'DataZone2011_simd2016.rds'))
  }
  
  data_simd <- data_simd %>% setNames(tolower(names(.))) %>%    #variables to lower case
    select(eval(tolower(file_name)), hb2019, ca2019, eval(simd_sc), eval(simd_hb), eval(simd_ca)) %>% 
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
  
  data_depr_simd[[list_pos]] <<- data_simd #assigning to list
  
}

###############################################.
# Function to create dictionaries for the different areas
create_dictionary <- function(area_name, area_code, filename) {
  dictio <- data_extraction %>% 
    select({{area_name}}, {{area_code}}) %>% 
    rename(areaname = {{area_name}}, code = {{area_code}}) %>% unique
  
  saveRDS(dictio, paste0(geo_lookup, filename, 'dictionary.rds'))
  
  dictio #retrieving object
}

###############################################.
## Part 1 - HSC locality lookup ----
###############################################.
#  This files comes from /conf/linkage/output/lookups/Unicode/Geography/HSCP Locality"
# Check that this is the latest version available
hscp_loc <- readRDS(paste0(geo_lookup, "HSCP Localities_DZ11_Lookup_20180903.rds")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  select(datazone2011, hscplocality, hscp2019name) %>% 
  arrange(hscplocality, hscp2019name)

##Create artificial standard 9 digit code to identify unique localityfor use 
#in matching files to generate indicator data.
##ScotPHO programs are set up to expect a field called "code" which contains the 
#standard geography code for an area but these technically haven't been created for HSCPs yet.
##Beware some localities might have common names to those used by other HSCPs 
#(e.g. 'East'/'West' are commonly used as locality names by more than one partnership ).
loc_seq <- seq_len(length(unique(hscp_loc$hscplocality)))
loc_zeros <- case_when(nchar(loc_seq) == 1 ~ "00000",
                       nchar(loc_seq) == 2 ~ "0000",
                       nchar(loc_seq) == 3 ~ "000")

loc_code <- paste0("S99", loc_zeros, loc_seq)

loc_code <- data.frame(hscp_locality = loc_code, hscplocality = unique(hscp_loc$hscplocality))

hscp_loc <- left_join(x = hscp_loc, y = loc_code, by = c("hscplocality"))

saveRDS(hscp_loc, paste0(geo_lookup, 'DataZone11_HSCLocality_Lookup.rds'))

###############################################.
## Part 2 - ADP lookup ----
###############################################.
# Creating lookup of ADPs with council area
adp_lookup <- data.frame(
  ca2019 = c("S12000005", "S12000006", "S12000008", 
             "S12000010", "S12000011", "S12000013", "S12000014", "S12000047", 
             "S12000017", "S12000018", "S12000019", "S12000020", "S12000021", 
             "S12000023", "S12000048", "S12000026", "S12000027", "S12000028", 
             "S12000029", "S12000030", "S12000033", "S12000034", "S12000035", 
             "S12000036", "S12000038", "S12000039", "S12000040", "S12000041", 
             "S12000042", "S12000050", "S12000045", "S12000049"),
  adp = c("S11000005", "S11000006", "S11000008", "S11000051", "S11000011", 
          "S11000032", "S11000013", "S11000014", "S11000016", "S11000017", 
          "S11000051", "S11000019", "S11000020", "S11000022", "S11000023", 
          "S11000025", "S11000026", "S11000027", "S11000052", "S11000029", 
          "S11000001", "S11000002", "S11000004", "S11000012", "S11000024", 
          "S11000030", "S11000031", "S11000003", "S11000007", "S11000052", 
          "S11000009", "S11000015"),
  adp_name = c("Clackmannanshire", "Dumfries & Galloway", "East Ayrshire", 
               "Mid and East Lothian", "East Renfrewshire", "Na h-Eileanan Siar", "Falkirk", 
               "Fife", "Highland", "Inverclyde", "Mid and East Lothian", "Moray", "North Ayrshire", 
               "Orkney Islands", "Perth & Kinross", "Scottish Borders", "Shetland Islands", 
               "South Ayrshire", "Lanarkshire", "Stirling", "Aberdeen City", 
               "Aberdeenshire", "Argyll & Bute", "Edinburgh, City of", "Renfrewshire", 
               "West Dunbartonshire", "West Lothian", "Angus", "Dundee City", 
               "Lanarkshire", "East Dunbartonshire", "Glasgow City"))

saveRDS(adp_lookup, paste0(geo_lookup, 'ADP_CA_lookup.rds'))

###############################################.
## Part 3  - Joining all geographies ----
###############################################.
# reading datazone lookup  from open data platform 
# which includes most geographies but adp and locality
# URL used for data extraction
open_data_url <- "https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id="

# Limit is set to 1 million, check nothing is bigger than that
data_extraction <-  GET(paste0(open_data_url, "395476ab-0720-4740-be07-ff4467141352", 
                               "&limit=1000000"))

# It's a JSON file, so transforming into something more usable in R
data_extraction <- rawToChar(data_extraction$content) %>% fromJSON()
data_extraction <- data_extraction$result$records %>% #extracting data  
  setNames(tolower(names(.)))   #variables to lower case

# Creating dz lookup
dz11_lookup <- data_extraction %>% 
  select(dz2011, iz2011, ca2011, hscp2016, hb2014) %>% 
  # the variables contain a year, but they have the latest(2019) codes
  rename(datazone2011 = dz2011, intzone2011 =iz2011, ca2019 = ca2011,
         hscp2019 = hscp2016, hb2019 = hb2014)

# merging localities
dz11_lookup <- left_join(dz11_lookup, hscp_loc, c("datazone2011")) %>% 
  select(-hscplocality, -hscp2019name)

# merging adps
dz11_lookup <- left_join(dz11_lookup, adp_lookup, c("ca2019")) %>% 
  select(-adp_name)

saveRDS(dz11_lookup, paste0(geo_lookup, 'DataZone11_All_Geographies_Lookup.rds'))

###############################################.
## Part 4 - Parent geography for IZ and locality ----
###############################################.
parent_lookup <- dz11_lookup %>% rename(hscp_partnership = hscp2019) %>% 
  select(intzone2011, hscp_locality, hscp_partnership) %>% unique

saveRDS(parent_lookup, paste0(geo_lookup, 'IZtoPartnership_parent_lookup.rds'))

###############################################.
## Part 5 - Create dictionary  to have the names and not codes ----
###############################################.
# Create HSC locality dictionary.
local_dictio <- hscp_loc %>% rename(areaname = hscplocality, code = hscp_locality) %>%
  select(areaname, code) %>% unique
saveRDS(local_dictio, paste0(geo_lookup, 'HSClocalitydictionary.rds'))

# Create ADP dictionary.
adp_dictio <- adp_lookup %>% rename(areaname = adp_name, code = adp) %>%
  select(-ca2019) %>% unique()

saveRDS(adp_dictio, paste0(geo_lookup, 'ADPdictionary.rds'))

# Creating dictionaries for council, health board, iz and hscp
iz_dictio <- create_dictionary(iz2011name, iz2011, "IZ11")
ca_dictio <- create_dictionary(ca2011name, ca2011, "CA")
hb_dictio <- create_dictionary(hb2014name, hb2014, "HB")
part_dictio <- create_dictionary(hscp2016name, hscp2016, "HSCP")

# create scotland dictionary.
scot_dictio <- data.frame(code = 'S00000001', areaname = "Scotland")
saveRDS(scot_dictio, paste0(geo_lookup, 'Scotlandictionary.rds'))

# Merge files together. 
code_dictio <- rbind(scot_dictio, hb_dictio, ca_dictio, adp_dictio, part_dictio, 
                     local_dictio, iz_dictio)
saveRDS(code_dictio, paste0(geo_lookup, 'codedictionary.rds'))

###############################################.
## Part 6 -Deprivation (SIMD) geographies ----
###############################################.
# Creating a data frame with all ranks for each year
data_depr_simd <- list() #creating empty list for placing data created by function

# The function creates the dataset and assigns it to the list
mapply(create_simd, file_name = "DataZone2001", simd_version = 2004, 
       year = 1996:2003, list_pos = 1:8) #simd version 2004
mapply(create_simd, file_name = "DataZone2001", simd_version = 2006, 
       year = 2004:2006, list_pos = 9:11) #simd version 2006
mapply(create_simd, file_name = "DataZone2001", simd_version = "2009v2", 
       year = 2007:2009, list_pos = 12:14) #simd version 2009
mapply(create_simd, file_name = "DataZone2001", simd_version = 2012, 
       year = 2010:2013, list_pos = 15:18) #simd version 2012
mapply(create_simd, file_name = "DataZone2011", simd_version = 2016, 
       year = 2014:2018, list_pos = 19:23) #simd version 2016

data_depr_simd <- do.call("rbind", data_depr_simd) # converting from list into dataframe

saveRDS(data_depr_simd, paste0(geo_lookup, 'deprivation_geography.rds'))

##END
