# Geography_lookup.R ----

# This script creates a number of geography lookup files required for both the profiles indicator updates and profiles tool itself.
# In short the files created in this script map 2011 datazones to all the various larger geographies as well as SIMD deprivation quintiles.
# This script also produces dictionaries that match geography codes with their corresponding geography names.
# 
# 1- production of population lookups used by profiles indicator updates
# 2- production of profiles tool indicator updates
# 3- production of SIMD split for indicators
# 4- use within scotpho profiles tool determining hierarchy of geographies 


# All geographies (with the exception of HSC Locality) exist on the GSS geography register
# https://www.nrscotland.gov.uk/files/geography/Policy/standard-names-codes-background-info.pdf
# The format of a standard geography code is
# S XX XXXXXX  'Country', 'Entity' (ie type of geography), 'Instance' (ie specific geography)


#Part 1 - HSC locality lookup.
#Part 2a - ADP lookup
#Part 2b - Police division lookup
#Part 3 - Joining all geographies 
#Part 4 - Parent geography for IZ and locality
#Part 5 - Create dictionary  to have the names and not codes.
#Part 6 - Deprivation (SIMD) geographies


###############################################.
## Packages and filepaths ----
###############################################.
library(dplyr)
library(readr)
library(httr) # api connection
library(jsonlite)  # transforming JSON files into dataframes
library(tidyr)
library(magrittr)


# path to geography lookups folder
geo_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/"

###############################################.
## Functions ----
###############################################.
# Function to extract data from the open data platform. The resource_id is
# static id used in the platform
extract_data <- function(resource_id) {
  open_data_url <- "https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id="
  
  # Limit is set to 1 million, check nothing is bigger than that
  data_extraction <-  GET(paste0(open_data_url, resource_id, "&limit=1000000"))
  
  # It's a JSON file, so transforming into something more usable in R
  data_extraction <- rawToChar(data_extraction$content) %>% fromJSON()
  data_extraction <- data_extraction$result$records %>% #extracting data  
    setNames(tolower(names(.)))   #variables to lower case
}

###############################################.
#Function to create deprivation rank data set for different years 
#and different simd versions
create_simd <- function(resource_id, year_list, simd_version) { #list_pos
  
  # Creating quintile/decile variables to keep, they vary with the version
  sc_decile_var <- paste0("simd", simd_version, "countrydecile")
  sc_quin_var <- paste0("simd", simd_version, "countryquintile")
  hb_quin_var <- paste0("simd", simd_version, "hbquintile")
  ca_quin_var <- paste0("simd", simd_version, "caquintile")
  hscp_quin_var <- paste0("simd", simd_version, "hscpquintile")
  
  
  # Extract the data, keep some variables and rename them
  data_simd <- extract_data(resource_id = resource_id) %>% 
    select(datazone, ca, hb, hscp, sc_decile_var, all_of(sc_quin_var), all_of(hb_quin_var), all_of(ca_quin_var), all_of(hscp_quin_var) ) %>% 
    rename(sc_decile = sc_decile_var, sc_quin = sc_quin_var, hb_quin = hb_quin_var, ca_quin = ca_quin_var, hscp_quin = hscp_quin_var)
  
  
  # recode simd 2004 and 2006, as they follow an inverse scale.
  if (simd_version %in% c("2004", "2006")) {
    data_simd <- data_simd %>%
      mutate_at(vars(sc_decile),
                ~recode(., "1" = 10, "2" = 9, "3" = 8, "4" = 7, "5" = 6, "6" = 5, "7" = 4, "8" = 3, "9" = 2, "10" = 1)) %>%
      mutate_at(vars(sc_quin, ca_quin, hb_quin, hscp_quin),
                ~recode(., "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1))
    
    
  } else {
    data_simd <- data_simd
  }
  
  # Creating a list of datasets with all the years used for each version
  data_simd_list <- lapply(year_list,
                           function(year_chosen){
                             data_simd %>% mutate(year = year_chosen, 
                                                  scotland = "S00000001")
                           } )
  # Joining all the dataframes in one
  data_simd <- do.call("rbind", data_simd_list) 
}

###############################################.
# Function to create dictionaries for the different areas
create_dictionary <- function(area_name, area_code, filename) {
  dictio <- dz_base %>% 
    select({{area_name}}, {{area_code}}) %>% 
    rename(areaname = {{area_name}}, code = {{area_code}}) %>% unique
  
  saveRDS(dictio, paste0(geo_lookup, filename, 'dictionary.rds'))
  
  dictio #retrieving object
}

###############################################.
## Part 1 - DZ to HSC locality lookup ----
###############################################.
# Source the datazone to HSCP locality lookup created by LIST and maintained in cl-out look-ups folder 
# /conf/linkage/output/lookups/Unicode/Geography/HSCP Locality"
# Confirm this is the latest version available of the lookup available (PIAs in LIST team are a good contact to establish if its the latest).

# Open latest DZ to HSCP locality lookup
hscp_loc <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.rds') %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  select(datazone2011, hscp_locality, hscp2019name) 


# When updating the locality lookup its generally a good idea to check what changes may have taken place between lookups as this can help with 
# explaining where/when indicator data might be different. The optional code below can help study differences but isn't essential to run.

# Changes introduced with lookup 20230804 ----
# New HSCP : Moray now 4 HSCP localities (replaced moray east/moray west with 4 new localities)

# Changes introduced with lookup  20240513 ----
# South Ayshire HSCP - 6 localities before and after the change but the names have updated and there are some changes to dz allocated 
# Inverclyde - used to be 3 localities but reducing to two (central inverclyde gone and split between east and west inverclyde looks like majority of dz go to west)



#############################################################################################.
## Optional code that helps determine differences between lookup versions
#
## 2022 lookup
# hscpl_2022 <- readRDS(/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/Archive/HSCP Localities_DZ11_Lookup_20220630.rds') %>% 
#   setNames(tolower(names(.))) %>%  #variables to lower case
#   select(datazone2011, hscp_locality, hscp2019name) 
# 
# loc_2022 <-hscpl_2022 %>%
#   group_by(hscp2019name, hscp_locality) |>
#   summarise(count_2022=n()) |>
#   ungroup()
#
## 2023 lookup
# hscpl_2023 <-  readRDS('/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/Archive/HSCP Localities_DZ11_Lookup_20230804.rds') %>% 
#   setNames(tolower(names(.))) %>%  #variables to lower case
#   select(datazone2011, hscp_locality, hscp2019name) 
# 
# loc_2023 <-hscpl_2023 %>%
#   group_by(hscp2019name, hscp_locality) |>
#   summarise(count_2023=n()) |>
#   ungroup()
# 
## 2024 lookup
# hscp_loc_2024 <-hscp_loc %>%
#   group_by(hscp2019name, hscp_locality) |>
#   summarise(count_2024=n()) |>
#   ungroup()
#
## joining old lookups enable you to see which territories are changing
## Studying the columns where NA appear in the files below helps you check which localities existed in which lookups
# joined <-full_join(loc_2022,loc_2023)
# joined2 <-full_join(joined,hscp_loc_2024)

# sort by locality name then partnership 
hscp_loc %<>% 
  arrange(hscp_locality, hscp2019name) %>%
  rename(loc_name = hscp_locality)


## Generate GSS geography code ----
# Create artificial standard 9 digit code to identify unique hscp locality
# GSS (government statistical service) codes do not exist for HSCP localities as these are not official geographies
# They are deemed too unstable to warrant creation of official numbering but ScotPHO profiles tool uses them to ease data processing
# so we must create them. 

##Beware some localities might have common names to those used by other HSCPs 
#(e.g. 'East'/'West' are commonly used as locality names by more than one partnership ).
# These should be removed/handled by GPD team when preparing the lookup in cl-out but worth
# being aware this is a possibility.

# create a sequential number (the starting number of this sequential number can be adjusted to ensure we don't recycle the same code)
# Each time the geography lookup codes for localities is run there is a risk we could apply a different code to a locality if there have been changes to the number or names of
# the localities (even if there are the same number of localities a change in spelling can impact on the sort order).

loc_seq <- seq_len(length(unique(hscp_loc$loc_name)))+127 # adding value to sequence will ensure that reuse same codes and risk confusing localities
loc_zeros <- case_when(nchar(loc_seq) == 1 ~ "00000",
                       nchar(loc_seq) == 2 ~ "0000",
                       nchar(loc_seq) == 3 ~ "000")

#generate sequential number for each locality that fits pattern of a GSS geography code
loc_code <- paste0("S99", loc_zeros, loc_seq)
loc_code <- data.frame(hscp_locality = loc_code, loc_name = unique(hscp_loc$loc_name))

#add that artificial GSS code to dataframe
hscp_loc <- left_join(x = hscp_loc, y = loc_code, by = c("loc_name"))

saveRDS(hscp_loc, paste0(geo_lookup, 'DataZone11_HSCLocality_Lookup.rds'))

###############################################.
## Part 2a - ADP lookup ----
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
## Part 2b - Police Division lookup ----
###############################################.
# Creating lookup of PDss with council area
pd_lookup <- data.frame(
  ca2019 = c("S12000005",
             "S12000006",
             "S12000008",
             "S12000010",
             "S12000011",
             "S12000013",
             "S12000014",
             "S12000017",
             "S12000018",
             "S12000019",
             "S12000020",
             "S12000021",
             "S12000023",
             "S12000026",
             "S12000027",
             "S12000028",
             "S12000029",
             "S12000030",
             "S12000033",
             "S12000034",
             "S12000035",
             "S12000036",
             "S12000038",
             "S12000039",
             "S12000040",
             "S12000041",
             "S12000042",
             "S12000045",
             "S12000047",
             "S12000048",
             "S12000049",
             "S12000050"
  ),
  pd = c("S32000008",
         "S32000005",
         "S32000004",
         "S32000012",
         "S32000018",
         "S32000010",
         "S32000008",
         "S32000010",
         "S32000013",
         "S32000012",
         "S32000015",
         "S32000004",
         "S32000010",
         "S32000012",
         "S32000010",
         "S32000004",
         "S32000019",
         "S32000008",
         "S32000015",
         "S32000015",
         "S32000003",
         "S32000006",
         "S32000013",
         "S32000003",
         "S32000012",
         "S32000017",
         "S32000017",
         "S32000018",
         "S32000016",
         "S32000017",
         "S32000018",
         "S32000019"
  ),
  pd_name = c("Forth Valley",
              "Dumfries and Galloway",
              "Ayrshire",
              "The Lothians and Scottish Borders",
              "Greater Glasgow",
              "Highlands and Islands",
              "Forth Valley",
              "Highlands and Islands",
              "Renfrewshire and Inverclyde",
              "The Lothians and Scottish Borders",
              "North East",
              "Ayrshire",
              "Highlands and Islands",
              "The Lothians and Scottish Borders",
              "Highlands and Islands",
              "Ayrshire",
              "Lanarkshire",
              "Forth Valley",
              "North East",
              "North East",
              "Argyll and West Dunbartonshire",
              "Edinburgh",
              "Renfrewshire and Inverclyde",
              "Argyll and West Dunbartonshire",
              "The Lothians and Scottish Borders",
              "Tayside",
              "Tayside",
              "Greater Glasgow",
              "Fife",
              "Tayside",
              "Greater Glasgow",
              "Lanarkshire"
  ))

saveRDS(pd_lookup, paste0(geo_lookup, 'PD_CA_lookup.rds'))


###############################################.
## Part 3  - Joining all geographies ----
###############################################.
# reading datazone lookup  from open data platform 
# which includes most geographies but adp and locality
# Resource ids can be accessed in the page for the datazone 2011 lookup here:
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels
dz_base <- extract_data("395476ab-0720-4740-be07-ff4467141352")

# Creating dz lookup
dz11_lookup <- dz_base %>% 
  select(datazone, intzone, ca, hscp, hb) %>% 
  # the variables contain a year, but they have the latest(2019) codes
  rename(datazone2011 = datazone, intzone2011 =intzone, ca2019 = ca,
         hscp2019 = hscp, hb2019 = hb)

# merging localities
dz11_lookup <- left_join(dz11_lookup, hscp_loc, by = "datazone2011") 

# merging adps
dz11_lookup <- left_join(dz11_lookup, adp_lookup, by = "ca2019") %>% 
  select(-loc_name, -hscp2019name, -adp_name)

# merging PDs
dz11_lookup <- left_join(dz11_lookup, pd_lookup, by = "ca2019") %>% 
  select(-pd_name)

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
local_dictio <- hscp_loc %>% rename(areaname = loc_name, code = hscp_locality) %>%
  select(areaname, code) %>% unique
saveRDS(local_dictio, paste0(geo_lookup, 'HSClocalitydictionary.rds'))

# Create ADP dictionary.
adp_dictio <- adp_lookup %>% rename(areaname = adp_name, code = adp) %>%
  select(-ca2019) %>% unique()

saveRDS(adp_dictio, paste0(geo_lookup, 'ADPdictionary.rds'))

# Create PD dictionary.
pd_dictio <- pd_lookup %>% rename(areaname = pd_name, code = pd) %>%
  select(-ca2019) %>% unique()

saveRDS(pd_dictio, paste0(geo_lookup, 'PDdictionary.rds'))

# Creating dictionaries for council, health board, iz and hscp
iz_dictio <- create_dictionary(intzonename, intzone, "IZ11")
ca_dictio <- create_dictionary(caname, ca, "CA")
hb_dictio <- create_dictionary(hbname, hb, "HB")
part_dictio <- create_dictionary(hscpname, hscp, "HSCP")

# create scotland dictionary.
scot_dictio <- data.frame(code = 'S00000001', areaname = "Scotland")
saveRDS(scot_dictio, paste0(geo_lookup, 'Scotlandictionary.rds'))

# Merge files together. 
code_dictio <- rbind(scot_dictio, hb_dictio, ca_dictio, adp_dictio, part_dictio, 
                     pd_dictio, local_dictio, iz_dictio)
saveRDS(code_dictio, paste0(geo_lookup, 'codedictionary.rds'))

###############################################.
## Part 6 -Deprivation (SIMD) geographies ----
###############################################.
# Joining together the extractions of each simd version. 
# Resource ids can be accessed in the page for each SIMD version here:
# https://www.opendata.nhs.scot/dataset/scottish-index-of-multiple-deprivation#
data_depr_simd <- rbind(
  create_simd("a97fca71-ebbb-4897-a611-88024a76ff21", year_list = 1996:2003, 
              simd_version = "2004"), #simd version 2004
  create_simd("6f871d03-d2af-4fe2-a615-d2d2ca76c3a5", year_list = 2004:2006, 
              simd_version = "2006"), #simd version 2006
  create_simd("d9738550-4cf9-428e-8453-c2aad463ff68", year_list = 2007:2009, 
              simd_version = "2009v2"), #simd version 2009
  create_simd("dd4b13d3-066b-4714-bb1f-730e1a1ee692", year_list = 2010:2013, 
              simd_version = "2012"), #simd version 2012
  create_simd("cadf715a-c365-4dcf-a6e0-acd7e3af21ec", year_list = 2014:2016, 
              simd_version = "2016"), #simd version 2016
  create_simd("acade396-8430-4b34-895a-b3e757fa346e", year_list = 2017:2024, 
              simd_version = "2020v2") #simd version 2020
) %>%
  merge(y=pd_lookup, by.x="ca", by.y="ca2019") %>%
  select(-ends_with("name"))

saveRDS(data_depr_simd, paste0(geo_lookup, 'simd_datazone_lookup.rds'))

###############################################.
## Part 7 - Geography lookup for profiles tool ----
###############################################.
opt_lookup <- readRDS(paste0(geo_lookup, "codedictionary.rds")) %>%
  mutate_all(factor) %>% # converting variables into factors
  #Creating geography type variable
  mutate(areatype = case_when(substr(code, 1, 3) == "S00" ~ "Scotland",
                              substr(code, 1, 3) == "S08" ~ "Health board",
                              substr(code, 1, 3) == "S12" ~ "Council area",
                              substr(code, 1, 3) == "S11" ~ "Alcohol & drug partnership",
                              substr(code, 1, 3) == "S99" ~ "HSC locality",
                              substr(code, 1, 3) == "S37" ~ "HSC partnership",
                              substr(code, 1, 3) == "S32" ~ "Police division",
                              substr(code, 1, 3) == "S02" ~ "Intermediate zone"),
         #Changing ands for & to reduce issues with long labels and " - " for "-"
         areaname = gsub(" and ", " & ", areaname),
         areaname = gsub(" - ", "-", areaname))

#Bringing parent geography information and formatting in one column with no duplicates
geo_parents <- readRDS(paste0(geo_lookup, "IZtoPartnership_parent_lookup.rds")) %>%
  gather(geotype, code, c(intzone2011, hscp_locality)) %>% distinct() %>%
  select(-geotype) %>% rename(parent_code = hscp_partnership)

# Merging to geo_lookup to obtain parent area name
geo_parents <- left_join(x=geo_parents, y=opt_lookup, by=c("parent_code" = "code")) %>%
  select(-c(areatype)) %>% rename(parent_area = areaname)

#Merging parents to geo_lookup
#opt_lookup <- left_join(x=opt_lookup, y=geo_parents, by="code", all.x = TRUE)


opt_lookup <- left_join(x=opt_lookup, y=geo_parents, by="code")

## No IZ should assigned to more than one hscp partnership in this file - check by filtering the geo type to IZ and the total should be 1279

###There are a number of IZ's with the same name which could be confusing, recoding to avoid this issue.
opt_lookup %<>%
  mutate(areaname = case_when(
    code == "S02001938" ~ "Woodside-Glasgow City",
    code == "S02001267" ~ "Woodside-Abeerdeen City",
    code == "S02002233" ~ "Western Edge-Perth & Kinross",
    code == "S02001475" ~ "Western Edge-Dundee City",
    code == "S02001620" ~ "Tollcross-City of Edinburgh",
    code == "S02001911" ~ "Tollcross-Glasgow City",
    code == "S02001671" ~ "Muirhouse-City of Edinburgh",
    code == "S02002137" ~ "Muirhouse-North Lanarkshire",
    code == "S02002358" ~ "Law-South Lanarkshire",
    code == "S02001469" ~ "Law-Dundee City",
    code == "S02002490" ~ "Ladywell-West Lothian",
    code == "S02002156" ~ "Ladywell-North Lanarkshire",
    code == "S02001528" ~ "Hillhead-East Dunbartonshire",
    code == "S02001953" ~ "Hillhead-Glasgow City",
    code == "S02001249" ~ "City Centre West-Aberdeen City",
    code == "S02001933" ~ "City Centre West-Glasgow City",
    code == "S02001250" ~ "City Centre East-Aberdeen City",
    code == "S02001932" ~ "City Centre East-Glasgow City",
    code == "S02001448" ~ "City Centre-Dundee City",
    code == "S02002449" ~ "City Centre-Stirling",
    code == "S02001307" ~ "Blackburn-Aberdeenshire",
    code == "S02002496" ~ "Blackburn-West Lothian",
    code == "S02001534" ~ "Wallyford and Whitecraig (IZ01)",
    code == "S02001535" ~ "Musselburgh South (IZ02)",
    code == "S02001536" ~ "Musselburgh West (IZ03)",
    code == "S02001537" ~ "Musselburgh North (IZ04)",
    code == "S02001538" ~ "Musselburgh East (IZ05)",
    code == "S02001539" ~ "Pinkie Braes (IZ06)",
    code == "S02001540" ~ "Prestonpans West (IZ07)",
    code == "S02001541" ~ "Prestonpans East (IZ08)",
    code == "S02001542" ~ "Cockenzie (IZ09)",
    code == "S02001543" ~ "Tranent North (IZ10)",
    code == "S02001544" ~ "Tranent South (IZ11)",
    code == "S02001545" ~ "Ormiston (IZ12)",
    code == "S02001546" ~ "Longniddry and Aberlady (IZ13)",
    code == "S02001547" ~ "Haddington Rural (IZ14)",
    code == "S02001548" ~ "Haddington North (IZ15)",
    code == "S02001549" ~ "Haddington South (IZ16)",
    code == "S02001550" ~ "Gullane and Drem (IZ17)",
    code == "S02001551" ~ "North Berwick North (IZ18)",
    code == "S02001552" ~ "North Berwick South (IZ19)",
    code == "S02001553" ~ "East Linton and Rural (IZ20)",
    code == "S02001554" ~ "Dunbar West (IZ21)",
    code == "S02001555" ~ "Dunbar East (IZ22)",
    code == "S02002460" ~ "Whitecrook (IZ01)",
    code == "S02002461" ~ "Singer and Clydebank South (IZ02)",
    code == "S02002462" ~ "Drumry (IZ03)",
    code == "S02002463" ~ "Clydebank (IZ04)",
    code == "S02002464" ~ "Clydebank North (IZ05)",
    code == "S02002465" ~ "Clydebank East (IZ06)",
    code == "S02002466" ~ "Duntocher (IZ07)",
    code == "S02002467" ~ "Dalmuir (IZ08)",
    code == "S02002468" ~ "Kilpatrick (IZ09)",
    code == "S02002469" ~ "Bowling (IZ10)",
    code == "S02002470" ~ "Dumbarton East (IZ11)",
    code == "S02002471" ~ "Dumbarton (IZ12)",
    code == "S02002472" ~ "Dalreoch (IZ13)",
    code == "S02002473" ~ "Leven (IZ14)",
    code == "S02002474" ~ "Bonhill (IZ15)",
    code == "S02002475" ~ "Alexandria (IZ16)",
    code == "S02002476" ~ "Balloch (IZ17)",
    code == "S02002477" ~ "Lomond (IZ18)",
    TRUE  ~  paste(areaname))) #Last line for the rest of cases

opt_lookup %<>%
  #Creating variable that includes area name and type for trend plotting
  mutate(areaname_full = paste(areaname, "-", areatype)) %>%
  mutate_if(is.character, factor) %>% #transforming into factors
  select(-c(parent_code)) %>%
  #Reducing length of the area type descriptor
  mutate(areaname_full = ifelse(areaname == "Scotland", "Scotland",
                                paste(areaname_full)),
         areaname_full = gsub("Health board", "HB", areaname_full),
         areaname_full = gsub("Council area", "CA", areaname_full),
         areaname_full = gsub("Alcohol & drug partnership", "ADP", areaname_full),
         areaname_full = gsub("HSC partnership", "HSCP", areaname_full),
         areaname_full = gsub("Police division", "PD", areaname_full),
         areaname_full = gsub("HSC locality", "HSCL", areaname_full),
         areaname_full = gsub("Intermediate zone", "IZ", areaname_full))

saveRDS(opt_lookup, paste0(geo_lookup, "opt_geo_lookup.rds"))


check <- readRDS(paste0(geo_lookup, "opt_geo_lookup_26082024.rds"))

##END
