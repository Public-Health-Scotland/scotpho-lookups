#Code to create geography shapefiles.
# Mainly used for the ScotPHO profiles tool

############################.
##Filepaths ----
############################.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/"
  shapefiles <- "/PHI_conf/ScotPHO/Profiles/Data/Shapefiles/"
} else  {
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/"
  shapefiles <- "//stats/ScotPHO/Profiles/Data/Shapefiles/"
}

############################.
##Packages ----
############################.
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(rgdal) #for reading shapefiles
library(rgeos) #for reducing size of shapefiles
library(rmapshaper) #for reducing size of shapefiles
library(maptools) #for dissolving dzs shp into localities

###############################################.
## Shapefiles ----
###############################################.
#Reading file with council shapefiles: https://www.spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=4B4BEB1B1E52BCA9D3C1FD531CC199F8#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
#making it small 29mb to 2.5.
ca_bound_orig <-readOGR(shapefiles, "pub_las") %>%
  rmapshaper::ms_simplify(keep=0.0025)  %>%
  setNames(tolower(names(.))) #variables to lower case

object.size(ca_bound_orig)

#Transforming coordinate system to the one leaflet needs
ca_bound_orig <- spTransform(ca_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(ca_bound_orig, dsn=shapefiles, "CA_simpl_2019", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Saving as rds as it is much faster to read
names(ca_bound_orig@data)[names(ca_bound_orig@data)=="local_auth"] <- "area_name"

saveRDS(ca_bound_orig, paste0(shapefiles, "CA_boundary.rds"))

##########################.
###Health board
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
hb_bound_orig<-readOGR(shapefiles,"SG_NHS_HealthBoards_2019") %>%
  rmapshaper::ms_simplify(keep=0.0025) %>%
  setNames(tolower(names(.))) #variables to lower case

object.size(hb_bound_orig)

#Transforming coordinate system to the one leaflet needs
hb_bound_orig <- spTransform(hb_bound_orig,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile to avoid the calculations.
writeOGR(hb_bound_orig, dsn=shapefiles, "HB_simpl_2019", driver="ESRI Shapefile", overwrite_layer=TRUE)

#Saving as rds as it is much faster to read
names(hb_bound_orig@data)[names(hb_bound_orig@data)=="hbcode"] <- "code"
names(hb_bound_orig@data)[names(hb_bound_orig@data)=="hbname"] <- "area_name"

saveRDS(hb_bound_orig, paste0(shapefiles, "HB_boundary.rds"))

##########################.
###HSC Partnership
#making it small 29mb to 2.5. Sometimes it fails, due to lack of memory (use memory.limits and close things).
hscp_bound_orig <- readOGR(shapefiles,"SG_NHS_IntegrationAuthority_2019") %>%
  rmapshaper::ms_simplify(keep=0.0025) %>%
  setNames(tolower(names(.))) #variables to lower case

object.size(hscp_bound_orig)

#Changing the projection to WSG84, the ones leaflet needs.
proj4string(hscp_bound_orig) #Checking projection
hscp_bound_orig <- spTransform(hscp_bound_orig, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Saving the simplified shapefile.
writeOGR(hscp_bound_orig, dsn=shapefiles, "HSCP_simpl_2019",
         driver="ESRI Shapefile", overwrite_layer=TRUE, verbose=TRUE,
         morphToESRI=TRUE)

names(hscp_bound_orig@data)[names(hscp_bound_orig@data)=="hiacode"] <- "code"
names(hscp_bound_orig@data)[names(hscp_bound_orig@data)=="hianame"] <- "area_name"

saveRDS(hscp_bound_orig, paste0(shapefiles, "HSCP_boundary.rds"))

###############################################.
# HSC locality
# Based on datazones 2011, so for some partnerships it might not fit their 'official'
# boundaries. Boundaries can be found in the SG website too
dz11_shp <-readOGR('/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/Data Zones 2011/',
                   "SG_DataZone_Bdry_2011") %>%
  setNames(tolower(names(.))) #variables to lower case

# Transforming projection
dz11_shp <- spTransform(dz11_shp,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

# Bringing lookup dz to locality
loc_look <- readRDS(paste0(lookups, 'Geography/DataZone11_HSCLocality_Lookup.rds'))

#merge with locality look up
dz11_shp@data <- left_join(dz11_shp@data, loc_look, by=c("datazone" = "datazone2011"))

#Dissolve datazone boundaries to create locality boundaries
locality_shp <- unionSpatialPolygons(dz11_shp, dz11_shp$hscp_locality)

# Adding a data slot to the shp with names and code.
loc_look %<>% select(-datazone2011) %>% unique() %>%  #one row per locality
  rename(area_name = loc_name, code = hscp_locality)
rownames(loc_look)  <- loc_look$code #need this to match with the shp

#Merging data with shapefile
locality_shp <- SpatialPolygonsDataFrame(locality_shp, loc_look)

# Simplifying shp to make it small
locality_shp <- locality_shp %>% rmapshaper::ms_simplify(keep=0.0050, keep_shapes = T)

#Saving the simplified shapefile.
writeOGR(locality_shp, dsn=shapefiles, "HSC_locality_simpl_2019",
         driver="ESRI Shapefile", overwrite_layer=TRUE, verbose=TRUE,
         morphToESRI=TRUE)

saveRDS(locality_shp, paste0(shapefiles, "HSC_locality_boundary.rds"))

##########################.
###Intermediate zone
# It comes from Improvement Service
iz_bound_orig <- readRDS(paste0(shapefiles, "IZshapes.rds")) %>% #IZ
  setNames(tolower(names(.))) #variables to lower case
names(iz_bound_orig@data)[names(iz_bound_orig@data)=="interzone"] <- "code"
names(iz_bound_orig@data)[names(iz_bound_orig@data)=="name"] <- "area_name"
iz_bound_orig$council <- gsub(" and ", " & ", iz_bound_orig$council)
iz_bound_orig$council <- gsub("Edinburgh", "City of Edinburgh", iz_bound_orig$council)
iz_bound_orig$council <- gsub("Eilean Siar", "Na h-Eileanan Siar", iz_bound_orig$council)

saveRDS(iz_bound_orig, paste0(shapefiles, "IZ_boundary.rds"))

##END