#===========
#make_bzone_hh_inc_qrtl_prop.R
#===========

# This script will create the make_bzone_hh_inc_qrtl_prop.csv file for the inputs
# The script will join the 2019 and 2045 data files

# File paths
proj_dir = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study/'

input = file.path(proj_dir, 'Data_to_process')

temp = file.path(getwd(), 'temp')
if(!dir.exists(temp)){ dir.create(temp)}

final = file.path(proj_dir, 'NVTA_Inputs_2020/inputs')

# Install/Load libraries --------------
source('data prep scripts/get_packages.R')

library(dplyr)
library(tidyverse)
library(tidycensus)
library(viridis)
library(leaflet)
library(readr)
library(sp)
library(sf)
library("rgeos")

# Load Census Data --------------
# add in census api key
fileName <- 'census_api.txt'

if(!file.exists(fileName)){
  stop(paste('Census API Key needed in as a plain text file in /n', getwd(), '/n go to https://api.census.gov/data/key_signup.html /n and save the API key as `census_api.txt`'))
}

mystring <- read_file(file.path(input, fileName))
api_key <- gsub('/r/n', '', mystring) #clean up in case text file has breaks

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron") # will check R environment for past api keys

# load census tract data
counties <- c(059, 600, 610) #enter county codes here
# HH income quartiles uses census table B19001
# Full table found at this link: https://api.census.gov/data/2016/acs/acs5/groups/B19001.html

# Download with geography
hh_income_raw <- get_acs(geography = "tract", table = "B19001",
                           state = "VA", county = counties, geometry = TRUE)

#filter for our columns
dwell_units_016 <- dwell_units_raw %>% filter(variable=="S1101_C01_016") # percent one unit households
dwell_units_017 <- dwell_units_raw %>% filter(variable=="S1101_C01_017") # percent two unit households
dwell_units_018 <- dwell_units_raw %>% filter(variable=="S1101_C01_018") # percent mobile home households
dwell_units_total <- dwell_units_raw %>% filter(variable=="S1101_C01_001") # total households


# Make sure all have the same GEOID
# The tract is value 6 - 9 in the census GEOID
identical(dwell_units_016$GEOID, dwell_units_018$GEOID) &
  identical(dwell_units_016$GEOID, dwell_units_017$GEOID) &
  identical(dwell_units_016$GEOID, dwell_units_total$GEOID)

#add all variables into single table, create VisionEval columns
dwell_units_geo <- dwell_units_016 %>% 
  mutate (one_unit = dwell_units_016$estimate, #bring in the 3 census variables
          mobile_home = dwell_units_018$estimate,
          two_unit = dwell_units_017$estimate,
          total = dwell_units_total$estimate) %>%
  mutate(SFDU = (one_unit + mobile_home)*total/100, #change census variables to VisionEval variables, needs to be actual number of units
         MFDU = two_unit*total/100) %>% 
  mutate(Geo = substr(GEOID, 6, 9)) %>%
  select("Geo", "SFDU", "MFDU",  "GEOID") %>% #filter dataframe columns
  replace(is.na(.), 0) #assume all NAs are 0



# Clean tract and TAZ geometries --------------
TAZ_geometry <- st_read(file.path(input, "FFXsubzone/FFX_Subzone.shp")) #load TAZ dataset
TAZ_geometry_sp <- as(TAZ_geometry, Class = "Spatial")  #make TAZ df into sp 
dwell_units_geo_sp = as_Spatial(dwell_units_geo) #make tract df into sp 

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
TAZ_geometry_sp_newproj <- spTransform(TAZ_geometry_sp, CRS = proj.USGS)
dwell_units_geo_sp_newproj <- spTransform(dwell_units_geo_sp, CRS = proj.USGS)


# Find intersection between TAZ and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(TAZ_geometry_sp_newproj, dwell_units_geo_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
n<-names(gI) #grab the ids/names from the gIntersection object
n<-data.frame(t(data.frame(strsplit(n," ",fixed=TRUE)))) #ids are combined so split into separate cells
colnames(n)[1:2]<-c("id_TAZ","id_tract") #add id names to differentiate


#find the overlapping area for all the TAZ-Tract objects
n$area<-sapply(gI@polygons, function(x) x@area) 
a<-data.frame(id=row.names(TAZ_geometry_sp_newproj), TAZ_N = TAZ_geometry_sp_newproj$TAZ_N)#subset TAZ dataset so only joining TAZ ids
df<-merge(n,a,by.x = "id_TAZ", by.y = "id", all.x=TRUE) #merge the TAZ ids into our dataset


#find the total area of every census tract
df <- df %>%   group_by(id_tract)%>%
  summarise(shape_area = sum(area))%>%
  right_join(df, by = "id_tract") 



dwell_units_geo$id_tract <- seq.int(nrow(dwell_units_geo)) #make column so we can join census tract df with intersection df
df2<- merge(df, dwell_units_geo, by = "id_tract", by.y = "id_tract", all.x=TRUE)

# Finalize dataframe -------------------------
df3 <- df2 %>% mutate(share.area = area/shape_area, #calculate % of tract in each TAZ
                      SFDU_this_area = SFDU * share.area, # multiply to get SFDU/MFDU in each intersected polygon
                      MFDU_this_area = MFDU * share.area) %>% 
  group_by(TAZ_N)%>%
  summarise(n = n(),
            SFDU = sum(SFDU_this_area), # add up tract-level SFDU/MFDUs for each TAZ 
            MFDU = sum(MFDU_this_area)) %>%
  mutate(Geo = TAZ_N,
         GQDU = 1) # no census variable for GQDU. Must use outside data source or assume as 1



# quality check that total SFDUs and MFDUs are same in TAZ and census tract files
identical(sum(df3$SFDU), sum(dwell_units_geo$SFDU)) &
  identical(sum(df3$MFDU),sum(dwell_units_geo$MFDU))

#duplicate 2019 data for 2045    
df3_copy <- df3
df3$Year <- 2019
df3_copy$Year <- 2045

#make final csv file and save to temp directory
bzone_dwelling_units_final <- rbind(df3, df3_copy) %>% select("Geo","Year",'SFDU','MFDU','GQDU') 
write.csv(bzone_dwelling_units_final, file.path(final, 'bzone_dwelling_units.csv'), row.names = FALSE) #save as csv in final directory

