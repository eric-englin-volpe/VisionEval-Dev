#===========
#Make_Bzone_Dwelling_Units.R
#===========

# This script will create the bzone_dwelling_units.csv file for the inputs
# The script will join the 2019 and 2045 data files

# File paths
proj_dir = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study/'

input = file.path(proj_dir, 'From_VDOT')

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
# add in census api key
fileName <- 'census_api.txt'

if(!file.exists(fileName)){
  stop(paste('Census API Key needed in as a plain text file in /n', getwd(), '/n go to https://api.census.gov/data/key_signup.html /n and save the API key as `census_api.txt`'))
}

mystring <- read_file(fileName)
api_key <- gsub('/r/n', '', mystring) #clean up in case text file has breaks

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron") # will check R environment for past api keys

# load census tract data
counties <- c(059, 600, 610) #enter county codes here
# Dwell units uses census table S1101
# Full table found at this link: https://api.census.gov/data/2016/acs/acs5/subject/groups/S1101.html

#download raw data
dwell_units_raw <- get_acs(geography = "tract", table = "S1101",
                           state = "VA", county = counties, geometry = FALSE)

# Download with geography
dwell_units_geo <- get_acs(geography = "tract", table = "S1101",
                           state = "VA", county = counties, geometry = TRUE)


#filter for our columns
dwell_units_016 <- dwell_units_raw %>% filter(variable=="S1101_C01_016") # one unit total households
dwell_units_017 <- dwell_units_raw %>% filter(variable=="S1101_C01_017") # two unit total households
dwell_units_018 <- dwell_units_raw %>% filter(variable=="S1101_C01_018") # mobile home total households

# Make sure all have the same GEOID
# The tract is value 6 - 9 in the census GEOID
identical(dwell_units_016$GEOID, dwell_units_018$GEOID) &
  identical(dwell_units_016$GEOID, dwell_units_017$GEOID)

result <- dwell_units_016 %>% mutate (one_unit = dwell_units_016$estimate, #bring in the 3 census variables
                                      mobile_home = dwell_units_018$estimate,
                                      two_unit = dwell_units_017$estimate) %>%
  mutate(SFDU = one_unit + mobile_home, #change census variables to VisionEval variables
         MFDU = two_unit,
         GQDU = 1) %>%
  mutate(Geo = substr(GEOID, 6, 9)) %>%
  select("Geo", "SFDU", "MFDU", "GQDU", "GEOID") #%>% #filter dataframe columns

# save tract-based temporary file

write.csv(result, file.path(temp, 'bzone_dwelling_units_by_tract.csv'), row.names = FALSE)




dwell_units_geo_016 <- dwell_units_geo %>% filter(variable=="S1101_C01_016") # one unit total households
dwell_units_geo_017 <- dwell_units_geo %>% filter(variable=="S1101_C01_017") # two unit total households
dwell_units_geo_018 <- dwell_units_geo %>% filter(variable=="S1101_C01_018") # mobile home total households

# Make sure all have the same GEOID
# The tract is value 6 - 9 in the census GEOID
identical(dwell_units_016$GEOID, dwell_units_018$GEOID) &
  identical(dwell_units_016$GEOID, dwell_units_017$GEOID)

result_geo <- dwell_units_geo_016 %>% mutate (one_unit = dwell_units_geo_016$estimate, #bring in the 3 census variables
                                      mobile_home = dwell_units_geo_018$estimate,
                                      two_unit = dwell_units_geo_017$estimate) %>%
  mutate(SFDU = one_unit + mobile_home, #change census variables to VisionEval variables
         MFDU = two_unit,
         GQDU = 1) %>%
  mutate(Geo = substr(GEOID, 6, 9)) %>%
  select("Geo", "SFDU", "MFDU", "GQDU", "GEOID") #%>% #filter dataframe columns

# Save the tract geography to temp
dwell_units_geo_sp = as_Spatial(dwell_units_geo)

rgdal::writeOGR(obj = dwell_units_geo_sp,
                dsn = temp,
                layer = 'bzone_tract',
                driver = 'ESRI Shapefile')

results_geo_sp = as_Spatial(result_geo)

rgdal::writeOGR(obj = results_geo_sp,
                dsn = temp,
                layer = 'bzone_tract_v2',
                driver = 'ESRI Shapefile')


################################################################################################
########### In Progress After this Point #######################################################


TAZ_geometry <- st_read(file.path(input, "FFXsubzone/FFX_Subzone.shp")) #load TAZ dataset

#Create TAZ vs Census Tract comparison plots
par(mfrow = c(1, 2))
plot(st_geometry(TAZ_geometry),
     main = 'TAZ')
plot(st_geometry(dwell_units_geo),
     main = 'Tract')

par(mfrow = c(1, 2))
plot(TAZ_geometry['SUB_POP15'],
     main = 'TAZ - population 2015')
plot(result_geo['SFDU'],
     main = 'Tract - SFDU')


#Clean the TAZ data, join to Census Tract data
TAZ_geometry2 <- st_transform(TAZ_geometry, 4269)
TAZ_geometry2_sp = as_Spatial(TAZ_geometry2)

###############################################################################
#Method 1: Simple Spatial Join
spatial_joined_df <- st_join(TAZ_geometry2, result_geo) #join TAZ and Census Tract data

#Aggregate data to TAZ level
spatial_joined_df2 <- spatial_joined_df %>% select("Geo","SFDU","MFDU","GQDU","GEOID","TAZ") %>%
                                         group_by(TAZ)%>%
                                            summarise(n = n(),
                                                      SFDU = mean(SFDU),
                                                      MFDU = mean(MFDU),
                                                      GQDU = mean(GQDU))
#Plot results to check SFDU at TAZ level
par(mfrow = c(1, 1))
plot(result_geo['SFDU'], main = "Census - SFDU")
plot(spatial_joined_df2['SFDU'], main = "TAZ - SFDU")

###############################################################################
#Method 2: gIntersection
library("rgeos")

#clean data
TAZ_geometry_sp <- as(TAZ_geometry, Class = "Spatial")
result_sp <- as(result, Class = "Spatial")

#gIntersection
gIntersection(TAZ_geometry2_sp, results_geo_sp, byid = TRUE)
gI <- gIntersection(TAZ_geometry2_sp, results_geo_sp, byid=TRUE, drop_lower_td=TRUE)
#plot(gI, add=TRUE, border="red", lwd=3) #plot goes over previous plot


int<-gIntersection(TAZ_geometry2_sp, results_geo_sp,byid=T) # Multiple polygons per province
n<-names(int)
n<-data.frame(t(data.frame(strsplit(n," ",fixed=TRUE))))

colnames(n)[1:2]<-c("id","id2")
n$area<-sapply(int@polygons, function(x) x@area)
a<-data.frame(id=row.names(TAZ_geometry2_sp),total.area=TAZ_geometry2_sp$SHAPE_AREA)
df<-merge(n,a,all.x=TRUE)

df$share.area<-df$area/df$total.area*100
df
