#===========
#Make_Bzone_Dwelling_Units.R
#===========

# This script will create the bzone_dwelling_units.csv file for the inputs
# The script will join the 2019 and 2045 data files


#Install/Load libraries
#--------------
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("tidycensus")
#install.packages("openxlsx")
#install.packages("readr")
#install.packages("rgeos")
library("dplyr")
library("tidyverse")
library(tidycensus)
library(viridis)
library("leaflet")
library(readr)
library(sp)
library(sf)
#add in census api key
fileName <- 'census_api.txt'
mystring <- read_file(fileName)
api_key<-gsub('\r\n', '', mystring) #clean up in case text file has breaks 

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron") # will check R environment for past api keys

# load census tract data
counties <- c(059, 600,610) #enter county codes here
#Dwell units uses census table S1101
#Full table found at this link: https://api.census.gov/data/2016/acs/acs5/subject/groups/S1101.html

#download raw data 
dwell_units_raw <-get_acs(geography = "tract", table = "S1101",
                      state = "VA", county = counties, geometry = FALSE)

#filter for our columns
dwell_units_016 <- dwell_units_raw %>% filter(variable=="S1101_C01_016") #one unit total households
dwell_units_017 <- dwell_units_raw %>% filter(variable=="S1101_C01_017") #two unit total households
dwell_units_018 <- dwell_units_raw %>% filter(variable=="S1101_C01_018") #mobile home total households

result <- dwell_units_016 %>% mutate (one_unit = dwell_units_016$estimate, #bring in the 3 census variables
                                      mobile_home = dwell_units_018$estimate,
                                      two_unit = dwell_units_017$estimate) %>% 
                              mutate(SFDU = one_unit + mobile_home, #change census variables to VisionEval variables
                                      MFDU = two_unit,
                                      GQDU = 1) %>% 
                              select("NAME", "SFDU", "MFDU", "GQDU") #%>% #filter dataframe columns
                            #  write.csv(., 'bzone_dwelling_units.csv', row.names = FALSE) #save final file


result_no_geo<-st_set_geometry(result3, NULL)
write.csv(result_no_geo, 'census_tract_dwelling_units.csv', row.names = FALSE)

################################################################################################
########### In Progress After this Point #######################################################

TAZ_geometry <- st_read(
  "TAZ geometry/FFXsubzone/FFX_Subzone.shp")


plot(st_geometry(TAZ_geometry))
plot(st_geometry(result))
plot(result['SFDU'])
plot(TAZ_geometry['SUB_POP15'])


TAZ_geometry_sp <- as(TAZ_geometry, Class = "Spatial")
result_sp <- as(result, Class = "Spatial")

library("rgeos")
gIntersection(TAZ_geometry_sp, result_sp)



plot(TAZ_geometry['geo'])
plot(result['SFDU'], add = TRUE, pch = 19, cex = 1)
box()

install.packages("mapview")
library(mapview)
mapview(polygon) + mapview(sample.spdf)

library(rgeos)
over(TAZ_geometry, result)

TAZ_geometry2 <-TAZ_geometry 
TAZ_geometry2 <- st_transform(TAZ_geometry2, st_crs(result))
plot(st_geometry(TAZ_geometry2))
plot(st_geometry(TAZ_geometry))
plot(st_geometry(result))

result_joined <- st_join(TAZ_geometry2, result)

st_join(result, TAZ_geometry)
