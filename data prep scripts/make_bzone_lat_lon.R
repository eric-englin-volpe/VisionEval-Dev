#===========
#make_bzone_lat_lon.R
#===========

# This script will create the bzone_lat_lon.csv input file


# File paths
proj_dir = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study/'

input = file.path(proj_dir, 'Data_to_process')

temp = file.path(getwd(), './../temp')
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



# Clean tract and TAZ geometries --------------
TAZ_geometry <- st_read(file.path(input, "FFXsubzone/FFX_Subzone.shp")) #load TAZ dataset

TAZ_geometry$centroids <- TAZ_geometry %>%
  st_centroid() %>% 
  st_transform(., 4326) %>% # move to standard lat lon WGS84 projection
  st_geometry()


bzone_lat_lon <- TAZ_geometry %>%
  mutate(Geo = TAZ_N,
        "Longitude" = st_coordinates(TAZ_geometry$centroids)[,1] ,
         "Latitude" = st_coordinates(TAZ_geometry$centroids)[,2]) %>%
  select(Geo, Latitude, Longitude ) %>%
  st_set_geometry(., NULL)
  


#duplicate 2019 data for 2045    
bzone_lat_lon_copy <- bzone_lat_lon
bzone_lat_lon$Year <- 2019
bzone_lat_lon_copy$Year <- 2045

#make final csv file and save to temp directory
bzone_lat_lon_final <- rbind(bzone_lat_lon, bzone_lat_lon_copy)  
write.csv(bzone_lat_lon_final, file.path(final, 'bzone_lat_lon.csv'), row.names = FALSE) #save as csv in final directory



################################################################################

# This section makes a plot to check if the centroids visually look like they are in the correct spot. 
TAZ_geometry <- st_transform(TAZ_geometry, 4326)
plot(st_geometry(TAZ_geometry))
#plot(TAZ_geometry[, 'centroids'], add = T, col = 'red', pch = 19)

plot(st_set_geometry(TAZ_geometry, 'centroids')[, 0], add = T, col = 'red', pch = 19)

