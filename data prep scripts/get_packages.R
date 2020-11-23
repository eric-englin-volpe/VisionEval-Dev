# Get all necessary packages across data prep and analysis scripts 
# add any packages your scripts require here. Keep in alphabetical order.

loadpacks <- c(
  'dplyr', 
  'tidyverse', 
  'tidycensus', 
  'viridis', 
  'leaflet', 
  'readr', 
  'sp', 
  'sf'
  )

for(i in loadpacks){
  if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)
  }
rm(i, loadpacks)
