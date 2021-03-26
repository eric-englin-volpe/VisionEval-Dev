# Create geo.csv for VDOT work

library(foreign)
library(tidyverse)
library(sf)

root = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study'

# create geo.csv

TAZ_geometry <- st_read(file.path(root, "From_VDOT/FFXsubzone/FFX_Subzone.shp")) #load TAZ dataset
TAZ <- st_set_geometry(TAZ_geometry, NULL) #remove geometry field

geo = TAZ %>%
  select(NAME, TAZ_N) %>%
  rename(Azone = NAME,
         Bzone = TAZ_N) %>%
  mutate(Czone = NA,
         Marea = "NVTA")

write.csv(geo, 
          file = file.path(root, 'NVTA_Inputs_2020/defs/geo.csv'),
          row.names = F)

# Make file templates

years = c(2019, 2045)


azone_template = expand.grid(unique(geo$Azone), years)
colnames(azone_template) = c('Geo', 'Year')

write.csv(azone_template, 
          file = file.path(root, 'NVTA_Inputs_2020/azone_template.csv'),
          row.names = F)

bzone_template = expand.grid(unique(geo$Bzone), years)
colnames(bzone_template) = c('Geo', 'Year')

write.csv(bzone_template, 
          file = file.path(root, 'NVTA_Inputs_2020/bzone_template.csv'),
          row.names = F)

  