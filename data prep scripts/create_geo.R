# Create geo.csv for VDOT work

library(foreign)
library(tidyverse)

root = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study'

# create geo.csv

taz <- read.dbf(file.path(root, 'From_VDOT/SUBZDATA_2019.dbf'))

mwcog <- read.dbf(file.path(root, 'From_VDOT/MWCOG_TAZ/TPBTAZ3722_TPBMod.dbf'))

# Counties to use
use_co = c('Fairfax',
           'Falls Church',
           'Fairfax City')

geo = mwcog %>%
  filter(NAME %in% use_co) %>%
  select(NAME, TAZ) %>%
  rename(Azone = NAME,
         Bzone = TAZ) %>%
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

  