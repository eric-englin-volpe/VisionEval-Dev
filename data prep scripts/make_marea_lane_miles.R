# Make marea_lane_miles.csv

# https://github.com/VisionEval/VisionEval/blob/master/sources/modules/VETransportSupply/inst/module_docs/AssignRoadMiles.md#marea_lane_milescsv

# geo, year, FwyLaneMi, ArtLaneMi

proj_dir = '//vntscex/dfs/Projects/PROJ-HW32A1/Task 2.9 - SHRP/SHRP2 C10-C04-C05-C16/Implementation/VisionEval/VDOT_Case_Study/NVTA_Inputs_2020'

# Check to see if geo.csv exists; read marea from geo

geo_file = file.path(proj_dir, 'defs', 'geo.csv')

if(file.exists(geo_file)){
  geo <- read.csv(geo_file)
} else {
  source('create_geo.R')
}

marea_name = unique(geo$Marea)

if(length(marea_name) > 1){
  stop('Only implemented for one Marea currently')
}

# Check to see run_parameters.json exists, and read in the years from that file 
run_file = file.path(proj_dir, 'defs', 'run_parameters.json')

if(file.exists(run_file)){
  run_params <- jsonlite::fromJSON(run_file)
} else {
  stop('Create defs/run_parameters.json first.')
}

years = run_params$Years

# Get HPMS shapefile and drop in 'Data to Process'

# Virginia_PR_2018

vjs <- jsonlite::read_json(path = 'https://geo.dot.gov/server/rest/services/Hosted/Virginia_2018_PR/FeatureServer/layers?f=pjson')


# try geojsonsf 

# library(geojsonsf)

#vsf <- geojson_sf(vjs)


# Nope -- just use 2017 layer

