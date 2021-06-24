library(ggplot2)
library(tidyverse)

#relies on scenario file with base included, as of now plots gave error regarding file
# path when put into an RMarkdown file

ve.runtime <- ifelse(grepl('Flynn', normalizePath('~/')), 
                     file.path(dirname('~/'), 'Desktop/VE_4-1-0/'),
                     ifelse(grepl('Lohmar', normalizePath('~/')), 
                            file.path(dirname('~/'), '<PATH_TO_SARAH_VISIONEVAL>'),
                            ifelse(grepl('englin', normalizePath('~/')), 
                                   file.path('C:/Users/eric.englin/Desktop/VisionEval/4_04_v3/'),
                                   NA)))

# Currently working on Percent Differences for Average Household statistics

readfile <- read.csv(file.path(ve.runtime, 'models', 'Scenario_Metrics_Marea.csv'))

#info for regional dvmt graphic 
dvmt <- readfile[ ,c(1,2,3,4,5,6,7,8)]
dvmt$regionalDVMT = rowSums(dvmt[,c(-1)])
dvmtbase = dvmt[1,9]
dvmt$modelName = substring(dvmt$modelName,8,15)
dvmt[1,1] = "NVTA_Base"

ffx_county_car_2045 = 27813424
ffx_city_car_2045 = 393417
fall_church_car_2045 = 151470
ffx_county_truck_2045 = 5614408
ffx_city_truck_2045 = 51371
fall_church_truck_2045 = 24944

MWCOG_model_dvmt = ffx_county_car_2045+ffx_city_car_2045+fall_church_car_2045+ffx_county_truck_2045+ffx_city_truck_2045+fall_church_truck_2045

#trip_generation_summary_motorized_dvmt = 3649551
#trip_generation_summary_non_motorized_dvmt = 317247

dvmt$regDVMTAbsDif = dvmt$regionalDVMT - MWCOG_model_dvmt
dvmt$regDVMTPerChange = (dvmt$regDVMTAbsDif/MWCOG_model_dvmt)*100
dvmt$color <- ifelse(dvmt$regDVMTPerChange > 0, "red","green")

dvmtplot1 <- data.frame(Model = dvmt$modelName,PercentChange = dvmt$regDVMTPerChange, 
                        color = dvmt$color)

ggplot(dvmtplot1, aes(x=Model, y=PercentChange, fill = color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Percent Change VisionEval DVMT and MWCOG MOES Base Model for 2045", x = "Model Name", y = "Percent Change")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Heavy Trucks
MWCOG_model_dvmt_heavy_trucks = ffx_county_truck_2045+ffx_city_truck_2045+fall_church_truck_2045

#trip_generation_summary_motorized_dvmt = 3649551
#trip_generation_summary_non_motorized_dvmt = 317247

dvmt$regDVMTAbsDif = dvmt$HvyTrkUrbanDvmt - MWCOG_model_dvmt_heavy_trucks
dvmt$regDVMTPerChange = (dvmt$regDVMTAbsDif/MWCOG_model_dvmt_heavy_trucks)*100
dvmt$color <- ifelse(dvmt$regDVMTPerChange > 0, "red","green")

dvmtplot2 <- data.frame(Model = dvmt$modelName,PercentChange = dvmt$regDVMTPerChange, 
                        color = dvmt$color)

ggplot(dvmtplot2, aes(x=Model, y=PercentChange, fill = color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Percent Change VisionEval DVMT and MWCOG MOES Base Model for 2045", x = "Model Name", y = "Percent Change")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

