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
dvmt$regDVMTAbsDif = dvmt$regionalDVMT - dvmtbase
dvmt$regDVMTPerChange = (dvmt$regDVMTAbsDif/dvmtbase)*100
dvmt$color <- ifelse(dvmt$regDVMTPerChange > 0, "red","green")
