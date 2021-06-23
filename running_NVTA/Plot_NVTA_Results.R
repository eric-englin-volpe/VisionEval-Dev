library(ggplot2)
library(tidyverse)

#relies on scenario file with base included, as of now plots gave error regarding file
# path when put into an RMarkdown file

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

dvmtplot1 <- data.frame(Model = dvmt$modelName,PercentChange = dvmt$regDVMTPerChange, 
                        color = dvmt$color)

ggplot(dvmtplot1, aes(x=Model, y=PercentChange, fill = color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Percent Change Regional DVMT", x = "Model Name", y = "Percent Change")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dvmtplot2 <- data.frame(Model = dvmt$modelName,AbsoluteChange = dvmt$regDVMTAbsDif, 
                        color = dvmt$color)

ggplot(dvmtplot2, aes(x=Model, y=AbsoluteChange, fill = color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Absolute Change Regional DVMT", x = "Model Name", y = "Absolute Change")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#info for commercial heavy truck gge
gge <- readfile[ , c(1,11,12,13)]
gge$modelName = substring(gge$modelName,8,15)
gge[1,1] = "NVTA_Base"
gge$ComHvyTruckGGE = rowSums(gge[,c(-1)])
ggebase = gge[1,5]
gge$ComHvyTruckGGEAbsDif = gge$ComHvyTruckGGE - ggebase
gge$ComHvyTruckGGEPerChange = (gge$ComHvyTruckGGEAbsDif/ggebase) *100
View(gge)


ggeplot1 <- data.frame(Model = gge$modelName,PercentChange = gge$ComHvyTruckGGEPerChange)

ggplot(ggeplot1, aes(x=Model, y=PercentChange)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Percent Change Com Hvy Truck GGE", x = "Model Name", y ="Percent Change")+
  theme(axis.text.x = element_text(angle = 45))


# Household Data Visualization
hhfile <- read.csv(file.path(ve.runtime, 'models', 'Scenario_Metrics_Hh_with_Base.csv'))
baseNVTA <- hhfile[hhfile$modelName=="VERSPM_NVTA",]
View(baseNVTA)


csvpath <- file.path(ve.runtime,"models","Scenario_Status.csv")
data <- read.csv(csvpath)

hh_mean_stat <- data.frame()

for(i in 1:nrow(data)){
  name <- data[i, "name"]
  print(name)
  
  stats <- c(name,mean(hhfile$DailyGGE[hhfile$modelName==name]),
  mean(hhfile$DailyKWH[hhfile$modelName==name]),
  mean(hhfile$DailyCO2e[hhfile$modelName==name]))
  
  hh_mean_stat <- rbind(hh_mean_stat,stats)
  
}
colnames(hh_mean_stat) <- c("ModelName", "AvgHhDailyGGE", 
                            "Avg Hh Daily KWH (GGE/day)",
                            "Avg Hh Daily CO2e (kg/day)")
rownames(hh_mean_stat) <- NULL

baseGGE = hh_mean_stat[1,2]
hh_mean_stat$GGEPerChange = (hh_mean_stat$AvgHhDailyGGE - baseGGE)

View(hh_mean_stat)


