library(ggplot2)
library(tidyverse)

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

#info for commercial heavy truck gge
gge <- readfile[ , c(1,11,12,13)]
gge$modelName = substring(gge$modelName,8,15)
gge[1,1] = "NVTA_Base"
gge$ComHvyTruckGGE = rowSums(gge[,c(-1)])
ggebase = gge[1,5]



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



