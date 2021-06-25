library(plotly)
library(tidyverse)

#relies on scenario file with base included, as of now plots gave error regarding file
# path when put into an RMarkdown file

ve.runtime <- ifelse(grepl('Flynn', normalizePath('~/')), 
                     file.path(dirname('~/'), 'Desktop/VE_4-1-0/'),
                     ifelse(grepl('Lohmar', normalizePath('~/')), 
                            'C:/VisionEval',
                            ifelse(grepl('englin', normalizePath('~/')), 
                                   'C:/Users/eric.englin/Desktop/VisionEval/4_04_v3/',
                                   NA)))

# Currently working on Percent Differences for Average Household statistics

readfile <- read.csv(file.path(ve.runtime, 'models', 'Scenario_Metrics_Marea_with_Base.csv'))

#info for regional dvmt graphic 
dvmt <- readfile %>%
  select(modelName, contains('Dvmt')) %>%
  rowwise() %>%
  mutate(modelName = sub('VERSPM_', '', modelName)) %>%
  mutate(regionalDVMT = sum(c_across(contains('Dvmt'))))

dvmt$modelName[dvmt$modelName == 'NVTA'] = "Base"

dvmtbase = as.numeric(dvmt %>% filter(modelName == 'Base') %>% select(regionalDVMT))


dvmt <- dvmt %>%
  mutate(regDVMTAbsDif = regionalDVMT - dvmtbase,
         regDVMTPerChange = (regDVMTAbsDif/dvmtbase)*100,
         color = ifelse(regDVMTPerChange > 0, "pos","neg"),
         pct_text = paste('Scenario:', modelName, '\n', round(regDVMTPerChange, 2), '%'),
         mi_text = paste('Scenario:', modelName, '\n', format(round(regDVMTAbsDif, 2),
                                                              scientific = F, 
                                                              big.mark = ','), 'mi/day'))

gp1 <- ggplot(dvmt, aes(x = modelName, y = regDVMTPerChange, fill = color,
                        text = pct_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(values = c('lightgreen', 'tomato'),
                    guide = F) +
  labs(title = "Percent Change Regional DVMT", x = "Scenario", y = "Percent Change") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gp2 <- ggplot(dvmt, aes(x = modelName, y = regDVMTAbsDif, fill = color,
                        text = mi_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(values = c('lightgreen', 'tomato'),
                    guide = F) +
  labs(title = "Absolute Change Regional DVMT (mi/day)", x = "Scenario", y = "Absolute Change")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gp1, tooltip = 'text')

ggplotly(gp2, tooltip = 'text')

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


