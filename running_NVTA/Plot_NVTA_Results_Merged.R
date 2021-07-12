
# This sections sets the location of the VisionEval installation for each of the Volpe team members.
# The result is that the object `ve.runtime` is the full path to where VisionEval is installed, and the results from the NVTA scenarios are located. 
ve.runtime <- ifelse(grepl('Flynn', normalizePath('~/')), 
                     file.path(dirname('~/'), 'Desktop/VE_4-1-0/'),
                     ifelse(grepl('lohmar', normalizePath('~/')), 
                            'C:/VisionEval',
                            ifelse(grepl('englin', normalizePath('~/')), 
                                   file.path('C:/Users/eric.englin/Desktop/VisionEval/4_04_v3/'),
                                   NA)))

# Make a directory to save JPEG outputs as well
fig_save = file.path(ve.runtime, 'models', 'NVTA_Figures')
if(!dir.exists(fig_save)) { dir.create(fig_save) }


library(tidyverse)
library(plotly) #install.packages('plotly')

# Read in scenario descriptions

scenario_desc <- readxl::read_excel(file.path(ve.runtime,
                                              'models',
                                              'Scenarios_Overview.xlsx'))

# clean up trailing whitespace
scenario_values <- scenario_desc %>%
  mutate(`Model Run Name` = gsub('\\s+', '', `Model Run Name`))

scenario_values <- scenario_values %>%
  mutate(ModelNameLabel = ifelse(scenario_values$Level == 'NA' |
        str_detect(scenario_values$Folder,'C0\\d'), scenario_values$`Model Run Name`,
        paste0(scenario_values$`Model Run Name`, scenario_values$Level)))

scenario_values <- scenario_values %>%
  mutate(`ModelNameLabel` = gsub('\\s+', '', `ModelNameLabel`))


## DVMT Plots

### Percent change 


marea_results <- read.csv(file.path(ve.runtime, 'models', 'Scenario_Metrics_Marea.csv'))

marea_results <- marea_results %>% 
  mutate(`modelName` = gsub('\\s+', '', `modelName`)) %>% 
  mutate(model_run_no = str_extract(modelName, '[A-Z]\\d{1,3}$')) %>% 
  left_join(scenario_values, by = c("modelName" = "ModelNameLabel")) 


marea_results <- marea_results %>%   
  mutate(modelName = ifelse(modelName == 'VERSPM_NVTA', 
                            '0 - Base',
                            paste(model_run_no, `Short Name`, sep = ' - ')))

#info for regional dvmt graphic

dvmt <- marea_results %>%
  select(modelName, contains('Dvmt')) %>%
  rowwise() %>%
  mutate(regionalDVMT = sum(c_across(contains('Dvmt'))))

dvmtbase = as.numeric(dvmt %>% filter(modelName == '0 - Base') %>% select(regionalDVMT))
#dvmtbase = as.numeric(dvmt %>% filter(modelName == 'VERSPM_NVTA') %>% select(regionalDVMT))

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
  scale_fill_manual(values = c('midnightblue', 'tomato'),
                    guide = F) +
  labs(title = "Percent Change Regional DVMT", x = "Scenario", y = "Percent Change") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gp1
# Save the output figure to disk for inclusion in a manuscript
ggsave(file.path(fig_save, 'DVMT_Rel_Change.jpeg'))

# And plot dynamically for viewing.
ggplotly(gp1, tooltip = 'text')


### Absolute change

gp2 <- ggplot(dvmt, aes(x = modelName, y = regDVMTAbsDif, fill = color,
                        text = mi_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(values = c('midnightblue', 'tomato'),
                    guide = F) +
  labs(title = "Absolute Change Regional DVMT (mi/day)", x = "Scenario", y = "Absolute Change")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_save, 'DVMT_Abs_Change.jpeg'))

ggplotly(gp2, tooltip = 'text')

## Energy Usage plots

### Truck energy usage relative change


#info for commercial heavy truck gge (sum columns k-m)
# use `matches()` to use regular expressions first to find variable names that start with 
# ComSvc or HvyTrk, and then a second one to find only those which also have GGE at the end
gge <- marea_results %>%
  select(modelName, matches('^(ComSvc|HvyTrk)')) %>%
  select(modelName, matches('GGE$')) %>%
  rowwise() %>%
  mutate(ComHvyTruckGGE = sum(c_across(contains('GGE'))))

ggebase = as.numeric(gge %>% filter(modelName == '0 - Base') %>% select(ComHvyTruckGGE))
#ggebase = as.numeric(gge %>% filter(modelName == 'VERSPM_NVTA') %>% select(ComHvyTruckGGE))

gge <- gge %>%
  mutate(ComHvyTruckGGEAbsDif = ComHvyTruckGGE - ggebase,
         ComHvyTruckGGEPerChange = (ComHvyTruckGGEAbsDif/ggebase)*100,
         sign = ifelse(ComHvyTruckGGEPerChange > 0, "pos","neg"),
         pct_text = paste('Scenario:', modelName, '\n', round(ComHvyTruckGGEPerChange, 2), '%'),
         mi_text = paste('Scenario:', modelName, '\n', format(round(ComHvyTruckGGEAbsDif, 2),
                                                              scientific = F, 
                                                              big.mark = ','), 'Gasoline gallone equiv.'))

# View(gge)

gp3 <- ggplot(gge, aes(x = modelName, y = ComHvyTruckGGEPerChange, fill = sign,
                       text = pct_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(values = c('midnightblue', 'tomato'),
                    guide = F) +
  labs(title = "Percent Change Commercial Service and  Heavy Truck GGE", x = "Scenario", y = "Percent Change")+
  theme(axis.text.x = element_text(angle = 45))

ggsave(file.path(fig_save, 'Truck_GGE_Pct_Change.jpeg'))

ggplotly(gp3, tooltip = 'text')



# Household Data Visualization

## Energy consumption - gasoline equivalents

# Use `cache = T` for faster loading
hhfile <- readr::read_csv(file.path(ve.runtime, 'models', 'Scenario_Metrics_Hh.csv'))

# Join in the scenario names
hhfile <- hhfile %>%
  mutate(`modelName` = gsub('\\s+', '', `modelName`)) %>% 
  mutate(model_run_no = str_extract(modelName, '[A-Z]\\d{1,3}$')) %>% 
  left_join(scenario_values, by = c("modelName" = "ModelNameLabel")) 

hhfile <- hhfile %>%
  mutate(modelName = ifelse(modelName == 'VERSPM_NVTA', 
                            '0 - Base',
                            paste(model_run_no, `Short Name`, sep = ' - ')))



# Calculate mode split
hhfile <- hhfile %>%
  rowwise() %>%
  mutate(total_trips = sum(WalkTrips, BikeTrips, TransitTrips, VehicleTrips),
         pct_Walk = WalkTrips / total_trips,
         pct_Bike = BikeTrips / total_trips,
         pct_Transit = TransitTrips / total_trips,
         pct_Vehicle = VehicleTrips / total_trips)

scenario_values <- hhfile %>%
  filter(modelName != '0 - Base') %>% # exclude the base scenario
  group_by(modelName) %>%
  summarize(sum_Hh_GGE = sum(DailyGGE),
            sum_Hh_kWh = sum(DailyKWH),
            sum_Hh_CO2e = sum(DailyCO2e),
            median_Income = median(Income),
            median_OwnCost = median(OwnCost, na.rm = T),
            median_pct_Walk = median(pct_Walk),
            median_pct_Bike = median(pct_Bike),
            median_pct_Transit = median(pct_Transit),
            median_pct_Vehicle = median(pct_Vehicle)
            )

base_values <- hhfile %>%
  filter(modelName == "0 - Base") %>%
  group_by(modelName) %>%
  summarize(base_Hh_GGE = sum(DailyGGE),
            base_Hh_kWh = sum(DailyKWH),
            base_Hh_CO2e = sum(DailyCO2e),
            median_Income = median(Income),
            median_OwnCost = median(OwnCost, na.rm = T),
            median_pct_Walk = median(pct_Walk),
            median_pct_Bike = median(pct_Bike),
            median_pct_Transit = median(pct_Transit),
            median_pct_Vehicle = median(pct_Vehicle)
            )

# We use base_values$... to compare the scenario values with the corresponding base values data frame

scenario_values <- scenario_values %>%
  mutate(GGEPerChange = ((sum_Hh_GGE - base_values$base_Hh_GGE) / base_values$base_Hh_GGE) * 100,
         KWHPerChange = ((sum_Hh_kWh - base_values$base_Hh_kWh) / base_values$base_Hh_kWh) * 100,
         CO2PerChange = ((sum_Hh_CO2e - base_values$base_Hh_CO2e) / base_values$base_Hh_CO2e) * 100,
         gge_color= ifelse(GGEPerChange > 0, "pos","neg"),
         gge_pct_text = paste('Scenario:', modelName, '\n', round(GGEPerChange, 2), '%'),
         gge_text = paste('Scenario:', modelName, '\n', format(round(sum_Hh_GGE, 2),
                                                               scientific = F, 
                                                               big.mark = ','), 'Gasoline gallone equiv.'),
         kwh_color= ifelse(KWHPerChange > 0, "pos","neg"),
         kwh_pct_text = paste('Scenario:', modelName, '\n', round(KWHPerChange, 2), '%'),
         kwh_text = paste('Scenario:', modelName, '\n', format(round(sum_Hh_kWh, 2),
                                                               scientific = F, 
                                                               big.mark = ','), 'kWh'),
         co2_color= ifelse(CO2PerChange > 0, "pos","neg"),
         co2_pct_text = paste('Scenario:', modelName, '\n', round(CO2PerChange, 2), '%'),
         co2_text = paste('Scenario:', modelName, '\n', format(round(sum_Hh_CO2e, 2),
                                                               scientific = F, 
                                                               big.mark = ','), 'g CO[2] equivalent'))


gp4 <- ggplot(scenario_values, aes(x = modelName, y = sum_Hh_GGE,
                                   text = gge_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Regional daily gasoline gallon equivalents", x = "Scenario", y = "GGE/Day")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_save, 'Regional_GGE_Total.jpeg'))

ggplotly(gp4, tooltip = 'text')

gp5 <- ggplot(scenario_values, aes(x = modelName, y = GGEPerChange,
                                   text = gge_pct_text, fill = gge_color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(values = c('midnightblue', 'tomato'),
                    guide = F) +
  labs(title = "Regional daily gasoline gallon equivalents % Change", x = "Scenario", y = "GGE/Day")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_save, 'Regional_GGE_Percent_Change.jpeg'))

ggplotly(gp5, tooltip = 'text')

### Energy consumption - electricity 


gp6 <- ggplot(scenario_values, aes(x = modelName, y = sum_Hh_kWh,
                                   text = kwh_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Regional KWH", x = "Scenario", y = "kWh/Day")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_save, 'Regional_kWh_Total.jpeg'))


ggplotly(gp6, tooltip = 'text')


gp7 <- ggplot(scenario_values, aes(x = modelName, y = KWHPerChange,
                                   text = kwh_pct_text, fill = kwh_color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  scale_fill_manual(values = c('midnightblue', 'tomato'),
                    guide = F) +
  labs(title = "Regional KWH Percent Change", x = "Scenario", y = "kWh/Day")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggsave(file.path(fig_save, 'Regional_kWh_Percent_Change.jpeg'))

ggplotly(gp7, tooltip = 'text')


### Emissions

gp8 <- ggplot(scenario_values, aes(x = modelName, y = sum_Hh_CO2e,
                                   text = co2_text)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Regional CO[2]e", x = "Model Name", y ="g CO[2] equivalent / Day")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file.path(fig_save, 'Regional_CO2e_Total.jpeg'))

ggplotly(gp8, tooltip = 'text')

gp9 <- ggplot(scenario_values, aes(x = modelName, y = CO2PerChange,
                                   text = co2_pct_text, fill = co2_color)) + 
  geom_bar(stat="identity") + theme_minimal() +
  labs(title = "Regional CO[2]e Percent Change", x = "Model Name", y ="g CO[2] equivalent / Day")+
  scale_fill_manual(values = c('midnightblue', 'tomato'),
                    guide = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file.path(fig_save, 'Regional_CO2e_Percent_Change.jpeg'))

ggplotly(gp9, tooltip = 'text')
