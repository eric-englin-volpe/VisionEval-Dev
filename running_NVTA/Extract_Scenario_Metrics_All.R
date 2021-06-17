
#Extract metrics from all run models in ScenarioStatus.csv and save the results

extract_scenario_metrics <- function(modelName, Year = '2045'){
  # Will return an error if the model doesn't exist yet
  mod <- openModel(modelName) 
  
  # Set groups to only the future year
  mod$groups <- Year
  
  # First extract Marea outputs
  mod$tables <- 'Marea'
  mod$fields <- c('UrbanHhDvmt',
                  'TownHhDvmt',
                  'RuralHhDvmt',
                  'HvyTrkUrbanDvmt',
                  'ComSvcUrbanDvmt',
                  'ComSvcTownDvmt',
                  'ComSvcRuralDvmt',
                  'LdvTotDelay',
                  'HvyTrkDelay',
                  'BusTotDelay',
                  'ComSvcUrbanGGE',
                  'ComSvcNonUrbanGGE',
                  'HvyTrkUrbanGGE',
                  'ComSvcUrbanKWH',
                  'ComSvcNonUrbanKWH',
                  'HvyTrkUrbanKWH',
                  'ComSvcUrbanCO2e',
                  'ComSvcNonUrbanCO2e',
                  'HvyTrkUrbanCO2e',
                  'BusGGE',
                  'RailGGE',
                  'VanGGE',
                  'BusKWH',
                  'RailKWH',
                  'VanKWH',
                  'BusCO2e',
                  'RailCO2e',
                  'VanCO2e'
  )
  # Review the selections:
  cat('Extracting \t', mod$groupsSelected, '\t',
      mod$tablesSelected, '\n', 
      paste(mod$fieldsSelected, collapse = '\t'))
  
  marea_results <- mod$extract(saveTo = F, quiet = T)
  
  # Household level
  # First clear the selections
  mod$tables <- ''
  mod$fields <- ''
  
  mod$tables <- 'Household'
  mod$fields <- c('DailyGGE',
                  'DailyKWH',
                  'DailyCO2e')
  
  cat('Extracting \t', mod$groupsSelected, '\t',
      mod$tablesSelected, '\n', 
      paste(mod$fieldsSelected, collapse = '\t'))
  
  hh_results <- mod$extract(saveTo = F, quiet = T)
  
  # Save output as a list of two data frames: Marea and Household level
  results = list(Marea = data.frame(modelName, marea_results[[1]]),
                 Hh = data.frame(modelName, hh_results[[1]]))
  
  results
}

#### Looping through the run scenarios to extract the information

#read in csv 
csvpath <- file.path(ve.runtime,"models","Scenario_Status.csv")
data <- read.csv(csvpath)

marea_compiled <- vector()
hh_compiled <- vector()


# go through models in csv and run them each
for(i in 1:nrow(data)){
  name <- data[i,"name"]
  cat("Extracting statistics from", name, '\n')
  results_2045 <- extract_scenario_metrics(name)
  
  marea <- results_2045[[1]] # Marea
  hh    <- results_2045[[2]] # Household
  
  marea_compiled <- rbind(marea_compiled, marea)
  hh_compiled <- rbind(hh_compiled, hh)
  
}


write.csv(marea_compiled, file.path(ve.runtime, 'models', 'Scenario_Metrics_Marea.csv'),
          row.names = F)
write.csv(hh_compiled,  file.path(ve.runtime, 'models', 'Scenario_Metrics_Hh.csv'),
          row.names = F)

View(marea_compiled)
View(hh_compiled)
