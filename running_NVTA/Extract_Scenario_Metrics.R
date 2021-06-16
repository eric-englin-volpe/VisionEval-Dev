# Extract model outputs from NVTA scenarios

# Marea:
# - Total household DVMT 
# - Heavy truck DVMT
# - Commerical service DVMT
# - Delays: LDV, HvyTrk, Bus
# - CO2e: commerical service urban and nonurban;
#         heavy truck urban;
#         bus + rail + van CO2
# - Total energy use: GGE, kWh for commerical service, heavy truck, and transit

# Household: 
# - DailyCO2e
# - Total energy use: GGE, kWh

# modelName <- 'VERSPM_NVTA' # can be populated from scenario tracking data frame

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


# Usage ----
RUN_EXAMPLE = F

if(RUN_EXAMPLE){
  results_2045 <- extract_scenario_metrics('VERSPM_NVTA')
  
  results_2045[[1]] # Marea
  
  head(results_2045[[2]]) # Household
  # In both, we save the model name because then we can rbind dataframes together across multiple models.
  
  results_base <- extract_scenario_metrics('VERSPM_NVTA', Year = 2019)

  View(marea_compare <- rbind(data.frame(Year = 2019, results_base[[1]]),
                              data.frame(Year = 2045, results_2045[[1]])))
    
}  
