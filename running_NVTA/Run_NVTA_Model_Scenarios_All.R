
# run the models created by Create_NVTA_Model_Scenarios_All.R script
# utilize Scenario_Status.csv file for model name and path

# read in csv 
csvpath <- file.path(ve.runtime,"models","Scenario_Status.csv")
data <- read.csv(csvpath)


# go through models in csv and run them each
for(i in 1:nrow(data)){
  
  name <- data[i, "name"]
  
  cat('Running', name, '\n\n')
  
  model <- openModel(name)
  model$run()

  # Update status when complete
  data[i, 'status'] <- model$status

}

# Update all model status
write.csv(data, csvpath, row.names = F)

