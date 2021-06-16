
# run the models created by Create_NVTA_Model_Scenarios_All.R script
# utilize Scenario_Status.csv file for model name and path

library(tidyverse)

#read in csv 
csvpath <- file.path(ve.runtime,"models","Scenario_Status.csv")
data <- read.csv(csvpath)


# go through models in csv and run them each
for(i in 1:nrow(data)){
  print(i)
  
  name <- data[i,"name"]
  
  #model <- openModel(file.path(ve.runtime,"models",name))
  model <- openModel(name)
  model$run()


 
}

