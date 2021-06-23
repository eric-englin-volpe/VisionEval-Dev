library(tidyverse)
library(tools)

#Set Up Info --
#scenario_inputs was downloaded from the VDOT Google drive and placed
# in the /models directory of Vision Eval
#updated run_parameters.json placed in /scenario_inputs directory
#script expects VERSPM_NVTA Model already created

#get the list of A-P scenarios
files <- list.dirs("models/scenario_inputs",full.names = FALSE,
                   recursive = FALSE)



# iterate through the A-P scenarios
# Create a dataframe of scenario names
modelNames <- vector()

for (item in files){
  cat('Preparing Scenario', item, '\n\n')
  name <- paste0("models/scenario_inputs/",item)
  models <- list.files(name, full.names = FALSE, recursive = FALSE,
                      pattern ="[2-9]")
  
  # run through each case excluding the 1 case
  for (case in models){
    cat('\tPreparing level', case, '\t')
    
    # create the model name i.e. VERSPM_NVTA_A2
    base <- openModel('VERSPM_NVTA')
    modelName <- paste0('VERSPM_NVTA_',item,case)
    print(modelName)
    
    locName <- paste0(name,"/", case)
    cat('Saving to', locName)
    toChange <- list.files(locName,full.names = FALSE)
    cat('Modifying file(s):\n', paste(toChange, collapse = '\n'), '\n')
    
    #create the model run folder
    if(!dir.exists(file.path(ve.runtime, "models", modelName))){
      runningModel <- base$copy(modelName)
    }
    
    #copy in modified 'run_parameters.json' for single year
    #origin <- file.path(ve.runtime,"models","scenario_inputs","run_parameters.json")
    #dest <- file.path(ve.runtime,"models",modelName,"defs","run_parameters.json")
    
    #param <- file.copy(origin, dest, overwrite = TRUE)
    
    
    # prepare to copy over user changed input files
    # Use ve.runtime to locate where VisionEval is installed
    
    for (f in toChange){
      from <- file.path(ve.runtime, locName, f)
      print(from)
      to <- file.path(ve.runtime, 'models', modelName, "inputs", f)
      print(to)
      
      # Check to see if the 'to' exists; some inputs are optional
      if(file.exists(to)){
        # verify the to and from files are different
        stopifnot(md5sum(from) != md5sum(to))
      }
      
      # Save to 'msg' to suppress the printing of the result ('TRUE')
      msg <- file.copy(from, to, overwrite = TRUE)
    }
  
    
  modelNames <- rbind(modelNames, data.frame(name = modelName,
                                             files = paste(toChange, collapse = ", "),
                                             location = runningModel$modelPath,
                                             status = runningModel$status))
  }

write.csv(modelNames, file.path(ve.runtime, 'models', 'Scenario_Status.csv'), row.names = F)
}

