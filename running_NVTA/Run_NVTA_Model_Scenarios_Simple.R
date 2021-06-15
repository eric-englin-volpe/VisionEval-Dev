library(tidyverse)
library(tools)

#Set Up Info --
#scenario_inputs was downloaded from the VDOT Google drive and placed
# in the /models directory of Vision Eval
#script expects VERSPM_NVTA Model already created

#get the list of A-P scenarios
files <- list.dirs("models/scenario_inputs",full.names = FALSE,
                   recursive = FALSE)

#iterate through the A-P scenarios
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
    
    # prepare to copy over user changed input files
    # Use ve.runtime to locate where VisionEval is installed
    
    for (f in toChange){
      from <- file.path(ve.runtime, locName, f)
      print(from)
      to <- file.path(ve.runtime, 'models', modelName, "inputs", f)
      print(to)
      
      # verify the to and from files are different
      stopifnot(md5sum(from) != md5sum(to))
      file.copy(from, to, overwrite = TRUE)
    }
  

    # open and run the model
    # runningModel <- openModel(modelName)
    # runningModel$run()
  }

}

