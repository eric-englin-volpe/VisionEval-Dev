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
  print(item)
  name <- paste0("models/scenario_inputs/",item)
  models <- list.files(name, full.names = FALSE, recursive = FALSE,
                      pattern ="[2-9]")
  
  # run through each case excluding the 1 case
  for (case in models){
    print(case)
    
    # create the model name i.e. VERSPM_NVTA_A2
    base <- openModel('VERSPM_NVTA')
    modelName <- paste0('VERSPM_NVTA_',item,case)
    print(modelName)
    
    locName <- paste0(name,"/",case)
    print(locName)
    toChange <- list.files(locName,full.names = FALSE)
    print(toChange)
    
    #create the model run folder
    if(!dir.exists(paste0("/models",modelName))){
    runningModel <- base$copy(modelName)
    }
    
    #prepare to copy over user changed input files
    
    for (f in toChange){
      from <- paste0("C:/VisionEval/",locName,"/",f)
      print(from)
      to <- paste0("C:/VisionEval/models/",modelName,"/inputs/",f)
      print(to)
      
      #verify the to and from files are different
      stopifnot(md5sum(from)!=md5sum(to))
      file.copy(from,to, overwrite =TRUE)
    }
  

    # open and run the model
    # runningModel <- openModel(modelName)
    # runningModel$run()
  }

}

