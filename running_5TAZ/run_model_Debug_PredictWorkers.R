#===========
#run_model.R - Modified to manually check the PredictWorkers
#===========

# Assumes model directory is named VERSPM_5TAZ, change as appropriate for how you named the model directory
# Assumes that VisionEval was launched in RStudio by double-clicking 'VisionEval.Rproj'. By doing this, the working directory is set to the location of your VisionEval folder on your machine. 

library(tidyverse)

model_dir <- 'models/VERSPM_5TAZ' # Change as appropriate

setwd(model_dir)

#--------------
library(visioneval)

planType <- 'callr'

#Initialize model
#----------------
initializeModel(
  ModelScriptFile = "run_model.R",
  ParamDir = "defs",
  RunParamFile = "run_parameters.json",
  GeoFile = "geo.csv",
  ModelParamFile = "model_parameters.json",
  LoadDatastore = FALSE,
  DatastoreName = NULL,
  SaveDatastore = TRUE
  )  
cat('run_model.R: initializeModel completed\n')

#---------------------------------
# Run for just 2019
Year = '2019'

runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
runModule("PredictWorkers",                  "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year) 

# !!!!!!!!!!
# Error in names(object) <- nm : 
# 'names' attribute [2] must be the same length as the vector [0]

runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
runModule("PredictIncome",                   "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
runModule("PredictHousing",                  "VELandUse",             RunFor = "AllYears",    RunYear = Year)
runModule("LocateEmployment",                "VELandUse",             RunFor = "AllYears",    RunYear = Year)
runModule("AssignLocTypes",                  "VELandUse",             RunFor = "AllYears",    RunYear = Year)

#---------------------------------

# see runModule for these steps, in sources/framework/visioneval/R/visioneval.R
# this writes to the data store using setInDatastore, in sources/framework/visioneval/R/datastore.R
ModuleName = 'PredictWorkers'
PackageName = 'VESimHouseholds'
RunFor = "AllYears"
RunYear = Year

BaseYear <- getModelState()$BaseYear # 2019
Function <- paste0(PackageName, "::", ModuleName)
Specs <- paste0(PackageName, "::", ModuleName, "Specifications")
M <- list()

M$Func <- eval(parse(text = Function))
M$Specs <- processModuleSpecs(eval(parse(text = Specs)))

if (is.list(M$Specs$Call)) {
  Call <- list(Func = list(), Specs = list())
  for (Alias in names(M$Specs$Call)) {
    Function <- M$Specs$Call[[Alias]]
    if (length(unlist(strsplit(Function, "::"))) == 
        1) {
      Pkg_df <- getModelState()$ModuleCalls_df
      if (sum(Pkg_df$Module == Function) != 0) {
        Pkg_df <- getModelState()$ModuleCalls_df
        Function <- paste(Pkg_df$Package[Pkg_df$Module == 
                                           Function], Function, sep = "::")
        rm(Pkg_df)
      }
      else {
        Pkg_df <- getModelState()$ModulesByPackage_df
        Function <- paste(Pkg_df$Package[Pkg_df$Module == 
                                           Function], Function, sep = "::")
        rm(Pkg_df)
      }
    }
    Specs <- paste0(Function, "Specifications")
    Call$Func[[Alias]] <- eval(parse(text = Function))
    Call$Specs[[Alias]] <- processModuleSpecs(eval(parse(text = Specs)))
    Call$Specs[[Alias]]$RunBy <- M$Specs$RunBy
  }
}
Errors_ <- character(0)
Warnings_ <- character(0)

# Get the List from the Datastore here
L <- getFromDatastore(M$Specs, RunYear = Year)

# Check the units after fetching from datastore, for one example attribute

M$Specs$Get[[8]]$NAME
M$Specs$Get[[8]]$UNITS



# -------------------------------
# Now manually running PredictWorkers steps here.

set.seed(L$G$Seed)
Ag <-
  c("Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")
Wk <- gsub("Age", "Wkr", Ag)
Re <- gsub("Age", "RelEmp", Ag)
Az <- as.vector(L$Year$Azone$Azone)

NumHh <- length(L$Year$Household$HhType)
#Initialize output list
Out_ls <- initDataList()
Out_ls$Year$Household <-
  list(
    Workers = integer(NumHh),
    Wkr15to19 = integer(NumHh),
    Wkr20to29 = integer(NumHh),
    Wkr30to54 = integer(NumHh),
    Wkr55to64 = integer(NumHh),
    Wkr65Plus = integer(NumHh)
  )
Out_ls$Year$Azone <-
  list(
    NumWkr = integer(length(L$Year$Azone))
  )

#Initialize Errors vector
Errors_ <- character(0)
#Define function to predict total number of workers for by household given
#N_ is a vector of the number of persons in the age group by household
#P_ is the the worker probability for the age group
#W is the total number of workers
getNumWkr <- function(N_, P_, W) {
  NumHh <- length(N_)
  NumWkr_Hh <- setNames(integer(NumHh), 1:NumHh) #Initialize count of workers
  HhIdx_Pr <- rep(1:length(N_), N_) #Vector of persons with household index
  PrsnProb_Pr <- rep(P_, N_) #Probability that each person is a worker
  WkrIdx_ <- sample(HhIdx_Pr, W, prob = PrsnProb_Pr ) #Sample household index
  WkrTab_ <- table(WkrIdx_) #Tabulate household index
  NumWkr_Hh[names(WkrTab_)] <- WkrTab_
  NumWkr_Hh
}
#Iterate through age groups and Azones and identify number of workers by
#age group for each household
PropHhWkr_HtAg <- VESimHouseholds::PropHhWkr_HtAg

# ERROR HAPPENS HERE - on pass 5 of 5 through the loop, for az 2
# Num of persons is all 0 for azone 2. So 

for (i in 1:length(Ag)) {
  NumWkr_Hh <- integer(NumHh)
  for (az in Az) {
    IsAz <- L$Year$Household$Azone == az
    NumWkr_Hh[IsAz] <- local({
      NumPrsn_ <- L$Year$Household[[Ag[i]]][IsAz]
      HhType_ <- L$Year$Household$HhType[IsAz]
      Probs_ <- PropHhWkr_HtAg[HhType_, Ag[i]]
      TotPrsn <- sum(NumPrsn_)
      RelEmp <- L$Year$Azone[[Re[i]]][L$Year$Azone$Azone == az]
      if (is.null(RelEmp)) RelEmp <- 1
      TotWkr <- round(sum(NumPrsn_ * Probs_) * RelEmp)
      if (TotWkr > TotPrsn) {
        #catch error
        MaxVal <- TotPrsn / sum(NumPrsn_ * Probs_)
        Msg <- paste0(
          "Error during run of PredictWorkers module! ",
          "The value of ", Re[i], " for Azone ", az, " in Year ", Year,
          " will result in more workers than people in that age category. ",
          "The maximum value of ", Re[i], " must be less than ",
          round(MaxVal, 2), ".")
        Errors_ <<- c(Errors_, Msg)
        NA
      } else {
        NumWkr_ <- integer(length(NumPrsn_))
        DoPredict_ <- NumPrsn_ > 0 & Probs_ > 0
        
        # ERROR HAPPENS HERE when DoPredict_ all FALSE
        # TODO: catch this error
        
        NumWkr_[DoPredict_] <-
          getNumWkr(NumPrsn_[DoPredict_], Probs_[DoPredict_], TotWkr)
        NumWkr_
      }
    })
    rm(IsAz)
  }
  Out_ls$Year$Household[[Wk[i]]] <- NumWkr_Hh
  rm(az)
}
rm(i)
#Add error messages if any
if (length(Errors_) != 0) {
  addErrorMsg("Out_ls", Errors_)
}
#Calculate the total number of workers
Out_ls$Year$Household$Workers <-
  mapply(sum,
         Out_ls$Year$Household$Wkr15to19,
         Out_ls$Year$Household$Wkr20to29,
         Out_ls$Year$Household$Wkr30to54,
         Out_ls$Year$Household$Wkr55to64,
         Out_ls$Year$Household$Wkr65Plus)
#Calculate the total number of workers by Azone
Out_ls$Year$Azone$NumWkr <-
  tapply(Out_ls$Year$Household$Workers, L$Year$Household$Azone, sum)[Az]
#Return the results
Out_ls


# -------------------------------
# Back to runModule steps 
# !!! This is the step that runs Calculate4DMeasures on the list L

R <- M$Func(L)
  
# Examine results from the runModule approach to this step
R$Year$Bzone$D1B[L$Year$Bzone$Bzone == 'Eug-41039003700'] # 54.2642, same as before

# Ok, so far looks good. Now use the standard approach to calling Calculate4DMeasures, then we will examine the ouput in the datastore

runModule("Calculate4DMeasures",             "VELandUse",             RunFor = "AllYears",    RunYear = Year)

# Now look at the output in the datastore
output_to_load = 'Datastore/2010/Bzone/D1B.Rda'

load(file.path(output_to_load))

attr(Dataset, 'UNITS') # Persons per square mile now!
Dataset[Bz == 'Eug-41039003700'] # 34729.09 persons per square mile

# When did the conversion happen? Multiple times, in fact.
# First, in initialzeModel, the default area is set to the units in defs/units.csv
# When running getFromDatastore(), conversion happens to put back in to the units in M$Specs.
# Then after the module calculations are complete, setInDatastore writes the values back to the datastore, using the default units.

# Manual runModule Steps --------------- 
# manually step through runModule, using these inputs. 

ModuleName = 'Calculate4DMeasures'
PackageName = 'VELandUse'
RunFor = "AllYears"
RunYear = Year

# Then step through runModule function manually
