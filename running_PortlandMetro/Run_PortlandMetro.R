# Debug Portland Metro

# 0. Default VERSPM ---- 
rs <- openModel('VERSPM')

# Don't need to run this, but this is how to run the default model
# rs$run() 


# 1. VERSPM_Metro ----

if(!dir.exists('models/VERSPM_Metro')){
  pmetro <- rs$copy('VERSPM_Metro')
  # Change the inputs and defs folders to the working version specific for Portland Metro
}

pmetro <- openModel('VERSPM_Metro')

# Check to make we have a version with the defs and inputs in place
if(!'Metro' %in% pmetro$runParams$Region){
  stop('Please get the defs and inputs files for Portland Metro replace the defs and inputs in this model directory')
}



# Run the model!

pmetro$run()

# If it exits with an error, clear the results with pmetro$clear(), edit, and run again


### Debug initialization issue ----
setwd('models/VERSPM_Metro')
# Open visioneval.R
ModelScriptFile = "run_model.R"
ParamDir = "defs"
RunParamFile = "run_parameters.json"
GeoFile = "geo.csv"
ModelParamFile = "model_parameters.json"
LoadDatastore = FALSE
DatastoreName = NULL
SaveDatastore = FALSE
SimulateRun = FALSE

# Walk through initializeModel()
# There are three errors because marea_transit_fuel.csv is used in the Initialize, CalculateCarbonIntensity, and AssignHhVehiclePowertrain for VEPowertrainsAndFuels

# To to line 609 in visioneval.R
#===============================
#CHECK AND PROCESS MODULE INPUTS
#===============================
#Set up a list to store processed inputs for all modules
ModuleCalls_df[27:29,]
#                    ModuleName           PackageName   RunFor Year
# 110                Initialize VEPowertrainsAndFuels AllYears Year
# 26   CalculateCarbonIntensity VEPowertrainsAndFuels AllYears Year
# 27  AssignHhVehiclePowertrain VEPowertrainsAndFuels AllYears Year
i = 27



# Fix bzone urban-town du proprortions
# Model Stage: /models/VERSPM_Metro 
# Error: Error in names(LocType_) <- HouseID_: 'names' attribute [985] must be the same length as the vector [984]
library(readr)
bzone_urban_town_du_proportions <- read_csv("models/VERSPM_Metro/inputs/bzone_urban-town_du_proportions.csv")
View(bzone_urban_town_du_proportions)

table(bzone_urban_town_du_proportions$Year)

# Nothing obvious here -- same number of entries for each year, at least
# Debug AssignLocTypes using R debugger tool ----


debug(VELandUse::AssignLocTypes)


pmetro$run()

# When it hits an error, then step through 

# Warning message:
#   In runModule("CreateHouseholds", "VESimHouseholds", RunFor = "AllYears",  :
#                  Module CreateHouseholds has reported one or more warnings. Check log for details.

undebug(VELandUse::AssignLocTypes)


# Manual debug of AssignLocTypes issue ----
# Launch from .Rproj

setwd('models/VERSPM_Metro')
# 1. Open up run_model.R, and go through the initializeModel step

Year = pmetro$runParams$BaseYear
# 2. Then go into the runModule steps, and run up to AssignLocType (don't run that one) for the base year

# 3. Open visioneval.R, go to runModule() function, and manually step through the steps
ModuleName = "AssignLocTypes"
PackageName = "VELandUse"
RunFor = "AllYears"
RunYear = Year
StopOnErr = TRUE
# now step through runModule()
# until the line 
#  #Get data from datastore
# L <- getFromDatastore(M$Specs, RunYear = Year)
# Look at list L

# Step through AssignLocTypes with this list L

# failing on double loop for assigning location types, for Bzone 228, SF housing type

# Problem: NumDU_Lt only sums to 984, but there are 985 households in this Bzone
# > Lt
# [1] "Urban" "Town"  "Rural"
# > NumDU_Lt
# Urban  Town Rural 
# 0   886    98 
# > sum(886,98)
# [1] 984

# PRoportions are right
# > Props_BzHtLt[228,,]
# Urban Town Rural
# SF     0  0.9   0.1
# MF     0  1.0   0.0
# GQ     0  1.0   0.0

# Problem occurs in splitInt, which results in one fewer single family house hold in Bzone 228
# > DU_BzHt_full[228,]
# SF  MF  GQ 
# 985 732   0 
# > 985*0.9
# [1] 886.5
# > 985*0.1
# [1] 98.5
# > DU_BzHtLt[228,,]
# Urban Town Rural
# SF     0  886    98
# MF     0  732     0
# GQ     0    0     0
# > 886+98
#[1] 984

DU_BzHtLt <- sweep(Props_BzHtLt, c(1,2), DU_BzHt_full, splitInt)

DU_BzHtLt <- sweep(Props_BzHtLt, c(1,2), DU_BzHt_full, splitInt)


# Sweep over bzones (dimension 1) and housing types (dimension 2)
# for our problem issue:
Props_BzHtLt[228,1,]
DU_BzHt_full[228,]

result <- splitInt(Props_ = Props_BzHtLt[228,1,],
                  Tot = DU_BzHt_full[228, 1])

stopifnot(identical(sum(result), DU_BzHt_full[228]))


# Now we feed these to splitInt()
# Props_ = Props_BzHtLt[228,1,]
# Tot = DU_BzHt_full[228,1]

# That was incorrect -- feeding in matrix actually, not vector 
# Props_ = Props_BzHtLt[228,,]
# Tot = DU_BzHt_full[228,]

# That also was incorrect. This is the right way to think about the sweep inputs.
# Because it is applied over each slice of the input array
Props_ = Props_BzHtLt[,,2] # Do 2 for Town
Tot = DU_BzHt_full


result <- splitInt(Props_, Tot)

result
# Urban  Town Rural 
# 0   886    98 
          

test_data = c(DU_BzHt_full, Props_BzHtLt, L)
save(list = c('DU_BzHt_full',
              'Props_BzHtLt',
              'L'), file = 'AssignLocTypes_TestData.RData')


# organize instead by bzone

array_out = array(NA, dim = c(length(Bz), length(Ht), length(Lt)),
                  dimnames = list(Bz, Ht, Lt))

Props_ = Props_BzHtLt[228,,]
Tot = DU_BzHt_full[228,]

Props_ = Props_BzHtLt[73,,]
Tot = DU_BzHt_full[73,]


for(i in 1:nrow(Props_BzHtLt)){
  for(j in 1:ncol(Props_BzHtLt)){
    array_out[i,j,] <- splitInt(Props_BzHtLt[i,j,], Tot = DU_BzHt_full[i,j])
  }
}


# Test function 
list_inputs <- function(Props_, Tot){
  SplitAll_ <- Props_ * Tot
  SplitInt_ <- round(SplitAll_)
  Rem <- sum(SplitAll_ - SplitInt_)
  
  list(Prop_in = Props_,
       Tot_in = Tot,
       Prop_x_Tot = Props_ * Tot, 
       Rems = Rem)
}

# # list inputs
li <- sweep(Props_BzHtLt[228,,], MARGIN = c(1,2), DU_BzHt_full[228,], list_inputs)
head(li[['Prop_in']])
str(li)
li[['Rems']]
# class(DU_BzHtLt)
# typeof(DU_BzHtLt)
# dim(DU_BzHtLt)
# sum(DU_BzHtLt[228,1,])

# Calculate4DMeasures issue ----
# Launch from .Rproj

setwd('models/VERSPM_Metro')
# 1. Open up run_model.R, and go through the initializeModel step

Year = pmetro$runParams$BaseYear
# 2. Then go into the runModule steps, and run up to AssignLocType (don't run that one) for the base year

# 3. Open visioneval.R, go to runModule() function, and manually step through the steps
# Issue arises from NA when a Bzone has all 0 employment, which end up as NAs.
ModuleName = "LocateEmployment"
# ModuleName = "Calculate4DMeasures"
PackageName = "VELandUse"
RunFor = "AllYears"
RunYear = Year
StopOnErr = TRUE

# in initializaiton, is that where NA values for RetEmp come from?
which(is.na(ProcessedInputs_ls$`VELandUse::LocateEmployment`$Data$Year$Bzone$RetEmp))
# No..
# Problem is in LocateEmployment itself


ActName = 'RetEmp'

calcEntropyTerm <- function(ActName) {
  Act_ <- D_df[[ActName]]
  ActRatio_ <- Act_ / D_df$TotAct
  LogActRatio_ <- ActRatio_ * 0
  LogActRatio_[Act_ != 0] <- log(Act_[Act_ != 0] / D_df$TotAct[Act_ != 0])
  ActRatio_ * LogActRatio_
}


E_df <- data.frame(
  Hh = calcEntropyTerm("NumHh"),
  Ret = calcEntropyTerm("RetEmp"),
  Svc = calcEntropyTerm("SvcEmp"),
  Oth = calcEntropyTerm("OthEmp")
)



# Verify outputs are the same for VERPAT after patch ----

rp <- openModel('VERPAT')

rp$run()

rp$tablesSelected <- c('Bzone', 'Household')
rp$extract()

orig_Hh_05 <- read.csv('models/VERPAT/output/Household_2005_1_2021-05-06_110257.csv')
orig_Bz_05 <- read.csv('models/VERPAT/output/Bzone_2005_1_2021-05-06_110257.csv')

new_Hh_05 <- read.csv('~/git/VisionEval-Dev/built/visioneval/4.0.5/runtime/models/VERPAT/output/Household_2005_1_2021-05-06_130119.csv')
new_Bz_05 <- read.csv('~/git/VisionEval-Dev/built/visioneval/4.0.5/runtime/models/VERPAT/output/Bzone_2005_1_2021-05-06_130119.csv')

identical(table(orig_Hh_05$HhPlaceTypes),
          table(new_Hh_05$HhPlaceTypes))

identical(table(orig_Hh_05$Income),
          table(new_Hh_05$Income))

identical(table(orig_Bz_05$UrbanPop),
          table(new_Bz_05$UrbanPop))

# All TRUE

# Verify outputs are the same for VERSPM after patch ----


rs <- openModel('VERPAT')

rs$run()

rs$extract()

orig_Hh <- read.csv('models/VERSPM/output/Household_2010_1_2021-05-06_110610.csv')
orig_Bz <- read.csv('models/VERSPM/output/Bzone_2010_1_2021-05-06_110610.csv')

new_Hh <- read.csv('~/git/VisionEval-Dev/built/visioneval/4.0.5/runtime/models/VERSPM/output/Household_2010_1_2021-05-06_130423.csv')
new_Bz <- read.csv('~/git/VisionEval-Dev/built/visioneval/4.0.5/runtime/models/VERSPM/output/Bzone_2010_1_2021-05-06_130423.csv')

identical(table(orig_Hh$LocType),
          table(new_Hh$LocType))

identical(table(orig_Hh$HouseType),
          table(new_Hh$HouseType))

identical(sum(orig_Bz$UrbanPop),
          sum(new_Bz$UrbanPop))
