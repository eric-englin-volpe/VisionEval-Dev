# Debug VDOT

# Can launch from the local version of an installed VisionEval,
# Or from a developer version, built from the VisionEval-Dev repository
# --> C:\Users\....\VisionEval-Dev\built\visioneval\4.0.4\runtime
# in either case, double-click the VisionEval.Rproj then open this file

# 0. Make sure VERSPM default works ----

rs <- openModel('VERSPM')
# rs$run() # do this only once. Afterwards, can just openModel and then initializeModel to get the previous model available

DEBUG_DEFAULT = F

if(DEBUG_DEFAULT){
  # This runs. Recreate the problem with VESPM. So manually step into VERSPM/run_model.R
  setwd('models/VERSPM')
  
  
  # open run_model.R
  # Run initializeModel() in run_model.R before you can getYears()
  
  Year = getYears()[1] # run for base year
  # Step into Createhouseholds
  
  ModuleName = "CreateHouseholds"
  PackageName = "VESimHouseholds"
  RunFor = "AllYears"
  RunYear = Year
  
  # Open visioneval.R, go to runModule() function, and step through it until R <- M$Func(L). Don't run that line.
  
  # Now go to CreateHouseholds.R
  # Step throgh this until the Prsn_AzAp step
  
  # > data.frame(L$Year$Azone, stringsAsFactors = FALSE)
  # Azone Marea Age0to14 Age15to19 Age20to29 Age30to54 Age55to64 Age65Plus AveHhSize Prop1PerHh GrpAge0to14 GrpAge15to19
  # PRSN RVMPO RVMPO    30193     10970     20557     52327     24840     29240        NA        0.3           0          666
  # GrpAge20to29 GrpAge30to54 GrpAge55to64 GrpAge65Plus
  # PRSN          382           66            7            0
  
  # >   #Make matrix of regular household persons by Azone and age group
  #   >   Prsn_AzAp <-
  #   +     as.matrix(data.frame(L$Year$Azone, stringsAsFactors = FALSE)[,Ap])
  # > Prsn_AzAp
  # Age0to14 Age15to19 Age20to29 Age30to54 Age55to64 Age65Plus
  # PRSN    30193     10970     20557     52327     24840     29240
  
  # this is what we should get with VDOT 
  
  # > names(L$Year$Azone)
  # [1] "Azone"        "Marea"        "Age0to14"     "Age15to19"    "Age20to29"    "Age30to54"    "Age55to64"    "Age65Plus" 
  # [9] "AveHhSize"    "Prop1PerHh"   "GrpAge0to14"  "GrpAge15to19" "GrpAge20to29" "GrpAge30to54" "GrpAge55to64" "GrpAge65Plus"
}

# 1. Try to run VERSPM_VDOT ----

if(!dir.exists('VERSPM_VDOT')){
  vdot <- rs$copy('VERSPM_VDOT')
  # Change the inputs and defs folders to match what is in the Google Drive
}
vdot <- openModel('VERSPM_VDOT')
vdot$run()

# Problem 1: Solved ---- 

NEEDS_WORK = F

if(NEEDS_WORK){
  # [1] "2021-03-26 15:12:03 -- Initializing Model. This may take a while."
  
  # 2. Step through run_model.R ----
  
  # Run the initializeModel steps, up until the module loop. 
  
  #   runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  # [1] "2021-03-26 13:34:59 -- Starting module 'CreateHouseholds' for year '2019'."
  # Error in `[.data.frame`(data.frame(L$Year$Azone, stringsAsFactors = FALSE),  : 
  #                           undefined columns selected
  
  # Process:
  Year = getYears()[1]
  ModuleName = "CreateHouseholds"
  PackageName = "VESimHouseholds"
  RunFor = "AllYears"
  RunYear = Year
  
  # Step into runModule() in C:\Users\Daniel.Flynn\Documents\git\VisionEval-Dev\sources\framework\visioneval\R
  
  # Run down to
      #Run module
      #----------
      # if (M$Specs$RunBy == "Region") {
      #   M$Specs$RunBy == "Region"
  
  # At this line, step into the curly braces (because will be true, RunBy == 'Region')
  
  # Failing on   R <- M$Func(L)
  # So need go to into CreateHouseholds function now.
  
  # Open C:\Users\Daniel.Flynn\Documents\git\VisionEval-Dev\sources\modules\VESimHouseholds\R\CreateHouseholds.R
  # And scroll down to CreateHouseholds function
  # also look at help file:
  # ?CreateHouseholds
  # 
  # # Failing on
  # #Make matrix of regular household persons by Azone and age group
  # Prsn_AzAp <-
  #   as.matrix(data.frame(L$Year$Azone, stringsAsFactors = FALSE)[,Ap])
  
  # SOLUTION: 
  # Fixed by correcting an error in 
}

# Problem 2: Solved
# AssignLocType was exiting with an error
# SOLUTION: Use new VisionEval-Dev version

# debug(VELanduse::AssignLocTypes)

# then vdot$run()

# Scratch code: 
# Process:
Year = getYears()[1]
ModuleName = "PredictHousing"
PackageName = "VELandUse"
RunFor = "AllYears"
RunYear = Year

L <- fetchModuleData('AssignLocTypes', 'VELandUse', Year = Year)


# Problem 3: 5TAZ model failing on Predict Housing ----

if(NEEDS_WORK){
  # table(sample(Bx, abs(UnitDiff), replace = TRUE, prob = GQUnitDemand_Bx/sum(GQUnitDemand_Bx)))
  # sample(Bx, abs(UnitDiff), replace = TRUE, prob = GQUnitDemand_Bx/sum(GQUnitDemand_Bx))
  
  # 2. Step through run_model.R ----
  
  # Run the initializeModel steps, up until the module loop. 
  
  #   runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  # [1] "2021-03-26 13:34:59 -- Starting module 'CreateHouseholds' for year '2019'."
  # Error in `[.data.frame`(data.frame(L$Year$Azone, stringsAsFactors = FALSE),  : 
  #                           undefined columns selected
  
  # Process:
  Year = getYears()[1]
  ModuleName = "CreateHouseholds"
  PackageName = "VESimHouseholds"
  RunFor = "AllYears"
  RunYear = Year
  
  # Step into runModule() in C:\Users\Daniel.Flynn\Documents\git\VisionEval-Dev\sources\framework\visioneval\R
  
  # Run down to
  #Run module
  #----------
  # if (M$Specs$RunBy == "Region") {
  #   M$Specs$RunBy == "Region"
  
  # At this line, step into the curly braces (because will be true, RunBy == 'Region')
  
  # Failing on   R <- M$Func(L)
  # So need go to into CreateHouseholds function now.
  
  # Open C:\Users\Daniel.Flynn\Documents\git\VisionEval-Dev\sources\modules\VESimHouseholds\R\CreateHouseholds.R
  # And scroll down to CreateHouseholds function
  # also look at help file:
  # ?CreateHouseholds
  # 
  # # Failing on
  # #Make matrix of regular household persons by Azone and age group
  # Prsn_AzAp <-
  #   as.matrix(data.frame(L$Year$Azone, stringsAsFactors = FALSE)[,Ap])
  
  # SOLUTION: 
  # Fixed by correcting an error in 
}
