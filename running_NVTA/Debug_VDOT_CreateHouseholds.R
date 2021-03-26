# Debug VDOT
#   runModule("CreateHouseholds",                "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
# [1] "2021-03-26 13:34:59 -- Starting module 'CreateHouseholds' for year '2019'."
# Error in `[.data.frame`(data.frame(L$Year$Azone, stringsAsFactors = FALSE),  : 
#                           undefined columns selected

# Process:
Year = '2019'
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
?CreateHouseholds

# Failing on
#Make matrix of regular household persons by Azone and age group
Prsn_AzAp <-
  as.matrix(data.frame(L$Year$Azone, stringsAsFactors = FALSE)[,Ap])
