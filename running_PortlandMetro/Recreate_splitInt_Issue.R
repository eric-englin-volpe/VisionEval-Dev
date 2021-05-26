# SplitInt issue

# Here is the splitInt function in AssignLocTypes module:

# Define a function to do a whole number splitting according to proportions
splitInt <- function(Props_, Tot) {
  SplitAll_ <- Props_ * Tot
  SplitInt_ <- round(SplitAll_)
  Rem <- sum(SplitAll_ - SplitInt_)
  if (Rem != 0) {
    RemTab_ <- table(
      sample(1:length(Props_), abs(as.integer(Rem)), replace = TRUE, prob = Props_)
    )
    SplitInt_[as.numeric(names(RemTab_))] <-
      SplitInt_[as.numeric(names(RemTab_))] + sign(Rem) * RemTab_
  }
  SplitInt_
}


# This works by feeding a dataframe of proportions of land types 'Lt' (urban, town, rural) by housing type 'Ht' (single family, multifamily, group quarters) for each Bzone 'Bz'. This is stored in the 3-dimensinal array Props_BzHtLt.
# Those proportions are then applied to the vector describing the number of housing types for each Bzone, stored in the 2-dimensional array DU_BzHt_full, on line 343 of AssignLocTypes.R

# ````
  # Calculate dwelling units by Bzone, housing type and location type
  DU_BzHtLt <- sweep(Props_BzHtLt, c(1,2), DU_BzHt_full, splitInt)
# ```

  
# For one particular set of inputs, this is now failing to correctly apply the splitInt, resulting in a mismatch between the length of the vector of names and housing IDs. The error occurs in the double loop on line 345 of AssignLocTypes.R.

# Problem: The resulting DU_BzHtLt for this Bzone and housing type only sums to 984, but there are 985 dwelling units in this Bzone for the SF housing type

# Should be 985:
# DU_BzHtLt[228,1,]
# Urban  Town Rural 
# 0   886    98 
# sum(DU_BzHtLt[228,1,])
# 984

# Correct number:
# DU_BzHt_full[228,1]
# [1] 985


# Proportions are not the problem
# > Props_BzHtLt[228,,]
# Urban Town Rural
# SF     0  0.9   0.1
# MF     0  1.0   0.0
# GQ     0  1.0   0.0

# Problem occurs in splitInt, which results in one fewer single family dwelling unit in Bzone 228
# > DU_BzHt_full[228,]
# SF  MF  GQ 
# 985 732   0 
# > 985*0.9
# [1] 886.5
# > 985*0.1
# [1] 98.5
# splitInt should sample with weighting to assign that 0.5 household appropirately between urban, town, and rural

# Recreating the issue manually!

Props_ = c(0.0, 0.9, 0.1)
names(Props_) = c("Urban", "Town", "Rural")

Tot = 985

result <- splitInt(Props_, Tot)

sum(result) # 985

# So why do we get an issue?

Props_ = Props_BzHtLt[228,1,]
Tot = DU_BzHt_full[228,1]

result <- splitInt(Props_, Tot)

sum(result) # 984 !!!

# Working on a solution ----
# using actual inputs

SplitAll_ <- Props_ * Tot
SplitInt_ <- round(SplitAll_)
Rem <- sum(SplitAll_ - SplitInt_)


sample(1:length(Props_), abs(Rem), replace = TRUE, prob = Props_)
# integer(0)

Rem = round(Rem, 0)

sample(1:length(Props_), abs(Rem), replace = TRUE, prob = Props_)
# [1] 2

if (Rem != 0) {
  RemTab_ <- table(
    sample(1:length(Props_), abs(Rem), replace = TRUE, prob = Props_)
  )
  SplitInt_[as.numeric(names(RemTab_))] <-
    SplitInt_[as.numeric(names(RemTab_))] + sign(Rem) * RemTab_
}
SplitInt_



splitInt <- function(Props_, Tot) {
  SplitAll_ <- Props_ * Tot
  SplitInt_ <- round(SplitAll_)
  Rem <- sum(SplitAll_ - SplitInt_)
  if (Rem != 0) {
    RemTab_ <- table(
      sample(1:length(Props_), abs(Rem), replace = TRUE, prob = Props_)
    )
    SplitInt_[as.numeric(names(RemTab_))] <-
      SplitInt_[as.numeric(names(RemTab_))] + sign(Rem) * RemTab_
  }
  SplitInt_
}
