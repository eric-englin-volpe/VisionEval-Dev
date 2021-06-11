# Run the NVTA model of VERSPM using the working draft inputs
# Now using scenarios

# Launch from the local version of an installed VisionEval,
# by double-clicking VisionEval.Rproj then open this file.

# 0. Default VERSPM ---- 
rs <- openModel('VERSPM_Scenarios')

# Don't need to run this, but this is how to run the default model
# rs$run() 


# 1. VERSPM_VDOT ----

if(!dir.exists('models/VERSPM_NVTA_Scenarios')){
  vdot <- rs$copy('VERSPM_NVTA_Scenarios')
  # Change the inputs and defs folders to match what is in the Google Drive
}

vdot <- openModel('VERSPM_NVTA_Scenarios')

# Check to make we have a version with the defs and inputs in place
if(vdot$runParams$Region != 'NVTA'){
  stop('Please get the defs and inputs file from the Google Drive and replace the defs and inputs in this model directory')
}

# return 


# Run the model!
vdot$run()

