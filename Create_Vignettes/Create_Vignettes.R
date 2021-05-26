# Create a vignette
# https://r-pkgs.org/vignettes.html

setwd('sources/modules/VEScenario')
usethis::use_vignette()
# modify

# then build
devtools::build_vignettes()

# Then run ve.build()

# and ve.run()

# Test to see if it built
library(VEScenario)

vignette('Scenarios_Tutorial')

browseVignettes()
