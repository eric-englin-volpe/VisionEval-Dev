ve.build()
ve.run()
rp <- openModel('VERPAT')

rp$run()
rp$tablesSelected <- c('Bzone', 'Household')
rp$extract()

rs <- openModel('VERSPM')
rs$run()
rs$extract()

# Validate VERPAT outputs are identical ----
# Using a run of VERPAT from current installer as the 'original'
# update paths and date-time of extracts as necessary

orig_root <- 'C:/Users/Daniel.Flynn/Desktop/VE_4-0-5'

orig_Hh_05 <- read.csv(file.path(orig_root, 'models/VERPAT/output/Household_2005_1_2021-05-06_110257.csv'))
orig_Bz_05 <- read.csv(file.path(orig_root, 'models/VERPAT/output/Bzone_2005_1_2021-05-06_110257.csv'))

new_root <- '~/git/VisionEval-Dev/built/visioneval/4.0.5/runtime/'

new_Hh_05 <- read.csv(file.path(new_root, 'models/VERPAT/output/Household_2005_1_2021-05-06_171723.csv'))
new_Bz_05 <- read.csv(file.path(new_root, 'models/VERPAT/output/Bzone_2005_1_2021-05-06_171723.csv'))

identical(orig_Hh_05, new_Hh_05)

identical(orig_Bz_05, new_Bz_05)

# All TRUE

# Validate VERSPM outputs are identical ----
# Using a run of VERSPM from current installer as the 'original'
# update paths and date-time of extracts as necessary

orig_Hh <- read.csv(file.path(orig_root, 'models/VERSPM/output/Household_2010_1_2021-05-06_110610.csv'))
orig_Bz <- read.csv(file.path(orig_root, 'models/VERSPM/output/Bzone_2010_1_2021-05-06_110610.csv'))

new_Hh <- read.csv(file.path(new_root, 'models/VERSPM/output/Household_2010_1_2021-05-06_172043.csv'))
new_Bz <- read.csv(file.path(new_root, 'models/VERSPM/output/Bzone_2010_1_2021-05-06_172043.csv'))

identical(orig_Hh, new_Hh)
# FALSE

identical(sum(orig_Hh$WalkTrips), sum(new_Hh$WalkTrips))

identical(sum(orig_Hh$Dvmt), sum(new_Hh$Dvmt))

sum(new_Hh$Dvmt)/sum(orig_Hh$Dvmt)
# off by 1% now.

# Investigate this Bzone
#Geo            Year	TotEmp	RetEmp	SvcEmp
#D410290002023	2010	1       0       0

# Versus this one
#D410290014002	2010	403	262	96
# Are walk trips / Dvmt same for one where there are no 0's? Should be.

identical(orig_Bz, new_Bz)

identical(table(orig_Hh$LocType),
          table(new_Hh$LocType))

identical(table(orig_Hh$HouseType),
          table(new_Hh$HouseType))

identical(sum(orig_Bz$UrbanPop),
          sum(new_Bz$UrbanPop))
