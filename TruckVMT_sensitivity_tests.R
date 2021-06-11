# Sensitivity testing for truck VMT growth assumptions

# Launch from VisionEval.Rproj

# Run default model VERSPM ----

rs <- openModel('VERSPM')
rs$run()

# Extract VMT, income, population, and emissions outputs

rs$tables <- c('Marea')
# View available fields
selected_fields <- rs$fields[rs$fields$Selected == 'Yes',]
# Select Pop, income, vmt, CO2e related fields
rs$fields <- c(selected_fields$Name[grep('Pop', selected_fields$Name)],
               selected_fields$Name[grep('Income', selected_fields$Name)],
               selected_fields$Name[grep('Dvmt', selected_fields$Name)],
               selected_fields$Name[grep('CO2', selected_fields$Name)])
               
rs$fieldsSelected      

# Extract to list for this run

results1 <- rs$extract(saveTo = FALSE)

# Heavy truck growth based on population ----
# Copy VERSPM run, clear results, modify region truck VMT growth assumptions 
rs_mod1 <- rs$copy('VERSPM_mod1')
rs_mod1$clear()

# Modify region base year DVMT 
file_to_mod <- 'region_base_year_dvmt.csv'

d <- read.csv(file.path(rs_mod1$dir('inputs'), file_to_mod))

# Make a change

d$HvyTrkDvmtGrowthBasis <- 'Population'

# Write it back

write.csv(d, file.path(rs_mod1$dir('inputs'), file_to_mod), row.names = F)

# Run model and extract 

rs_mod1$run()

# Extract VMT, income, population, and emissions outputs

rs_mod1$tables <- c('Marea')
# View available fields
selected_fields <- rs_mod1$fields[rs_mod1$fields$Selected == 'Yes',]
# Select Pop, income, vmt, CO2e related fields
rs_mod1$fields <- c(selected_fields$Name[grep('Pop', selected_fields$Name)],
               selected_fields$Name[grep('Income', selected_fields$Name)],
               selected_fields$Name[grep('Dvmt', selected_fields$Name)],
               selected_fields$Name[grep('CO2', selected_fields$Name)])

rs_mod1$fieldsSelected      

# Extract to list for this run

results_mod1 <- rs_mod1$extract(saveTo = FALSE)

# Compare future year total Commercial service VMT and Heavy Truck VMT

future_year_marea1 <- names(results1)[grep(paste('Marea', rs$runParams$Years[2], sep = '_'), names(results1))] 

results1[[future_year_marea1]]

# Heavy truck Urban Dvmt is the sum of Fwy, Art, and Other
sum_HvyTrkDvmt1 <- results1[[future_year_marea1]][['HvyTrkUrbanDvmt']]

future_year_marea_mod1 <- names(results_mod1)[grep(paste('Marea', rs_mod1$runParams$Years[2], sep = '_'), names(results_mod1))] 

results_mod1[[future_year_marea_mod1]]

sum_HvyTrkDvmt_mod1 <- results_mod1[[future_year_marea_mod1]][['HvyTrkUrbanDvmt']]

cat('Percent change in sum heavy truck Dvmt between two scenarios: \n',
    round(100* (sum_HvyTrkDvmt_mod1 - sum_HvyTrkDvmt1) / sum_HvyTrkDvmt1, 2), '%')



sum_ComSvcDvmt1 <- sum(results1[[future_year_marea1]][c('ComSvcUrbanDvmt',
                                                        'ComSvcTownDvmt',
                                                        'ComSvcRuralDvmt')])

sum_ComSvcDvmt_mod1 <- sum(results_mod1[[future_year_marea_mod1]][c('ComSvcUrbanDvmt',
                                                                    'ComSvcTownDvmt',
                                                                    'ComSvcRuralDvmt')])

cat('Percent change in sum commercial service Dvmt between two scenarios: \n',
    round(100* (sum_ComSvcDvmt_mod1 - sum_ComSvcDvmt1) / sum_ComSvcDvmt1, 2), '%')

# Commercial sevice VMT growth based on income ----
# VERSPM run, clear results, modify region truck VMT growth assumptions
rs_mod2 <- rs$copy('VERSPM_mod2')
rs_mod2$clear()

# Modify region base year DVMT 
file_to_mod <- 'region_base_year_dvmt.csv'

d <- read.csv(file.path(rs_mod2$dir('inputs'), file_to_mod))

# Make a change

d$ComSvcDvmtGrowthBasis <- 'Income'

# Write it back

write.csv(d, file.path(rs_mod2$dir('inputs'), file_to_mod), row.names = F)

# Run model and extract 

rs_mod2$run()

# Extract VMT, income, population, and emissions outputs

rs_mod2$tables <- c('Marea')
# View available fields
selected_fields <- rs_mod2$fields[rs_mod2$fields$Selected == 'Yes',]
# Select Pop, income, vmt, CO2e related fields
rs_mod2$fields <- c(selected_fields$Name[grep('Pop', selected_fields$Name)],
                    selected_fields$Name[grep('Income', selected_fields$Name)],
                    selected_fields$Name[grep('Dvmt', selected_fields$Name)],
                    selected_fields$Name[grep('CO2', selected_fields$Name)])

rs_mod2$fieldsSelected      

# Extract to list for this run

results_mod2 <- rs_mod2$extract(saveTo = FALSE)

# Compare future year total Commercial service VMT and Heavy Truck VMT 

future_year_marea1 <- names(results1)[grep(paste('Marea', rs$runParams$Years[2], sep = '_'), names(results1))] 

results1[[future_year_marea1]]

# Heavy truck Urban Dvmt is the sum of Fwy, Art, and Other
sum_HvyTrkDvmt1 <- results1[[future_year_marea1]][['HvyTrkUrbanDvmt']]

future_year_marea_mod2 <- names(results_mod2)[grep(paste('Marea', rs_mod2$runParams$Years[2], sep = '_'), names(results_mod2))] 

results_mod2[[future_year_marea_mod2]]

sum_HvyTrkDvmt_mod2 <- results_mod2[[future_year_marea_mod2]][['HvyTrkUrbanDvmt']]

cat('Percent change in sum heavy truck Dvmt between two scenarios: \n',
    round(100* (sum_HvyTrkDvmt_mod2 - sum_HvyTrkDvmt1) / sum_HvyTrkDvmt1, 2), '%')



sum_ComSvcDvmt1 <- sum(results1[[future_year_marea1]][c('ComSvcUrbanDvmt',
                                                        'ComSvcTownDvmt',
                                                        'ComSvcRuralDvmt')])

sum_ComSvcDvmt_mod2 <- sum(results_mod2[[future_year_marea_mod2]][c('ComSvcUrbanDvmt',
                                                                    'ComSvcTownDvmt',
                                                                    'ComSvcRuralDvmt')])

cat('Percent change in sum commercial service Dvmt between two scenarios: \n',
    round(100* (sum_ComSvcDvmt_mod2 - sum_ComSvcDvmt1) / sum_ComSvcDvmt1, 2), '%')


# TODO ----
# - make this a more general purpose comparison across any set of performance metrics
# - turn the modify input / run / extract into a simple function for ease of repeating
# - consider doing all of this in VEScenarios instead (but will )