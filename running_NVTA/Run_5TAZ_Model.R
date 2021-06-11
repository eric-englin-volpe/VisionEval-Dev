# Run the 5TAZ hypothetical model of VERSPM 

# Launch from the local version of an installed VisionEval,
# by double-clicking VisionEval.Rproj then open this file.

# 0. Default VERSPM ---- 
rs <- openModel('VERSPM')

# Don't need to run this, but this is how to run the default model
# rs$run() 


# VERSPM_5TAZ ----

if(!dir.exists('models/VERSPM_5TAZ')){
  vdot <- rs$copy('VERSPM_5TAZ')
  # Change the inputs and defs folders to match what is in the Google Drive
}

# Open the model. 
vdot <- openModel('VERSPM_5TAZ')

# Check to make we have a version with the defs and inputs in place
if(vdot$runParams$Region != 'HYPO'){
  stop('Please get the defs and inputs file from the Google Drive and replace the defs and inputs in this model directory')
}



# Run the model!
# If you have already run it, skip to extract steps
vdot$run()



# Extract all to csv. This is slow but extracts everything
vdot$extract()

# Focused extraction ----
# Start here after running openModel
# Or more focused extraction, for household-level metrics
vdot$tables <- c('Household')

# Extract Dvmt, trip metrics, and DailyCO2e at household level, with additional variables 
vdot$fields <- c('HhId', 'Azone', 'HhSize',
                 'Dvmt', 'DailyCO2e', 
                 'TransitTrips', 'BikeTrips', 'VehicleTrips', 'WalkTrips',
                 'AveVehTripLen')



vdot$tables <- c('Marea')

# Extract Dvmt, trip metrics, and DailyCO2e at household level, with additional variables 
vdot$fields <- c('NonUrbanAveSpeed', 'LdvAveSpeed')

# TODO: double check the units on LdvAveSpeed	from Marea. Currently outputting as MI/DAY instead of MI/HR as stated it should be in module docs.

# See everything you've selected, with the units
vdot$list(index=TRUE)

# Create a results data frame
results <- vdot$extract(saveTo = FALSE)

# Clear the selections to make other selections
vdot$tables = ''
vdot$fields = ''

# Visualize results for the selected years. Put them together in a single data frame
hh_2019 <-  results[[1]]
hh_2019$Year = 2019

hh_2045 <-  results[[2]]
hh_2045$Year = 2045

d <- rbind(hh_2019, hh_2045)

# Example visual. Summarize the distribution of Dvmt by county in the two years
library(tidyverse)

ggplot(d, aes(x = Dvmt, fill = as.factor(Year))) +
  geom_histogram(binwidth = 5) +
  facet_grid(Year ~ Azone, scales = 'free_y')

View(d)

d1 <- d %>%
  group_by(Year, Azone) %>%
  summarize(sum_DVMT = sum(Dvmt),
            sum_Pop = sum(HhSize),
            sum_DailyCO2e = sum(DailyCO2e),
            sum_WalkTrips = sum(WalkTrips),
            sum_BikeTrips = sum(BikeTrips),
            sum_TransitTrips = sum(TransitTrips),
            sum_VehicleTrips = sum(VehicleTrips))


View(d1)

ggplot(d1, aes(y = sum_WalkTrips, x = as.factor(Year), fill = Azone)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ Azone, scales = 'free_y') +
  xlab('Year') + ylab('Walk trips by county') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Walk trips by year and Azone')

# Per capita walk trips
ggplot(d1, aes(y = sum_WalkTrips / sum_Pop, x = as.factor(Year), fill = Azone)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ Azone, scales = 'free_y') +
  xlab('Year') + ylab('Per capita walk trips by county') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Walk trips by year and Azone')

ggplot(d1, aes(y = sum_VehicleTrips, x = as.factor(Year), fill = Azone)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ Azone, scales = 'free_y') +
  xlab('Year') + ylab('Vehicle trips by county') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Vehicle trips by year and Azone')

ggplot(d1, aes(y = sum_DVMT, x = as.factor(Year), fill = Azone)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ Azone, scales = 'free_y') +
  xlab('Year') + ylab('DVMT by county') +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('DVMT by year and Azone')

# Plot DVMT by DailyCO2e by year and county,
# using hh size to make data smaller

d2 <- d %>%
  group_by(Year, Azone, HhSize) %>%
  summarize(count = n(),
            sum_DVMT = sum(Dvmt),
            sum_Pop = sum(HhSize),
            sum_DailyCO2e = sum(DailyCO2e),
            sum_WalkTrips = sum(WalkTrips),
            sum_BikeTrips = sum(BikeTrips),
            sum_TransitTrips = sum(TransitTrips),
            sum_VehicleTrips = sum(VehicleTrips))

ggplot(d2, aes(x = sum_DVMT, y = sum_DailyCO2e, color = as.factor(Year))) +
  geom_point(aes(size = count)) +
  geom_text(aes(label = HhSize), color = 'black') + 
  facet_wrap(~ Azone, scales = 'free') +
  ylab('Daily CO2e') + xlab('DVMT') +
  ggtitle('DVMT by year and Azone')

# Validate units: is Daily CO2e in kg/day or g/day?

ggplot(d2, aes(x = sum_DVMT, y = sum_DailyCO2e, color = as.factor(Year))) +
  geom_point(aes(size = HhSize)) +
  scale_size(range = c(2, 10)) + 
  facet_wrap(~ Azone, scales = 'free') +
  ylab('Daily CO2e') + xlab('DVMT') +
  ggtitle('DVMT and CO2 emissions by year and Azone')


# Per capita Daily CO2e and DVMT
ggplot(d2, aes(x = sum_DVMT / sum_Pop, y = sum_DailyCO2e / sum_Pop, color = as.factor(Year))) +
  geom_point(aes(size = count)) +
  geom_text(aes(label = HhSize), size = 4, color = 'grey20') +
  theme_bw() +
  scale_size(range = c(2, 15), name = 'Count of households') + 
  scale_color_discrete(name = 'Year') +
  facet_wrap(~ Azone) + 
  ylab('Per capita daily CO2e (kg/day/person) ') + xlab('Per capita DVMT (miles/day/person)') +
  ggtitle('Per capita DVMT and CO[2]e emissions by year, county, and household size')
ggsave('DVMT_CO2_per_capita.jpeg', width = 7, height = 5)

# TODO: consider walk, transit, bike trips vs vehicle trips
# Consider 

