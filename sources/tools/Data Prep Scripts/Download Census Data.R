#===========
#Download Census Data.R
#===========

#This script will create the geo.csv file with the following levels:
#         Azone: County
#         Bzone: Census Tract
#         Czone: NAs
#         Marea: Metro Area

#This script provides an example of 2 counties in Northern Virginia and creates a map to confirm data source

#Install/Load libraries
#--------------
#install.packages("tigris")
#install.packages("dplyr")
library("tigris")
library("dplyr")

#Download Census data
#--------------
# Find state & county FIPS codes at this link:
#       https://www.census.gov/prod/techdoc/cbp/cbp95/st-cnty.pdf

counties <- c(059, 600) #enter county codes here
tracts <- tracts(state = 'VA', county =  counties, cb=TRUE)

levels <- c('059'= "Fairfax County",'600' = "Fairfax City") #change county codes to county names
df2 <- data.frame(wxfrom = names(levels), wxto = levels1, stringsAsFactors=FALSE, row.names=NULL)
tracts <- dplyr::left_join(tracts, df2, by=c("COUNTYFP"="wxfrom"))
tracts$Azone <- tracts$wxto
tracts$Bzone <- tracts$TRACTCE
tracts$Czone <- "NA"
tracts$Marea <- "NVTA"
Geo_df <- tracts[c('Azone','Bzone',"Czone",'Marea')]
Geo_df <- dplyr::select(as.data.frame(Geo_df), -geometry)
write.csv(Geo_df,"C:\\Users\\eric.englin\\Desktop\\VisionEval\\geo.csv", row.names = FALSE) #need to configure file location


#Create visualization map to confirm counties
#--------------

#install.packages("leaflet")
library("leaflet")

leaflet(tracts) %>%
  addTiles() %>%
  addPolygons()
