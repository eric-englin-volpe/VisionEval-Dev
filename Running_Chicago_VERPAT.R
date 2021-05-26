# Default VERPAT ---- 
rpat <- openModel('VERPAT')

# Run the default model
rpat$run() 

# Select Azone, Bzone, Household, and Marea geographies.
rpat$tablesSelected <- c('Azone', 'Bzone', 'Household', 'Marea')

# Extract all outputs to csv files
rpat$extract()

# rpat$clear()
# Extract Global and Vehicles outputs separately ----



# Find out what tables are in the datastore to output ----

setwd('models/VERPAT')

#---------------------------
#Prepare for datastore query
#---------------------------
#Creates a list which includes:
#1) Path to datastore
#2) Functions for reading data given the datastore type
#3) Datastore listing 
QPrep_ls <- prepareForDatastoreQuery(
  DstoreLocs_ = "Datastore", 
  DstoreType = "RD")

#----------------------------------
#Make an inventory of the datastore
#----------------------------------
#This creates a zip archive which documents all the datasets in the datastore.
#Archive is organized by group. Within each group folder is a set of CSV files,
#one for each table in the group. Each CSV file lists the datasets included in
#the table giving the dataset name, data type, units, and description.
documentDatastoreTables(
  SaveArchiveName = "DatastoreDocumentation", 
  QueryPrep_ls = QPrep_ls)

