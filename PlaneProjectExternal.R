# set your working directory
setwd("C://Users//crabtreec//Desktop//R Project") # Enter the filepath to your desktop here, we will use this as your working directory

# install needed packages
install.packages("dplyr")
install.packages("magrittr")

# load needed packages
library(dplyr)
library(magrittr)

# The purpose of this script is to match addresses from the FAA's private/corporate aircraft database (314K records)
# (available here: http://www.faa.gov/licenses_certificates/aircraft_certification/aircraft_registry/releasable_aircraft_download/
# to the home addresses for alumni and friends in our database (212K records). The hope is that this will
# identify potential prospects by flagging them as potential plane owners

planes <- read.csv("MASTER.txt") # read in the FAA file (which you've unzipped and saved to your working directory as "MASTER.txt")
planes <- tbl_df(planes) # table for easy printing

planesfilter <- select(planes, NAME, STREET, CITY, STATE) # filter for needed columns
planesfilter<- tbl_df(planesfilter) # table for easy reading

# add the HasPlane variable (defaulted to 1) and the AddMatch variable (a stripped down address character string)
planesfilter <- mutate(planesfilter, AddMatch = paste(STREET, CITY, STATE, sep = " "))
planesfilter$AddMatch <- gsub("[[:space:]]", "", planesfilter$AddMatch) #remove all spaces
planesfilter <- select(planesfilter, AddMatch)

recordbase <- read.csv("YOURCONSTITUENTBASE.csv") # read in your record base with ID number, Home Address Line 1, Home City, Home State
recordbase <- tbl_df(recordbase) #table for easy printing

# select the needed columns to create the AddMatch string
recordbase <- select(recordbase, Entity.ID, Home.Address.Line.1, Home.City, Home.State)

# create a matching AddMatch variable from the addresses and format it
recordbase$AddMatch <- paste(recordbase$Home.Address.Line.1, recordbase$Home.City, recordbase$Home.State, sep = " ")
recordbase$AddMatch <- gsub("[[:space:]]", "", recordbase$AddMatch) #remove all spaces
recordbase$AddMatch <- toupper(recordbase$AddMatch)

# isolate records from our database that match the FAA database
planeadds <- intersect(recordbase$AddMatch, planesfilter$AddMatch)
recordbase <- filter(recordbase, AddMatch %in% planeadds, AddMatch != "")
recordbase <- recordbase %>% select(Entity.ID) 

# write the file to the working drive for upload to Reeher
write.csv(recordbase, file = "PotentialPlanes.csv", row.names = FALSE)
