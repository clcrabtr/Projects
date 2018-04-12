# set your working directory
setwd("C://Users//crabtreec//Desktop//R Projects//Elections") # Enter the filepath to your desktop here, we will use this as your working directory

# install needed packages
install.packages("dplyr")
install.packages("magrittr")

# load needed packages
library(dplyr)
library(magrittr)

elections <- read.csv("Hawaii.txt") # read in the FEC file (which you've unzipped and saved to your working directory as "FileName.txt")
elections <- tbl_df(elections) # table for easy printing

electionsfilter <- select(elections, NAME, STREET, CITY, STATE) # filter for needed columns
electionsfilter<- tbl_df(electionsfilter) # table for easy reading

electionsfilter <- mutate(electionsfilter, AddMatch = paste(STREET, CITY, STATE, sep = " "))
electionsfilter$AddMatch <- gsub("[[:space:]]", "", electionsfilter$AddMatch) #remove all spaces
electionsfilter <- select(electionsfilter, AddMatch)

recordbase <- read.csv("YOURCONSTITUENTBASE.csv") # read in the record base with ID number, Home Address Line 1, Home City, Home State
recordbase <- tbl_df(recordbase) #table for easy printing

# select the needed columns to create the AddMatch string
recordbase <- select(recordbase, Entity.ID, Home.Address.Line.1, Home.City, Home.State)

# create a matching AddMatch variable from the addresses and format it
recordbase$AddMatch <- paste(recordbase$Home.Address.Line.1, recordbase$Home.City, recordbase$Home.State, sep = " ")
recordbase$AddMatch <- gsub("[[:space:]]", "", recordbase$AddMatch) #remove all spaces
recordbase$AddMatch <- toupper(recordbase$AddMatch)

# isolate records from the database that match the Election database
electionadds <- intersect(recordbase$AddMatch, electionsfilter$AddMatch)
recordbase <- filter(recordbase, AddMatch %in% electionadds, AddMatch != "")
recordbase <- recordbase %>% select(Entity.ID) 

# write the file to the working drive
write.csv(recordbase, file = "ElectionDonors.csv", row.names = FALSE)
