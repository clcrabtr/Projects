### Clear Environment

rm(list = ls())


## Set working directory
setwd('H:\\R Projects\\990 Project')


## Install packages
install.packages('readr')
install.packages('tidyverse')
install.packages("dplyr")
install.packages("magrittr")
install.packages('writexl')

## Load  packages
library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)
library(readxl)
library(writexl)

# Upload Complete 990 Directory from 4 saved IRS files

Data_1 <- read_csv(url('https://www.irs.gov/pub/irs-soi/eo1.csv'))

Data_2 <- read_csv(url('https://www.irs.gov/pub/irs-soi/eo2.csv'))

Data_3 <- read_csv(url('https://www.irs.gov/pub/irs-soi/eo3.csv'))

Data_4 <- read_csv(url('https://www.irs.gov/pub/irs-soi/eo4.csv'))



# Combine 4 data files into one dataframe

Combined_Data_Original <- rbind(Data_1 , Data_2 , Data_3, Data_4)
Combined_Data <- Combined_Data_Original[,c(1,2,4,5,6,7)]


# Re-format ZIP Codes to match DMS Output
Combined_Data$ZIP <- substr(Combined_Data$ZIP, 0, 5)


# Upload the 990s GP is interested in comparing 

DMS_List <- read_excel("MG_PG Assigned FN Accounts - 6.4.18.xlsx")


# Pull Foundations whose ZIP code matches the GP list.

ZIP_Match <- subset(Combined_Data, ZIP %in% DMS_List$`ZIP Code`)


# Create EIN_List variable for the 990 Scraper
EIN_List <- as.list(ZIP_Match$EIN)


##Create dataframe to facilitate address matching##


# Pulls specific variables needed for address matching script, writes as .txt.
# Note: This will take a little time

Combined_ADDR <- data.frame(ZIP_Match$EIN, ZIP_Match$NAME, ZIP_Match$STREET, ZIP_Match$CITY, ZIP_Match$STATE)
write.table(Combined_ADDR, file = 'Master.txt', sep = ',')


### Tells you where the file just downloaded
getwd()




###
### Master.txt contains all those foundations whose ZIP matches the DMS list, as a text document. 
### 
### The next section will match their listed address to the foundations selected from DMS. 
###

# Load Master.txt
Foundation <- read.csv("Master.txt")

# Convert to table for easy reading
Foundation <- tbl_df(Foundation) 

# rename Foundation columns
names(Foundation) <- c('EIN', 'NAME', 'STREET', 'CITY', 'STATE')

# filter for needed columns
Foundation_Filter <- select(Foundation, EIN, NAME, STREET, CITY, STATE) 

# table for easy reading
Foundation_Filter <- tbl_df(Foundation_Filter) 

# add the Foundation_Filter variable (defaulted to 1) and the AddMatch variable (a stripped down address character string)
Foundation_Filter <- mutate(Foundation_Filter, AddMatch = paste(STREET, CITY, STATE, sep = " "))
Foundation_Filter$AddMatch <- gsub("[[:space:]]", "", Foundation_Filter$AddMatch)
Address_No_Spaces <- Foundation_Filter
toupper(Address_No_Spaces$AddMatch)
Foundation_Filter <- select(Foundation_Filter, AddMatch)

# change DMS_List columns to match script
names(DMS_List) <- c('Entity.ID', 'Name', 'Staff', 'Home.Address.Line.1', 'Home.City', 'Home.State', 'ZIP')

# rename DMS_List
# table for easy printing
recordbase <- DMS_List 
recordbase <- tbl_df(recordbase) 

# select the needed columns to create the AddMatch string
recordbase <- select(recordbase, Entity.ID, Home.Address.Line.1, Home.City, Home.State)

# create a matching AddMatch variable from the addresses and format it
recordbase$AddMatch <- paste(recordbase$Home.Address.Line.1, recordbase$Home.City, recordbase$Home.State, sep = " ")
recordbase$AddMatch <- gsub("[[:space:]]", "", recordbase$AddMatch) 
recordbase$AddMatch <- toupper(recordbase$AddMatch)
Record_Base_Match <- recordbase

# isolate records from our database that match the IRS directory
foundationadds <- intersect(recordbase$AddMatch, Foundation_Filter$AddMatch)
recordbase <- filter(recordbase, AddMatch %in% foundationadds, AddMatch != "")
recordbase <- recordbase %>% select(Entity.ID) 


# Make it possible to match DMS ID to EIN

Full_Merge <- merge(Record_Base_Match, Address_No_Spaces, by = 'AddMatch', all.x = FALSE)

# add back in DMS information
recordadd <- merge(recordbase, DMS_List, by.x = 'Entity.ID', by.y = 'Entity.ID', all.x = TRUE)

# rename columns again for clarity
names(recordadd) <- c('Entity.ID', 'Name', 'Staff', 'Address', 'City', 'State', 'ZIP')

# Make types match for inner join
Combined_Data_Original$EIN <- as.numeric(Combined_Data_Original$EIN)

# select relevant columns from Combined_Data_Original
Combined_Data_Final <- Combined_Data_Original[, c(1, 18, 24,25,26, 27, 11)]

# write the file to the working drive for upload to Reeher
write_xlsx(recordadd, path = "./PotentialFoundations.xlsx", col_names = TRUE)

