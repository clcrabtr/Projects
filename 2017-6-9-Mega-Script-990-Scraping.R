###############################################################################
###############################################################################
###############################################################################
###                                                                         ###
###                                                                         ###
###    ALSAC Property - do not use or redistribute without consent of       ###
###    of the script contact/author.                                        ###
###                                                                         ###
###                                                                         ###
###                                                                         ###
###    This script created by Center of Excellence in Analytics (COEA).     ###
###                                                                         ###
###                                                                         ###
###############################################################################
###############################################################################
###############################################################################
###
###
###
###############################################################################
###############################################################################
###############################################################################
###
###        Script: IRS 990 Scraper Mega Script
###
###       Version: 1.1
###
###       Project: IRS 990 Scraper
###
###       Contact: Peter Albrecht (PETER.ALBRECHT@STJUDE.ORG)
###
###       Purpose: One-stop-shop for pulling financial information of indicated
###                non-profit institutions.  This pulls financial information
###                for any non-profit including executive compensation.
###
###          Code: Created by Peter Albrecht. 
###
### First version: April 27, 2017
###
###  Last Updated: June 9, 2017
###
###
###############################################################################
###############################################################################
###############################################################################

###############################################################################
###############################################################################
###############################################################################
###
###  Notes:
###
###         1.  Need to add sections renaming organization names (removing 
###             line breaks and spaces before/after organization name) to the
###             script structure and logic section. (6/9/2017)
###
###         2.  Need to specify 'cash' and 'non-cash' fields for the financial
###             info section. (6/9/2017)
###
###         3.  This scraper will only scrape 990s that have been electronically
###             provided by the IRS! The initial 990 dump only included data up
###             to 2014 and subsequent electronic E-files.  Institutions are 
###             ONLY required to e-file their 990's with they have $10 million
###             in assets AND have 245+ employees.  Otherwise, the data is not
###             electronically filed and therefore unavailable for scraping
###             for data after 2014.  To get that data, you must specially
###             order a DVD or wait for the IRS to digitize paper claims (which
###             can literally take years). (6/9/2017)
###
###             See https://www.opensecrets.org/news/2016/06/irs-releasing-electronically-filed-nonprofit-tax-data/
###
###         4.  For R-specific XML scraping check out https://lecy.github.io/Open-Data-for-Nonprofit-Research/Quick_Guide_to_XML_in_R.html
###             (6/9/2017)
###
###############################################################################
###############################################################################
###############################################################################

###
###
###  Script Structure and Logic 
###
###  Section 1:  This section is used to initialize the script.
###
###      Section 1.1:    Loads packages necessary to run the script fully.
###
###      Section 1.2:    Clears working environment of all objects. This helps reduce
###                      inheritance errors from ghost code.
###
###      Section 1.3:    Set beginning time of script.
###
###
###  Section 2:  This section compiles a list of all available IRS documents for
###              designated non-profit organizations.
###
###      Section 2.1:    Input a list of the EIN numbers belonging to non-profits
###                      that information should be compiled on.
###
###      Section 2.2:    This loop fetches all electronic IRS documents related to
###                      EIN's input in section 3.  A list of all available objects
###                      is then compiled into a data frame called Compiled.Objects.List.
###
###      Section 2.3:    Saves fetched data to a CSV.
###
###      Section 2.4:    The IRS database fields do not match across all years.  This means
###                      that the XML path (which is what is actually pulled to retrieve data)
###                      differs across different IRS tax years.  In short, the IRS tax forms
###                      from before 2014 will have one XML path structure while IRS tax forms
###                      for 2014 and later will have another XML path structure.
###
###                      With that being explained, Section 5 subsets the Compiled.Objects.List
###                      data frame into IRS forms match one time period.  The end result two
###                      lists:  URL.List.Current and URL.List.Deprecated.  Both of these lists
###                      are then fed into separate loops to pull the desired data fields.  THey
###                      are fed into separate loops because the XML field paths do not match,
###                      which means the loops must differ slightly in parameter specification.
###
###  Section 3:  This section fetches financial information indicated in the Metric.List.  This
###              section queries the URL's pulled from section 2 and pulls the selected metrics.
###              The end result is a data frame and saved CSV containing rows of data of financial
###              data for every year.
###
###      Section 3.1:    Input a list of the desired financial metrics to be pulled from the 990s.
###                      The text strings need to exactly match the XML paths in the actual XML document.
###
###                      For this script, this means the values directly attributable to
###                      organizational financial information (revenue, employees, etc.)
###
###                      Note that you can view the entire structure of the XML object by
###                      using the xml_structure function and referncing your XML object.
###
###                      It also may be easier to inspect the structure nesting a handful of
###                      R XML commands like so:
###
###                      xml_name(xml_children(xml_find_first((INSERT_XML_OBJECT_HERE, "PUT PATH HERE"))))
###
###                      REVISION UPDATE: Earlier versions pasted the METRIC and not the PATH.  This caused
###                      zero-length problems because some metrics had different paths.   So, now the user
###                      indicates the path.
###
###
###      Section 3.2:  This is the loop for compiling information for the objects listed in 
###                    the URL.List.Current object, which represent IRS forms after 2014.  A previous version
###                    had code for pulling IRS data from before 2014, but the script has not been updated 
###                    to pull that information due to time.
###
###                    The loop pulls the data from the XML fields and loops to bind them together to form
###                    one complete row with complete financial information from a particular 990.
### 
###                    This process is repeated for every object listed in the URL.List.Current
###                    list and the data are row binded on top of each other to create one
###                    completed data frame with all of the information.  The compiled information
###                    will be stored in a data frame object called Compiled.Data.Current.
###
###                    In short, values are generated going from 'left to right' to populate columns
###                    to create a row. When a row is complete, the row is held in memory and then is
###                    added below the other previously created rows in memory.
###
###                    Also note that the 'if' statement for each pull is because some XML paths are 
###                    blank for some 990s.  If a path was blank, the saved object would actually be
###                    a zero-length object (not 0), which then makes it impossible to bind the
###                    objects together to form a data frame.  In short, the if statements are there
###                    to replace blank XML paths with -9999's as a placeholder so data frames can be
###                    bound together and looped.
###
###      Section 3.3:  This saves the information compiled in section 3.2 to a CSV.
###
###
###  Section 4:  This section fetches compensation data for individuals affiliated with the queried
###              non-profits.  Compensation data is pulled for each individual and creates a data
###              frame which will have 1 row for each person compensated by a non-profit.
###
###      Section 4.1:  Indicate the number of compensation fields to be scraped.
###
###                    20 is an arbitrary number.  20 was just used to ensure the capture
###                    of every possible executive listed in the IRS form.  The script
###                    pulls data from the IRS form from top to bottom -- and because
###                    IRS forms do not have a particular order of listing executives 
###                    (as in, the first person listed as an executive is not necessarily
###                    the CEO), this script scrapes 20 slots worth.  Most of the time this
###                    will mean the script will actually scrape blank fields because many
###                    organizations do not list 20 people.
###
###                    However, the na.omit() function is there to remove any possible
###                    scraped values that are actualyl blank.
###
###      Section 4.2:  Input a list of XML values that should be scraped from a
###                    specific IRS 990 form.  The text strings need to exactly match
###                    the XML paths in the actual XML document.
###
###                    For this script, this means the values directly attributable to
###                    executive compensation.
###
###                    Note that you can view the entire structure of the XML object by
###                    using the xml_structure function and referncing your XML object.
###
###                    It also may be easier to inspect the structure nesting a handful of
###                    R XML commands like so:
###
###                    xml_name(xml_children(xml_find_first((INSERT_XML_OBJECT_HERE, "PUT PATH HERE"))))
###
###      Section 4.3:  This is a triple-nested loop for compiling compensation data for individuals
###                    listed in the 990.
###
###                    The loop's first level is at the document level.
###
###                    The loop's second level is at the individual person level.
###
###                    The loop's third level is at the metric level.
###
###                    In short, the loop pulls an IRS document and then pulls compensation data for
###                    each person listed on the IRS form.
###
###                    Data is first compiled at the individual level, which then are bound at the
###                    document level.  Meaning, one person's data is collected and then that person's
###                    data is bound together with another person's data that is on the same document.
###                    This process is repeated for everyone listed on that document.
###
###                    Then, that data is bound to data belonging to other documents to create a data frame
###                    containing compensation data for all individuals in every document.
###
###      Section 4.4:  This saves the data compiled in section 4.3
###
###


###
###  Section 1.0: Beginning of Script
###

###
###  Section 1.1:  Load Packages
###

library(xml2)
library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)

###
###  Section 1.2: Clear working environment
###

rm(list = ls())

###
###  Section 1.3:  Set beginning time.
###

Beginning <- Sys.time()

###
###  Section 2.1:  Indicate Requested Institutions
###

EIN.List <- as.list(c("131788491" , #American Cancer Society - Unknown
                      "135613797" , #American Heart Association - 2015
                      "530196605" , #American Red Cross
                      "131635294" , #United Way
                      "580660607" , #Salvation Army
                      "362193608" , #Shriner's Hospital
                      "135562976" , #Boys and Girls Club of America  - 2014
                      "042774441" , #Boston's Children Hospital
                      "231352166" , #Children's Hospital of Philadelphia
                      "951690977" , #Children's Hospital of Los Angeles 
                      "351044585" , #ALSAC
                      "620646012" , #St. Jude
                      "310833936" , #Cincinnati
                      "910564748" , #Seattle
                      "741100555" , #Texas Children's Hospital
                      "010782751"   #Nationwide Childrens Hospital - Columbus
))

###
###  Section 2.2  Loop for fetching available 990 documents for requested IEN's.
###

i = 1

p = 1

for (EIN.Loop in EIN.List) {
  
  i = p
  
  tryCatch({
    
    print(paste("Querying data for EIN", EIN.Loop , sep = " "))
    
    Queried.Data.Index <- fromJSON(paste("http://irs-xml-search.herokuapp.com/search?ein=" , EIN.Loop , sep = "")) 
    
    Query.Object.Filter <- Queried.Data.Index %>%
      mutate(Tax.Period.Date = as.Date(paste(TAX_PERIOD, "01", sep = ""), format = "%Y%m%d"))
    
    if (exists(("Compiled.Objects.List"))) {Compiled.Objects.List <- rbind(Compiled.Objects.List, Query.Object.Filter)} else {Compiled.Objects.List <- Query.Object.Filter}
    
    print(paste("Return Information for" , unique(Queried.Data.Index$TAXPAYER_NAME) , "compiled." , sep = " "))
    
    print(paste( round( i / length(EIN.List) * 100, digits = 3) , "% complete." , sep = ""))
    
    p = i + 1
    
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

###
###  Section 2.3:  Save fetched 990 documents.
###

Now <- Sys.time()

Lesser.Now <- gsub("[^0-9\\.]", "", Now) 

write.csv(Compiled.Objects.List , file = paste(Lesser.Now , "-Requested-990-Forms-List.csv" , sep = ""), row.names =  FALSE)

getwd()

###
###  Section 2.4: Splice fetched documents by date.
###

Requested.Returns.Current <- Compiled.Objects.List %>%
  filter(Tax.Period.Date > as.Date("2014-01-01"))

URL.List.Current <- as.list(Requested.Returns.Current$url)

Requested.Returns.Deprecated <- Compiled.Objects.List %>%
  filter(Tax.Period.Date < as.Date("2014-01-01"))

URL.List.Deprecated <- as.list(Requested.Returns.Deprecated$url)


###
###  Section 3.0:
###

###
###  Section 3.1:  List desired financial data to be pulled from
###                990.
###

List.Of.Paths = as.list(c(
  "/Return/ReturnHeader/TaxPeriodEndDt" ,
  "/Return/ReturnHeader/TaxPeriodBeginDt" ,
  "/Return/ReturnHeader/Filer/EIN" ,
  "/Return/ReturnHeader/Filer/BusinessName" ,
  "/Return/ReturnHeader/Filer/USAddress/AddressLine1Txt" ,                                                                   
  "/Return/ReturnHeader/Filer/USAddress/CityNm" ,                                                                     
  "/Return/ReturnHeader/Filer/USAddress/StateAbbreviationCd" ,                                                                   
  "/Return/ReturnHeader/Filer/USAddress/ZIPCd" ,
  "/Return/ReturnData/IRS990/ActivityOrMissionDesc" ,                                                                            
  "/Return/ReturnData/IRS990/VotingMembersGoverningBodyCnt" ,                                                                    
  "/Return/ReturnData/IRS990/VotingMembersIndependentCnt" ,                                                                      
  "/Return/ReturnData/IRS990/TotalEmployeeCnt" ,                                                                                 
  "/Return/ReturnData/IRS990/TotalVolunteersCnt" ,                                                                               
  "/Return/ReturnData/IRS990/TotalGrossUBIAmt" ,                                                                                  
  "/Return/ReturnData/IRS990/NetUnrelatedBusTxblIncmAmt" ,                                                                        
  "/Return/ReturnData/IRS990/PYContributionsGrantsAmt",                                                                          
  "/Return/ReturnData/IRS990/CYContributionsGrantsAmt",                                                                          
  "/Return/ReturnData/IRS990/PYProgramServiceRevenueAmt",                                                                        
  "/Return/ReturnData/IRS990/CYProgramServiceRevenueAmt",                                                                        
  "/Return/ReturnData/IRS990/PYInvestmentIncomeAmt",                                                                             
  "/Return/ReturnData/IRS990/CYInvestmentIncomeAmt",                                                                             
  "/Return/ReturnData/IRS990/PYOtherRevenueAmt",                                                                                 
  "/Return/ReturnData/IRS990/CYOtherRevenueAmt",                                                                                 
  "/Return/ReturnData/IRS990/PYTotalRevenueAmt",                                                                                 
  "/Return/ReturnData/IRS990/CYTotalRevenueAmt",                                                                                 
  "/Return/ReturnData/IRS990/PYGrantsAndSimilarPaidAmt",                                                                         
  "/Return/ReturnData/IRS990/CYGrantsAndSimilarPaidAmt",                                                                         
  "/Return/ReturnData/IRS990/PYBenefitsPaidToMembersAmt",                                                                        
  "/Return/ReturnData/IRS990/CYBenefitsPaidToMembersAmt",                                                                        
  "/Return/ReturnData/IRS990/PYSalariesCompEmpBnftPaidAmt",                                                                      
  "/Return/ReturnData/IRS990/CYSalariesCompEmpBnftPaidAmt",                                                                      
  "/Return/ReturnData/IRS990/PYTotalProfFndrsngExpnsAmt",                                                                        
  "/Return/ReturnData/IRS990/CYTotalProfFndrsngExpnsAmt",                                                                        
  "/Return/ReturnData/IRS990/CYTotalFundraisingExpenseAmt" ,                                                                     
  "/Return/ReturnData/IRS990/PYOtherExpensesAmt" ,                                                                               
  "/Return/ReturnData/IRS990/CYOtherExpensesAmt" ,                                                                               
  "/Return/ReturnData/IRS990/PYTotalExpensesAmt" ,                                                                               
  "/Return/ReturnData/IRS990/CYTotalExpensesAmt",                                                                                
  "/Return/ReturnData/IRS990/PYRevenuesLessExpensesAmt" ,                                                                        
  "/Return/ReturnData/IRS990/CYRevenuesLessExpensesAmt" ,                                                                        
  "/Return/ReturnData/IRS990/TotalAssetsBOYAmt" ,                                                                                
  "/Return/ReturnData/IRS990/TotalAssetsEOYAmt"  ,                                                                               
  "/Return/ReturnData/IRS990/TotalLiabilitiesBOYAmt"  ,                                                                          
  "/Return/ReturnData/IRS990/TotalLiabilitiesEOYAmt"  ,                                                                          
  "/Return/ReturnData/IRS990/NetAssetsOrFundBalancesBOYAmt",                                                                     
  "/Return/ReturnData/IRS990/NetAssetsOrFundBalancesEOYAmt"
  
  
))

###
###  Section 3.2:  Loop for scraping metrics listed in section 3.1.
###

i = 1

p = 1

for (URL.Loop in URL.List.Current){
  
  i = p
  
  print(paste("Querying data for", URL.Loop , sep = " "))
  
  Queried.Data <- read_xml(x = URL.Loop, options=NULL )
  
  xml_ns_strip(Queried.Data)
  
  tryCatch({
    
    for (Path in List.Of.Paths) {
      
      Temp.Value <- xml_text(xml_find_all(Queried.Data, Path))
      
      if (length(Temp.Value) > 0) {Temp <- Temp.Value} else {Temp.Value <- "-99999"}
      
      Temp.DF <- as.data.frame(Temp.Value)
      
      colnames(Temp.DF)[1] <- gsub(".*/" , "", Path)
      
      if (exists(("Temp.Institutional.Data.Current"))) {Temp.Institutional.Data.Current <- cbind(Temp.Institutional.Data.Current, Temp.DF)} else {Temp.Institutional.Data.Current <- Temp.DF}
      
    }
    
    if (exists(("Compiled.Institutional.Data.Current"))) {Compiled.Institutional.Data.Current <- rbind(Compiled.Institutional.Data.Current, Temp.Institutional.Data.Current)} else {Compiled.Institutional.Data.Current <- Temp.Institutional.Data.Current}
    
    rm(Temp.Institutional.Data.Current)
    
    print(paste("Return Information for" , unique(URL.Loop) , "compiled." , sep = " "))
    
    print(paste( round( i / length(URL.List.Current) * 100, digits = 3) , "% complete." , sep = ""))
    
    p = i + 1
    
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


###
###  Sub-Section (not in section notes currently)
###
###  When the organization name data is pulled, there are line breaks before and after the actual organization name.
###  This means that the cell value of the organization name appears blank but actually is not (it is just on a 
###  different line.)
###
###  These two lines of code: 
###
###  1. Remove the line breaks from the cell. This puts the entire organizational name
###  on line line so it is visble.
###
###  2. Removes the line spaces that are before and after the oranizational name.
###
###  In short, these two small lines of code make the organizational name easier to read.
### 
###

Compiled.Institutional.Data.Current$BusinessName <- str_replace_all(Compiled.Institutional.Data.Current$BusinessName, "[\r\n]" , "")
Compiled.Institutional.Data.Current$BusinessName <- str_sub(Compiled.Institutional.Data.Current$BusinessName, 9, -7)

###
###  Section 3.3:  Save compiled financial data to CSV.
###

Now <- Sys.time()

Lesser.Now <- gsub("[^0-9\\.]", "", Now) 

write.csv(Compiled.Institutional.Data.Current, file = paste(Lesser.Now , "-Requested-Institutional-Data.csv" , sep = ""), row.names =  FALSE)


getwd()

###
###  Section 4.0:
###

###
###  Section 4.1:  Input number for querying compensation.
###

Exec.Numbers <- c("1" ,
                  "2" ,
                  "3" ,
                  "4" ,
                  "5" ,
                  "6" ,
                  "7" ,
                  "8" ,
                  "9" ,
                  "10" ,
                  "11" ,
                  "12" ,
                  "13" ,
                  "14" ,
                  "15" ,
                  "16" ,
                  "17" ,
                  "18" ,
                  "19" ,
                  "20")

###
###  Section 4.2:  Input desired metrics for pulling
###                compensation.
###

# List.Of.Paths <- c(
#   "/Return/ReturnHeader/Filer/EIN" ,
#   "/Return/ReturnHeader/Filer/BusinessName" ,
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/PersonNm" ,                                                 
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/TitleTxt" ,                                                
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/BaseCompensationFilingOrgAmt" ,                           
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/CompensationBasedOnRltdOrgsAmt"  ,                         
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/BonusFilingOrganizationAmount" ,                           
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/BonusRelatedOrganizationsAmt" ,                            
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/OtherCompensationFilingOrgAmt" ,                          
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/OtherCompensationRltdOrgsAmt" ,                           
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/DeferredCompensationFlngOrgAmt" ,                         
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/DeferredCompRltdOrgsAmt" ,                                
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/NontaxableBenefitsFilingOrgAmt" ,                          
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/NontaxableBenefitsRltdOrgsAmt" ,                           
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/TotalCompensationFilingOrgAmt" ,                           
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/TotalCompensationRltdOrgsAmt" ,                            
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/CompReportPrior990FilingOrgAmt" ,                         
#   "/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[1]/CompReportPrior990RltdOrgsAmt" 
#   
# )


###
###  Section 4.3:  Loop for compiling compensation data.
###

i = 1

p = 1

###
###  Section 4.2:  Input desired metrics for pulling
###                compensation.
###

List.Of.Metrics <- c(
  "PersonNm" ,
  "TitleTxt" ,
  "BaseCompensationFilingOrgAmt" ,
  "CompensationBasedOnRltdOrgsAmt"  ,
  "BonusFilingOrganizationAmount" ,
  "BonusRelatedOrganizationsAmt" ,
  "OtherCompensationFilingOrgAmt" ,
  "OtherCompensationRltdOrgsAmt" ,
  "DeferredCompensationFlngOrgAmt" ,
  "DeferredCompRltdOrgsAmt" ,
  "NontaxableBenefitsFilingOrgAmt" ,
  "NontaxableBenefitsRltdOrgsAmt" ,
  "TotalCompensationFilingOrgAmt" ,
  "TotalCompensationRltdOrgsAmt" ,
  "CompReportPrior990FilingOrgAmt" ,
  "CompReportPrior990RltdOrgsAmt"

  )


###
###  Section 4.3:  Loop for compiling compensation data.
###

i = 1

p = 1

for (URL.Loop in URL.List.Current){
  
  i = p
  
  print(paste("Querying data for", URL.Loop , sep = " "))
  
  tryCatch({
    
    Queried.Data <- read_xml(x = URL.Loop, options=NULL )
    
    xml_ns_strip(Queried.Data)
    
    ###
    ###  Pull metrics for compensated individuals
    ###
    
    for (I in Exec.Numbers){
      
      ###
      ###  This loop level loops through the designated metrics
      ###  for each individual compensated.
      ###
      
      for(Metric in List.Of.Metrics){
        
        Temp.Value <- xml_text(xml_find_first(Queried.Data, paste('/Return/ReturnData/IRS990ScheduleJ/RltdOrgOfficerTrstKeyEmplGrp[' , I , ']/' , Metric , sep = "" ) ))
        
        if (length(Temp.Value) > 0) {Temp <- Temp.Value} else {Temp.Value <- "-99999"}
        
        Temp.DF <- as.data.frame(Temp.Value)
        
        colnames(Temp.DF)[1] <- Metric
        
        if (exists(("Temp.Exec.Comp.Data.Current.Single"))) {Temp.Exec.Comp.Data.Current.Single <- cbind(Temp.Exec.Comp.Data.Current.Single, Temp.DF)} else {Temp.Exec.Comp.Data.Current.Single <- Temp.DF}
        
      }
      
      if (exists(("Temp.Exec.Comp.Data.Current"))) {Temp.Exec.Comp.Data.Current<- rbind(Temp.Exec.Comp.Data.Current, Temp.Exec.Comp.Data.Current.Single)} else {Temp.Exec.Comp.Data.Current<- Temp.Exec.Comp.Data.Current.Single}
      
      rm(Temp.Exec.Comp.Data.Current.Single)
      
    }
    
    ###
    ###  Establish EIN, Non-Profit Name, and Tax Year
    ###
    
    EIN <- xml_text(xml_find_all(Queried.Data, "/Return/ReturnHeader/Filer/EIN"))
    
    if (length(EIN) > 0) {EIN <- EIN} else {EIN <- "-99999"}
    
    Org.Name <- xml_text(xml_find_all(Queried.Data, "/Return/ReturnHeader/Filer/BusinessName"))
    
    if (length(Org.Name) > 0) {Org.Name <- Org.Name} else {Org.Name <- "-99999"}
    
    Tax.Year  <- xml_text(xml_find_all(Queried.Data, "/Return/ReturnHeader/TaxYr" ))
    
    if (length(Tax.Year) > 0) {Tax.Year <- Tax.Year} else {Tax.Year <- "-99999"}
    
    Temp.Exec.Comp.Data.Current$EIN <- EIN
    
    Temp.Exec.Comp.Data.Current$Org.Name <- Org.Name
    
    Temp.Exec.Comp.Data.Current$Tax.Year <- Tax.Year
    
    if (exists(("Compiled.Exec.Comp.Data.Current"))) {Compiled.Exec.Comp.Data.Current <- rbind(Compiled.Exec.Comp.Data.Current, Temp.Exec.Comp.Data.Current)} else {Compiled.Exec.Comp.Data.Current <- Temp.Exec.Comp.Data.Current}
    
    print(paste("Executive Compensation for" , Org.Name , "completed." , sep = " " ))
    
    rm(Temp.Exec.Comp.Data.Current)
    
    print(paste( round( i / length(URL.List.Current) * 100, digits = 3) , "% complete." , sep = ""))
    
    p = i + 1
    
    #if(is.na(Compiled.Exec.Comp.Data.Current$PersonNm)) {Compiled.Exec.Comp.Data.Current$PersonNm <- "UNKNOWN"} else {Compiled.Exec.Comp.Data.Current$PersonNm <- Compiled.Exec.Comp.Data.Current$PersonNm}
    
    #Compiled.Exec.Comp.Data.Current <- na.omit(Compiled.Exec.Comp.Data.Current)
    
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

###
###  Sub-Section (not in section notes currently)
###
###  When the organization name data is pulled, there are line breaks before and after the actual organization name.
###  This means that the cell value of the organization name appears blank but actually is not (it is just on a 
###  different line.)
###
###  These two lines of code: 
###
###  1. Remove the line breaks from the cell. This puts the entire organizational name
###  on line line so it is visble.
###
###  2. Removes the line spaces that are before and after the oranizational name.
###
###  In short, these two small lines of code make the organizational name easier to read.
### 
###

Compiled.Exec.Comp.Data.Current$Org.Name <- str_replace_all(Compiled.Exec.Comp.Data.Current$Org.Name, "[\r\n]" , "")
Compiled.Exec.Comp.Data.Current$Org.Name <- str_sub(Compiled.Exec.Comp.Data.Current$Org.Name, 9, -7)


###
###  Section 4.4: Save compiled compensation data to CSV. 
###

Now <- Sys.time()

Lesser.Now <- gsub("[^0-9\\.]", "", Now) 

write.csv(Compiled.Exec.Comp.Data.Current , file = paste(Lesser.Now , "-Requested-Compensation-Data.csv" , sep = ""), row.names =  FALSE)

getwd()

###
###  End of script.
###