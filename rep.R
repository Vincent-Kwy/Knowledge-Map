
library(readr)
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(gtsummary)
library(dplyr)

options(tigris_use_cache = TRUE)

# TIDYCENSUS: https://walker-data.com/tidycensus/index.html
# CENSUS HELP: https://walker-data.com/census-r/index.html 

#######################################################################  
#####################  GET CCS DATA FROM PLATFORM ####################
####################################################################### 

# Import dataframe from CCS platform
data <- read_csv("~/Library/CloudStorage/Box-Box/ccs-knowledge/ccs-data/Urban Heat Survey.csv")


# Extract name of object 
obj <- data[2, 2]
obj <- c(obj$...2)

rec_date <- data[6, 2]
rec_date <- c(rec_date$...2)

#MAKE QUESTION ROW COLNAMES
data <- data.frame(data)
names(data)[1:dim(data)[2]] <- data[9,]

#CLEAR ROWS FROM DATAFRAME
data <- data[-c(1:9),]

#EXTRACT ONLY DEMOGRAPHIC. 
rep_data <- data[,c(12:25)]

# DATA SHOULD BE UNIQUE USERNAME (SOME DATASETS ALLOW MULTUPLE USER ENTRIES)
# rep_data <- 

# ADD CENSUS rep_data, TRACT, CITY, BLOCK data
census_full1 <- rep_data %>% 
  geocode(
  address = 'Home address',
  method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies')
)

census_full1$census_tract_full <- paste(census_full1$state_fips, census_full1$county_fips,census_full1$census_tract, sep="")
census_full1$census_block_full <- paste(census_full1$state_fips, census_full1$county_fips,census_full1$census_tract,census_full1$census_block, sep="")


# Aggregate data by user selection in the APP with demographic variables to prepare for representiveness. The categories should match the survey we will use
agg_tract <- census_full1  %>% 
  group_by(census_tract_full) %>%
  select(Gender:`Education Level`,census_tract_full) %>%
  tbl_summary(
    by = census_tract_full,
    statistic = list(
      all_continuous() ~ "{median} ({mean},{sd})"
    )) 
  
agg_block <- census_full1  %>% 
  group_by(census_block_full) %>%
  select(Gender:`Education Level`,census_block_full) %>%
  tbl_summary(
    by = census_block_full,
    statistic = list(
      all_continuous() ~ "{median} ({mean},{sd})"
    ))



# CONVERT THE ABOVE RESULTS TO A DATAFRAME TO MERGE/COMPARE WITH ASC DATA
  
#######################################################################  
######  GET ACS DATA, BUT NEED TO DECIDE ON VARIABLES TO EXTRACT. #####
#######################################################################  


# https://walker-data.com/tidycensus/articles/basic-usage.html
# https://docs.google.com/document/d/1kr-1v03qlHZzEHIb0Uc507I0PtCVf1Gh9091lXtRVfI/edit 

# GET TRACT INFORMATION
wi_tracts_acs_raceeth <- get_acs(
    state = "WI",
    geography = "tract",
    variables = c(hispanic.no = "B03001_002E")
    )

# GET BLOCK INFORMATION...BLOCK ONLY AVAILABLE WITH get_decennial()
wi_block_acs_raceeth <- get_acs(
  state = "WI",
  geography = "cbg",
  variables = c(hispanic.no = "B03001_002E")
)


# GET COUNTY INFORMATION
wi_county_acs_raceeth <- get_acs(
  state = "WI",
  geography = "county",
  variables = c(hispanic.no = "B03001_002E")
)
#######################################################################  
##################  VISUAL Representativeness COMPARISON. #############
#######################################################################  
# COREY NOTES: https://docs.google.com/document/d/1KKYegJFzQxoAeYQ76arKVQMQHi3aMMToyHKBg6e2OOk/edit

# COMPARE GENDER

# COMPARE HISPANIC

# COMPARE RACE

# COMPARE AGE

# COMPARE HOUSEHOLD INCOME

# COMPARE EDUCATIONAL LEVEL 


#######################################################################  
##################  STATISTICAL Representativeness COMPARISON #########
#######################################################################  
# STARTING EXAMPLE: https://academic.oup.com/jamiaopen/article/4/3/ooab077/6374690 


