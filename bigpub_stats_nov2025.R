########################  set working environment ################################


rm(list = ls()) # clear working environment


############################################################################################
############################## load packages ###########################################

library(dplyr)



#########################################################################################
############################### read in data ################################################

####### 1. Load BOLD data 

all_sdnhm <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/sdnhm_COIdata.csv") 
#csv is from another r script. file was too large to upload to Github repo. so I filtered out for all SDNHM data using collectors. 
#That is what this csv file is.

####### 2. Clean and subset BOLD data 

# 2a. filter Exact.site for observatory sites (5) and filter Sampling.protocol for malaise trap only

sdnhm_obs_mal <- all_sdnhm %>%
  filter(
    Exact.Site %in% c("Tierra Del Sol SDAA", "Lopez Ridge Vernal Pools", "Picacho State Park", "Wheatley Ranch", "Anza Borrego UC Reserve"),
    Sampling.Protocol %in% c("Malaise Trap", "Malaise trap"))

# 2b. make sure i have november 2023 data for PSP and ABUCR since that was missing originally
unique_sites_dates <- sdnhm_obs_mal %>%
  distinct(Exact.Site, Collection.Date.y) ## all pairs are there! yay! 

# 2c. create dataframe that removes all rows without assigned BIN
sdnhm_noNABINs <- sdnhm_obs_mal %>%
  filter(!is.na(BIN)) 
  
  #number of rows with BIN = NA
  sum(is.na(sdnhm_obs_mal$BIN)) #7505 specimens ~8.5%
  
# 2d. Using collection.date.y column, create a new column that only has month and year
  sdnhm_month.year <- sdnhm_obs_mal %>%
    mutate(
      Month_Year = format(as.Date(Collection.Date.y, format = "%d-%b-%y"), "%b-%y")
    )
    
    #do same for dataframe with no NA bins
    no.NA.BINs_month.year <- sdnhm_noNABINs %>%
      mutate(
        Month_Year = format(as.Date(Collection.Date.y, format = "%d-%b-%y", "%b-%y"))
      )

## calculate abundance

####### 2. Load climate data 
