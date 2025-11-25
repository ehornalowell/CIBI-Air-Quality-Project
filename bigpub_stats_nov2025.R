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

# 2b. Mutate "Malaise trap" to match "Malaise Trap" so only one unique value in sampling.protocol column
sdnhm_obs_mal <- sdnhm_obs_mal %>%
  mutate(Sampling.Protocol = recode(Sampling.Protocol,
                                    "Malaise trap" = "Malaise Trap"))

# 2c. make sure i have november 2023 data for PSP and ABUCR since that was missing originally
unique_sites_dates <- sdnhm_obs_mal %>%
  distinct(Exact.Site, Collection.Date.y) ## all pairs are there! yay! 

# 2d. create dataframe that removes all rows without assigned BIN
sdnhm_noNABINs <- sdnhm_obs_mal %>%
  filter(!is.na(BIN)) 
  
  #number of rows with BIN = NA
  sum(is.na(sdnhm_obs_mal$BIN)) #7505 specimens ~8.5%
  
# 2e. Using collection.date.y column, create a new column that only has month and year
  sdnhm_month.year <- sdnhm_obs_mal %>%
    mutate(
      Month_Year = format(as.Date(Collection.Date.y, format = "%d-%b-%y"), "%b-%y")
    )
    
    #do same for dataframe with no NA bins
    no.NA.BINs_month.year <- sdnhm_noNABINs %>%
      mutate(
        Month_Year = format(as.Date(Collection.Date.y, format = "%d-%b-%y"), "%b-%y")
      )

# 2f. Remove unnecessary columns - only keep ones for downstream analysis 
    clean_sdnhm <- sdnhm_month.year %>%
      select(-c(Project.Code, Identifier, Collectors, Collection.Date.y, Elev, Collection.Date.Accuracy, Habitat, Sampling.Protocol))
      
    # do the same for dataframe without values BIN = NA
    clean_sdnhm_noNABIN <- no.NA.BINs_month.year %>%
      select(-c(Project.Code, Identifier, Collectors, Collection.Date.y, Elev, Collection.Date.Accuracy, Habitat, Sampling.Protocol))


####### 3. Quick summary stats 
    ## total number of specimens sequenced at five observatory sites, malaise traps: 94,122
    
    ## number of specimens not assigned a BIN : 7505
    
    ## number of specimens ID to Fam, Subfam, Genus, Species
    
