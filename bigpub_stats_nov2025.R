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

#filter Exact.site for observatory sites (5) and filter Sampling.protocol for malaise trap only

sdnhm_obs_mal <- all_sdnhm %>%
  filter(
    Exact.Site %in% c("Tierra_Del_Sol_SDAA", "Lopez_Ridge_Vernal_Pools", "Picacho_State_Park", "Wheatley_Ranch", "Anza_Borrego_UC_Reserve"),
    Sampling.Protocol == "Malaise trap")

#filter Sampling_protocol for only malaise trap
#sdnhm_obs_mal <- filter(sdnhm_obs, Sampling_protocol == "Malaise_Trap")

## calculate abundance

####### 2. Load climate data 
