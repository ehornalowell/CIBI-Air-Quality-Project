## testing 123
########################  set working environment ################################


rm(list = ls()) # clear working environment


############################################################################################
############################## load packages ###########################################

library(dplyr)



#########################################################################################
############################### read in data ################################################

####### 1. Load BOLD data 

all_sdnhm <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/sdnhm_COIdata.csv")

#filter Exact.site for observatory sites (5)
#sdnhm_obs <- filter(all_sdnhm, Exact_site %in% c("Tierra_Del_Sol_SDAA", "Lopez_Ridge_Vernal_Pools", "Picacho_State_Park", "Wheatley_Ranch", "Anza_Borrego_UC_Reserve"))

#filter Sampling_protocol for only malaise trap
#sdnhm_obs_mal <- filter(sdnhm_obs, Sampling_protocol == "Malaise_Trap")

## calculate abundance

####### 2. Load climate data 
