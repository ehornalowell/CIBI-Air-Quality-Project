########################  set working environment ################################


rm(list = ls()) # clear working environment


############################################################################################
############################## load packages ###########################################

library(dplyr)
library(ggplot2)
library(tidyr)


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

    
    

##########################################################################################################################
####################### summary stats & diversity calculations ###########################################################

####### 3. Quick summary stats 
# 3a. total number of specimens sequenced at five observatory sites, malaise traps: 94,122
    
# 3b. number of specimens not assigned a BIN : 7505
    
# 3c. number of specimens ID to Fam, Subfam, Genus, Species -- bar graph
    id.stats <- clean_sdnhm_noNABIN %>%
      summarise(across(everything(), ~sum(!is.na(.) & . !="")))
          ## specimens ID to Order = ALL 86617
          ## specimens ID to Family = 84477
          ## specimens ID to Subfamily = 39871
          ## specimens ID to Genus = 35892
          ## specimens ID to Species = 15798
    
    # make df long for fig
    id.stats.long <- id.stats %>%
      pivot_longer(
        cols = everything(),
        names_to = "Taxonomic_Level",
        values_to = "Specimen_Count"
      )
    
    #subset specific rows 
    id.stats.long.filtered <- id.stats.long %>%
      filter(Taxonomic_Level %in% c("Order", "Family", "Subfamily", "Genus", "Species"))
    
    #reorder rows so figure is in taxonomic order - highest to lowest level
    id.stats.long.filtered$Taxonomic_Level <- factor(
      id.stats.long.filtered$Taxonomic_Level,
      levels = c("Order", "Family", "Subfamily", "Genus", "Species")
    )
    
    #simple bar graph
    ggplot(id.stats.long.filtered, aes(x = Taxonomic_Level, y = Specimen_Count)) +
      geom_col() +
      theme_classic()
    
# 3d. number of specimens ID to level vs. not identified to that level. -- figure 
    # calculate total # of specimens in your dataset
    total_n <- nrow(clean_sdnhm_noNABIN)
    
    # create vector of taxonomy columns im about to evaluate
    tax_levels <- c("Order", "Family", "Subfamily", "Genus", "Species")
    
    # create new table
    tax_summary <- tibble(
      Taxonomic_Level = tax_levels, # first column will be taxonomic levels
      Identified = sapply( #apply same calculation to each element of tax_apply (aka each column)
        tax_levels,
        function(col) sum(!is.na(clean_sdnhm_noNABIN[[col]]) & clean_sdnhm_noNABIN[[col]] != "") # calculate how many specimens were identified at each level
      )
    ) %>%
      mutate(
        Total = total_n,
        Not_Identified = Total - Identified ##building two more columns in dataframe 
      ) %>%
      pivot_longer( #pivot from wide to long format to be able to plot
        cols = c(Identified, Not_Identified),
        names_to = "Status",
        values_to = "Count"
      )
    
    #order taxonomic levels properly for xaxis order of columns
    tax_summary <- tax_summary %>%
      mutate(
        Taxonomic_Level = factor(
          Taxonomic_Level,
          levels = c("Order", "Family", "Subfamily", "Genus", "Species")
        )
      )
    
    #plot figure! one bar per taxonomic level, with identified vs. not identified. stacked
    ggplot(tax_summary, aes(x = Taxonomic_Level, y = Count, fill = Status)) +
      geom_col() +
      theme_classic() +
      labs(
        x = "Taxonomic Level",
        y = "Number of Specimens",
        fill = ""
      )

############    
    