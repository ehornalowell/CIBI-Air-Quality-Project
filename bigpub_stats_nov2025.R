########################  set working environment ################################


rm(list = ls()) # clear working environment


############################################################################################
############################## load packages ###########################################

library(dplyr) #QC/QA
library(tidyr) #QC/QA
library(ggplot2) # figs
#library(patchwork) # simple way to combine separate ggplots into same graphic
library(vegan) # shannon diversity calculation
library(lubridate) # formatting dates 
library(purrr)



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

# 3e. recreate figure from above, but for each of the 5 observatory sites 
    tax_levels <- c("Order", "Family", "Subfamily", "Genus", "Species")
    
    tax_summary_site <- clean_sdnhm_noNABIN %>%
      # keep only site + the taxonomy columns we care about
      select(Exact.Site, all_of(tax_levels)) %>%
      
      # go to long format: one row per specimen × taxonomic level
      pivot_longer(
        cols = all_of(tax_levels),
        names_to = "Taxonomic_Level",
        values_to = "Taxon"
      ) %>%
      
      # group by site and taxonomic level
      group_by(Exact.Site, Taxonomic_Level) %>%
      summarise(
        Total = n(),  # total specimens at this site for this level
        Identified = sum(!is.na(Taxon) & Taxon != ""),
        Not_Identified = Total - Identified,
        .groups = "drop"
      ) %>%
      
      # wide → long for plotting identified vs not
      pivot_longer(
        cols = c(Identified, Not_Identified),
        names_to = "Status",
        values_to = "Count"
      ) %>%
      
      # order taxonomic levels for the x-axis
      mutate(
        Taxonomic_Level = factor(
          Taxonomic_Level,
          levels = c("Order", "Family", "Subfamily", "Genus", "Species")
        )
      )
    ##plot faceted fig!
    ggplot(tax_summary_site,
           aes(x = Taxonomic_Level, y = Count, fill = Status)) +
      geom_col() +
      facet_grid(. ~ Exact.Site, scales = "fixed", switch = "x") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside"
      ) +
      labs(
        x = "Taxonomic Level",
        y = "Number of Specimens",
        fill = ""
      )
    
    
######### 4. calculating diversity measures

# 4a (i). Calculating Abundance for every unique month_year * exact.site combination (aka for every month at every site)
    abund <- clean_sdnhm_noNABIN %>%
      group_by(Exact.Site, Month_Year)%>%
      summarize(Abundance = n(),
                .groups = "drop")
    
    #joining column back to original dataframe clean_sdnhm_noNABIN
    clean_sdnhm_noNABIN <- clean_sdnhm_noNABIN %>%
      left_join(abund, by = c("Exact.Site", "Month_Year"))
    
# 4a (ii). Calculating abundance for every unique month_year * exact.site combination for each Order. 
    abund_orders <- clean_sdnhm_noNABIN %>%
      group_by(Exact.Site, Month_Year, Order) %>%
      summarize(Abundance = n(),
                .groups = "drop")
    
    
# 4b (i). Calculating Species Richness for every unique month_year * exact.site combination (aka for every month at every site)
   spr <- clean_sdnhm_noNABIN %>%
     group_by(Exact.Site, Month_Year) %>%
     summarize(Species_Richness = n_distinct(BIN),
               .groups = "drop")
   
   # joining column back to original dataframe clean_sdnhm_noNABIN
   clean_sdnhm_noNABIN <- clean_sdnhm_noNABIN %>%
   left_join(spr, by = c("Exact.Site", "Month_Year"))

# 4b (ii). Calculating Species Richness for every unique month_year * exact.site combination for each Order
   spr_orders <- clean_sdnhm_noNABIN %>%
     group_by(Exact.Site, Month_Year, Order) %>%
     summarize(Species_Richness = n_distinct(BIN),
               .groups = "drop") 
   
   
# 4c (i). Calculating Shannon Diversity Index using vegan() pacakage
   sdiv <- clean_sdnhm_noNABIN %>%
     #count individuals per bin per site per month
     group_by(Exact.Site, Month_Year, BIN) %>%
     summarise(BIN_Abundance = n(), 
               .groups = "drop") %>%
     #now calculate shannon per site * month 
     group_by(Exact.Site, Month_Year) %>%
     summarise(Shannon.Diversity = diversity(BIN_Abundance, index = "shannon"),
               .groups = "drop")

  # add shannon diversity index to clean_sdnhm_noNABIN dataframe 
   clean_sdnhm_noNABIN <- clean_sdnhm_noNABIN %>%
     left_join(sdiv, by = c("Exact.Site", "Month_Year"))

# 4c (ii). Calculating Shannon.Diversity Index for month_year * exact.site combination for each Order
   sdiv_orders <- clean_sdnhm_noNABIN %>%
     group_by(Exact.Site, Month_Year, Order, BIN) %>%
     summarise(BIN_abundance = n(),
               .groups = "drop") %>%
     group_by(Exact.Site, Month_Year, Order) %>%
     summarise(Shannon_Diversity = diversity(BIN_abundance, index = "shannon"),
               .groups = "drop")

######################################################################################################################
##################################### Load in PM2.5 data ############################################################# 
   
####### 5. Load pm2.5 data from github repo
pm2.5 <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/SDNHM.sites_V5GL0502.csv")
   
###### 6. CLEAN pm2.5 data

# 6a. convert month column format to month_year 
   pm2.5_dates <- pm2.5  %>%
     mutate(
       Month_Year = format(as.Date(month, format = "%m/%d/%Y"), "%b-%y")
     )
# 6b. Change Tierra Del Sol to Tierra Del Sol SDAA to match clean_sdnhm_noNABIN dataframe before merging
   pm2.5_dates <- pm2.5_dates %>%
     mutate(Exact.Site = recode(Exact.Site,
                                       "Tierra Del Sol" = "Tierra Del Sol SDAA"))
   
# 6c. remove columns i don't need for analysis
   clean_pm2.5 <- pm2.5_dates %>%
     select(-c("month", "Site.Code"))

# 6d. combine clean_pm2.5 dataframe and clean_sdnhm_noNABIN dataframe 
   combined_poll.div <- clean_sdnhm_noNABIN %>%
     left_join(clean_pm2.5, by = c("Exact.Site", "Month_Year"))

# 6e. create dataframe with date, site, diversity measures, pm2.5 measures
   site_month_dataUSE <- combined_poll.div %>%
     group_by(Exact.Site, Month_Year) %>%
     slice(1L) %>%                 # keep just 1 row per site-month-- can do this because same value for abundance, sp richness, shannon diversity, and pm2.5 for all rows belonging to same site*date combo
     ungroup() %>%
     select(
       Exact.Site,
       Month_Year,
       Abundance,
       Species_Richness,
       Shannon.Diversity,
       GWRPM25.ugm.3
     )

# 6f. create dataframe with date, site, order, diversity measures, pm2.5 measures. 
   site_month_order_div_df <- list(sdiv_orders, spr_orders, abund_orders) %>%   # first all diversity measures 
     reduce(left_join, by = c("Exact.Site", "Month_Year", "Order"))
   site_month_order_df <- site_month_order_div_df %>%                           # add pm2.5 data
     left_join(clean_pm2.5, by = c("Exact.Site", "Month_Year"))
   
###########################################################################################################################
################################## Insect diversity and PM2.5 data visualization ############################################
   
######## 7. Plot shannon div with pm2.5 across dates for each site
   
# 7a. convert month_date into real dates and put them in chronological order
   
   site_month_dataUSE <- site_month_dataUSE %>%
     mutate(
       Month_Year_date = parse_date_time(Month_Year, "b-y") # "Aug-22" is a real date
     ) %>%
     arrange(Month_Year_date) %>%
     mutate(Month_Year = factor(Month_Year, levels = unique(Month_Year)))
   
# 7b. compute scaling factor for PM2.5 so that it is correctly positioned (vertically wise) on right y-axis.
   max_shannon <- max(site_month_dataUSE$Shannon.Diversity, na.rm = TRUE)
   max_pm25    <- max(site_month_dataUSE$GWRPM25.ugm.3,      na.rm = TRUE)
   
   scale_factor <- max_shannon / max_pm25
   
# 7c.   create facetted dual-axis plot
   p_shann <- ggplot(site_month_dataUSE, aes(x = Month_Year, color = Exact.Site)) +
     # Shannon diversity — solid line
     geom_line(
       aes(y = Shannon.Diversity, group = Exact.Site),
       size = 1
     ) +
     geom_point(
       aes(y = Shannon.Diversity, group = Exact.Site),
       size = 2
     ) +
     # PM2.5 — dashed line, same color as site
     geom_line(
       aes(y = GWRPM25.ugm.3 * scale_factor, group = Exact.Site),
       linetype = "dashed",
       size = 0.9
     ) +
     geom_point(
       aes(y = GWRPM25.ugm.3 * scale_factor, group = Exact.Site),
       shape = 1,   # hollow points so they look different
       size = 2
     ) +
     scale_y_continuous(
       name = "Shannon diversity",
       sec.axis = sec_axis(
         ~ . / scale_factor,
         name = "PM2.5 (µg/m³)"
       )
     ) +
     facet_wrap(~ Exact.Site, ncol = 1) +  # or ncol = 2 if you prefer a grid
     labs(
       x = "Date",
       color = "Site",
       title = "Shannon diversity vs. PM2.5"
     ) +
     theme_classic() +
     theme(
       axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom"
     )

# 7d. scatterplot 
    ggplot(site_month_dataUSE, aes(x = GWRPM25.ugm.3, y = Shannon.Diversity, color = Exact.Site)) +
      geom_point(
        aes(group = Exact.Site)
      ) #
     # geom_line(
     #   aes(group = Exact.Site),
     #   size = 2
     # ) +
      theme_classic()
   

# 7e. scatterplot for dip, lep, coleo
      ## Diptera: 
          site_month_order_df %>%
            mutate(highlight = ifelse(Order == "Diptera", "Diptera", "Other")) %>%
            ggplot(aes(x = GWRPM25.ugm.3, y = Shannon_Diversity, color = highlight)) +
            geom_point() +
            scale_color_manual(values = c("Diptera" = "blue4", "Other" = "lightgrey")) +
            theme_classic() +
            labs(color = "")
      ## Lepidoptera: 
          site_month_order_df %>%
            mutate(highlight = ifelse(Order == "Lepidoptera", "Lepidoptera", "Other")) %>%
            ggplot(aes(x = GWRPM25.ugm.3, y = Shannon_Diversity, color = highlight)) +
            geom_point() +
            scale_color_manual(values = c("Lepidoptera" = "purple3", "Other" = "lightgrey")) +
            theme_classic() +
            labs(color = "")
      ## Coleoptera:
          site_month_order_df %>%
            mutate(highlight = ifelse(Order == "Coleoptera", "Coleoptera", "Other")) %>%
            ggplot(aes(x = GWRPM25.ugm.3, y = Shannon_Diversity, color = highlight)) +
            geom_point() +
            scale_color_manual(values = c("Coleoptera" = "red", "Other" = "lightgrey")) +
            theme_classic() +
            labs(color = "")
      ## Hymenoptera    
          site_month_order_df %>%
            mutate(highlight = ifelse(Order == "Hymenoptera", "Hymenoptera", "Other")) %>%
            ggplot(aes(x = GWRPM25.ugm.3, y = Shannon_Diversity, color = highlight)) +
            geom_point() +
            scale_color_manual(values = c("Hymenoptera" = "turquoise3", "Other" = "lightgrey")) +
            theme_classic() +
            labs(color = "")

# 7f. all four orders, dip, lep, coleo, hym on same fig - scatterplot, pm2.5 vs shannon div.
          
  # Define your 4 focal orders + their colors
  focal_colors <- c(
    "Diptera"       = "blue4",
    "Lepidoptera"   = "purple3",
    "Coleoptera"    = "red",
    "Hymenoptera"   = "turquoise3",
    "Other"         = "lightgrey"
  )
  #fig
  site_month_order_df %>%
    mutate(Order_group = ifelse(Order %in% names(focal_colors)[1:4], Order, "Other")) %>%
    ggplot(aes(x = GWRPM25.ugm.3, y = Shannon_Diversity, color = Order_group)) +
    geom_point() +
    scale_color_manual(values = focal_colors) +
    theme_classic() +
    labs(color = "Order")


######## 8. Plot Species_Richness with pm2.5 across dates for each site   

# 8a. Computing scaling factor for PM2.5       
   max_Sp_Richness <- max(site_month_dataUSE$Species_Richness, na.rm = TRUE)
   #use max pm2.5 from step 7b.
   
   scale_factor.b <- max_Sp_Richness / max_pm25
   
# 8b. create facetted dual axis plot- sp. richness and pm2.5 values over time. 
  p_rich <- ggplot(site_month_dataUSE, aes(x = Month_Year, color = Exact.Site)) +
     # Shannon diversity — solid line
     geom_line(
       aes(y = Species_Richness, group = Exact.Site),
       size = 1
     ) +
     geom_point(
       aes(y = Species_Richness, group = Exact.Site),
       size = 2
     ) +
     # PM2.5 — dashed line, same color as site
     geom_line(
       aes(y = GWRPM25.ugm.3 * scale_factor.b, group = Exact.Site),
       linetype = "dashed",
       size = 0.9
     ) +
     geom_point(
       aes(y = GWRPM25.ugm.3 * scale_factor.b, group = Exact.Site),
       shape = 1,   # hollow points so they look different
       size = 2
     ) +
     scale_y_continuous(
       name = "Species Richness",
       sec.axis = sec_axis(
         ~ . / scale_factor.b,
         name = "PM2.5 (µg/m³)"
       )
     ) +
     facet_wrap(~ Exact.Site, ncol = 1) +  # or ncol = 2 if you prefer a grid
     labs(
       x = "Date",
       color = "Site",
       title = "Species richness vs. PM2.5"
     ) +
     theme_classic() +
     theme(
       axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom"
     )

# 8c. scatterplot 
  ggplot(site_month_dataUSE, aes(x = GWRPM25.ugm.3, y = Species_Richness, color = Exact.Site)) +
    geom_point(
      aes(group = Exact.Site)
    ) +
    geom_line(
      aes(group = Exact.Site),
      size = 2
    ) +
  theme_classic()
  
  
########## 9. Plot Abundance with pm2.5 across dates for each site   

# 9a. Computing scaling factor for PM2.5       
   max_Abundance <- max(site_month_dataUSE$Abundance, na.rm = TRUE)
   #use max pm2.5 from step 7b.
   
   scale_factor.c <- max_Abundance / max_pm25
   
# 9b. create faceted dual axis plot- abundance and pm2.5 values over time. 
  p_abund <- ggplot(site_month_dataUSE, aes(x = Month_Year, color = Exact.Site)) +
     # Shannon diversity — solid line
     geom_line(
       aes(y = Abundance, group = Exact.Site),
       size = 1
     ) +
     geom_point(
       aes(y = Abundance, group = Exact.Site),
       size = 2
     ) +
     # PM2.5 — dashed line, same color as site
     geom_line(
       aes(y = GWRPM25.ugm.3 * scale_factor.c, group = Exact.Site),
       linetype = "dashed",
       size = 0.9
     ) +
     geom_point(
       aes(y = GWRPM25.ugm.3 * scale_factor.c, group = Exact.Site),
       shape = 1,   # hollow points so they look different
       size = 2
     ) +
     scale_y_continuous(
       name = "Abundance",
       sec.axis = sec_axis(
         ~ . / scale_factor.c,
         name = "PM2.5 (µg/m³)"
       )
     ) +
     facet_wrap(~ Exact.Site, ncol = 1) +  # or ncol = 2 if you prefer a grid
     labs(
       x = "Date",
       color = "Site",
       title = "Abundance vs. PM2.5"
     ) +
     theme_classic() +
     theme(
       axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "bottom"
     )
   
# 9c. Scatterplot
  ggplot(site_month_dataUSE, aes(x = GWRPM25.ugm.3, y = Abundance, color = Exact.Site)) +
    geom_point(
      aes(group = Exact.Site)
    ) +
    geom_line(
      aes(group = Exact.Site),
      size = 2
    ) +
  theme_classic()
   
   