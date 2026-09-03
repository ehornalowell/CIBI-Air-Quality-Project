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
library(corrplot) # correlation matrix visualizations
library(factoextra) # PCA visualizations
library(FactoMineR)
library(stats) #glms()

#########################################################################################
############################### read in barcode data ################################################

####### 1. Load BOLD data 

all_sdnhm <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/data/DNA_data/sdnhm_COIdata.csv") 
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

# 2g. Subset dataset to only rows from 02-2023 to 10-2023. Only using 2023 data for analysis.
clean_sdnhm_noNABIN <- clean_sdnhm_noNABIN %>%
  filter(Month_Year %in% c("Feb-23","Mar-23","Apr-23","May-23",
                           "Jun-23","Jul-23","Aug-23","Sep-23","Oct-23"))

# do the same for dataframe with bins = NA
clean_sdnhm <- clean_sdnhm %>%
  filter(Month_Year %in% c("Feb-23", "Mar-23", "Apr-23", "May-23", "Jun-23", "Jul-23", "Aug-23", "Sep-23", "Oct-23"))


##########################################################################################################################
####################### 3. summary stats & diversity calculations ###########################################################

####### 3. Quick summary stats 
# 3a. total number of specimens sequenced at five observatory sites, malaise traps: 71223

# 3b. number of specimens not assigned a BIN : 6347

# 3c. number of specimens ID to Fam, Subfam, Genus, Species -- bar graph
id.stats <- clean_sdnhm_noNABIN %>%
  summarise(across(everything(), ~sum(!is.na(.) & . !="")))
## specimens ID to Order = ALL 71223
## specimens ID to Family = 69299
## specimens ID to Subfamily = 32224
## specimens ID to Genus = 28143
## specimens ID to Species = 12848

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


##########################################################################################################################
###################################################################################################################
######### 4. calculating diversity measures     ########################################################################

# 4a. Calculating abundance for every unique month_year * exact.site combination for each Order. 
abund_orders <- clean_sdnhm_noNABIN %>%
  filter(Order %in% c("Diptera", "Hymenoptera", "Leipdoptera", "Hemiptera", "Coleoptera")) %>% #Filter out five orders will use in downstream analysis
  group_by(Exact.Site, Month_Year, Order) %>%
  summarize(Abundance = n(),
            .groups = "drop")


# 4b. Calculating Species Richness for every unique month_year * exact.site combination for each Order
spr_orders <- clean_sdnhm_noNABIN %>%
  group_by(Exact.Site, Month_Year, Order) %>%
  summarize(Species_Richness = n_distinct(BIN),
            .groups = "drop")


# 4c Calculating Shannon.Diversity Index (using Vegan Package) for month_year * exact.site combination for each Order
sdiv_orders <- clean_sdnhm_noNABIN %>%
  group_by(Exact.Site, Month_Year, Order, BIN) %>%
  summarise(BIN_abundance = n(),
            .groups = "drop") %>%
  group_by(Exact.Site, Month_Year, Order) %>%
  summarise(Shannon_Diversity = diversity(BIN_abundance, index = "shannon"),
            .groups = "drop")

# 4d combine abundance, species richness, and shannon biodiversity dataframes by site*date*order into a single dataframe   
stats_df <- sdiv_orders %>%
  left_join(spr_orders, by = c("Exact.Site", "Month_Year", "Order")) %>%
  left_join(abund_orders, by = c("Exact.Site", "Month_Year", "Order"))

# 4e. change Exact.Site names in stats_df to acronyms so it will match num.days.on.trap_df and I can successfully join both DFs
stats_df<- stats_df %>%
  mutate(Exact.Site = recode(Exact.Site, "Anza Borrego UC Reserve" = "ABUCR", "Picacho State Park" = "PSP", "Wheatley Ranch" = "WR", "Tierra Del Sol SDAA" = "TDS", "Lopez Ridge Vernal Pools" = "LRVP"))

# 4e load # of days on trap csv and then join # of days on trap csv to stats_df - will use # of days on trap as variable in model
#read in csv from github repo
num.days.on.trap_df <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/refs/heads/main/data/sampling_days.csv") 
num.days.on.trap_df <- num.days.on.trap_df %>% rename(Exact.Site = Site) #rename column so it matches stats_df column name
#subset data for Feb-23 through Oct-23
num.days.on.trap_df <- num.days.on.trap_df%>%
  filter(Month_Year %in% c("Feb-23","Mar-23","Apr-23","May-23",
                           "Jun-23","Jul-23","Aug-23","Sep-23","Oct-23"))

#join both dfs: 
stats_df <- stats_df %>%
  left_join(num.days.on.trap_df, by = c("Exact.Site", "Month_Year"))
#################################################################
###### use this DF for lms 
###### stats_df = site, date, order, abundance, species richness, and shannon diversity index
################################################################


######################################################################################################################
##################################### Load in PM2.5 data ############################################################# 

####### 5. Load pm2.5 data from github repo
## previous pm2.5 data without sep-aug 2022 data -- pm2.5 <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/data/abiotic_var/SDNHM.sites_V5GL0502.csv")
pm2.5 <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/data/abiotic_var/SDNHM.sites_V5GL0502.HybridPM25_alldates.csv")   

###### 6. CLEAN pm2.5 data

# 6a (i) convert month column format to month_year 
pm2.5_dates <- pm2.5 %>%
  mutate(
    Month_Year = format(ymd(month), "%b-%y")
  )
# 6a (ii) subset data for feb-23 through oct-23     
pm2.5_dates<- pm2.5_dates %>%
  filter(Month_Year %in% c("Feb-23","Mar-23","Apr-23","May-23",
                           "Jun-23","Jul-23","Aug-23","Sep-23","Oct-23"))


# 6b. Change Tierra Del Sol to Tierra Del Sol SDAA to match clean_sdnhm_noNABIN dataframe before merging
pm2.5_dates <- pm2.5_dates %>%
  mutate(Exact.Site = recode(Exact.Site,
                             "Tierra Del Sol" = "Tierra Del Sol SDAA"))

# 6c. remove columns i don't need for analysis
clean_pm2.5 <- pm2.5_dates %>%
  select(-c("month", "Exact.Site")) %>% 
  rename(Exact.Site = Site.Code) 

# 6d. combine clean_pm2.5 dataframe and stats_df dataframe 
stats_df <- stats_df %>%
  left_join(clean_pm2.5, by = c("Exact.Site", "Month_Year"))


####################################################################################################################################
################################## Load in SMOKE and metDATA#######################################################

# 7a. Read in Smoke csv
SMOKEdata <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/data/abiotic_var/hms_smoke_SDNHMsites_11feb2026.csv")

# 7b. Read in grid_MET csv 
METdata <- read.csv("https://raw.githubusercontent.com/ehornalowell/CIBI-Air-Quality-Project/main/data/abiotic_var/gridMET_SDNHMsites_monthlyvalues_11feb26.csv")
METdata <- rename(METdata, site.name = site_name)

# 7c. combine gridMET and SMOKE datasets into a single dataframe
s.met.data <- SMOKEdata %>%
  left_join(METdata, by = c("site.name", "date.on.trap", "date.off.trap"))
  #change month.x column from year-date-day format to month-year format
  s.met.data <- s.met.data %>%
    mutate(Month_Year = format(as.Date(month.x), "%b-%y"))
  #remove duplicate month column
  s.met.data <- select(s.met.data, -c("month.y", "month.x", "n_days.x", "n_days.y", "date.on.trap", "date.off.trap"))
  #rename column to Exact.Site to match PM2.5 and stats_df datasets
  s.met.data <- rename(s.met.data, Exact.Site = site.name)
  #move Month_Year column to be right after Exact.Site column
  s.met.data <- relocate(s.met.data, Month_Year, .after = Exact.Site)
  
  #add smoke to stats_df 
  stats_df <- stats_df %>%
    left_join(
      s.met.data %>% 
        select("Exact.Site", "Month_Year", "n_smoke"),
      by = c("Exact.Site", "Month_Year"))
  
####################
####################
# BEFORE CAN ADD MET DATA TO STATS_DF, NEED TO FIGURE OUT WHICH OF THESE VARIABLES i WILL INCLUDE IN MODEL. 
# TO DO SO:
  # A) COMBINE MET, SMOKE, AND PM2.5 DATASETS
  # B) RUN CORRELATION MATRIX
  # C) RUN PCA TO REDUCE # OF VARIABLES IN MODEL


### 8A. COMBINE MET, SMOKE, AND PM2.5 DATASETS
  #rename values in Exact.Site column in clean_pm2.5 dataframe. make them short hand so that they match values in s.met.data dataframe
clean_pm2.5 <- clean_pm2.5 %>%
  mutate(Exact.Site = recode(Exact.Site, "Anza Borrego UC Reserve" = "ABUCR", "Picacho State Park" = "PSP", "Wheatley Ranch" = "WR", "Tierra Del Sol SDAA" = "TDS", "Lopez Ridge Vernal Pools" = "LRVP"))

# 8a (i). join s.met.data and clean_pm2.5 dataset
abiotic.data <- clean_pm2.5 %>%
  left_join(s.met.data, by = c("Month_Year", "Exact.Site"))

# 8a (ii). remove columns that are not necessary for correlation matrix
clean.abiotic.data <- abiotic.data[, c(1:4, 10:12, 15, 18)]
  #switch column order to be cleaner
  clean.abiotic.data <- clean.abiotic.data %>%
    relocate("Month_Year", .before = "GWRPM25.ugm.3") %>%
    rename(PM2.5 = GWRPM25.ugm.3)

## add 
### 8B. RUN CORRELATION MATRIX TO DETERMINE FINAL ABIOTIC VARIABLES IN MODELS
  # 8B (i). Correlation for met DATA
  
  # Make DF to run correlation of meteorological var. excluding first few columns with dates on/off, site names, etc. 
  METcor <- cor(METdata[, 5:14], method ="pearson")
  
  # visualize correlation matrix (two different corr matrices)
    #First correlation matrix: all met variables 
  corrplot(METcor, tl.cex = 0.6, method = 'number') #  tl.cex reduces text size. method = number gives correlation numbers rather than different sized color dots. 
    #RESULTS:
      # max/min humidity, max/min air temp, and mean vapor pressure deficit are the most correlated. 
   
    #Second Correlation matrix: correlation removing some of the variables included previously that were too correlated. Removed: n days, surface down swelling, min temp, specific mean humidity, mean vapor pressure    
  METcor.2 <- cor(METdata[, c(6,7,8,11,14)]) 
  #visualize
  corrplot(METcor.2, tl.cex = 0.6, method = 'number') 
  # kept precipitation accumulation, max/min rel humidity mean, min rel humidity, max air temp mean, wind speed mean
  
# 8B (ii). Correlation for smoke data AND met data.
  
  # Correlation matrix for smoke data selecting columns that are appropriate 
  Scor <- cor(SMOKEdata[, 6:11], method = "pearson")
  corrplot(Scor, tl.cex = 0.6, method = 'number') ## think need to choose between 'n_smoke' (number of smoke days in sampling period) and 'perc_smoke' (percent of air filled with smoke ?) 
  
  #3. CORR combining smoke and meteorological datasets
  S.M.cor <- cor(s.met.data[, c(3, 9:11, 13:15, 17)], method = "pearson")
  corrplot(S.M.cor, tl.cex = 0.6, method = 'number')
  ## as of now, I want to keep 'n_smoke', 'precipitation_accumulation_mm', 'max_relative_humidity_mean', 'max_air_temperature_mean_K', 'wind_speed_ms_mean'. None of these are overly correlated, and I think they ahve most important biological significance. 
  
  #4. CORR using clean.abiotic.data df - these are the 6 abiotic variables I want to use in model: PM2.5, n_smoke, precip_accumulation_mm, max_rel_humidity_mean, max_air_temp_mean_K, wind_speed_ms_mean
  a.v.corr <- cor(clean.abiotic.data[, c(3:6, 8:9)], method = "pearson")
  corrplot(a.v.corr, tl.cex = 0.6, method = 'number')

### 8C. RUN PCA TO REDUCE # OF VARIABLES IN MODEL

# 8C. (i) PCA on four MET variables that will be included in final model:
  Meteor.pca.all <- PCA(clean.abiotic.data[ ,c(5,6,8,9)], scale = TRUE)
  
# 8C. (ii) Figures for pub:   
  # Biplot with points = month/site observation with symbols denoting the five different sites. 
  fig.SI.1 <- fviz_pca_biplot(
    Meteor.pca.all,
    habillage = clean.abiotic.data$Exact.Site,
    geom.ind = "point",
    pointsize = 4,
    addEllipses = FALSE,
    axes = c(1, 2),
    label = "var",
    repel = TRUE,
    col.var = "black",
    arrowsize = 0.7
  )
  Final.Fig.SI.1 <- fig.SI.1 +
    scale_color_manual(values = rep("black", 5)) + #set all site symbol colors = black. # of symbols = # of unique sites, sites to be distinguished by shape.
    scale_shape_manual(values = c(16, 17, 15, 18, 0)) + #assign specific point shapes to site
    labs(colour = "Sites", shape = "Sites") + #rename color and shape legend title to "Sites"
    scale_fill_discrete(guide = "none") #remove fill legend, aka default grouping legend ("Groups") that fviz_pca_biplot() creates when using habillage
  ## Export fig.
  ggsave("Fig.SI.1.pdf",
         plot = Final.Fig.SI.1,
         width = 18,
         height = 22,
         units = "cm")
  
  # scree plot- graph of eigenvalues/variances associated with components
  Final.Fig.SI.2 <- fviz_eig(Meteor.pca.all,
           barfill = "black",
           barcolor = "black",
           linecolor = "red") +
    labs(title = NULL, x = "Principle Components", y = "Percentage of variances explained") +
    theme_classic()
  ##Export Fig
  ggsave("Fig.SI.2.pdf",
         plot = Final.Fig.SI.2,
         width = 11,
         height = 11,
         units = "cm")
  
  # variable contribution figure - PC1
  Final.Fig.SI.3 <- fviz_contrib(Meteor.pca.all, choice = "var", axes = 1, fill = "black", color = "black") + #max_rel_humidity, max_air_temp, and precip_accumulation
    scale_x_discrete("Variables", labels = c("max_relative_humidity_mean" = "Max Relative Humidity Avg.", 
                                "max_air_temperature_mean_K" = "Max Air Temperature Avg.", 
                                "precipitation_accumulation_mm" = "Precipitation Accumulation", 
                                "wind_speed_ms_mean" = "Wind Speed Avg.")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = NULL, y = "% Contribution to PC1") 
  ## Export Fig 
  ggsave("Fig.SI.3.pdf",
         plot = Final.Fig.SI.3,
         width = 18, 
         height = 22, 
         units = "cm")
    
  # variable contribution figure - PC2
  Final.Fig.SI.4 <- fviz_contrib(Meteor.pca.all, choice = "var", axes = 2, fill = "black", color = "black") + #wind speed contributes mosts
    scale_x_discrete("Variables", labels = c("max_relative_humidity_mean" = "Max Relative Humidity Avg.", 
                                             "max_air_temperature_mean_K" = "Max Air Temperature Avg.", 
                                             "precipitation_accumulation_mm" = "Precipitation Accumulation", 
                                             "wind_speed_ms_mean" = "Wind Speed Avg.")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = NULL, y = "% Contribution to PC2") 
    ##Export Fig
    ggsave("Fig.SI.4.pdf",
           plot = Final.Fig.SI.4,
           width = 18,
           height = 22, 
           units = "cm")

# 8C. (iii) Extract individual PCA scores (site/month combinations) & add them to stats_df 
  
  #create DF containing PC1 and PC2 scores, along with corresponding Exact.Site and Month_year
  pca_scores <- bind_cols(
    # Select the Exact.Site & Month_Year columns from the original data set.
    # These identify each observation and will be used later to merge the PCA scores with other data frames.
    clean.abiotic.data %>%
      select(Exact.Site, Month_Year),
    #Extract PCA scores for each obs.
    as.data.frame(Meteor.pca.all$ind$coord) %>%
      #keep only first two PCs and rename them PC1 and PC2
      select(PC1 = Dim.1, PC2 = Dim.2)
  )
  
  # Add PCs back to stats_df
  stats_df <- left_join(
    stats_df,
    pca_scores,
    by = c("Exact.Site", "Month_Year")
  )
 
###################################################################################################################################################
##############################################      MODELS      ####################################################################################
#############################################################################################################################################
  
# 9. (a) 