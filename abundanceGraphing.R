library(coda)
library(ggplot2)
library(dplyr)
library(scales)

#### Loading in & Wrangling Model Results ####
# Let's load in the results
my_mod <- readRDS("./results/modelResults.RDS")

# The output of a JAGS model is kind of unwieldy. It's much easier to work with
# if we break out the iterations into a matrix structure
mc <- coda::as.mcmc.list(my_mod$mcmc)
mc <- as.matrix(mc)
# This dataset includes all iterations. We don't really need /that/ much
# data to make the figures, so let's sub-sample it out to a more manageable
# number
set.seed(554)
mc_sub <- mc[sample(1:nrow(mc), 20000), ]
rm(mc)
# Finally we'll use @mfidino's function 'split_mcmc' to make it easier to call
# individual variables
source("./functions/split_mcmc.R")
mc <- split_mcmc(mc_sub)
rm(mc_sub, my_mod)

# Let's get some plausible covariate ranges for the stuff we're interested in 
# graphing
load("./results/data_list.Rdata")
range(data_list$PM)    # -1.77 to 2.43
range(data_list$smoke) # -0.65 to 3.30
range(data_list$PC1)   # -1.75 to 4.86
# remember these are the scaled numbers, not the "real" covariate ranges. we
# will make some "pretty" numbers to graph with later

#### Abundance & PM2.5 ####
# create the dataset of covariates
predV_PM <- cbind(1,                                # intercept
                  seq(-1.77, 2.43, length.out=200), # range of PM2.5 data
                  mean(data_list$smoke),            # other variables held at their means
                  mean(data_list$PC1))
predV_PM <- t(predV_PM)
# create the matrix of model coefficients
pred_Lambda <- array(NA,
                    dim = c(nrow(mc$mu.mu.beta0),
                            ncol(predV_PM),
                            4))                   # because we have 3 Orders & and the overall response
pred_Lambda[,,1] <- cbind(mc$mu.beta0[,1], mc$mu.beta1[,1], mc$mu.beta2[,1], 
                              mc$mu.beta3[,1]) %*% predV_PM
pred_Lambda[,,2] <- cbind(mc$mu.beta0[,2], mc$mu.beta1[,2], mc$mu.beta2[,2], 
                              mc$mu.beta3[,2]) %*% predV_PM
pred_Lambda[,,3] <- cbind(mc$mu.beta0[,3], mc$mu.beta1[,3], mc$mu.beta2[,3], 
                              mc$mu.beta3[,3]) %*% predV_PM
pred_Lambda[,,4] <- cbind(mc$mu.mu.beta0, mc$mu.mu.beta1, mc$mu.mu.beta2, 
                          mc$mu.mu.beta3) %*% predV_PM
# remember lambda is part of a log function in the model. so to transform
# back to counts, we need to take the exponent
count_Lambda <- exp(pred_Lambda) 

# remember ALSO that the model is autoregressive, so we need to account
# for that term and its effects on the final count(s) of insects
predVauto_PM <- cbind(1,                                # intercept
                      seq(-1.77, 2.43, length.out=200), # range of PM2.5 data
                      mean(data_list$smoke),
                      mean(data_list$PC1),
                      1)                                # for the autoregressive term
predVauto_PM <- t(predVauto_PM)
predauto_Lambda <- array(NA, dim = c(nrow(mc$mu.omega),
                                     ncol(predVauto_PM),
                                     4))
predauto_Lambda[,,1] <- cbind(mc$mu.beta0[,1], mc$mu.beta1[,1], mc$mu.beta2[,1], 
                              mc$mu.beta3[,1], mc$mu.phi) %*% predVauto_PM
predauto_Lambda[,,2] <- cbind(mc$mu.beta0[,2], mc$mu.beta1[,2], mc$mu.beta2[,2], 
                              mc$mu.beta3[,2], mc$mu.phi) %*% predVauto_PM
predauto_Lambda[,,3] <- cbind(mc$mu.beta0[,3], mc$mu.beta1[,3], mc$mu.beta2[,3], 
                              mc$mu.beta3[,3], mc$mu.phi) %*% predVauto_PM
predauto_Lambda[,,4] <- cbind(mc$mu.mu.beta0, mc$mu.mu.beta1, mc$mu.mu.beta2, 
                              mc$mu.mu.beta3, mc$mu.phi) %*% predVauto_PM
countAuto_Lambda <- exp(predauto_Lambda)

# so the true count of individuals is added together, and we'll get some
# quantiles for graphin the credible intervals
trueLambda <- count_Lambda + countAuto_Lambda
data <- apply(trueLambda,
              c(2,3),
              quantile,
              probs = c(0.025, 0.25, 0.5, 0.75, 0.975))


# now, the actual graph part
# but first, let's return to the "pretty" numbers for the axis labels
# let's parse down the data to the range we actually used
rawData <- read.csv("./data/stats_df.csv")
rawData$Month <- lubridate::parse_date_time(rawData$Month_Year, "b-y")
filteredData <- rawData %>%
  filter(between(Month, as.Date("2023-02-01"), as.Date("2023-10-01"))) %>%
  filter_out(Order == "Hemiptera") %>%
  group_by(BIN) %>%
  mutate(totalAbundance = sum(Abundnace)) %>%
  filter_out(totalAbundance < 220)
range(filteredData$PM2.5)
# the original data ranges from 4.2 to 15 ppm

# now let's bind it all together in a single dataframe for ggplot
abun_PM2.5 <- rbind(data.frame(group = "Overall Average",
                               order = "Overall\nAverage",
                               PM = seq(4, 15, length.out = 200), # "real" values of PM2.5
                               lambda = data[3,,4],
                               upper = data[4,,4],
                               lower = data[2,,4]),
                    data.frame(group = "Order-Specific",
                               order = "Diptera",
                               PM = seq(4, 15, length.out = 200),
                               lambda = data[3,,1],
                               upper = min(data[2,,4]),
                               lower = min(data[2,,4])),
                    data.frame(group = "Order-Specific",
                               order = "Hymenoptera",
                               PM = seq(4, 15, length.out = 200),
                               lambda = data[3,,2],
                               upper = min(data[2,,4]),
                               lower = min(data[2,,4])),
                    data.frame(group = "Order-Specific",
                               order = "Lepidoptera",
                               PM = seq(4, 15, length.out = 200),
                               lambda = data[3,,3],
                               upper = min(data[2,,4]),
                               lower = min(data[2,,4]))
                    )

p1<-ggplot(abun_PM2.5, aes(x = PM, y = lambda)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = order), 
              show.legend = FALSE) +
  scale_fill_manual(values = c("#FFF","#FFF","#FFF","#e0e0e0")) +
  geom_line(aes(color = order, linewidth = group)) +
  scale_color_manual(values = c("#56b4e9","#e69f00","#cc79a7","#000")) +
  scale_linewidth_manual(values = c(0.75,1.25), guide = "none") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_log10(expand = c(0,0),
                guide = "axis_logticks")+
  labs(x=expression("PM"[2.5]~"(ppm)"), 
                    y="Abundance", color="", linewidth="") +
  annotate("point", x = 9.5, y = 9.1, shape = 42, size = 6, color = "#cc79a7") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=8), axis.title=element_text(size=10), 
        legend.text=element_text(size=8))
ggsave("./results/abundPM25.eps", plot = p1,
       width = 11, height = 8, units = "cm", dpi = 600)

#### Abundance & Smoke ####
# create the dataset of covariates
# "Smoke days" is not a continuous variable, but a count of days. So I'll need 
# to center and scale the full range of possible smoke-days from the original
# dataset
rawData <- read.csv("./data/stats_df.csv")
rawData$Month <- lubridate::parse_date_time(rawData$Month_Year, "b-y")
filteredData <- rawData %>%
  filter(between(Month, as.Date("2023-02-01"), as.Date("2023-10-01"))) %>%
  filter_out(Order == "Hemiptera") %>%
  group_by(BIN) %>%
  mutate(totalAbundance = sum(Abundnace)) %>%
  filter_out(totalAbundance < 160)
mean(filteredData$n_smoke) # 1.058154
sd(filteredData$n_smoke)   # 1.721541
# the original data ranges from 0 to 7
smokeScale <- c(-0.6205646, -0.0447429, 0.5310788, 1.169, 1.682722, 2.258544, 
                2.834366, 3.410187)
# these are basically the centered & scaled versions of 0,1,2,3,4,5,6,7

# then we'll do the same thing as we did for PM2.5 to create an
# array of model coefficients
predV_Smoke <- cbind(1,                
                  mean(data_list$PM), 
                  smokeScale,
                  mean(data_list$PC1))
predV_Smoke <- t(predV_Smoke)

pred_Lambda <- array(NA,
                     dim = c(nrow(mc$mu.mu.beta0),
                             ncol(predV_Smoke),
                             4))                   
pred_Lambda[,,1] <- cbind(mc$mu.beta0[,1], mc$mu.beta1[,1], mc$mu.beta2[,1], 
                          mc$mu.beta3[,1]) %*% predV_Smoke
pred_Lambda[,,2] <- cbind(mc$mu.beta0[,2], mc$mu.beta1[,2], mc$mu.beta2[,2], 
                          mc$mu.beta3[,2]) %*% predV_Smoke
pred_Lambda[,,3] <- cbind(mc$mu.beta0[,3], mc$mu.beta1[,3], mc$mu.beta2[,3], 
                          mc$mu.beta3[,3]) %*% predV_Smoke
pred_Lambda[,,4] <- cbind(mc$mu.mu.beta0, mc$mu.mu.beta1, mc$mu.mu.beta2, 
                          mc$mu.mu.beta3) %*% predV_Smoke
count_Lambda <- exp(pred_Lambda) 

predVauto_Smoke <- cbind(1,                
                      mean(data_list$PM), 
                      smokeScale,
                      mean(data_list$PC1),
                      1)                   
predVauto_Smoke <- t(predVauto_Smoke)
predauto_Lambda <- array(NA, dim = c(nrow(mc$mu.omega),
                                     ncol(predVauto_Smoke),
                                     4))
predauto_Lambda[,,1] <- cbind(mc$mu.beta0[,1], mc$mu.beta1[,1], mc$mu.beta2[,1], 
                              mc$mu.beta3[,1], mc$mu.phi) %*% predVauto_Smoke
predauto_Lambda[,,2] <- cbind(mc$mu.beta0[,2], mc$mu.beta1[,2], mc$mu.beta2[,2], 
                              mc$mu.beta3[,2], mc$mu.phi) %*% predVauto_Smoke
predauto_Lambda[,,3] <- cbind(mc$mu.beta0[,3], mc$mu.beta1[,3], mc$mu.beta2[,3], 
                              mc$mu.beta3[,3], mc$mu.phi) %*% predVauto_Smoke
predauto_Lambda[,,4] <- cbind(mc$mu.mu.beta0, mc$mu.mu.beta1, mc$mu.mu.beta2, 
                              mc$mu.mu.beta3, mc$mu.phi) %*% predVauto_Smoke
countAuto_Lambda <- exp(predauto_Lambda)

trueLambda <- count_Lambda + countAuto_Lambda
data <- apply(trueLambda,
              c(2,3),
              quantile,
              probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

#again, plop it all in a dataframe for ggplot
abun_Smoke <- rbind(data.frame(group = "Overall Average",
                               order = "Overall\nAverage",
                               smoke = c(0,1,2,3,4,5,6,7), # "real" values of smoke
                               lambda = data[3,,4],
                               upper = data[4,,4],
                               lower = data[2,,4]),
                    data.frame(group = "Order-Specific",
                               order = "Diptera",
                               smoke = c(0,1,2,3,4,5,6,7),
                               lambda = data[3,,1],
                               upper = data[4,,1],
                               lower = data[2,,1]),
                    data.frame(group = "Order-Specific",
                               order = "Hymenoptera",
                               smoke = c(0,1,2,3,4,5,6,7),
                               lambda = data[3,,2],
                               upper = data[4,,2],
                               lower = data[2,,2]),
                    data.frame(group = "Order-Specific",
                               order = "Lepidoptera",
                               smoke = c(0,1,2,3,4,5,6,7),
                               lambda = data[3,,3],
                               upper = data[4,,3],
                               lower = data[2,,3]))

# i'm setting up a shared position jitter so the dots don't overlap
# and the lines connect to the dots correctly
shared_position <- position_jitterdodge(jitter.width = 0.2,
                                        dodge.width = 0.75,
                                        seed = 42)
p2<-ggplot(abun_Smoke, aes(x = smoke, y = lambda, color = order)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=shared_position) +
  geom_line(aes(linetype = order), show.legend = FALSE,
            position=shared_position) +
  scale_color_manual(values = c("#56b4e9","#e69f00","#cc79a7","#000")) +
  scale_linetype_manual(values = c("dashed","blank","dashed","blank"))+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7),
    expand = c(0,0.5)) +
  scale_y_log10(expand = c(0.1,0),
                breaks = c(1, 3, 10, 30, 100),
                guide = "axis_logticks")+
  labs(x="Number of Days with Smoke", 
       y="Abundance", color="", linetype = "") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text=element_text(size=8), axis.title=element_text(size=10), 
        legend.text=element_text(size=8))
ggsave("./results/abundSmoke.eps",
       plot = p2,
       width = 11,
       height = 8,
       units = "cm",
       dpi = 600)

#### Supplemental Figures ####
library(wesanderson)
#### Total Site Abundance Through Time ####
# i set up the model to monitor 'Nsite', or the total number of
# individuals of the 54 modeled species estimated to be at each 
# site in each month
# so, we can model changes in total insect abundance through time
totalAbund <- apply(mc$Nsite,
                    c(2,3),
                    quantile,
                    probs = c(0.025, 0.5, 0.975))
data <- rbind(data.frame(site = "ABUCR",
                         month = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"),
                         N = totalAbund[2,1,],
                         upper = totalAbund[3,1,],
                         lower = totalAbund[1,1,]),
              data.frame(site = "LRVP",
                         month = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"),
                         N = totalAbund[2,2,],
                         upper = totalAbund[3,2,],
                         lower = totalAbund[1,2,]),
              data.frame(site = "PSP",
                         month = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"),
                         N = totalAbund[2,3,],
                         upper = totalAbund[3,3,],
                         lower = totalAbund[1,3,]),
              data.frame(site = "TDS",
                         month = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"),
                         N = totalAbund[2,4,],
                         upper = totalAbund[3,4,],
                         lower = totalAbund[1,4,]),
              data.frame(site = "WR",
                         month = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"),
                         N = totalAbund[2,5,],
                         upper = totalAbund[3,5,],
                         lower = totalAbund[1,5,])
)
data$month <- factor(data$month, levels = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"))
shared_position <- position_jitterdodge(jitter.width = 0.2,
                                        dodge.width = 0.75,
                                        seed = 42)
# note the transformation of the y-axis to the log10 scale to accomodate the
# range of data from 1,000 to 100,000,000
ggplot(data, aes(x = month, y = N, color=site, group = site)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = shared_position) +
  geom_line(linetype = 2, position = shared_position, show.legend = FALSE) +
  scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", n=5)) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_y_log10(expand = c(0.01,0.01), 
                guide = "axis_logticks",
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x="", 
       y="Total Insect Abundance", color="Site") +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=8), axis.text.y=element_text(size = 8))
