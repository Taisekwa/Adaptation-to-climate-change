# Pick up data from RDMstatsDay files and calculate scaling parameters to include clover in BASGRA predictions
# Produce a file with scaling parameters
# Read in measured data file and multiply by scaling parameters

# Script is divided into two parts
# 1) considered that the data are not normal and thus uses the median and 25th percentile for the calculations
# 2) assumes the data are normal and thus uses the mean and standard deviation for the calculations


setwd("I:/Andrea/BASGRA")

# load packages
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

longlats <- c("175.425_-37.875") # OWL farm
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
  
rcps <- c("RCP8.5", "RCP4.5")

for (rcp in rcps){  
  # read in BASGRA data
  file_PGRstats_name <- paste("I:/Andrea/BASGRA/Output for OWL farm/RDMstatsDay_OWLfarm_", longlats, "_", rcp, "_PAW60.csv", sep = "")
  file_PGRstats <- read.csv(file_PGRstats_name)
  
  # read in OWL farm data
  PGRdata <- read_csv("Output for OWL farm/OWLfarmPGRfromJo.csv")
  PGRdata <- rbind(PGRdata[8:12,],PGRdata[1:7,]) # rearrange the data so that it runs from Jan-Dec

  ##############################################################################
  
  # Data are non-normal, so use median and percentile to calculate scaling parameter
  PGRstats_medians <- file_PGRstats[15:26,4:7] # pick up just medians 
  colnames(PGRstats_medians) <- c("X2010med", "X2040med", "X2070med", "X2090med")
  PGRstats_Q25 <- file_PGRstats[87:98,5:7] # pick up just 25th percentiles
  colnames(PGRstats_Q25) <- c("X2040Q25", "X2070Q25", "X2090Q25") # percentiles column names
  PGRstats_nonnormal <- cbind(PGRstats_medians, PGRstats_Q25) # combine the necessary data
  rownames(PGRstats_nonnormal) <- month
  PGRstats_nonnormal <- PGRstats_nonnormal %>%  
    mutate(scaling_optimistic_2040 = 1+(X2040med - X2010med)/X2010med) %>% 
    mutate(scaling_optimistic_2070 = 1+(X2070med - X2010med)/X2010med) %>% 
    mutate(scaling_optimistic_2090 = 1+(X2090med - X2010med)/X2010med) %>% 
    mutate(scaling_realistic_2040 = 1+(X2040Q25 - X2010med)/X2010med) %>% 
    mutate(scaling_realistic_2070 = 1+(X2070Q25 - X2010med)/X2010med) %>% 
    mutate(scaling_realistic_2090 = 1+(X2090Q25 - X2010med)/X2010med) # calculate the scaling parameters
  
  PGRscaling_nonnormal <- PGRstats_nonnormal[,8:13] # pick up just the scaling parameters
  
  # save the scaling parameters
  file_name <- paste("I:/Andrea/BASGRA/Output for OWL farm/RDMscaling_OWLfarm_", longlats, "_", rcp, "_PAW60_nonnormal.csv", sep="")
  file <- write.table(PGRscaling_nonnormal, file=file_name, quote = FALSE, sep = ",", col.names = NA)
  
  # multiply the observed data with the scaling parameters to predict PGR for ryegrass and clover
  PGRscaled_nonnormal <- sapply(PGRscaling_nonnormal, '*', PGRdata$long_term_average)
  colnames(PGRscaled_nonnormal) <- c("scaled_optimistic_2040", "scaled_optimistic_2070", "scaled_optimistic_2090", "scaled_realistic_2040", "scaled_realistic_2070", "scaled_realistic_2090")
  rownames(PGRscaled_nonnormal) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
  
  # save the ryegrass + clover PGR predictions
  file_name <- paste("I:/Andrea/BASGRA/Output for OWL farm/RDMscaled_OWLfarm_", longlats, "_", rcp, "_PAW60.csv", sep="")
  file <- write.table(PGRscaled_nonnormal, file=file_name, quote = FALSE, sep = ",", col.names = NA)

  ##############################################################################
  
  # plot curves
  num <- c(1:12)
  PGRscaled <- as.data.frame(cbind(num, PGRscaled_nonnormal))
  
  linethickness = 1
  
  ggmed2 <- ggplot(PGRscaled, aes(x=num)) + 
    
    geom_line(aes(y=scaled_optimistic_2040, colour='2040'), size = linethickness) +
    geom_line(aes(y=scaled_optimistic_2070, colour='2070'), size = linethickness) +
    geom_line(aes(y=scaled_optimistic_2090, colour='2090'), size = linethickness) +
    geom_line(aes(y=scaled_realistic_2040, colour='2040')) +
    geom_line(aes(y=scaled_realistic_2070, colour='2070')) +
    geom_line(aes(y=scaled_realistic_2090, colour='2090')) +
    geom_line(aes(y=PGRdata$long_term_average)) +
    labs(title=paste("non-normal", rcp), x="Month", y="Pasture Growth Rate (kg DM/ha/day)") +
    scale_x_continuous(breaks=c(1:12), labels=month) 

  
  print(ggmed2)  
  
  ##############################################################################
        
  # Calculate scaling parameter using mean and standard deviation if assuming normal data
  PGRstats_means <- file_PGRstats[3:14,4:7] # pick up just means 
  colnames(PGRstats_means) <- c("X2010mean", "X2040mean", "X2070mean", "X2090mean")
  PGRstats_sd <- file_PGRstats[63:74,5:7] # pick up just standard deviation
  colnames(PGRstats_sd) <- c("X2040sd", "X2070sd", "X2090sd") # sd column names
  PGRstats_normal <- cbind(PGRstats_means, PGRstats_sd)
  rownames(PGRstats_normal) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
  PGRstats_normal <- PGRstats_normal %>%  
    mutate(scaling_optimistic_2040 = 1+(X2040mean - X2010mean)/X2010mean) %>% 
    mutate(scaling_optimistic_2070 = 1+(X2070mean - X2010mean)/X2010mean) %>% 
    mutate(scaling_optimistic_2090 = 1+(X2090mean - X2010mean)/X2010mean) %>% 
    mutate(scaling_realistic_2040 = 1+(X2040mean - X2040sd - X2010mean)/X2010mean) %>% 
    mutate(scaling_realistic_2070 = 1+(X2070mean - X2070sd - X2010mean)/X2010mean) %>% 
    mutate(scaling_realistic_2090 = 1+(X2090mean - X2090sd - X2010mean)/X2010mean) 
  
  PGRscaling_normal <- PGRstats_normal[,8:13]
  
  file_name <- paste("I:/Andrea/BASGRA/Output for OWL farm/RDMscaling_OWLfarm_", longlats, "_", rcp, "_PAW60_normal.csv", sep="")
  file_PGRstats <- write.table(PGRscaling_normal, file=file_name, quote = FALSE, sep = ",", col.names = NA)
  
  PGRscaled_normal <- sapply(PGRscaling_normal, '*', PGRdata$long_term_average)
  colnames(PGRscaled_normal) <- c("scaled_optimistic_2040", "scaled_optimistic_2070", "scaled_optimistic_2090", "scaled_realistic_2040", "scaled_realistic_2070", "scaled_realistic_2090")
  rownames(PGRscaled_normal) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
  
  file_name <- paste("I:/Andrea/BASGRA/Output for OWL farm/RDMscaled_OWLfarm_", longlats, "_", rcp, "_PAW60_normal.csv", sep="")
  file <- write.table(PGRscaled_normal, file=file_name, quote = FALSE, sep = ",", col.names = NA)
  
  ##############################################################################
  
  # plot curves
  PGRscaled <- as.data.frame(cbind(num, PGRscaled_normal))

  
  ggmed2 <- ggplot(PGRscaled, aes(x=num)) + 
    
    geom_line(aes(y=scaled_optimistic_2040, colour='2040'), size = linethickness) +
    geom_line(aes(y=scaled_optimistic_2070, colour='2070'), size = linethickness) +
    geom_line(aes(y=scaled_optimistic_2090, colour='2090'), size = linethickness) +
    geom_line(aes(y=scaled_realistic_2040, colour='2040')) +
    geom_line(aes(y=scaled_realistic_2070, colour='2070')) +
    geom_line(aes(y=scaled_realistic_2090, colour='2090')) +
    geom_line(aes(y=PGRdata$long_term_average)) +
    
    labs(title=paste(rcp, "normal"), x="Month", y="Pasture Growth Rate (kg DM/ha/day)") +
    scale_x_continuous(breaks=c(1:12), labels=month) 
  
  print(ggmed2)  

 
}



