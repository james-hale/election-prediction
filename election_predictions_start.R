# Import Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sqldf)

# Read 2015 Election Data
ge_2015 <- read.csv("C:\\Users\\James\\Downloads\\2015-UK-general-election-data-results-WEB\\RESULTS FOR ANALYSIS.csv")

# Reduce Columns
general_info <- ge_2015[1:650,1:9]
party_votes <- ge_2015[1:650, c("Press.Association.ID.Number", "C", "DUP", "Green", "Ind", "Ind2", "Lab", "Lab.Co.op", "LD", "PC", "SNP", "UKIP", "UUP")]
ge_2015_clean <- left_join(general_info, party_votes)

