# Import Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sqldf)
library(varhandle)

# Read 2015 Election Data
#ge_2015 <- read.csv("C:\\Users\\James\\Downloads\\2015-UK-general-election-data-results-WEB\\RESULTS FOR ANALYSIS.csv")
ge_2015 <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\RESULTS FOR ANALYSIS.csv")
ge_2010 <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\ge_2010_results.csv")


# Cleanup

# General Info
general_info2015 <- ge_2015[1:650,1:9]
colnames(general_info2015) <- c("Press_ID", "Constituency15", "Region15", "Country15", "Constituency_ID15", "Constituency_Type", "Year", "Electorate15", "Valid_vote_count15")

general_info2010 <- ge_2010[1:650,1:6]
colnames(general_info2010) <- c("Press_ID", "Constituency10", "Region10", "Year10", "Electorate10", "Valid_vote_count10")

# Convert valid votes to numeric
general_info2015$Valid_vote_count15 <- as.numeric(gsub(",", "", unfactor(general_info2015$Valid_vote_count)))
general_info2010$Valid_vote_count10 <- as.numeric(general_info2010$Valid_vote_count)

# Convert electorate to numeric
general_info2015$Electorate15 <- as.numeric(gsub(",", "", unfactor(general_info2015$Electorate15)))
general_info2010$Electorate10 <- as.numeric(general_info2010$Electorate10)


# Votes by party per election
party_votes2015 <- ge_2015[1:650, c("Press.Association.ID.Number", "C", "DUP", "Green", "Ind", "Ind2", "Lab", "Lab.Co.op", "LD", "PC", "SDLP", "SNP", "SF", "UKIP", "UUP")]
party_votes2010 <- ge_2010[1:650, c("Press.Association.Reference", "Con", "DUP", "Grn", "Ind1", "Ind2", "Lab", "LD", "PC", "SDLP", "SNP", "SF", "UKIP")]

# Clean colnames
party_colnames15 <- c("Press_ID", "Con15", "DUP15", "Grn15", "Ind1_15", "Ind2_15", "Lab15", "Lab_coop15", "LD15", "PC15", "SDLP15", "SNP15", "SF15", "UKIP15", "UUP15")
party_colnames10 <- c("Press_ID", "Con10", "DUP10", "Grn10", "Ind1_10", "Ind2_10", "Lab10", "LD10", "PC10", "SDLP10", "SNP10", "SF10", "UKIP10")

colnames(party_votes2015) <- party_colnames15
colnames(party_votes2010) <- party_colnames10

# Calculate votes for 'other'
other2015 <- data.frame(cbind(general_info2015$Press_ID, (general_info2015$Valid_vote_count - rowSums(party_votes2015[,-which(names(party_votes2015) %in% c("Press_ID"))], na.rm = TRUE, dims = 1))))
colnames(other2015) <- c("Press_ID", "Other2015")
other2010 <- data.frame(cbind(general_info2010$Press_ID, (general_info2010$Valid_vote_count - rowSums(party_votes2010[,-which(names(party_votes2010) %in% c("Press_ID"))], na.rm = TRUE, dims = 1))))
colnames(other2010) <- c("Press_ID", "Other2010")

# Join it all together
clean1 <- left_join(general_info2015, party_votes2015)
clean2 <- left_join(clean1, other2015)
clean3 <- left_join(clean2, general_info2010)
clean4 <- left_join(clean3, party_votes2010)
clean5 <- left_join(clean4, other2010)

# Add national average poll data immediately prior, 1-, 3-, 6- and 12 month prior to election for Con/Lab/LD/UKIP/Green
poll_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\poll_averages.csv")

clean6 <- left_join(clean5, poll_data)

# Convert region to a categorical variable
for(t in unique(clean6$Region15)) {
  clean6[paste("Region15",t,sep="")] <- ifelse(clean6$Region15==t,1,0)
}

# Convert constituency type to a categorical variables
for(t in unique(clean6$Constituency_Type)) {
  clean6[paste("Constituency_Type",t,sep="")] <- ifelse(clean6$Constituency_Type==t,1,0)
}

# Remove unnecessary columns
clean7 <- clean6[,-which(names(clean6) %in% c("Region15", "Country15", "Constituency_Type", "Constituency_ID15", "Region10", "Year10"))]

# Bring in more data for joining
business_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\business_data_by_constituency.csv")
population_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\population_data_by_constituency.csv")
wages_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\wages_data_by_constituency.csv")
oow_benefits_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\out_of_work_benefits_by_constituency.csv")
child_poverty_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\child_poverty_data_by_constituency.csv")
unemployment_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\unemployment_data_by_constituency.csv")
youth_unemployment_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\youth_unemployment_data_by_constituency.csv")
incapacity_benefits_data <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\incapacity_benefits_data_by_constituency.csv")

# Import 2005 data (form slightly different)
clean7$Constituency15 <- sapply(clean7$Constituency15, tolower)

ge_2005 <- read.csv("C:\\Users\\james.hale\\Documents\\Election\\election-prediction-master\\election-prediction-master\\2005_election_results.csv")
ge_2005$Name_2005 <- sapply(ge_2005$Name_2005, tolower)
clean8 <- left_join(clean7, ge_2005, by=c("Constituency15" = "Name_2005"))
