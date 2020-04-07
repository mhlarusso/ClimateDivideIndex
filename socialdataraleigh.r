# Climate Divide: Social Data
# Authors: Mallory LaRusso, Jordan Lewis, Michelle Koop, Nicole Carr
# Date 4/6/2020

######################################## MEDIAN HOUSEHOLD INCOME DATA #################################################################################

setwd("~/Desktop/2019/Climate Project/MedianHouseHoldIncomebyTract")

# Load library dplyr
library(dplyr)

# Read in .csv that contains median household income and other information
medians <- read.csv("ACS_10_SF4_B19013_with_ann.csv")

# Delete extraneous rows and columns
medians <- medians[c(-1, -454, -456, -794, -1499), c(-1,-3, -4, -5,-7)]

# Name the columns of 'medians'
col_names <- c("Tract", "Median_HHI")
names(medians) <- col_names

# Convert the data type of information to numeric. 
# MUST FIRST CONVERT TO  CHARACTER or else it will not preserve the information
medians$Median_HHI <- as.character(medians$Median_HHI)
medians$Median_HHI <- as.numeric(medians$Median_HHI)

######################################## INCOME BRACKET DATA #################################################################################

# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project/HouseHoldIncomeByTract_NumberOfPeopleInIncomeBracker")

# Read in .csv that contains income bracket data and other information
brackets <- read.csv("ACS_10_SF4_B19001_with_ann.csv")

# Delete extraneous rows and columns
brackets <- brackets[c(-1, -454, -456, -794, -1499), c(-1,-3, -4, -5)]


# Income brackets in thousands and USD
# TOT_#_#.#### represents the inclusive total number of people within that income bracket. 
# MOE is margin of error
colnames <- c("Tract", "Total_Pop_Bracket", "MOE_Pop", "TOT_0_9.999", "MOE_0_9.999", "TOT_10_14.999", 
              "MOE_10_14.999", "TOT_15_19.999", "MOE_15_19.999", "TOT_20_24.999", "MOE_20_24.999",
              "TOT_25_29.999", "MOE_25_29.999", "TOT_30_34.999", "MOE_30_34.999", "TOT_35_39.999",
              "MOE_35_39.999", "TOT_40_44.999", "MOE_40_44.999", "TOT_45_49.999", "MOE_45_49.999",
              "TOT_50_59.999", "MOE_50_59.999", "TOT_60_74.999", "MOE_60_74.999", "TOT_75_99.999", 
              "MOE_75_99.999", "TOT_100_124.999", "MOE_100_124.999", "TOT_125_149.999", "MOE_125_149.999",
              "TOT_150_199.999", "MOE_150_199.999", "TOT_200_", "MOE_200_")

# Name the columns of 'brackets'
names(brackets) <- colnames

######################################## RACE DATA ##########################################################################################

# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project/RaceAndOtherCharactersticsByTract")

# Read in .csv that contains population race data
race <- read.csv("DEC_10_SF1_SF1DP1_with_ann.csv")

# Delete extraneous rows, and alter columns
race <- race[-1, c(2, 4, 154:261)]

names(race)[1] <- "Tract"
names(race)[2] <- "Tot_Pop"

######################################## OCCUPATION DATA ###################################################################################

# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project/OccupationNumbersOfPopulation_PercentageInPoverty")

# Read in .csv that contains occupation and industry data
occupation <- read.csv("ACS_10_5YR_DP03_with_ann.csv")

# Delete extraneous rows, and alter columns
occupation <- occupation[-1,c(2,4,5,72:183,512:515)]

# Bind columns and name 1st element Tract
percent_occupationdata <- occupation[,grepl("HC03_",names(occupation))]
percent_occupationdata <- cbind(occupation$GEO.id2, percent_occupationdata)
names(percent_occupationdata)[1] <- "Tract"

# Bind columns and name 1st element Tract
estimate_occupationdata <- occupation[,grepl("HC01_",names(occupation))]
estimate_occupationdata <- cbind(occupation$GEO.id2, estimate_occupationdata)
names(estimate_occupationdata)[1] <- "Tract"

# Merge all aforementioned data into social_data_raleigh
bracketsandestimate <- merge(brackets,estimate_occupationdata, by.x="Tract")
bracketsandestimateandmedians <- merge(bracketsandestimate, medians, by.x = "Tract")
bemandpercent <- merge(bracketsandestimateandmedians, percent_occupationdata, by.x = "Tract")
social_data_raleigh <- merge(bemandpercent,race,by.x = "Tract")

######################################## SOCIAL VULERABILITY DATA ##########################################################################
# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project/EPHTN_M605_D_112817")

# Read in .csv that contains social vulnerability index
svi <- read.csv("data_112818.csv")

# Alter columns of data frame
svi <- svi[,c(3,6)]

# Name 1st element Tract and 2nd element SVI
names(svi)[1] <- "Tract"
names(svi)[2] <- "SVI"

# Merge all aforementioned data into all_social_data_1
all_social_data1 <- merge(social_data_raleigh,svi,by.x= "Tract")

######################################## TREE COVERAGE DATA ################################################################################
# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project/Forest Coverage")

# Read in .csv that contains tree coverage data
trees <- read.csv("data_114838.csv")

# Alter columns in data frame
trees <- trees[,c(3,6)]

# Name 1st element Tract and 2nd element Tree_Coverage_Percent
names(trees)[1] <- "Tract"
names(trees)[2] <- "Tree_Coverage_Percent"

########### Check this to make sure as.numeric did not alter data. If so, then change to as.character then as.numeric to preserve data
# Convert to percent
trees$Tree_Coverage_Percent <- as.numeric(gsub("%","", trees$Tree_Coverage_Percent))
trees$Tree_Coverage_Percent <- trees$Tree_Coverage_Percent / 100

# Merge into all_social_data_2
all_social_data2 <- merge(all_social_data1,trees,by="Tract")

######################################## WATER COVERAGE DATA ################################################################################
# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project/Water Coverage")

# Read in .csv that contains water coverage data
water <- read.csv("data_114646.csv")

# Alter columns in data frame
water <- water[,c(3,6)]

# Name 1st element tract and 2nd element Water_coverage_Percent
names(water)[1] <- "Tract"
names(water)[2] <- "Water_Coverage_Percent"

########### Check this to make sure as.numeric did not alter data. If so, then change to as.character then as.numeric to preserve data.
# Convert to percent
water$Water_Coverage_Percent <- as.numeric(gsub("%","", water$Water_Coverage_Percent))
water$Water_Coverage_Percent <- water$Water_Coverage_Percent / 100

# Merge into all_social_data3
all_social_data3 <- merge(all_social_data2,water,by.x="Tract")

########################################  RALEIGH TRACTS ###################################################################################
# Reset working directory to interact with other data
setwd("~/Desktop/2019/Climate Project")

# Read in .csv that contains census tract data
raleigh_tracts <- read.csv("Raleigh Census Tracts.csv")

# Delete extraneous rows
raleigh_tracts <- raleigh_tracts[-8,]

# Name raleigh_tracts 2nd element "Tract"
names(raleigh_tracts)[2] <- "Tract"

######################################## RALEIGH SOCIAL DATA ###############################################################################
# Merge into raleigh_data 
raleigh_data <- merge(raleigh_tracts, all_social_data3, by.x = "Tract")

# Delete extraneous columns
raleigh_data <- raleigh_data[,-2]
