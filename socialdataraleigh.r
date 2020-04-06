setwd("~/Desktop/2019/Climate Project/MedianHouseHoldIncomebyTract")

library(dplyr)
medians <- read.csv("ACS_10_SF4_B19013_with_ann.csv")
medians <- medians[c(-1, -454, -456, -794, -1499), c(-1,-3, -4, -5,-7)]
#medians[ , c(4,5)] <- 0

col_names <- c("Tract", "Median_HHI")
names(medians) <- col_names
medians$Median_HHI <- as.character(medians$Median_HHI)
medians$Median_HHI <- as.numeric(medians$Median_HHI)

dim(medians)

#for (i in 1:2157){
  #medians$LL_HHI[i]  <- medians$Median_HHI[i]-medians$MOE_HHI[i]
#}

#for(i in 1:2157){
  #medians$UL_HHI[i]  <- medians$Median_HHI[i]+medians$MOE_HHI[i]
#}

#90% confidence interval. could be a non-sampling error (data collection problem)
#not accounting for errors during data collection
#where does the 0 in MOE come from?



setwd("~/Desktop/2019/Climate Project/HouseHoldIncomeByTract_NumberOfPeopleInIncomeBracker")
brackets <- read.csv("ACS_10_SF4_B19001_with_ann.csv")
brackets <- brackets[c(-1, -454, -456, -794, -1499), c(-1,-3, -4, -5)]

#income brackets in thousands
colnames <- c("Tract", "Total_Pop_Bracket", "MOE_Pop", "TOT_0_9.999", "MOE_0_9.999", "TOT_10_14.999", 
              "MOE_10_14.999", "TOT_15_19.999", "MOE_15_19.999", "TOT_20_24.999", "MOE_20_24.999",
              "TOT_25_29.999", "MOE_25_29.999", "TOT_30_34.999", "MOE_30_34.999", "TOT_35_39.999",
              "MOE_35_39.999", "TOT_40_44.999", "MOE_40_44.999", "TOT_45_49.999", "MOE_45_49.999",
              "TOT_50_59.999", "MOE_50_59.999", "TOT_60_74.999", "MOE_60_74.999", "TOT_75_99.999", 
              "MOE_75_99.999", "TOT_100_124.999", "MOE_100_124.999", "TOT_125_149.999", "MOE_125_149.999",
              "TOT_150_199.999", "MOE_150_199.999", "TOT_200_", "MOE_200_")
names(brackets) <- colnames

#install.packages("tibble")
#library("tibble")
#for (i in 2:31){
  #add_column(brackets, d = 0, .after = brackets[, i+1])
  #add_column(brackets, .after = brackets[, i+2])
#}


#race data
setwd("~/Desktop/2019/Climate Project/RaceAndOtherCharactersticsByTract")

race <- read.csv("DEC_10_SF1_SF1DP1_with_ann.csv")
race <- race[-1, c(2, 4, 154:261)]
names(race)[1] <- "Tract"
names(race)[2] <- "Tot_Pop"
#occupation data
setwd("~/Desktop/2019/Climate Project/OccupationNumbersOfPopulation_PercentageInPoverty")

occupation <- read.csv("ACS_10_5YR_DP03_with_ann.csv")

#get only the variables we want

occupation <- occupation[-1,c(2,4,5,72:183,512:515)]
percent_occupationdata <- occupation[,grepl("HC03_",names(occupation))]
percent_occupationdata <- cbind(occupation$GEO.id2, percent_occupationdata)
names(percent_occupationdata)[1] <- "Tract"

estimate_occupationdata <- occupation[,grepl("HC01_",names(occupation))]
estimate_occupationdata <- cbind(occupation$GEO.id2, estimate_occupationdata)
names(estimate_occupationdata)[1] <- "Tract"

bracketsandestimate <- merge(brackets,estimate_occupationdata, by.x="Tract")
bracketsandestimateandmedians <- merge(bracketsandestimate, medians, by.x = "Tract")
bemandpercent <- merge(bracketsandestimateandmedians, percent_occupationdata, by.x = "Tract")
social_data_raleigh <- merge(bemandpercent,race,by.x = "Tract")


#HD is race, HC is occupation



#vulnerability data
setwd("~/Desktop/2019/Climate Project/EPHTN_M605_D_112817")
svi <- read.csv("data_112818.csv")
svi <- svi[,c(3,6)]
names(svi)[1] <- "Tract"
names(svi)[2] <- "SVI"

all_social_data1 <- merge(social_data_raleigh,svi,by.x= "Tract")


#tree coverage data
setwd("~/Desktop/2019/Climate Project/Forest Coverage")

trees <- read.csv("data_114838.csv")
trees <- trees[,c(3,6)]
names(trees)[1] <- "Tract"
names(trees)[2] <- "Tree_Coverage_Percent"

trees$Tree_Coverage_Percent <- as.numeric(gsub("%","", trees$Tree_Coverage_Percent))
trees$Tree_Coverage_Percent <- trees$Tree_Coverage_Percent / 100

all_social_data2 <- merge(all_social_data1,trees,by="Tract")

#water coverage data
setwd("~/Desktop/2019/Climate Project/Water Coverage")

water <- read.csv("data_114646.csv")
water <- water[,c(3,6)]
names(water)[1] <- "Tract"
names(water)[2] <- "Water_Coverage_Percent"
water$Water_Coverage_Percent <- as.numeric(gsub("%","", water$Water_Coverage_Percent))
water$Water_Coverage_Percent <- water$Water_Coverage_Percent / 100

all_social_data3 <- merge(all_social_data2,water,by.x="Tract")

#only raleigh census tracts
setwd("~/Desktop/2019/Climate Project")

raleigh_tracts <- read.csv("Raleigh Census Tracts.csv")
raleigh_tracts <- raleigh_tracts[-8,]

names(raleigh_tracts)[2] <- "Tract"
raleigh_data <- merge(raleigh_tracts, all_social_data3, by.x = "Tract")

raleigh_data <- raleigh_data[,-2]


#next week:

# combine weather and social data -- want everything in terms of census tract (average zip code data for census tract)
# use percent for median hhi and occupation data bc conflicting total pop values
# health data -- health insurance, heat stroke, asthma? 
# pca, regression (models supported by papers), index 



