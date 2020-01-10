#Climate Divide Project - DST 2019-2020

# Authors: Michelle Koop, Nicole Carr, Jordan Lewis, Mallory LaRusso

# Instructions for user to do to run program:
  # Change the city names in cityname variable (line 20)
  # Get the zip codes for each city from the supplementary excel document, 
    # just copy and paste into the "c()" (line 23)
  # Change the length of num_cols by looking at tract matrix (making num_cols big right now)


# Installing necessary packages
library("readxl")


# Importing the zip code and census tract data from https://www.huduser.gov/portal/datasets/usps_crosswalk.html
# Select the crosswalk type to be "ZIP-TRACT"
# Select Data Year and Quarter to be "3rd Quarter 2019"
zipandcensus <- read_excel("ZIP_TRACT_092019.xlsx")
num_tracts <- dim(zipandcensus)[1]
cityname <- "Toledo"

# Retrieve all of the zip codes in a city from https://www.zip-codes.com/city and turn them into a vector
zip_codes<- c(  43601
              , 43603
              , 43604
              , 43605
              , 43606
              , 43607
              , 43608
              , 43609
              , 43610
              , 43611
              , 43612
              , 43613
              , 43614
              , 43615
              , 43617
              , 43620
              , 43623
              , 43635
              , 43652
              , 43654
              , 43656
              , 43657
              , 43659
              , 43660
              , 43661
              , 43666
              , 43667
              , 43681
              , 43682
              , 43697
              , 43699)
# Convert the current type to characters
city <- as.character(zip_codes)

# Obtain the number of zipcodes or length of the vector for the chosen city.
num_zips <- length(city)


# Use a for loop to make a list of lists of each zip code and the census tract it maps to
zip_list <- list()
for (i in 1:num_zips){
  tractlist <- list()
  counter <- 0
  
  for (j in 1:num_tracts) {
    if (zipandcensus$zip[j] == city[i]) {
      counter <- counter + 1
      tractlist[[counter]] <- zipandcensus$tract[j]
    } 
  } 
  tractlist2 <- c(city[i],tractlist)
  zip_list[[i]] <- tractlist2
}
zip_list


# Create matrix out of zip_list (which is a list of lists) in order to create a dataframe to write to a csv
num_cols = 40
tractmatrix <- matrix(ncol = num_cols, nrow = num_zips) 

for (i in 1:num_zips){
  for (j in 1:length(zip_list[[i]])){
    tractmatrix[i,j] <- zip_list[[i]][[j]]
  }
}
tractmatrix  # Look at tract matrix to find out how many columns you need for num_cols. 
             # Try 50 to start with, narrow down to the longest list of observations


tractdataframe <- data.frame(tractmatrix)
tractdataframe

# Cross refrence csv of every zip code in the city and all the census tracts in it
write.csv(tractdataframe, paste(cityname,"Cross Reference.csv"))

# Get a list of every census tract in city
tracts <- c()
count = 0
for (i in 1:num_zips){
  for(j in 2:num_cols){
    tracts[count] <- tractmatrix[i,j]
    count = count + 1
  }
}


# Delete duplicates
uniquetracts <- unique(tracts)

# Write the census tracts to a .csv
write.csv(uniquetracts, paste(cityname, "Census Tracts.csv"))

# Collate the the city zip codes that have at least one census tract to their respective tracts
columntwo <- tractmatrix[, 2]
Zip_codes_with_tracts1 <- c()
count = 0
for (i in 1:num_zips){
  count = count + 1
  if (!is.na(columntwo[i])) { # Only uses zip codes with at least one or more tract(s)
    Zip_codes_with_tracts1[count] <- zip_codes[i]
  }
}

# Creates offical zip code and tract list by deleting tracts that map to more than one zip code 
# from zip_codes_with_tracts1
Zip_codes_with_tracts <- unique(Zip_codes_with_tracts1)
Zip_codes_with_tracts

# Writes a .csv of every zip code in city
write.csv(Zip_codes_with_tracts, paste(cityname, "Zip Codes.csv"))

