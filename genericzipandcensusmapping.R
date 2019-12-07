#Climate Divide Project - DST 2019-2020

#Authors: Michelle Koop, Nicole Carr, Jordan Lewis, Mallory LaRusso


#different things to do for each city:
# change the city names in cityname variable
# get the zip codes for each city
# change the length of num_cols by looking at tract matrix (making num_cols big right now)


#installing necessary packages
library("readxl")


#grabbing the zip code and census tract data
zipandcensus <- read_excel("ZIP_TRACT_092019.xlsx")
num_tracts <- dim(zipandcensus)[1]
cityname <- "Toledo"

#get all of the codes in city from https://www.zip-codes.com/city
zip_codes<- c(, 43601
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

city <- as.character(zip_codes)
num_zips <- length(city) 


#for loop to make a list of lists of each zip code and the census tract it maps to
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


#create matrix out of zip_list (which is a list of lists) in order to create a dataframe to write to a csv

num_cols = 40

tractmatrix <- matrix(ncol = num_cols, nrow = num_zips) 

for (i in 1:num_zips){
  for (j in 1:length(zip_list[[i]])){
    tractmatrix[i,j] <- zip_list[[i]][[j]]
  }
}
tractmatrix #look at tract matrix to find out how many columns you need for num_cols. try 50 to start with, narrow down to the longest list of observations


tractdataframe <- data.frame(tractmatrix)
tractdataframe

#cross refrence csv of every zip code in the city and all the census tracts in it
write.csv(tractdataframe, paste(cityname,"Cross Reference.csv"))

#get a list of every census tract in city
tracts <- c()
count = 0
for (i in 1:num_zips){
  for(j in 2:num_cols){
    tracts[count] <- tractmatrix[i,j]
    count = count + 1
  }
}


#delete duplicates
uniquetracts <- unique(tracts)

#write to csv
write.csv(uniquetracts, paste(cityname, "Census Tracts.csv"))

#need to find the zip codes from the list of city zip codes that have census tracts

columntwo <- tractmatrix[, 2]
Zip_codes_with_tracts1 <- c()
count = 0
for (i in 1:num_zips){
  count = count + 1
  if (!is.na(columntwo[i])) {
    Zip_codes_with_tracts1[count] <- zip_codes[i]
  }
}

Zip_codes_with_tracts <- unique(Zip_codes_with_tracts1)
Zip_codes_with_tracts

#write a csv of every zip code in city
write.csv(Zip_codes_with_tracts, paste(cityname, "Zip Codes.csv"))

