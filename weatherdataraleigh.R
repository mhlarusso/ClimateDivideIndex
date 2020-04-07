# Climate Divide: Weather Data
# Authors: Mallory LaRusso, Jordan Lewis, Michelle Koop, Nicole Carr
# Date 4/6/2020

setwd("~/Desktop/2019/Climate Project")

# Install package tidyverse
install.packages("tidyverse")

# Load package
library(tidyverse) 

# Read in excel files that contain Raleigh, NC weather data for June 21, July 21, and August 21 of 2010
raleigh_temps_june <- readxl::read_xlsx("Raleigh Weather (1).xlsx", sheet = "June 21, 2010")
raleigh_temps_july <- readxl::read_xlsx("Raleigh Weather (1).xlsx", sheet = "July 21, 2010")
raleigh_temps_august <- readxl::read_xlsx("Raleigh Weather (1).xlsx", sheet = "August 21, 2010")

# Read in .csv RaleighCrossReference that contains list of tracts and then zipcodes within the tract
cross_reference <- read.csv("RaleighCrossReference.csv")

# Delete the first column
cross_reference <- cross_reference[,-1]

#List of all zip codes used in Raleigh, NC
raleigh_zip <- read.csv("RaleighZipCodes.csv")
reverse_cross_reference <- matrix()

# Initialize the following:
raleigh_data$high_temp_avg = 0
raleigh_data$low_temp_avg = 0
raleigh_data$dewpoint_avg = 0
raleigh_data$wind_speed_avg = 0
raleigh_data$wind_gust_avg = 0
raleigh_data$cloud_cover_avg = 0 

# Traverse through the census tracts of Raleigh
for (h in raleigh_tracts$Tract){
  listo <- c()
  m = 1
  # Initialize the following:
  june_high_temp = 0
  june_low_temp = 0
  june_dewpoint = 0
  june_wind_speed = 0
  june_wind_gust = 0
  june_cloud_cover = 0
  june_count = 0
  july_high_temp = 0
  july_low_temp = 0
  july_dewpoint = 0
  july_wind_speed = 0
  july_wind_gust = 0
  july_cloud_cover = 0
  july_count = 0
  aug_high_temp = 0
  aug_low_temp = 0
  aug_dewpoint = 0
  aug_wind_speed = 0
  aug_wind_gust = 0
  aug_cloud_cover = 0
  aug_count = 0
  
  # Create a loop that maps tracts to zip codes contained in that tract
  for (i in 1:dim(cross_reference)[1]){
    for (j in 1:dim(cross_reference)[2]){
      if(h == cross_reference[i,j] && !is.na(cross_reference[i,j])){
        listo[m] = cross_reference$X1[i]
        m = m + 1
      } 
    }
  } 
  
  # Traverse through zip codes and continually sum all values for each of the following variables for June
  for (k in 1:dim(raleigh_temps_june)[1]){
     if (raleigh_temps_june[k,1] %in% listo){
       june_count = june_count + 1 # Counter for total number of observations in June
       june_high_temp = june_high_temp + raleigh_temps_june[k,2] # 2 is the 2nd column of the raleigh_temps_june file, where the high temperature data was
       june_low_temp = june_low_temp + raleigh_temps_june[k,4] # 4 is the 4th column of the raleigh_temps_june file, where the low temperature data was
       june_dewpoint = june_dewpoint + raleigh_temps_june[k,6] # 6 is the 6th column of the raleigh_temps_june file, where the dewpoint data was
       june_wind_speed = june_wind_speed + raleigh_temps_june[k,8] # 8 is the 8th column of the raleigh_temps_june file, where the windspeed data was
       june_wind_gust = june_wind_gust+ raleigh_temps_june[k,9] # 9 is the 9th column of the raleigh_temps_june file, where the wind gust data was
       june_cloud_cover = june_cloud_cover + raleigh_temps_june[k,10] # 10 is the 10th column of the raleigh_temps_june file, where the cloud cover data was
     }
  } 
    # Calculate and assign averaged June data to corresponding variables
    june_high_temp_avg = june_high_temp/june_count
    june_low_temp_avg = june_low_temp/june_count
    june_dewpoint_avg = june_dewpoint/june_count
    june_wind_speed_avg = june_wind_speed/june_count
    june_wind_gust_avg = june_wind_gust/june_count
    june_cloud_cover_avg = june_cloud_cover/june_count
    
    # Traverse through zip codes and continually sum all values for each of the following variables for July
    for (k in 1:dim(raleigh_temps_july)[1]){
      if (raleigh_temps_july[k,1] %in% listo){
        july_count = july_count + 1 # Counter for total number of observations in July
        july_high_temp = july_high_temp + raleigh_temps_july[k,2] # 2 is the 2nd column of the raleigh_temps_july file, where the high temperature data was
        july_low_temp = july_low_temp + raleigh_temps_july[k,4] # 4 is the 4th column of the raleigh_temps_july file, where the low temperature data was
        july_dewpoint = july_dewpoint + raleigh_temps_july[k,6] # 6 is the 6th column of the raleigh_temps_july file, where the dewpoint data was
        july_wind_speed = july_wind_speed + raleigh_temps_july[k,8] # 8 is the 8th column of the raleigh_temps_july file, where the wind speed data was
        july_wind_gust = july_wind_gust+ raleigh_temps_july[k,9] # 9 is the 9th column of the raleigh_temps_july file, where the wind gust data was
        july_cloud_cover = july_cloud_cover + raleigh_temps_july[k,10] # 10 is the 10th column of the raleigh_temps_july file, where the cloud cover data was
      }
    } 
    # Calculate and assign averaged July ata to corresponding variables
    july_high_temp_avg = july_high_temp/july_count
    july_low_temp_avg = july_low_temp/july_count
    july_dewpoint_avg = july_dewpoint/july_count
    july_wind_speed_avg = july_wind_speed/july_count
    july_wind_gust_avg = july_wind_gust/july_count
    july_cloud_cover_avg = july_cloud_cover/july_count
    
    # Traverse through zip codes and continually sum all values for each of the following variables for August
    for (k in 1:dim(raleigh_temps_august)[1]){
      if (raleigh_temps_august[k,1] %in% listo){
        aug_count = aug_count + 1 # Counter for total number of observations in August
        aug_high_temp = aug_high_temp + raleigh_temps_august[k,2] # 2 is the 2nd column of the raleigh_temps_august file, where the high temp data was
        aug_low_temp = aug_low_temp + raleigh_temps_august[k,4] # 4 is the 4th column of the raleigh_temps_august file, where the low temp data was
        aug_dewpoint = aug_dewpoint + raleigh_temps_august[k,6] # 6 is the 6th column of the raleigh_temps_august file, where the dewpoint data was
        aug_wind_speed = aug_wind_speed + raleigh_temps_august[k,8] # 8 is the 8th column of the raleigh_temps_august file, where the wind speed data was
        aug_wind_gust = aug_wind_gust+ raleigh_temps_august[k,9] # 9 is the 9th column of the raleigh_temps_august file, where the wind gust data was
        aug_cloud_cover = aug_cloud_cover + raleigh_temps_august[k,10] # 10 is the 10th column of the raleigh_temps_august file, where the cloud cover data was
      }
    } 
    # Calculate and assign averaged August data to corresponding variables
    aug_high_temp_avg = aug_high_temp/aug_count
    aug_low_temp_avg = aug_low_temp/aug_count
    aug_dewpoint_avg = aug_dewpoint/aug_count
    aug_wind_speed_avg = aug_wind_speed/aug_count
    aug_wind_gust_avg = aug_wind_gust/aug_count
    aug_cloud_cover_avg = aug_cloud_cover/aug_count
    
    # Calculate and assign averaged data over all three months to corresponding variables
    high_temp_avg = (june_high_temp_avg + july_high_temp_avg + aug_high_temp_avg)/3
    low_temp_avg = (june_low_temp_avg + july_low_temp_avg + aug_low_temp_avg)/3
    dewpoint_avg = (june_dewpoint_avg + july_dewpoint_avg + aug_dewpoint_avg)/3
    wind_speed_avg = (june_wind_speed_avg + july_wind_speed_avg + aug_wind_speed_avg)/3
    wind_gust_avg = (june_wind_gust_avg + july_wind_gust_avg + aug_wind_gust_avg)/3
    cloud_cover_avg = (june_cloud_cover_avg + july_cloud_cover_avg + aug_cloud_cover_avg)/3
    
    # Assign weather data to the corresponding variable listed below, and to the appropriate census tract
    for (z in 1:dim(raleigh_data)[1]){
      if (raleigh_data[z,1] == h){
        raleigh_data$high_temp_avg[z] = high_temp_avg
        raleigh_data$low_temp_avg[z] = low_temp_avg
        raleigh_data$dewpoint_avg[z] = dewpoint_avg
        raleigh_data$wind_speed_avg[z] = wind_speed_avg
        raleigh_data$wind_gust_avg[z] = wind_gust_avg
        raleigh_data$cloud_cover_avg[z] = cloud_cover_avg
      }
    }
  } 

# Filter and select columns that contain unecessary information in raleigh_data and delete them
clean_raleigh_data <- raleigh_data %>% select(-contains("MOE")) %>% select(-contains("HC01_VC166")) %>% select(-contains("HCO3_VC28")) %>% select(-contains("HC03_VC49")) 
clean_raleigh_data <- clean_raleigh_data %>% select(-contains("HD01_S076")) %>% select(-contains("HD02_S076")) %>% select(-contains("HD02_S106")) %>% select(-contains("HD02_S113"))
clean_raleigh_data <- clean_raleigh_data %>% select(-contains("HD01_S076")) %>% select(-contains("HD02_S076")) %>% select(-contains("HD02_S106")) %>% select(-contains("HD02_S113"))
