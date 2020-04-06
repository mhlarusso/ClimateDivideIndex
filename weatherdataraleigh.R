setwd("~/Desktop/2019/Climate Project")

raleigh_temps_june <- readxl::read_xlsx("Raleigh Weather (1).xlsx", sheet = "June 21, 2010")
raleigh_temps_july <- readxl::read_xlsx("Raleigh Weather (1).xlsx", sheet = "July 21, 2010")
raleigh_temps_august <- readxl::read_xlsx("Raleigh Weather (1).xlsx", sheet = "August 21, 2010")

cross_reference <- read.csv("RaleighCrossRefrence.csv")
cross_reference <- cross_reference[,-1]

#list of all zip codes in raleigh
raleigh_zip <- read.csv("RaleighZipCodes.csv")
reverse_cross_reference <- matrix()

raleigh_data$high_temp_avg = 0
raleigh_data$low_temp_avg = 0
raleigh_data$dewpoint_avg = 0
raleigh_data$wind_speed_avg = 0
raleigh_data$wind_gust_avg = 0
raleigh_data$cloud_cover_avg = 0 


for (h in raleigh_tracts$Tract){
  listo <- c()
  m = 1
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
  
  
  for (i in 1:dim(cross_reference)[1]){
    for (j in 1:dim(cross_reference)[2]){
      if(h == cross_reference[i,j] && !is.na(cross_reference[i,j])){
        listo[m] = cross_reference$X1[i]
        m = m + 1
      } 
    }
  } #average here . at this point we have a list of all zip codes in a certian tract
  for (k in 1:dim(raleigh_temps_june)[1]){
     if (raleigh_temps_june[k,1] %in% listo){
       june_count = june_count + 1
       june_high_temp = june_high_temp + raleigh_temps_june[k,2]
       june_low_temp = june_low_temp + raleigh_temps_june[k,4]
       june_dewpoint = june_dewpoint + raleigh_temps_june[k,6]
       june_wind_speed = june_wind_speed + raleigh_temps_june[k,8]
       june_wind_gust = june_wind_gust+ raleigh_temps_june[k,9]
       june_cloud_cover = june_cloud_cover + raleigh_temps_june[k,10]
     }
   } 
    june_high_temp_avg = june_high_temp/june_count
    june_low_temp_avg = june_low_temp/june_count
    june_dewpoint_avg = june_dewpoint/june_count
    june_wind_speed_avg = june_wind_speed/june_count
    june_wind_gust_avg = june_wind_gust/june_count
    june_cloud_cover_avg = june_cloud_cover/june_count
    
    for (k in 1:dim(raleigh_temps_july)[1]){
      if (raleigh_temps_july[k,1] %in% listo){
        july_count = july_count + 1
        july_high_temp = july_high_temp + raleigh_temps_july[k,2]
        july_low_temp = july_low_temp + raleigh_temps_july[k,4]
        july_dewpoint = july_dewpoint + raleigh_temps_july[k,6]
        july_wind_speed = july_wind_speed + raleigh_temps_july[k,8]
        july_wind_gust = july_wind_gust+ raleigh_temps_july[k,9]
        july_cloud_cover = july_cloud_cover + raleigh_temps_july[k,10]
      }
    } 
    july_high_temp_avg = july_high_temp/july_count
    july_low_temp_avg = july_low_temp/july_count
    july_dewpoint_avg = july_dewpoint/july_count
    july_wind_speed_avg = july_wind_speed/july_count
    july_wind_gust_avg = july_wind_gust/july_count
    july_cloud_cover_avg = july_cloud_cover/july_count
    
    for (k in 1:dim(raleigh_temps_august)[1]){
      if (raleigh_temps_august[k,1] %in% listo){
        aug_count = aug_count + 1
        aug_high_temp = aug_high_temp + raleigh_temps_august[k,2]
        aug_low_temp = aug_low_temp + raleigh_temps_august[k,4]
        aug_dewpoint = aug_dewpoint + raleigh_temps_august[k,6]
        aug_wind_speed = aug_wind_speed + raleigh_temps_august[k,8]
        aug_wind_gust = aug_wind_gust+ raleigh_temps_august[k,9]
        aug_cloud_cover = aug_cloud_cover + raleigh_temps_august[k,10]
      }
    } 
    aug_high_temp_avg = aug_high_temp/aug_count
    aug_low_temp_avg = aug_low_temp/aug_count
    aug_dewpoint_avg = aug_dewpoint/aug_count
    aug_wind_speed_avg = aug_wind_speed/aug_count
    aug_wind_gust_avg = aug_wind_gust/aug_count
    aug_cloud_cover_avg = aug_cloud_cover/aug_count
    
    high_temp_avg = (june_high_temp_avg + july_high_temp_avg + aug_high_temp_avg)/3
    low_temp_avg = (june_low_temp_avg + july_low_temp_avg + aug_low_temp_avg)/3
    dewpoint_avg = (june_dewpoint_avg + july_dewpoint_avg + aug_dewpoint_avg)/3
    wind_speed_avg = (june_wind_speed_avg + july_wind_speed_avg + aug_wind_speed_avg)/3
    wind_gust_avg = (june_wind_gust_avg + july_wind_gust_avg + aug_wind_gust_avg)/3
    cloud_cover_avg = (june_cloud_cover_avg + july_cloud_cover_avg + aug_cloud_cover_avg)/3
    
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


# zip code error for raleigh -- 27965 (Poplar Branch, beach area) should be 27695 (raleigh) 

#cleaning up raleigh data so there are no margins of error

#for (k in length(colnames)){
 # if (repl("MOE",colnames[k])){
    #remove kth column from dataset
  #}
#}
#install.packages("tidyverse")
library(tidyverse)

library(tidyverse) 
clean_raleigh_data <- raleigh_data %>% select(-contains("MOE")) %>% select(-contains("HC01_VC166")) %>% select(-contains("HCO3_VC28")) %>% select(-contains("HC03_VC49")) 
clean_raleigh_data <- clean_raleigh_data %>% select(-contains("HD01_S076")) %>% select(-contains("HD02_S076")) %>% select(-contains("HD02_S106")) %>% select(-contains("HD02_S113"))
clean_raleigh_data <- clean_raleigh_data %>% select(-contains("HD01_S076")) %>% select(-contains("HD02_S076")) %>% select(-contains("HD02_S106")) %>% select(-contains("HD02_S113"))











#for (i in raleigh_tracts$Tract){
  #listo <- list()
  #for (k in raleigh_temps_june$`Zip Code`){
    
  #}
 #}












#old loop
#listo <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#m <- 2

#listo <- list()
#for (k in (1:length(raleigh_tracts))){
  #listo[[k]] = list()
    #raleigh_tracts[k,2]
  #for (i in (1:dim(cross_reference)[1])){
    #for (j in 2:23){
       #if (raleigh_tracts[k,2] == cross_reference[i,j] && !is.na(cross_reference[i,j])){
        #listo[m] = cross_reference[i,1]
        #m = m+1
      #}
    #} 
  #} 
  #rbind(reverse_cross_reference,listo)
#} 


# once we get this, we loop through the weather data and average all the zip codes for the census tract
# if tract = zip, add them to a "sum" and divide by the number of elements
# cbind the new weather data to raleigh_data
# figure out how to make the index!!!
# repeat for the other cities. :o) 

