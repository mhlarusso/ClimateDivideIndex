# Climate Divide Analysis
# Authors: Mallory LaRusso, Jordan Lewis, Michelle Koop, Nicole Carr
# Date 4/6/2020

# Install package ggfortify
install.packages("ggfortify")

# Load libraries
library(ggplot2)
library(ggfortify)
library(dplyr)

# Rename columns of clean_raleigh_data
# Legend:
## ctw = Commute to work
## _perc = percent of people, if there is not a "_perc" ending then the form of the data is the number of.
## 
clean_raleigh_data <- clean_raleigh_data %>%
  rename(
    employment_status = HC01_VC04,
    ctw = HC01_VC28,
    ctw_alone = HC01_VC29,
    ctw_carpool = HC01_VC30,
    ctw_public_transportation = HC01_VC31,
    ctw_walk = HC01_VC32,
    ctw_other_means = HC01_VC33,
    ctw_wfh = HC01_VC34,
    occ_employed_pop = HC01_VC40 ,
    occ_mgmnt_bus_sci_art = HC01_VC41,
    occ_service = HC01_VC42,
    occ_sales = HC01_VC43,
    occ_natres_constr_maint = HC01_VC44,
    occ_prod_trans_moving = HC01_VC45,
    ind_employed_pop = HC01_VC49,
    ind_agriculture_forestry_fishing_hunting =  HC01_VC50,
    ind_constr = HC01_VC51,
    ind_manufacturing = HC01_VC52,
    ind_wholesale_trade = HC01_VC53,
    ind_retail_trade = HC01_VC54,
    ind_transportation_warehousing_utilities = HC01_VC55,
    ind_information = HC01_VC56,
    ind_finance_insurance_realestate_rental_leasing = HC01_VC57,
    ind_professional_scientific_management_waste_management = HC01_VC58,
    ind_education_healthcare_social_work = HC01_VC59,
    ind_arts_entertain_recreation = HC01_VC60,
    ind_other_services = HC01_VC61,
    ind_public_admin = HC01_VC62,
    ctw_perc = HC03_VC28,
    ctw_alone_perc = HC03_VC29,
    ctw_carpool_perc = HC03_VC30,
    ctw_public_transportation_perc =  HC03_VC31,
    ctw_walk_perc = HC03_VC32,
    ctw_other_means_perc = HC03_VC33,
    ctw_wfh_perc = HC03_VC34,
    occ_employed_pop_perc = HC03_VC40,
    occ_mgmnt_bus_sci_art_perc = HC03_VC41,
    occ_service_perc = HC03_VC42,
    occ_sales_perc =  HC03_VC43,
    occ_natres_constr_maint_perc= HC03_VC44,
    occ_prod_trans_moving_perc = HC03_VC45,
    ind_agriculture_forestry_fishing_hunting_perc = HC03_VC50,
    ind_constr_perc =  HC03_VC51,
    ind_manufacturing_perc = HC03_VC52, 
    ind_wholesale_trade_perc = HC03_VC53, 
    ind_retail_trade_perc = HC03_VC54,
    ind_transportation_warehousing_utilities_perc = HC03_VC55,
    ind_information_perc = HC03_VC56,
    ind_finance_insurance_realestate_rental_leasing_perc =  HC03_VC57,
    ind_professional_scientific_management_waste_management_perc = HC03_VC58,
    ind_education_healthcare_social_work_perc =  HC03_VC59,
    ind_arts_entertain_recreation_perc = HC03_VC60,
    ind_other_services_perc = HC03_VC61,
    ind_public_admin_perc = HC03_VC62,
    one_race_perc = HD02_S077,
    white = HD01_S078,
    white_perc = HD02_S078,
    black =  HD01_S079,
    black_perc = HD02_S079,
    american_indian_or_alaskan = HD01_S080,
    american_indian_or_alaskan_perc =  HD02_S080,
    asian =  HD01_S081,
    asian_perc = HD02_S081,
    indian = HD01_S082,
    indian_perc = HD02_S082,
    chinese = HD01_S083,
    chinese_perc = HD02_S083,
    filipino =  HD01_S084 ,
    filipino_perc = HD02_S084,
    japanese = HD01_S085,
    japanese_perc = HD02_S085,
    korean = HD01_S086,
    korean_perc = HD02_S086,
    vietnamese =  HD01_S087,
    vietnamese_perc =  HD02_S087,
    other_asian =  HD01_S088,
    other_asian_perc = HD02_S088,
    hawaiian_and_islander = HD01_S089,
    hawaiian_and_islander_perc = HD02_S089,
    hawaiian = HD01_S090,
    hawaiian_perc = HD02_S090,
    guamanian_chamorro = HD01_S091,
    guamanian_chamorro_perc = HD02_S091,
    samoan = HD01_S092,
    samoan_perc = HD02_S092,
    pacific_islander = HD01_S093,
    pacific_islander_perc = HD02_S093,
    other_race = HD01_S094,
    other_race_perc = HD02_S094,
    two_plus_races = HD01_S095,
    two_plus_races_perc = HD02_S095,
    white_amerind_alaska = HD01_S096, 
    white_amerind_alaska_perc = HD02_S096,
    white_asian = HD01_S097,
    white_asian_perc = HD02_S097,
    white_black = HD01_S098,
    white_black_perc = HD02_S098,
    white_other_race = HD01_S099,
    white_other_race_perc = HD02_S099,
    white_or_mixed_white = HD01_S100,
    white_or_mixed_white_perc = HD02_S100,
    black_or_mixed_black = HD01_S101,
    black_or_mixed_black_perc = HD02_S101,
    amerind_alaskan_or_mixed = HD01_S102,
    amerind_alaskan_or_mixed_perc = HD02_S102,
    asian_or_mixed_asain = HD01_S103,
    asian_or_mixed_asian_perc = HD02_S103,
    hawaiian_islander_or_mixed = HD01_S104,
    hawaiian_islander_or_mixed_perc = HD02_S104,
    some_other_mixed_races = HD01_S105,
    some_other_mixed_races_perc = HD02_S105,
    hispanic_or_latino = HD01_S106,
    hispanic_or_latino_any = HD01_S107,
    hispanic_or_latino_any_perc = HD02_S107,
    mexican = HD01_S108,
    mexican_perc = HD02_S108,
    puerto_rican = HD01_S109,
    puerto_rican_perc = HD02_S109,
    cuban = HD01_S110,
    cuban_perc = HD02_S110,
    other_hispanic = HD01_S111,
    other_hispanic_perc = HD02_S111,
    not_hispanic_or_latino = HD01_S112,
    not_hispanic_or_latino_perc = HD02_S112,
    hispanic_latino_and_race = HD01_S113,
    hispanic_latino_and_hispanic = HD01_S114,
    hispanic_latino_and_hispanic_perc = HD02_S114,
    hispanic_and_white = HD01_S115,
    hispanic_and_white_perc = HD02_S115,
    hispanic_and_black =  HD01_S116,
    hispanic_and_black_perc = HD02_S116,
    hispanic_and_amerind_alaskan = HD01_S117,
    hispanic_and_amerind_alaskan_perc = HD02_S117, 
    hispanic_and_asain = HD01_S118,
    hispanic_and_asain_alone = HD02_S118,
    hispanic_and_other_race = HD01_S120,
    hispanic_and_other_race_perc = HD02_S120,
    hispanic_and_two_races = HD01_S121,
    hispanic_and_two_races_perc =  HD02_S121,
    hispanic_and_hawaiian = HD01_S119,
    hispanic_and_hawaiian_perc =  HD02_S119,
    not_hispanic = HD01_S122,
    not_hispanic_perc = HD02_S122
  ) 

# Rename separately due to errors in original run
clean_raleigh_data <- clean_raleigh_data %>% 
  rename(
  ctw_mean_travel = HC01_VC36,
  ctw_mean_travel_perc = HC03_VC36
)

# Rename separately due to errors in original run
clean_raleigh_data <- clean_raleigh_data %>% 
    rename(one_race = HD01_S077)

# Filter and select columns that contain unecessary information in clean_raleigh_data and delete them
clean_raleigh_data <- clean_raleigh_data %>% select(-contains("HD01")) %>% select(-contains("HD02"))


# The following is still in the works:
first.analysis <- prcomp(raleigh_data[,c(208,209)], center=TRUE, scale=TRUE)
first.analysis <- prcomp(raleigh_data[,c(207,210)], center=TRUE, scale=TRUE)

big.analysis <- prcomp(raleigh_data[,c(208:215)], center = TRUE, scale = TRUE)
summary(big.analysis)
str(big.analysis)
print(big.analysis)
summary(first.analysis)

autoplot(first.analysis)

