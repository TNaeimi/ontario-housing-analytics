###########################################################
# Install Packages and Load libraries 
###########################################################

install.packages(c(
  "tidyverse",
  "dplyr",
  "lubridate",
  "ggplot2",
  "gt"
))

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gt)


### Setting up the data folder 
dataFolder = "./Data/Raw/"

Dwel_Compl <-  read.csv(paste0(dataFolder ,  "Historical Completions by Dwelling Type.csv"), skip =2)
Dwel_Start <-  read.csv(paste0(dataFolder ,  "Historical Starts by Dwelling Type.csv"), skip =2)
Rental_mrk <-  read.csv(paste0(dataFolder , "Historical Rental Market Statistics Summary.csv"), skip =2)
Absorbed_Single <-  read.csv(paste0(dataFolder , "Absorbed_Single.csv"), skip =2)
Absorbed_semiDetached <-  read.csv(paste0(dataFolder , "Absorbed_semiDetached.csv"), skip =2)
Unabsorbed_Single <-  read.csv(paste0(dataFolder , "Unabsorbed_Single.csv"), skip =2)
Unabsorbed_semiDetached <-  read.csv(paste0(dataFolder , "Unabsorbed_semiDetached.csv"), skip =2)


###########################################################
## Data cleaning 
# renaming columns, selecting columns , mutationg
# removing , from numerical fields
# casting different column types
###########################################################

Dwel_Compl %>% select(Date = X, Single , Semi_Detach = Semi.Detached, Row , Apartment, Total) %>% 
  summary()


Dwel_Compl_Cl01 <- Dwel_Compl %>% select(Date = X, Single , Semi_Detach = Semi.Detached, Row , Apartment, Total) %>% 
  mutate(
    Single = as.numeric(gsub(",", "", Single)),
    Semi_Detach = as.numeric(gsub(",", "" , Semi_Detach)),
    Row = as.numeric(gsub(",", "", Row)),
    Apartment = as.numeric(gsub(",","", Apartment)),
    Total = as.numeric(gsub(",", "", Total))
  )

Dwel_Compl_Cl02 <- Dwel_Compl_Cl01 %>% 
  mutate(Year = str_extract(Dwel_Compl_Cl01$Date,"^[^ ]+"),
         Month = word(Date , 2))


Dwel_Compl_Cl03 <- Dwel_Compl_Cl02 %>%
  mutate(
    Date = my(paste(Month, Year))
  )

write.csv(Dwel_Compl_Cl03 , "./Data/Processed/Dwel_Compl_Trend.csv")


#######################################################################################
###############################  Dwel_Start   #########################################
#######################################################################################

Dwel_Start %>% select(Date = X, Single , Semi_Detach = Semi.Detached, Row , Apartment, Total = All) %>% 
  summary()


Dwel_Start_01 <- Dwel_Start %>% select(Date = X, Single , Semi_Detached = Semi.Detached, Row , Apartment, Total = All) %>% 
  mutate(
    Single = as.numeric(gsub(",", "", Single)),
    Semi_Detach = as.numeric(gsub(",", "" , Semi_Detached)),
    Row = as.numeric(gsub(",", "", Row)),
    Apartment = as.numeric(gsub(",","", Apartment)),
    Total = as.numeric(gsub(",", "", Total))
  )

head(Dwel_Start_01)

Dwel_Start_02 <- Dwel_Start_01 %>% 
  mutate(Month = str_extract(Dwel_Start_01$Date,"^[^_]+"),
         Year = substr(Dwel_Start_01$Date, 5 , 6),
         Year = as.numeric(Year))


Dwel_Start_02$Year <- ifelse(
  Dwel_Start_02$Year < 10,
  Dwel_Start_02$Year + 2000,
  ifelse(
    Dwel_Start_02$Year <= 26,
    Dwel_Start_02$Year + 2000,
    Dwel_Start_02$Year + 1900
  )
)


write.csv(Dwel_Start_02 , "./Data/Processed/Dwel_Start_Trend.csv")

#######################################################################################
###############################  Rental_mrk   #########################################
#######################################################################################

Rental_mrk %>% select( Date = X , Vacancy_Rate= Vacancy.Rate...., Availability_Rate = Availability.Rate....,
                       Average_Rent = Average.Rent....,Median_Rent = Median.Rent...., Change = X..Change,
                       Units) %>% summary()


Rental_mrk_01 <- Rental_mrk %>% select( Date = X , Vacancy_Rate= Vacancy.Rate...., Availability_Rate = Availability.Rate....,
                                        Average_Rent = Average.Rent....,Median_Rent = Median.Rent...., Change = X..Change,
                                        Units) %>% mutate(
                                          Availability_Rate = parse_number(Availability_Rate),
                                          Change = parse_number(Change),
                                          Average_Rent = as.numeric(gsub(",", "" , Average_Rent)),
                                          Median_Rent =  as.numeric(gsub(",", "" , Median_Rent)),                    
                                          Units = as.numeric(gsub(",", "" , Units))
                                        )

str(Rental_mrk_01)


Rental_mrk_02 <- Rental_mrk_01 %>%  
  mutate(
    Vacancy_Rate = as.numeric(Vacancy_Rate),
    Availability_Rate = as.numeric(Availability_Rate),
    Average_Rent = as.numeric(Average_Rent),
    Median_Rent = as.numeric(Median_Rent),
    Change = as.numeric(Change),
    Units = as.numeric(Units)
  )


Rental_mrk_03 <- Rental_mrk_02 %>% 
  mutate(Year = str_extract(Rental_mrk_02$Date,"^[^ ]+"),
         Month = word(Date , 2))

Rental_mrk_04 <- Rental_mrk_03 %>%
  mutate(
    Date = my(paste(Month, Year))
  )


write.csv(Rental_mrk_04 , "./Data/Processed/RentalMarket_Trend.csv")


#######################################################################################
###############################  Absorbed_Single   ####################################
#######################################################################################


Absorbed_Single_01 <- Absorbed_Single %>%
  select(
    Municipal = X,
    Median,
    Average,
    Units
  ) %>%
  mutate(
    Median = as.numeric(gsub(",", "", gsub("\\*\\*", "", Median))),
    Average = as.numeric(gsub(",", "", gsub("\\*\\*", "", Average))),
    Units = as.numeric(gsub(",", "", Units)),
    Type = "Single",
    Status = "Absorbed"
  ) %>%
  filter(Municipal != "Ontario")

write.csv(Absorbed_Single_01 , "./Data/Processed/Absorbed_Single_01.csv")

#######################################################################################
###############################  Absorbed_semiDetached   ##############################
#######################################################################################

Absorbed_semiDetached_01 <- Absorbed_semiDetached %>%
  select(
    Municipal = X,
    Median,
    Average,
    Units
  ) %>%
  mutate(
    Median = as.numeric(gsub(",", "", gsub("\\*\\*", "", Median))),
    Average = as.numeric(gsub(",", "", gsub("\\*\\*", "", Average))),
    Units = as.numeric(gsub(",", "", Units)),
    Type = "Semi_Detached",
    Status = "Absorbed"
  ) %>%
  filter(Municipal != "Ontario")


write.csv(Absorbed_semiDetached_01 , "./Data/Processed/Absorbed_semiDetached_01.csv")
#######################################################################################
###############################  Unabsorbed_Single_01   ##############################
#######################################################################################

Unabsorbed_Single_01 <- Unabsorbed_Single %>%
  select(
    Municipal = X,
    Median,
    Average,
    Units
  ) %>%
  mutate(
    Median = as.numeric(gsub(",", "", gsub("\\*\\*", "", Median))),
    Average = as.numeric(gsub(",", "", gsub("\\*\\*", "", Average))),
    Units = as.numeric(gsub(",", "", Units)),
    Type = "Single",
    Status = "Unabsorbed"
  ) %>%
  filter(Municipal != "Ontario")

write.csv(Unabsorbed_Single_01 , "./Data/Processed/Unabsorbed_Single_01.csv")

#######################################################################################
###############################  Unabsorbed_semiDetached   ############################
#######################################################################################

Unabsorbed_semiDetached_01 <- Unabsorbed_semiDetached %>%
  select(
    Municipal = X,
    Median,
    Average,
    Units
  ) %>%
  mutate(
    Median = as.numeric(gsub(",", "", gsub("\\*\\*", "", Median))),
    Average = as.numeric(gsub(",", "", gsub("\\*\\*", "", Average))),
    Units = as.numeric(gsub(",", "", Units)),
    Type = "Semi_Detached",
    Status = "Unabsorbed"
  ) %>%
  filter(Municipal != "Ontario")

write.csv(Unabsorbed_semiDetached_01 , "./Data/Processed/Unabsorbed_semiDetached_01.csv")
