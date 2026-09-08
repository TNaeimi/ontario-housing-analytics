###########################################################
# Install Packages and Load libraries 
###########################################################

install.packages(c(
  "tidyverse",
  "dplyr",
  "lubridate",
  "ggrepel",
  "ggplot2",
  "gt",
  "forcats",
  "tidymodels",
  "rsample",
  "yardstick",
  "randomForest"
))



library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(gt)
library(forcats)
library(tidymodels)
library(rsample)
library(yardstick)
library(randomForest)


### Setting up the data folder 
dataFolder = "./Data/Raw/"
#########################################################
### Data cleaning and preparation
#########################################################

#################### Age_of_Pop Data #################### 

Age_of_Pop <- read.csv(paste0(dataFolder, "Age of Population.csv"),skip = 2)

Age_of_Pop <- Age_of_Pop %>% mutate(Municipality = X) %>% 
  select(Municipality,X0_14,X15_24,X25_34,X35_44,X45_54,X55_64,Total.65,Total_POP= Total)

Age_of_Pop_01 <- Age_of_Pop %>%
  mutate(
    across(
      -Municipality,
      ~ as.numeric(gsub(",", "", .))
    )
  )

Age_of_Pop_MSTR <- Age_of_Pop_01


write.csv(Age_of_Pop_MSTR , "./Data/Processed/Age_of_Pop_MSTR.csv")

#################### Household Income Data #################### 
####### owners
Household_Inc_Owrs <- read.csv(paste0(dataFolder,"Household Income - Average and Median_Owners.csv"),skip = 2)


Household_Inc_Owrs <- Household_Inc_Owrs %>% mutate(Municipality = X  , HouseholdStatus = "Owners") %>% 
  select(Municipality, HouseholdStatus, Average.Household.Income.Before.Taxes = Average.Household.Income.Before.Taxes ,Median.Household.Income.Before.Taxes ,Average.Household.Income.After.Taxes ,Median.Household.Income.After.Taxes)

Household_Inc_Owrs_01 <- Household_Inc_Owrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )

####### Renters

Household_Inc_Renrs <- read.csv(paste0(dataFolder,"Household Income_Average and Median_Renters.csv"),skip = 2)


Household_Inc_Renrs <- Household_Inc_Renrs %>% mutate(Municipality = X  , HouseholdStatus = "Renters") %>% 
  select(Municipality, HouseholdStatus, Average.Household.Income.Before.Taxes = Average.Household.Income.Before.Taxes ,Median.Household.Income.Before.Taxes ,Average.Household.Income.After.Taxes ,Median.Household.Income.After.Taxes)

Household_Inc_Renrs_01 <- Household_Inc_Renrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )



Household_Inco <- rbind(Household_Inc_Owrs_01 ,Household_Inc_Renrs_01 )

Household_Inco_01 <- Household_Inco %>% group_by(Municipality) %>% 
  summarise(Ave_Incom_Onw_Rent = min(Median.Household.Income.After.Taxes))



Household_Inco_02 <- Household_Inco_01 %>% mutate(Houhold_Mon_Inco = Ave_Incom_Onw_Rent / 12)


Household_Inco_MSTR <- Household_Inco_02 


write.csv(Household_Inco_MSTR , "./Data/Processed/Household_Inco_MSTR.csv")


#################### Household Size Data #################### 
####### owners
Household_Size_Owrs <- read.csv(paste0(dataFolder,"Household Size_Owners.csv"),skip = 2)


Household_Size_Owrs <- Household_Size_Owrs %>% mutate(Municipality = X  , HouseholdStatus = "Owners") %>% 
  select(Municipality, HouseholdStatus, One.Person.Household, Two.Person.Household, Three.Person.Household, Four.Person.Household, Five.Or.More.Person.Household,  Total)

Household_Size_Owrs_01 <- Household_Size_Owrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )

####### Renters

Household_Size_Renrs <- read.csv(paste0(dataFolder, "Household Size_Renters.csv"),skip = 2)

Household_Size_Renrs <- Household_Size_Renrs %>% mutate(Municipality = X  , HouseholdStatus = "Renters") %>% 
  select(Municipality, HouseholdStatus, One.Person.Household, Two.Person.Household, Three.Person.Household, Four.Person.Household, Five.Or.More.Person.Household,  Total)

Household_Size_Renrs_01 <- Household_Size_Renrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


Household_Size <- rbind(Household_Size_Owrs_01 ,Household_Size_Renrs_01 )

Household_Size <- Household_Size %>%
  group_by(Municipality) %>%
  summarise(
    across(
      c(One.Person.Household,
        Two.Person.Household,
        Three.Person.Household,
        Four.Person.Household,
        Five.Or.More.Person.Household),
      ~ mean(.x, na.rm = TRUE)
    )
  )

Household_Size <- Household_Size %>% filter(Municipality != "Ontario")

Household_Size_MSTR <- Household_Size

write.csv(Household_Size_MSTR , "./Data/Processed/Household_Size_MSTR.csv")


#################### Household Type Data #################### 
####### Owners
Household_Type_Owrs <- read.csv(paste0(dataFolder,"Household Type_Owners.csv"),skip = 2)


Household_Type_Owrs <- Household_Type_Owrs %>% mutate(Municipality = X  , HouseholdStatus = "Owners") %>% 
  select(Municipality, HouseholdStatus, Couple.With.Children, Couple.Without.Children, Total.Lone..Parent.Households, Multiple..Family ,Total.One..Person.Households, Other.Non.Family, Total)

Household_Type_Owrs_01 <- Household_Type_Owrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


####### Renters

Household_Type_Renrs <- read.csv(paste0(dataFolder,"Household Type_Renters.csv"),skip = 2)


Household_Type_Renrs <- Household_Type_Renrs %>% mutate(Municipality = X  , HouseholdStatus = "Renters") %>% 
  select(Municipality, HouseholdStatus, Couple.With.Children, Couple.Without.Children, Total.Lone..Parent.Households, Multiple..Family ,Total.One..Person.Households, Other.Non.Family, Total)

Household_Type_Renrs_01 <- Household_Type_Renrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )



Household_Type <- rbind(Household_Type_Owrs_01 ,Household_Type_Renrs_01 )


Household_Type <- Household_Type %>%
  group_by(Municipality) %>%
  summarise(
    across(
      c(Couple.With.Children,
        Couple.Without.Children,
        Total.Lone..Parent.Households,
        Multiple..Family,
        Total.One..Person.Households,
        Other.Non.Family),
      ~ mean(.x, na.rm = TRUE)
    )
  )

Household_Type <- Household_Type %>% filter(Municipality != "Ontario")

Household_Type_MSTR <- Household_Type

write.csv(Household_Type_MSTR , "./Data/Processed/Household_Type_MSTR.csv")

#################### Immigrant Household Data #################### 
####### Owners
Immigrant_Household_Owrs <- read.csv(paste0(dataFolder,"Immigrant Houshold_Owners.csv"),skip = 2)


Immigrant_Household_Owrs <- Immigrant_Household_Owrs %>% mutate(Municipality = X  , HouseholdStatus = "Owners") %>% 
  select(Municipality,HouseholdStatus, Non.Immigrant, Non.Permanent.Resident, Immigrant, Landed.Before.2006, Landed.2006.to.2015, Recent.Immigrants..Landed.2016.2021. , Total)

Immigrant_Household_Owrs_01 <- Immigrant_Household_Owrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )

####### Renters
Immigrant_Household_Renrs <- read.csv(paste0(dataFolder,"Immigrant Houshold_Renters.csv"),skip = 2)


Immigrant_Household_Renrs <- Immigrant_Household_Renrs %>% mutate(Municipality = X  , HouseholdStatus = "Renters") %>% 
  select(Municipality,HouseholdStatus, Non.Immigrant, Non.Permanent.Resident, Immigrant, Landed.Before.2006, Landed.2006.to.2015, Recent.Immigrants..Landed.2016.2021. , Total)

Immigrant_Household_Renrs_01 <- Immigrant_Household_Renrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )



Immigrant_Household <- rbind(Immigrant_Household_Owrs_01 ,Immigrant_Household_Renrs_01 )


Immigrant_Household <- Immigrant_Household %>%
  group_by(Municipality) %>%
  summarise(
    across(
      c(Non.Immigrant,
        Non.Permanent.Resident,
        Immigrant,
        Landed.Before.2006,
        Landed.2006.to.2015,
        Recent.Immigrants..Landed.2016.2021.),
      ~ mean(.x, na.rm = TRUE)
    )
  )

Immigrant_Household <- Immigrant_Household %>% filter(Municipality != "Ontario")

Immigrant_Household_MSTR <- Immigrant_Household

write.csv(Immigrant_Household_MSTR , "./Data/Processed/Immigrant_Household_MSTR.csv")

#################### Monthly Shelter Costs Data #################### 
####### Owners
Monthly_Shelter_Costs_Owrs <- read.csv(paste0(dataFolder,"Monthly Shelter Costs_Owners.csv"),skip = 2)


Monthly_Shelter_Costs_Owrs <- Monthly_Shelter_Costs_Owrs %>% mutate(Municipality = X  , HouseholdStatus = "Owners") %>% 
  select(Municipality,HouseholdStatus,Less_500 = Less.Than..500.Per.Month,
         X500_999 = X.500.to..999.Per.Month,
         X1000_1499 = X.1.000.to..1.499.Per.Month,
         X1500_1999 = X.1.500.to..1.999.Per.Month,
         X2000_plus = X.2.000.And.Over.Per.Month,
         Total = Total.Non.Farm..Non.Band.Households)


Monthly_Shelter_Costs_Owrs_01 <- Monthly_Shelter_Costs_Owrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


Monthly_Shelter_Costs_Owrs_01 <- Monthly_Shelter_Costs_Owrs_01 %>%
  mutate(
    Estimated_Monthly_Shelter_Cost =
      (
        Less_500 * 250 +
          X500_999 * 750 +
          X1000_1499 * 1250 +
          X1500_1999 * 1750 +
          X2000_plus * 2500
      ) / Total
  )

####### Renters

Monthly_Shelter_Costs_Renrs <- read.csv(paste0(dataFolder,"Monthly Shelter Costs_Renters.csv"),skip = 2)


Monthly_Shelter_Costs_Renrs <- Monthly_Shelter_Costs_Renrs %>% mutate(Municipality = X  , HouseholdStatus = "Renters") %>% 
  select(Municipality,HouseholdStatus,Less_500 = Less.Than..500.Per.Month,
         X500_999 = X.500.to..999.Per.Month,
         X1000_1499 = X.1.000.to..1.499.Per.Month,
         X1500_1999 = X.1.500.to..1.999.Per.Month,
         X2000_plus = X.2.000.And.Over.Per.Month,
         Total = Total.Non.Farm..Non.Band.Households)


Monthly_Shelter_Costs_Renrs_01 <- Monthly_Shelter_Costs_Renrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


Monthly_Shelter_Costs_Renrs_01 <- Monthly_Shelter_Costs_Renrs_01 %>%
  mutate(
    Estimated_Monthly_Shelter_Cost =
      (
        Less_500 * 250 +
          X500_999 * 750 +
          X1000_1499 * 1250 +
          X1500_1999 * 1750 +
          X2000_plus * 2500
      ) / Total
  )


Monthly_Shelter_Costs <- rbind(Monthly_Shelter_Costs_Owrs_01 ,Monthly_Shelter_Costs_Renrs_01 )


Monthly_Shelter_Costs_MeanEstimate <- Monthly_Shelter_Costs %>%
  group_by(Municipality) %>%
  summarise(Estimated_Monthly_Shelter_Cost = mean(Estimated_Monthly_Shelter_Cost))


Monthly_Shelter_Costs_MeanEstimate <- Monthly_Shelter_Costs_MeanEstimate %>% filter(Municipality != "Ontario")

Monthly_Shelter_Costs_MSTR <- Monthly_Shelter_Costs_MeanEstimate

write.csv(Monthly_Shelter_Costs_MSTR , "./Data/Processed/Monthly_Shelter_Costs_MSTR.csv")

#################### Mortgages Data #################### 

Mortgages <- read.csv(paste0(dataFolder ,"Morgages.csv") ,skip = 2)


Mortgages <- Mortgages %>% mutate(Municipality = X ) %>% 
  select(Municipality, 
         With.a.Mortgage, Without.a.Mortgage, Total.Private.Households.With.Mortgage.Status.Known)


Mortgages_01 <- Mortgages %>%
  mutate(
    across(
      -Municipality ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


Mortgages_01 <- Mortgages_01 %>% filter(Municipality != "Ontario")

Mortgages_MSTR <- Mortgages_01

write.csv(Mortgages_MSTR , "./Data/Processed/Mortgages_MSTR.csv")

#################### Monthly Shelter Costs Data #################### 
####### Owners
Structure_Owrs <- read.csv(paste0(dataFolder ,"Structure_Owners.csv") ,skip = 2)

head(Structure_Owrs)

Structure_Owrs <- Structure_Owrs %>% mutate(Municipality = X  , HouseholdStatus = "Owners") %>% 
  select(Municipality, HouseholdStatus,
         Single.Detached, 
         Semi.Detached,
         Row,
         Duplex,
         Low.Rise.Apt.,
         High.Rise.Apt.,
         Other,
         Total_structure = Total )

Structure_Owrs_01 <- Structure_Owrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


######## Renters
Structure_Renrs <- read.csv(paste0(dataFolder ,"Strucure _ Renters.csv") ,skip = 2)

Structure_Renrs <- Structure_Renrs %>% mutate(Municipality = X  , HouseholdStatus = "Renters") %>% 
  select(Municipality, HouseholdStatus,
         Single.Detached, 
         Semi.Detached,
         Row,
         Duplex,
         Low.Rise.Apt.,
         High.Rise.Apt.,
         Other,
         Total_structure = Total )

Structure_Renrs_01 <- Structure_Renrs %>%
  mutate(
    across(
      -c(Municipality,HouseholdStatus) ,
      ~ as.numeric(gsub(",", "", .))
    )
  )


Structure_Type <- rbind(Structure_Owrs_01 ,Structure_Renrs_01 )

Structure_Type <- Structure_Type %>%
  group_by(Municipality) %>%
  summarise(
    across(
      c(Single.Detached,
        Semi.Detached,
        Row ,
        Duplex ,
        Low.Rise.Apt.,
        High.Rise.Apt.,
        Other ,
        Total_structure),
      ~ mean(.x, na.rm = TRUE)
    )
  )

Structure_Type <- Structure_Type %>% filter(Municipality != "Ontario")

Structure_Type_MSTR <- Structure_Type

write.csv(Structure_Type_MSTR , "./Data/Processed/Structure_Type_MSTR.csv")
