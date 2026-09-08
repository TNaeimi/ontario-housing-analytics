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


### Setting up the data folder and loading data
dataFolder = "./Data/Processed/"


Housing_Integrated_df_01 <- read.csv(paste0(dataFolder, "Housing_Integrated_df_ForModeling.csv"))

########################################################################
########################### Feature engineering ########################
########################################################################

########### Age proportion
Housing_Integrated_df_02 <- Housing_Integrated_df_01 %>%
  mutate(
    Pct_0_14  = 100 * X0_14 / Total_POP,
    Pct_15_24 = 100 * X15_24 / Total_POP,
    Pct_25_34 = 100 * X25_34 / Total_POP,
    Pct_35_44 = 100 * X35_44 / Total_POP,
    Pct_45_54 = 100 * X45_54 / Total_POP,
    Pct_55_64 = 100 * X55_64 / Total_POP,
    Pct_65_Plus = 100 * Total.65 / Total_POP
  )

############## houshold size proportion
Housing_Integrated_df_03 <- Housing_Integrated_df_02 %>%
  mutate(
    Total_Households =
      One.Person.Household +
      Two.Person.Household +
      Three.Person.Household +
      Four.Person.Household +
      Five.Or.More.Person.Household,
    
    Share_One_Person =
      One.Person.Household / Total_Households,
    
    Share_Two_Person =
      Two.Person.Household / Total_Households,
    
    Share_Three_Person =
      Three.Person.Household / Total_Households,
    
    Share_Four_Person =
      Four.Person.Household / Total_Households,
    
    Share_FivePlus_Person =
      Five.Or.More.Person.Household / Total_Households
  )
############### Houshold type proportion

Housing_Integrated_df_04 <- Housing_Integrated_df_03 %>%
  mutate(
    Total_Household_Type =
      Couple.With.Children +
      Couple.Without.Children +
      Total.Lone..Parent.Households +
      Multiple..Family +
      Total.One..Person.Households +
      Other.Non.Family,
    
    Share_Couple_With_Children =
      Couple.With.Children / Total_Household_Type,
    
    Share_Couple_Without_Children =
      Couple.Without.Children / Total_Household_Type,
    
    Share_Lone_Parent =
      Total.Lone..Parent.Households / Total_Household_Type,
    
    Share_Multiple_Family =
      Multiple..Family / Total_Household_Type,
    
    Share_One_Person_Household =
      Total.One..Person.Households / Total_Household_Type,
    
    Share_Other_Non_Family =
      Other.Non.Family / Total_Household_Type
  )


################# Imigrant Proportion
Housing_Integrated_df_05 <- Housing_Integrated_df_04 %>%
  mutate(
    Share_Immigrant = Immigrant / Total_POP,
    
    Share_Pre2006 =
      Landed.Before.2006 / Immigrant,
    
    Share_2006_2015 =
      Landed.2006.to.2015 / Immigrant,
    
    Share_Recent_Immigrant =
      Recent.Immigrants..Landed.2016.2021. / Immigrant
  )


##################### Housing Structure Share

Housing_Integrated_df_06 <- Housing_Integrated_df_05 %>%
  mutate(
    Apartment_Share =
      (Low.Rise.Apt. + High.Rise.Apt.) / Total_structure,
    
    Detached_Share =
      Single.Detached / Total_structure,
    
    Missing_Middle_Share =
      (Semi.Detached + Row + Duplex) / Total_structure
  )



write.csv(Housing_Integrated_df_06, "./Data/Processed/Housing_Integrated_df_FE.csv",row.names = FALSE)

