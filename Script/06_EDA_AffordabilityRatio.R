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


Age_of_Pop_MSTR <- read.csv(paste0(dataFolder, "Age_of_Pop_MSTR.csv"))

Household_Inco_MSTR <- read.csv(paste0(dataFolder, "Household_Inco_MSTR.csv"))

Household_Size_MSTR <- read.csv(paste0(dataFolder, "Household_Size_MSTR.csv"))

Household_Type_MSTR <- read.csv(paste0(dataFolder, "Household_Type_MSTR.csv"))

Immigrant_Household_MSTR <- read.csv(paste0(dataFolder, "Immigrant_Household_MSTR.csv"))

Monthly_Shelter_Costs_MSTR <- read.csv(paste0(dataFolder, "Monthly_Shelter_Costs_MSTR.csv"))

Mortgages_MSTR <- read.csv(paste0(dataFolder, "Mortgages_MSTR.csv"))

Structure_Type_MSTR <- read.csv(paste0(dataFolder, "Structure_Type_MSTR.csv"))


###########################################################
# Create output figures folder
###########################################################

if (!dir.exists("./outputs/figures")) {dir.create("./outputs/figures" , recursive = TRUE)}

############### Integrating all MSTR datasets #############

Housing_Integrated_df <- Age_of_Pop_MSTR %>%
  inner_join(Household_Inco_MSTR, by = "Municipality") %>%
  inner_join(Household_Size_MSTR, by = "Municipality") %>%
  inner_join(Household_Type_MSTR, by = "Municipality") %>%
  inner_join(Immigrant_Household_MSTR, by = "Municipality") %>%
  inner_join(Monthly_Shelter_Costs_MSTR, by = "Municipality") %>%
  inner_join(Mortgages_MSTR, by = "Municipality") %>%
  inner_join(Structure_Type_MSTR, by = "Municipality")



############### Affordability_Ratio #############

Housing_Integrated_df_01 <- Housing_Integrated_df %>%
  mutate(
    Affordability_Ratio = Estimated_Monthly_Shelter_Cost / (Ave_Incom_Onw_Rent /12)
    
  )

write.csv(Housing_Integrated_df_01 , "./Data/Processed/Housing_Integrated_df_ForModeling.csv")


############## Explanatory Data Analysis ###################
ontarioAffordabilityRatio_01 <- Housing_Integrated_df_01 %>%
  mutate(Municipality = fct_reorder(Municipality, Affordability_Ratio, median)) %>%
  ggplot(aes(x = Affordability_Ratio, y = Municipality, fill = Municipality)) +
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Affordability Ratio Across Ontario 2021",
       subtitle = "Ordered by Median Ratio",
       y = "Ontario Municipality",
       x = "Affordability Ratio") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      size = 6,
      face = "bold"
    ),
    axis.title.x = element_text(
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  )


########### saving plot
ggsave(
  filename = "./outputs/figures/ontarioAffordabilityRatio_01.png",
  plot = ontarioAffordabilityRatio_01,
  width = 10,
  height = 6,
  dpi = 300
)

# adding a midpoint of the municipalities on the y-axis
midpoint <- n_distinct(Housing_Integrated_df_01$Municipality) / 4 + 0.3

ontarioAffordabilityRatio_02 <- Housing_Integrated_df_01 %>%
  mutate(
    Municipality = fct_reorder(
      Municipality,
      Affordability_Ratio,
      median
    )
  ) %>%
  ggplot(
    aes(
      x = Affordability_Ratio,
      y = Municipality,
      fill = Municipality
    )
  ) +
  geom_boxplot(show.legend = FALSE) +
  
  # 30% affordability threshold
  geom_vline(
    xintercept = 0.30,
    colour = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  # Horizontal line at Trent Hill
  geom_hline(
    yintercept = midpoint,
    colour = "blue",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Affordability Ratio Across Ontario 2021",
    subtitle = "Ordered by Median Ratio",
    y = "Ontario Municipality",
    x = "Affordability Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      size = 6,
      face = "bold"
    ),
    axis.title.x = element_text(
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  )

########### saving plot
ggsave(
  filename = "./outputs/figures/ontarioAffordabilityRatio_02.png",
  plot = ontarioAffordabilityRatio_01,
  width = 10,
  height = 6,
  dpi = 300
)

#################################### Income_vs_Affordability #################################### 

Income_vs_Affordability <- ggplot(Housing_Integrated_df_01,
       aes(Houhold_Mon_Inco, Affordability_Ratio)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(
    title = "Income vs Affordability Across Ontario 2021",
    subtitle = "Higher income → lower affordability challenges",
    y = "Affordability Ratio (%)",
    x = "Estimated Houshold Monthly Income ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 16),
    plot.title = element_text(
      face = "bold",
      size = 22
    )
  )
########### saving plot
ggsave(
  filename = "./outputs/figures/Income_vs_Affordability.png",
  plot = Income_vs_Affordability,
  width = 10,
  height = 6,
  dpi = 300
)

#################################### ShelterCost_vs_Affordability #################################### 


ShelterCost_vs_Affordability <- ggplot(Housing_Integrated_df_01,
       aes(Estimated_Monthly_Shelter_Cost,
           Affordability_Ratio)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title = "Shelter Cost vs Affordability Across Ontario 2021",
    subtitle = "Higher shelter costs → higher affordability challenges",
    y = "Affordability Ratio (%)",
    x = "Estimated Shelter Cost Monthly ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(
      size = 20,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 20,
      face = "bold",
      margin = ggplot2::margin(r = 15)
    ),
    plot.subtitle = element_text(
      face = "bold",
      size = 14),
    plot.title = element_text(
      face = "bold",
      size = 20
    )
  )


########### saving plot
ggsave(
  filename = "./outputs/figures/ShelterCost_vs_Affordability.png",
  plot = ShelterCost_vs_Affordability,
  width = 10,
  height = 6,
  dpi = 300
)
