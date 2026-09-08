###########################################################
# Install Packages and Load libraries 
###########################################################

install.packages(c(
  "tidyverse",
  "dplyr",
  "lubridate",
  "ggrepel",
  "ggplot2",
  "gt"
))

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(gt)

### Setting up the data folder 
dataFolder = "./Data/Processed/"

Rental_mrk_04 <-  read.csv(paste0(dataFolder , "RentalMarket_Trend.csv"))
###########################################################
# Create output figures folder
###########################################################
if (!dir.exists("./outputs/figures")) {dir.create("./outputs/figures" , recursive = TRUE)}

###########################################################
# plotting rental market trend in 3 versions
###########################################################

Rental_Affordability_Market_Tightness_01 <- ggplot(data = Rental_mrk_04) +
  geom_point(
    aes(
      x = Median_Rent,
      y = Vacancy_Rate,
      size = Units,
      colour = Units
    ),
    alpha = 0.8
  ) +
  scale_colour_viridis_c(name = "Rental Units") +
  scale_size_continuous(name = "Rental Units") +
  guides(
    colour = guide_legend(order = 1),
    size = guide_legend(order = 1)
  ) +
  labs(
    title = "Rental Affordability and Market Tightness",
    subtitle = "Ontario Rental Market 1990-2025",
    x = "Average Rent ($)",
    y = "Vacancy Rate (%)"
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")
  )

###########################################################
# save plot into ./figures folder
###########################################################
ggsave(
  filename = "./outputs/figures/Rental_Affordability_Market_Tightness_01.png",
  plot = Rental_Affordability_Market_Tightness_01,
  width = 10,
  height = 6,
  dpi = 300
)

########### second version: median rent cut off added to the plot
Rental_Affordability_Market_Tightness_02 <- ggplot(data = Rental_mrk_04) +
  geom_hline(
    yintercept = 3,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = mean(Rental_mrk_04$Median_Rent, na.rm = TRUE),
    linetype = "dashed",
    color = "blue",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      x = Median_Rent,
      y = Vacancy_Rate,
      size = Units,
      colour = Units
    ),
    alpha = 0.8
  ) +
  scale_colour_viridis_c(name = "Rental Units") +
  scale_size_continuous(name = "Rental Units") +
  guides(
    colour = guide_legend(order = 1),
    size = guide_legend(order = 1)
  ) +
  labs(
    title = "Rental Affordability and Market Tightness",
    subtitle = "Ontario Rental Market 1990-2025",
    x = "Average Rent ($)",
    y = "Vacancy Rate (%)"
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")
  )


########### Third version years added to data points
ggsave(
  filename = "./outputs/figures/Rental_Affordability_Market_Tightness_02.png",
  plot = Rental_Affordability_Market_Tightness_02,
  width = 10,
  height = 6,
  dpi = 300
)

###################
Rental_Affordability_Market_Tightness_03 <-  ggplot(data = Rental_mrk_04) +
  geom_hline(
    yintercept = 3,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = mean(Rental_mrk_04$Median_Rent, na.rm = TRUE),
    linetype = "dashed",
    color = "blue",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      x = Median_Rent,
      y = Vacancy_Rate,
      size = Units,
      colour = Units
    ),
    alpha = 0.8
  ) +
  geom_text_repel(
    aes(
      x = Median_Rent,
      y = Vacancy_Rate,
      label = Year
    ),
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_viridis_c(name = "Rental Units") +
  scale_size_continuous(name = "Rental Units") +
  guides(
    colour = guide_legend(order = 1),
    size = guide_legend(order = 1)
  ) +
  labs(
    title = "Rental Affordability and Market Tightness",
    subtitle = "Ontario Rental Market 1990-2025",
    x = "Average Rent ($)",
    y = "Vacancy Rate (%)"
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")
  )



ggsave(
  filename = "./outputs/figures/Rental_Affordability_Market_Tightness_03.png",
  plot = Rental_Affordability_Market_Tightness_03,
  width = 10,
  height = 6,
  dpi = 300
)

###########################################################
#  creating  Rent Affordability Index 
###########################################################

Rental_mrk_05 <-  Rental_mrk_04 %>% mutate(Rent_Index = Average_Rent / Average_Rent[Year == 1990],
                                           Units_Per_Vacant = Units * Vacancy_Rate / 100)



Rental_Plot <- Rental_mrk_06 %>%
  select(Date, Normalized_Rent_Index, Normalized_Units_Per_Vacant) %>%
  pivot_longer(
    cols = c(Normalized_Rent_Index, Normalized_Units_Per_Vacant),
    names_to = "Metric",
    values_to = "Value"
  )

###########################################################
#  plotting  Rent Affordability Index 
###########################################################
RentGrowth_vs_AvailableRentalSupply <-  ggplot(
  Rental_Plot,
  aes(
    x = Date,
    y = Value,
    color = Metric
  )
) +
  geom_smooth(
    method = "loess",
    span = 0.3,
    se = TRUE,
    linewidth = 0.8,
    alpha = 0.2
  ) +
  geom_line(
    linewidth = 0.3
  ) +
  geom_point(
    size = 1
  ) +
  labs(
    title = "Rent Growth versus Available Rental Supply",
    subtitle = "Normalized Indices (1990 = 0, Peak = 1)",
    x = "Year",
    y = "Normalized Index"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = "./outputs/figures/RentGrowth_vs_AvailableRentalSupply.png",
  plot = RentGrowth_vs_AvailableRentalSupply,
  width = 10,
  height = 6,
  dpi = 300
)

