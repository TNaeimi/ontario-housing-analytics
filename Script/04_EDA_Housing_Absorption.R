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

Absorbed_Single_df <-  read.csv(paste0(dataFolder , "Absorbed_Single_01.csv"))
Absorbed_semiDetached_df <-  read.csv(paste0(dataFolder , "Absorbed_semiDetached_01.csv"))
Unabsorbed_Single_df <-  read.csv(paste0(dataFolder , "Unabsorbed_Single_01.csv"))
Unabsorbed_semiDetached_df <-  read.csv(paste0(dataFolder , "Unabsorbed_semiDetached_01.csv"))

###########################################################
# Create output figures folder
###########################################################
if (!dir.exists("./outputs/figures")) {dir.create("./outputs/figures" , recursive = TRUE)}


###########################################################
# Creating an integrated df for plotting
###########################################################

Housing_Market <- bind_rows(
  Absorbed_Single_df,
  Absorbed_semiDetached_df,
  Unabsorbed_Single_df,
  Unabsorbed_semiDetached_df
)

###########################################################
# plotting 
###########################################################

Municipal_Summary <- Housing_Market %>%
  group_by(Municipal) %>%
  summarise(
    Median_Price = mean(Median, na.rm = TRUE),
    Total_Units = sum(Units, na.rm = TRUE),
    Absorption_Rate =
      sum(Units[Status == "Absorbed"]) /
      sum(Units)
  )
HousingMarketacrossOntariomunicipalities <-  ggplot(
  Municipal_Summary,
  aes(
    x = Median_Price,
    y = Absorption_Rate,
    size = Total_Units,
    colour = Total_Units
  )
) +
  geom_point(alpha = 0.8) +
  ggrepel::geom_text_repel(
    aes(label = Municipal)
  ) +
  guides(
    colour = guide_legend(order = 1),
    size = guide_legend(order = 1)
  )+
  labs(
    title = "Comparing housing demand, prices, and inventory across Ontario municipalities",
    subtitle = "Higher absorption rates indicate stronger demand for new housing inventory",
    x = "Median Price $",
    y = " Absorption_Rate %"
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

########### saving plot
ggsave(
  filename = "./outputs/figures/HousingMarketacrossOntariomunicipalities.png",
  plot = HousingMarketacrossOntariomunicipalities,
  width = 10,
  height = 6,
  dpi = 300
)

