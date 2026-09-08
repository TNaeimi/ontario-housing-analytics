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

list.dirs(".")
dataFolder = "./Data/Processed/"

Dwel_Compl_Cl03 <-  read.csv(paste0(dataFolder , "Dwel_Compl_Trend.csv"))
Dwel_Start_03 <-  read.csv(paste0(dataFolder , "Dwel_Start_Trend.csv"))

###########################################################
# Create output figures folder
###########################################################

if (!dir.exists("./outputs/figures")) {dir.create("./outputs/figures" , recursive = TRUE)}
if (!dir.exists("./outputs/reports")) {dir.create("./outputs/reports" , recursive = TRUE)}

###########################################################
# converting Dwel_Compl_Cl03 into long format for plotting
###########################################################

Dwel_Long_Compl_03 <- Dwel_Compl_Cl03 %>%
  pivot_longer(
    cols = c(Single, Semi_Detach, Row, Apartment),
    names_to = "Dwelling_Type",
    values_to = "Count"
  )

Dwel_Long_Compl_03 <- Dwel_Long_Compl_03 %>% mutate(Date = as.Date(Date))


Historical_Completions_DwellingTyp <-  ggplot(Dwel_Long_Compl_03,
       aes(x = Date, y = Count, color = Dwelling_Type)) +
  geom_line() +
  labs(
    title = "Historical Completions by Dwelling Type January 1990 to January 2026",
    subtitle = "Ontario Municipalities ",
    x = "Date",
    y = "Count",
    color = "Dwelling Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)
    )
  )



ggsave(
  filename = "./outputs/figures/Historical_Completions_DwellingTyp.png",
  plot = Historical_Completions_DwellingTyp,
  width = 10,
  height = 6,
  dpi = 300
)
###########################################################
# grouping and summarizing Dwel_Compl_Cl03 
###########################################################

Dwel_Compl_Cl04 <- Dwel_Compl_Cl03 %>% group_by(Year) %>% 
  summarise(Ave_Single = mean(Single),
            Ave_Semi_Detached = mean(Semi_Detach),
            Ave_Row = mean(Row),
            Ave_Apartment = mean(Apartment))

Dwel_Long_Compl_04 <- Dwel_Compl_Cl04 %>%
  pivot_longer(
    cols = starts_with("Ave_"),
    names_to = "Dwelling_Type",
    values_to = "Average_Completions"
  ) %>% 
  mutate(
    Dwelling_Type = sub("^Ave_", "", Dwelling_Type)
  )


###########################################################
# creating a color schema for plotting 
###########################################################

COMMON_COLORS <- c(
  "Apartment" = "#F8766D",
  "Row" = "#7CAE00",
  "Semi_Detached" = "#00BFC4",
  "Single" = "#C77CFF"
)

###########################################################
# plotting Dwel_Long_Compl_04
###########################################################

annual_dwelling_completion_by_type <- ggplot(
  Dwel_Long_Compl_04,
  aes(
    x = as.numeric(Year),
    y = Average_Completions,
    color = Dwelling_Type
  )
) +
  geom_line(alpha = 0.25, linewidth = 0.8) +
  geom_smooth(
    method = "loess",
    span = 0.4,
    se = FALSE,
    linewidth = 0.8
  ) +
  coord_cartesian(
    xlim = c(1990, 2026),
    ylim = c(0, 4500)
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 4500, by = 1000)
  ) +
  scale_color_manual(
    values = COMMON_COLORS
  ) +
  labs(
    title = "Average Annual Dwelling Completions by Type",
    subtitle = "Ontario Municipalities 1990-2026",
    x = "Year",
    y = "Average Completions",
    color = "Dwelling Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")
  )


###########################################################
# save plotting 
# Create figures folder if it does not exist
###########################################################



ggsave(
  filename = "./outputs/figures/annual_dwelling_completion_by_type.png",
  plot = annual_dwelling_completion_by_type,
  width = 10,
  height = 6,
  dpi = 300
)

###########################################################
# calculating the share of house types
###########################################################
Table_Share_Compl <- Dwel_Compl_Cl04 %>%
  mutate(
    Total = Ave_Single + Ave_Semi_Detached  + Ave_Row + Ave_Apartment,
    Single = round(Ave_Single / Total * 100, 1),
    Semi_Detached = round(Ave_Semi_Detached  / Total * 100, 1),
    Row = round(Ave_Row / Total * 100, 1),
    Apartment = round(Ave_Apartment / Total * 100, 1)
  ) %>%
  select(
    Year,
    Single,
    Semi_Detached,
    Row,
    Apartment
  )
Table_Share_Compl <- Table_Share_Compl %>%
  filter(Year %in% c("1990", "2025"))


###########################################################
# save report 
# Create figures folder if it does not exist
###########################################################

write.csv(Table_Share_Compl , "./outputs/reports/HousingcompletionsShare_by_dwellingType.csv")

Table_Share_Compl %>%
  filter(Year %in% c("1990", "2025")) %>%
  gt() %>%
  tab_header(
    title = "Housing Completions by Dwelling Type (%)"
  )

#############################################################################################
###############################  Dwel_Start_Trend   #########################################
#############################################################################################

###########################################################
# grouping and summarizing Dwel_Start_03
# converting Dwel_Start_03 into long format for plotting
###########################################################
Dwel_Start_04 <- Dwel_Start_03 %>% group_by(Year) %>% 
  summarise(Ave_Single = mean(Single),
            Ave_Semi_Detached = mean(Semi_Detach),
            Ave_Row = mean(Row),
            Ave_Apartment = mean(Apartment))

Dwel_Long_Start_04 <- Dwel_Start_04 %>%
  pivot_longer(
    cols = starts_with("Ave_"),
    names_to = "Dwelling_Type",
    values_to = "Average_Completions"
  ) %>% 
  mutate(
    Dwelling_Type = sub("^Ave_", "", Dwelling_Type)
  )
###########################################################
# plotting Dwel_Long_Start_04
###########################################################

annual_dwelling_start_by_type <-  ggplot(
  Dwel_Long_Start_04,
  aes(
    x = as.numeric(Year),
    y = Average_Completions,
    color = Dwelling_Type
  )
) +
  geom_line(alpha = 0.25, linewidth = 0.8) +
  geom_smooth(
    method = "loess",
    span = 0.4,
    se = FALSE,
    linewidth = 0.8
  ) +
  coord_cartesian(
    xlim = c(1990, 2026),
    ylim = c(0, 4500)
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2025, by = 5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 4500, by = 1000)
  ) +
  scale_color_manual(
    values = COMMON_COLORS
  ) +
  labs(
    title = "Average Annual Dwelling Starts by Type",
    subtitle = "Ontario Municipalities 1990-2026",
    x = "Year",
    y = "Average Starts",
    color = "Dwelling Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(face = "bold")
  )


ggsave(
  filename = "./outputs/figures/annual_dwelling_start_by_type.png",
  plot = annual_dwelling_completion_by_type,
  width = 10,
  height = 6,
  dpi = 300
)

###########################################################
# calculating the share of house types
###########################################################
Table_Share_Start <- Dwel_Start_04 %>%
  mutate(
    Total = Ave_Single + Ave_Semi_Detached  + Ave_Row + Ave_Apartment,
    Single = round(Ave_Single / Total * 100, 1),
    Semi_Detached = round(Ave_Semi_Detached  / Total * 100, 1),
    Row = round(Ave_Row / Total * 100, 1),
    Apartment = round(Ave_Apartment / Total * 100, 1)
  ) %>%
  select(
    Year,
    Single,
    Semi_Detached,
    Row,
    Apartment
  )
Table_Share_Start <- Table_Share_Start %>%
  filter(Year %in% c("1990", "2025"))

###########################################################
# save report 
###########################################################

write.csv(Table_Share_Start , "./outputs/reports/HousingStartShare_by_dwellingType.csv")

Table_Share_Start %>%
  filter(Year %in% c("1990", "2025")) %>%
  gt() %>%
  tab_header(
    title = "Housing Start by Dwelling Type (%)"
  )
