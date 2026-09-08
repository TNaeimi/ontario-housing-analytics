
###########################################################
# Install Packages and Load libraries 
###########################################################
# Install packages (only needs to be run once)

install.packages(c(
  "sf",
  "dplyr",
  "ggplot2",
  "ggspatial",
  "prettymapr"
))


library(sf)
library(dplyr)
library(ggplot2)
library(sf)
library(ggplot2)
library(ggspatial)
library(prettymapr)


### Setting up the data folder 
dataFolder = "./Data/Raw/"


municipality_sf <- st_read(
  paste0(dataFolder, "Shapefiles/Municipalities.shp")
)

names(municipality_sf)
plot(municipality_sf["MUNICIPA_8"])

Municipal_ForMapping <-  read.csv(paste0(dataFolder ,  "Municipal_ForMapping.csv"))

names(Municipal_ForMapping)

Municipal_ForMapping_01 <- Municipal_ForMapping %>%
  mutate(
    MatchedName = recode(
      Name,
      "Barrie" = "BARRIE",
      "Belleville - Quinte West" = "BELLEVILLE",
      "Bracebridge T" = "BRACEBRIDGE",
      "Brantford" = "BRANTFORD",
      "Brighton MU" = "BRIGHTON",
      "Brock TP" = "BROCK",
      "Brockville" = "BROCKVILLE",
      "Centre Wellington" = "CENTRE WELLINGTON",
      "Chatham-Kent" = "CHATHAM-KENT",
      "Cobourg" = "COBOURG",
      "Collingwood" = "COLLINGWOOD",
      "Cornwall" = "CORNWALL",
      "Elliot Lake" = "ELLIOT LAKE",
      "Erin T" = "ERIN",
      "Gravenhurst T" = "GRAVENHURST",
      "Greater Napanee T" = "GREATER NAPANEE",
      "Greater Sudbury / Grand Sudbury" = "GREATER SUDBURY",
      "Guelph" = "GUELPH",
      "Haldimand County CY" = "HALDIMAND COUNTY",
      "Hamilton" = "HAMILTON",
      "Hawkesbury" = "HAWKESBURY",
      "Huntsville T" = "HUNTSVILLE",
      "Ingersoll" = "INGERSOLL",
      "Kawartha Lakes" = "KAWARTHA LAKES",
      "Kenora" = "KENORA",
      "Kincardine MU" = "KINCARDINE",
      "Kingston" = "KINGSTON",
      "Kitchener - Cambridge - Waterloo" = "KITCHENER",
      "Lambton Shores MU" = "LAMBTON SHORES",
      "London" = "LONDON",
      "Meaford MU" = "MEAFORD",
      "Midland" = "MIDLAND",
      "Norfolk" = "NORFOLK COUNTY",
      "North Bay" = "NORTH BAY",
      "North Perth MU" = "NORTH PERTH",
      "Orillia" = "ORILLIA",
      "Oshawa" = "OSHAWA",
      "Ottawa" = "OTTAWA",
      "Owen Sound" = "OWEN SOUND",
      "Pembroke" = "PEMBROKE",
      "Petawawa" = "PETAWAWA",
      "Peterborough" = "PETERBOROUGH",
      "Port Hope" = "PORT HOPE",
      "Prince Edward County CY" = "PRINCE EDWARD",
      "Sarnia" = "SARNIA",
      "Saugeen Shores T" = "SAUGEEN SHORES",
      "Sault Ste. Marie" = "SAULT STE. MARIE",
      "Scugog TP" = "SCUGOG",
      "South Dundas MU" = "SOUTH DUNDAS",
      "South Huron MU" = "SOUTH HURON",
      "St. Catharines - Niagara" = "ST. CATHARINES",
      "Stratford" = "STRATFORD",
      "The Nation / La Nation M" = "THE NATION",
      "Thunder Bay" = "THUNDER BAY",
      "Tillsonburg" = "TILLSONBURG",
      "Timmins" = "TIMMINS",
      "Toronto" = "TORONTO",
      "Trent Hills MU" = "TRENT HILLS",
      "Wasaga Beach" = "WASAGA BEACH",
      "West Grey MU" = "WEST GREY",
      "West Nipissing / Nipissing Ouest M" = "WEST NIPISSING",
      "Windsor" = "WINDSOR",
      "Woodstock" = "WOODSTOCK",
      "cambrige" = "CAMBRIDGE",
      "waterloo" = "WATERLOO"
    )
  )

Municipal_ForMapping_01 <- Municipal_ForMapping_01 %>% 
  select(Municipality = MatchedName,
         Affordability_Ratio)

municipality_sf <- municipality_sf %>% 
  mutate(Municipality = municipality_sf$MUNICIPA_8)

municipality_map <- municipality_sf %>%
  left_join(
    Municipal_ForMapping_01,
    by = "Municipality"
  )


HousingAffordabilityMap <- ggplot() +
  
  annotation_map_tile(
    type = "osm",
    zoomin = 0
  ) +

  geom_sf(
    data = municipality_map,
    aes(fill = Affordability_Ratio),
    color = "grey70",
    linewidth = 0.15,
    alpha = 0.85
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#D6EAF8",
      "#AED6F1",
      "#F7DC6F",
      "#F5B041",
      "#CB4335"
    ),
    name = "Affordability\nRatio"
  ) +
  
  labs(
    title = "Housing Affordability Across Ontario Municipalities",
    subtitle = "Higher affordability ratios indicate greater housing affordability challenges",
    caption = "Map Source: OSM basemap"
  ) +
  
  coord_sf() +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 12,
      face = "bold"
    ),
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    legend.text = element_text(
      size = 10
    ),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

HousingAffordabilityMap
ggsave(
  filename = "./outputs/figures/HousingAffordabilityMap_Ontario.png",
  plot = HousingAffordabilityMap,
  width = 12,
  height = 10,
  dpi = 300
)
##################### creating 4 classes for affordability ratio ############

municipality_map <- municipality_map %>%
  mutate(
    Affordability_Class = cut(
      Affordability_Ratio,
      breaks = c(
        0.19,
        0.23,
        0.28,
        0.33,
        0.38
      ),
      labels = c(
        "Low",
        "Moderate",
        "High",
        "Very High"
      ),
      include.lowest = TRUE
    )
  )


municipality_map_Class <-  ggplot(municipality_map) +
  geom_sf(
    aes(fill = Affordability_Class),
    color = "white",
    linewidth = 0.1
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    direction = 1
  ) +
  labs(
    title = "Housing Affordability Categories Across Ontario"
  ) +
  theme_minimal()

municipality_map_Class


ggsave(
  filename = "./outputs/figures/municipality_map_Class.png",
  plot = municipality_map_Class,
  width = 12,
  height = 10,
  dpi = 300
)


#################### plotting affordability classes with OSM base map
Affordability_Class_Map <- ggplot() +
  annotation_map_tile(
    type = "osm",
    zoomin = 0
  ) + 
  geom_sf(
    data = municipality_map,
    aes(fill = Affordability_Class),
    color = "white",
    linewidth = 0.1,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#D6EAF8",
      "Moderate" = "#F7DC6F",
      "High" = "#F5B041",
      "Very High" = "#CB4335"
    )
  ) +
  coord_sf() +
  labs(
    title = "Housing Affordability Classes",
    subtitle = "Ontario Municipalities",
    caption = "Map Source: OSM basemap"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 12,
      face = "bold"
    ),
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    legend.text = element_text(
      size = 10
    ),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

Affordability_Class_Map
ggsave(
  filename = "./outputs/figures/Affordability_Class_Map.png",
  plot = Affordability_Class_Map,
  width = 12,
  height = 10,
  dpi = 300
)

