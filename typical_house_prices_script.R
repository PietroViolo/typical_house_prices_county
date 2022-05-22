#---------------------------------------------------------------------------#
# Nom : typical_house_prices_script.R                         			        #
# Description : Compare median salary and house prices in US counties       #
# Auteur: Pietro Violo                                                      #
# Date : 22 mai 2022                                                        #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

#'* Libraries *

library(tidyverse)
library(viridis)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(broom)
library(mapproj)

#'* Data import and data wrangling *

# Keep only last column

typical_home_value <- read.csv("typical_home_value.csv")

typical_home_value <- typical_home_value[,c(1:8,277)]

# Separate county name and state name

county_household_income <- read.csv("county_household_income.csv")

county_household_income <- county_household_income %>% 
  separate(Geographic.Area.Name, c("RegionName", "State"), ", ") %>% 
  mutate(State = state.abb[match(State,state.name)])

# Left join

df_joined <- left_join(county_household_income, typical_home_value, by = c("RegionName", "State"))
df_joined <- df_joined %>% 
  select(RegionName, State, Household.median.income, X2022.04.30)

df_joined <- df_joined %>% mutate(Household.median.income = as.double(Household.median.income),
                     X2022.04.30 = as.double(X2022.04.30))

# Calculate new measure
df_joined <- df_joined %>% mutate(years_to_buy = X2022.04.30/Household.median.income)

# Remove "County"

df_joined <-df_joined %>% mutate(region = tolower(state.name[match(State,state.abb)]),
                                 subregion = tolower(str_replace(RegionName, " County","")))

#binned
#df_joined <- df_joined %>% mutate(binned_years_to_buy = case_when(years_to_buy < 2 ~ "Less than two years",
#                                                                  years_to_buy >= 2 & years_to_buy))

#quantile(df_joined$years_to_buy, na.rm = T)


#'* Plot graph *

us_counties <- map_data("county")

us_counties <- left_join(us_counties, df_joined, by = c("region", "subregion"))

ggplot(data = us_counties,
       aes(x = long, y = lat, group = group, fill = years_to_buy)) +
  geom_polygon() +
  ggtitle("How many years does it takes to buy a house with the median salary in 2022?",
          subtitle = "In each county, given the median salary and the typical house price, \nhow many years does it take to pay off a house if 100 % of the salary goes torwards the house?")+
  coord_map(projection = "albers", lat = 45, lat1 = 55) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 20, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
  scale_fill_continuous(type = "viridis",
                        name = "Years to buy a house") 



#
