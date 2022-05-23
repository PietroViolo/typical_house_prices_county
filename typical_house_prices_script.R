#---------------------------------------------------------------------------#
# Nom : typical_house_prices_script.R                         			        #
# Description : Compare median salary and house prices in US counties       #
# Auteur: Pietro Violo                                                      #
# Date : 22 mai 2022                                                        #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

#'* Libraries *

#devtools::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(viridis)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(broom)
library(mapproj)
library(urbnmapr)

#'* Data import and data wrangling *

# Keep only last column

#RegionID
typical_home_value <- read.csv("typical_home_value.csv")

typical_home_value <- typical_home_value[,c(1:9,277)] %>% mutate(MunicipalCodeFIPS = as.character(MunicipalCodeFIPS),
                                                                 StateCodeFIPS = as.character(StateCodeFIPS))

typical_home_value <- typical_home_value %>% mutate(MunicipalCodeFIPS = case_when(nchar(MunicipalCodeFIPS)==1 ~ paste("00",MunicipalCodeFIPS,sep=""),
                                                            nchar(MunicipalCodeFIPS)==2 ~ paste("0",MunicipalCodeFIPS,sep=""),
                                                            nchar(MunicipalCodeFIPS)==3~MunicipalCodeFIPS),
                              StateCodeFIPS = case_when(nchar(StateCodeFIPS)==1 ~ paste("0",StateCodeFIPS, sep = ""),
                                                        nchar(StateCodeFIPS)==2 ~ StateCodeFIPS),
                              county_fips = paste(StateCodeFIPS,MunicipalCodeFIPS, sep = ""))

# Separate county name and state name

county_household_income <- read.csv("county_household_income.csv")

county_household_income <- county_household_income %>% 
  separate(Geographic.Area.Name, c("RegionName", "State"), ", ") %>% 
  mutate(State = state.abb[match(State,state.name)])

#Produce county fips
county_household_income <- county_household_income %>% mutate(county_fips = substr(id,10,14))

# Left join

df_joined <- left_join(county_household_income, typical_home_value, by = c("county_fips"))
df_joined <- df_joined %>% 
  select(Household.median.income, X2022.04.30,county_fips)

df_joined <- df_joined %>% mutate(Household.median.income = as.double(Household.median.income),
                     X2022.04.30 = as.double(X2022.04.30))

# Calculate new measure
df_joined <- df_joined %>% mutate(years_to_buy = X2022.04.30/Household.median.income)

#binned
#df_joined <- df_joined %>% mutate(binned_years_to_buy = case_when(years_to_buy < 2 ~ "Less than two years",
#                                                                  years_to_buy >= 2 & years_to_buy))

#quantile(df_joined$years_to_buy, na.rm = T)


#'* Plot graph *

#urbnmapr::counties

household_data <- left_join(counties, df_joined, by = "county_fips") %>% 
  mutate(years_to_buy = case_when(years_to_buy < 2 ~ "[0, 2[",
                                 years_to_buy>= 2 & years_to_buy < 4 ~ "[2, 4[",
                                  years_to_buy>= 4 & years_to_buy < 6 ~ "[4, 6[",
                                  years_to_buy >= 6 & years_to_buy < 8 ~ "[6, 8[",
                                  years_to_buy >= 8 & years_to_buy < 10 ~ "[8, 10[",
                                  years_to_buy >= 10 & years_to_buy < 16 ~ "[10, 15[",
                                  years_to_buy >= 15 ~ "15 +"))

my_palette <- inferno(7)


png("typical_house_prices.png", res = 300, width = 4400, height = 2600)

ggplot(data = household_data,
       aes(x = long, y = lat, group = group, fill = years_to_buy)) +
  geom_polygon() +
  ggtitle("Number of years of the median household income to equate to the value of a typical home, for US counties in 2022",
          subtitle = "The Zillow Home Value Index (ZHVI) is a smoothed, seasonally adjusted measure of the typical home value and market changes across a given region and housing type. \nIt reflects the typical value for homes in the 35th to 65th percentile range. \n The measure is then divided by the county's median household income to obtain the number of years required to equate the home value.")+
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
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))+
  scale_fill_manual( 
    values=my_palette, 
    name="Number of years of median household income", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1),
    limits = c("[0, 2[","[2, 4[", "[4, 6[",
               "[6, 8[", "[8, 10[", "[10, 15[", "15+"),
    na.value = "grey35"
    

  )

dev.off()
