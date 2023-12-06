install.packages("sf")

library(sf)

kansas_counties <- st_read("C:\\Users\\rmukherjee\\Downloads\\tl_rd22_20_cousub\\tl_rd22_20_cousub.shp")

head(kansas_counties)


kansas_county_area <- read.csv("C:\\Users\\rmukherjee\\Desktop\\HDSC 824 Week 11 assignnment_flex_files\\KansasCounties.csv")

kansas_crime_long <- read.csv("C:\\Users\\rmukherjee\\Desktop\\HDSC 824 Week 11 assignnment_flex_files\\KansasCrimeLong.csv")

kansas_pop <- read.csv("C:\\Users\\rmukherjee\\Desktop\\HDSC 824 Week 11 assignnment_flex_files\\KansasPopulation.csv")


kansas_counties$NAME <- gsub("\\s", "", kansas_counties$NAME)
kansas_county_area$County <- gsub("\\s", "", kansas_county_area$County)
kansas_crime_long$County <- gsub("\\s", "", kansas_crime_long$County)
kansas_pop$County <- gsub("\\s","",kansas_pop$County)

kansas_county_area$County <- paste0(toupper(substr(kansas_county_area$County, 1, 1)), substr(kansas_county_area$County, 2, nchar(kansas_county_area$County)))


library(dplyr)

kansas_counties <- kansas_counties %>% rename(County=NAME)


merge1 <- left_join(kansas_county_area,kansas_counties, by= "County",multiple= "all")

merge2 <- left_join(kansas_counties, kansas_county_area, by= "County",multiple= "all")

library(ggplot2)
ggplot(data = merge2) +
  geom_sf(aes(fill = Area)) + # Fill counties based on area # Add area text
  theme_minimal() +
  labs(fill = "Area")



library(tidyr)

df_crime <- kansas_crime_long %>% pivot_longer(
            cols = -County,
            names_to = "Year",
            values_to ="Crime_Rate"
)


df_crime$Year <- substring(df_crime$Year, 2)


df_pop1 <- kansas_pop %>% select(County,Year, Population)

df_pop <- df_pop1 %>% filter(Year %in% c(2012,2013,2014,2015,2016,2017))

