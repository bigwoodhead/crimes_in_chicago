##### Crimes in Chicago - Data exploration
# Crime rate per District population
# Yearly percent change for most frequent crimes
# Districts where most crime occurs
# Murder rate by comparison

## Acknowledgement
# Some code from Umesh was used: https://www.kaggle.com/umeshnarayanappa/d/currie32/crimes-in-chicago/exploring-chicago-crimes-2012-2016/code
# I discovered highcharter - thank you Umesh!



#### Libraries

## Load libraries
library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
library(ggplot2)
#library(zoo)


#### Get raw data

## Set working directory
#! Change to your working directory here !#
setwd("C:/Users/Muhsin Karim/Documents/Data/raw_data/kaggle/crimes_in_chicago")

## Load in each file
df1 <- read_csv("Chicago_Crimes_2001_to_2004.csv")
df2 <- read_csv("Chicago_Crimes_2005_to_2007.csv")
df3 <- read_csv("Chicago_Crimes_2008_to_2011.csv")
df4 <- read_csv("Chicago_Crimes_2012_to_2017.csv")

## Bind rows
df <- rbind(df1, df2, df3, df4)
#rm(df1, df2, df3, df4)

## Remove incomplete years
df <- df[!(df$Year %in% c("2001", "2017")), ]


### Get Community Area name
# Data from: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6

dfCommunity <- read_csv("CommAreas.csv")
dfCommunity <- dfCommunity %>% 
  select(`Community Area Code` = AREA_NUMBE, 
         `Community Area` = COMMUNITY)

colnames(df)[colnames(df) == "Community Area"] <- "Community Area Code"
  
df <- left_join(df, dfCommunity, by = "Community Area Code")

## Create a table mapping District to Community Area
district_community <- na.omit(df) %>% select(District, `Community Area`) %>% distinct(District, `Community Area`) %>% arrange(District, `Community Area`)


### Dates and time

## Create datetimes
df$Datetime <- as.POSIXct(df$Date, format="%m/%d/%Y %I:%M:%S %p")
df$`Updated On` <- as.POSIXct(df$`Updated On`, format="%m/%d/%Y %I:%M:%S %p")


## Create datetime parts
df$Day <- factor(day(df$Datetime))
df$Month <- factor(month(df$Datetime, label = TRUE))
df$Year <- factor(year(df$Datetime))
df$Weekday <- factor(wday(df$Datetime, label = TRUE))
df$Hour <- factor(hour(df$Datetime))

## Create date
df$Date <- as.Date(df$Datetime)


#### Remove duplicates

## Create unique identifier
df$Identifier <- paste(df$ID, df$`Case Number`, sep = "-")

## Remove duplicates based on ID and Case Number
df <- df[order(df$`Updated On`, decreasing = T), ]
if (sum(duplicated(df$Identifier)) > 0) {df <- df[-which(duplicated(df$Identifier)), ]}


### Create timeseries

## By Date
by_date <- na.omit(df) %>% group_by(Date) %>% summarise(Total = n())
#tseries_date <- xts(by_date$Total, order.by = as.POSIXct(by_date$Date))

## By Year
by_year <- na.omit(df) %>% group_by(Year) %>% summarise(Total = n()) %>% arrange(Year)

# ## Creating timeseries of arrests made
# Arrests_by_Date <- na.omit(chicagocrimes20122016[chicagocrimes20122016$Arrest == 'True',]) %>% group_by(Date) %>% summarise(Total = n())
# arrests_tseries <- xts(Arrests_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

# ## By Location
# by_location <- chicagocrimes20122016 %>% group_by(`Location Description`) %>% summarise(Total = n()) %>% arrange(desc(Total))

# ## By Primary Type
# by_primary_type <- df %>% group_by(`Primary Type`) %>% summarise(Total = n()) %>% arrange(desc(Total))
# 
# ## By District
# by_district <- df %>% group_by(District) %>% summarise(Total = n()) %>% arrange(desc(Total))

# ## By Ward
# by_ward <- df %>% group_by(Ward) %>% summarise(Total = n()) %>% arrange(desc(Total))
# 
# ## By FBI Code
# by_fbi_code <- df %>% group_by(`FBI Code`) %>% summarise(Total = n()) %>% arrange(desc(Total))
# 
# ## By Arrest
# by_arrest <- df %>% group_by(Arrest) %>% summarise(Total = n()) %>% arrange(desc(Total))
# 
# ## By Domestic
# by_domestic <- df %>% group_by(Domestic) %>% summarise(Total = n()) %>% arrange(desc(Total))



#### Percent change by year

## Group by Year and get Percent Change
by_year$Previous <- lag(by_year$Total, 1, na.pad = TRUE)
by_year$`Percent Change` <- round((by_year$Total - by_year$Previous) / by_year$Total * 100)


## Plot number of crimes per year
hchart(by_year, "column", hcaes(x = Year, y = Total, color = Year)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Number of Crimes by Year") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)
  # Try line plot

# The number of crimes peaked in 2003 and has decreased since 2005. When reporting the change in crime over time, the percent
# increased or decrease from a previous period is used. The next plot displays the percentages change in the number of crime 
# between each year. 


## Plot percent change
hchart(by_year, "column", hcaes(x = Year, y = `Percent Change`, color = Year)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Crime Percent Change per Year") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)
  # Need to display the value on bar

# Since 2006, the crime percent change from previous years has decreased, which is good. There are large swings in early years.
# The percent change increased from 2002 to 2003 (bar at the '2003' label). It decreased between 2004 and 2003, then increased 
# from 2004 to 2005. What accounts for these swings? Let's exmaine this per crime type ("Primary Type").   


#### Theft

### Percent Change

by_year_theft <- na.omit(df) %>% filter(`Primary Type` == "THEFT") %>% group_by(Year) %>% summarise(Total = n()) %>% arrange(Year)
by_year_theft$Previous <- lag(by_year_theft$Total, 1, na.pad = TRUE)
by_year_theft$`Percent Change` <- round((by_year_theft$Total - by_year_theft$Previous) / by_year_theft$Total * 100)

hchart(by_year_theft, "column", hcaes(x = Year, y = `Percent Change`, color = Year)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Theft: Crime Percent Change per Year") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


### Get Percentage change between two years

## Theft - 2002 and 2003 - explain the increase in theft
primary_type <- "THEFT"
period1 = "2002"
period2 = "2003"
diff_theft <- na.omit(df) %>% filter(`Primary Type` == primary_type) %>% filter(Year %in% c(period1, period2)) %>% group_by(Year, District) %>% summarise(Total = n())
diff_theft <- diff_theft %>% spread(Year, Total)
diff_theft$`Percent Change` <- round(as.numeric(unlist((diff_theft[ , 3] - diff_theft[ , 2]) / diff_theft[ , 3] * 100)))
diff_theft <- diff_theft %>% arrange(desc(`Percent Change`))

# Disticts 15 (36%), 10 (35%), 12 and 25 (33%) had the greatest crime percent change from 2002 to 2003. Most of these Districts
# (15, 10, 12) are part of the Far Northwest Side of Chicago. 
# 15: AUSTIN, ENGLEWOOD, NEAR WEST SIDE
# 10: AVONDALE, BRIGHTON PARK, EAST GARFIELD PARK, GARFIELD RIDGE, HUMBOLDT PARK...
# 12

head(diff_theft)


### Treemap using raw counts!



# Let's get an idea of how these two two years differ.

# Year by Month
period_counts <- na.omit(df) %>% filter(Year %in% c(period1, period2)) %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period <- ggplot(period_counts, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Year and Month")
print(gg_period)


# Weekday by Hour
period_counts <- na.omit(df) %>% filter(Year %in% c(period1, period2)) %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period <- ggplot(period_counts, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Weekday and Hour")
print(gg_period)



# Weekday by Hour
period1_counts <- na.omit(df) %>% filter(Year == period1) %>% group_by(Year, Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period1_counts, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Period1 - Number of Crimes by Weekday and Hour")

period2_counts <- na.omit(df) %>% filter(Year == period2) %>% group_by(Year, Weekday, Hour) %>% summarise(Total = n())

gg_period2 <- ggplot(period2_counts, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Period2 - Number of Crimes by Weekday and Hour")

grid.arrange(gg_period1, gg_period2, ncol = 2)



# Weekday by Hour - Jan to Apr
period1_counts <- na.omit(df) %>% filter(Year == period1) %>% filter(Month %in% c("Jan", "Feb", "Mar", "Apr")) %>% group_by(Year, Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period1_counts, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Period1 - Number of Crimes by Weekday and Hour")

period2_counts <- na.omit(df) %>% filter(Year == period2) %>% filter(Month %in% c("Jan", "Feb", "Mar", "Apr")) %>% group_by(Year, Weekday, Hour) %>% summarise(Total = n())

gg_period2 <- ggplot(period2_counts, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Period2 - Number of Crimes by Weekday and Hour")

grid.arrange(gg_period1, gg_period2, ncol = 2)









## Theft - 2003 and 2004 - explain the decrease in theft
# primary_type <- "THEFT"
# period1 = "2003"
# period2 = "2004"
# diff_theft <- na.omit(df) %>% filter(`Primary Type` == primary_type) %>% filter(Year %in% c(period1, period2)) %>% group_by(Year, District) %>% summarise(Total = n())
# diff_theft <- diff_theft %>% spread(Year, Total)
# diff_theft$`Percent Change` <- round(as.numeric(unlist((diff_theft[ , 3] - diff_theft[ , 2]) / diff_theft[ , 3] * 100)))
# diff_theft <- diff_theft %>% arrange(`Percent Change`)

# Repeat...



## Battery
# As above


## Criminal damage
# As above


## Narcotics
# As above


## Assult
# As above






# Subset years 

