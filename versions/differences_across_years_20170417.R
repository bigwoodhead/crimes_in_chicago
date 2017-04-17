##### Crimes in Chicago - Differences across years 
## Compare crime from 2006 and 2016


## Acknowledgement
# Code from Umesh was used: 
# https://www.kaggle.com/umeshnarayanappa/d/currie32/crimes-in-chicago/exploring-chicago-crimes-2012-2016/code
# Thanks for bring highcharter to my attention!
#---------------------------------------------------------------------------------------------------------------------------


#### Libraries

## Load libraries
library(readr)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
suppressPackageStartupMessages(library(lubridate))
library(ggplot2)
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(gridExtra))


#### Get raw data

## Set working directory
setwd("C:/Users/Muhsin Karim/Documents/Data/raw_data/kaggle/crimes_in_chicago")

## Load in each file
df1 <- read_csv("Chicago_Crimes_2001_to_2004.csv")
df2 <- read_csv("Chicago_Crimes_2005_to_2007.csv")
df3 <- read_csv("Chicago_Crimes_2008_to_2011.csv")
df4 <- read_csv("Chicago_Crimes_2012_to_2017.csv")

## Bind rows
df <- rbind(df1, df2, df3, df4)
rm(df1, df2, df3, df4)

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


#### Dates and time

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


#### Create timeseries

# ## By Date
# by_date <- na.omit(df) %>% group_by(Date) %>% summarise(Total = n())
#tseries_date <- xts(by_date$Total, order.by = as.POSIXct(by_date$Date))

## By Year
by_year <- na.omit(df) %>% group_by(Year) %>% summarise(Total = n()) %>% arrange(Year)


#### All crimes

### Percent change across years

## Group and get percent change
by_year$Previous <- lag(by_year$Total, 1, na.pad = TRUE)
by_year$`Percent Change` <- round((by_year$Total - by_year$Previous) / by_year$Total * 100)


## Plot number of crimes per year
hchart(by_year, "column", hcaes(x = Year, y = Total, color = Year)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Number of Crimes by Year") %>%
  hc_credits(enabled = TRUE, text = "Sources: Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)

# The number of crimes peaked in 2003 (470,958 crimes) then decreased since 2005. I would like to 
# investigate the differences in crime across years, namely the percent increase or decrease in 
# crime and the possible reasons that account for changes given the data. Here is the percent change 
# between each year.


## Plot percent change
hchart(by_year, "column", hcaes(x = Year, y = `Percent Change`, color = Year)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Crime Percent Change per Year") %>%
  hc_credits(enabled = TRUE, text = "Sources: Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)

# Since 2006, the crime percent change from previous years has decreased. There are large swings in 
# early years. The number of crimes increased from 2002 to 2003 by 27% (bar at the '2003' x-axis label). It 
# decreased by 22% from 2003 and 2004, then increased by 14% from 2004 to 2005. What accounts for 
# these swings? I suspect it is due to data entry/record keeping as opposed to a dramatic change in crime in
# early 2000s. I discuss this at the end - see "Crime from 2002 to 2004".


# Let's look at specific crime types starting with theft.


##### Crime between 2006 and 2016

### Create data frame with select years

dfTen <- df[df$Year %in% c("2006", "2016"), ] 


#### Theft 2006 and 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "THEFT"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Essentially all Districts experienced a decrease in theft. The table below shows the percent decreased in crime
# per District.

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")


# Not all Community Areas experienced a drop in theft. The table below shows that Avalon Park had the greates 
# decrease in theft (154%). Oakland experiences a 41 % increased in theft. In Oakland's defence, I do not have
# the population numbers in 2006 and 2016. Perhaps there was a significant increase in the residence of 
# Oaklands that accounts for part of the increase in theft. 

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread


### Year by Month

## 2006 
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Theft in 2006")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Theft in 2016")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# The numbers of theft are proportional across the months over both years. The summer months are a time for theft 
# in Chicago.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Theft in 2006")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Theft in 2016")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Again, theft is proportional across both years per weekday and hour. There does appear to be a theft hotspot 
# around weekdays at noon during 2006 (possible because residents are at work, leaving homes vacant). In 2016, 
# the hotspots are on Thursday and Fridays at 6 pm.   


#### Battery 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "BATTERY"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# All Districts experienced a decrease in battery. What is battery? "Battery is a criminal offense 
# involving the unlawful physical acting upon a threat, distinct from assault which is the act of 
# creating apprehension of such contact". OK.   

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Battery per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# It's difficult to see, but the table below also indicates that the numbers for battery decreases from all but two
# Community Areas (Hegewisch and Loop).

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Battery in 2006")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Battery in 2016")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Battery peaked in July in 2006, then June in 2016. Both summer months.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Weekday and Hour")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Both years display a peak in battery during the weekends from midnight to 3 pm. People are being 
# "phyically acted" opon early morning during weekends. This suggests that people are out of a 
# Friday and Saturday night which carried over to early Saturday and Sunday monrings, respectively.


#### Criminal Damage 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "CRIMINAL DAMAGE"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# All Districts have reduced the incidence of criminal damage from 2006 to 2016.

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# All Community Areas reduced the incidence of criminal damage across the years.

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Criminal Damage by Year and Month")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Criminal Damage by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Across both years, the proportion of criminal damage is similar.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Criminal Damage by Weekday and Hour")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Criminal Damage by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# There appears to be a hotspot of criminal damage Friday and Saturday nights in 2016. 


#### Narcotics 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "NARCOTICS"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Narcotics per District") +
  theme_minimal()

# Was there a war on narcotics in Chicago between 2006 and 2016? The number of reported incidents
# in District 1 reduced by a factor of 13 from 2006 to 2016. Likewise for allCommunity Areas.

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Narcotics by Year and Month")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Narcotics by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# In 2006, the incidence of narcotics crimes peaked in May and August. In contrast, the crime peaked in March 
# to May (spring). This differs from other crimes that tend to peak during the summer months.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Narcotics by Weekday and Hour")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Narcotics by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# In 2006, the incidence of narcotics were concenrtrated around Tuesdays to Fridays at noon and 9 and 10 pm.
# In 2016, the hotspot was during weekdays at 7 pm. 


#### Assault 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "ASSAULT"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Assault per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# All Districts experienced a decrease in the number of assaults from 2006 to 2016.

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Assault per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# Community Areas that experienced an increase in the numbers for assault include Edison Park (19%)
# and Archer Heights (10%).

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Assaults by Year and Month")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Assaults by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Looking at both years, assaults peaks in May and summer months.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Assaults by Weekday and Hour")

## 2016
period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Assaults by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# The distributions of assaults are similar across both years.


#### Burglary 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "BURGLARY"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Burglary per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Buglary decreased in all Districts from 2006 to 2016.

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Burglary per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")


# 

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread

# A number of Community Areas experienced an increase in burglaries including Dunning (33%), 
# Forest Glen (22%) and Loop (20%).


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Burglary by Year and Month")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Burglary by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# In 2006, the peak months for burglary were July, August (summer) and Octover (autumn, or as Americans
# call it, fall). In 2016, the peak for burglary stretched across summer and fall.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Burglary by Weekday and Hour")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Burglary by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Across both years, burglary peaks on weekdays at 8 am. A number of homes would be vacant at this hour,
# for the early risers. Perhaps the burglars wait for the occupant to leave their home, then they 
# burgle soon after. Creepy.


#### Deceptive Practice 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "DECEPTIVE PRACTICE"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  ggtitle("Numbers of Deceptive Practice per District") +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# This is a different plot compared to the previous crimes. Roughly hald the Districts, and the Community 
# Areas have experinced an increase in deceptive practice. 

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Deceptive Practice per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Deceptive Practice by Year and Month")

## 2016
period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Deceptive Pratice by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# In 2006, deceptive practice peaked in August whereas it peaked in October during 2016.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Deceptive Practice by Weekday and Hour")

## 2016
period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Deceptive Practice by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Both years experience a peak in deceptive practice during weekdays at 9 am and noon.Why? 
# Deceptive practice is generally the writing of bad cheques. People are possibly learning they were issues a bad 
# first thing in the morning or at lunch time. Are these the times when people have a moment check that their
# cheque was bad, or when the bank notified the vitum? There was also a peak at midnight in 2016. 


#### Motor vehicle theft 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "MOTOR VEHICLE THEFT"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Motor vehicle theft per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# All Distrcts and all but one Community Areas experienced a decrease in motor vehicle theft.

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Motor vehicle theft per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

#

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread

 
### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Motor vehicle theft by Year and Month")

## 2016
period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Motor vehicle theft by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# The hotspots for motor vehicle theft shift across 2006 to 2016. The peak in 2006 was Janurary and December 
# for 2016. The mid-range of crime in 2006 was during summer, whereas it was summer to fall. The lowest 
# incidence of motor vehicle theft in 2006 was the start of summer, yet was in the start of spring.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Motor vehicle theft by Weekday and Hour")

period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

## 2016
gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Numbers of Motor vehicle theft by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Motor vehicle theft has a hotspot across all days around 9pm to midnight. In 2006, there was a greater
# proportion of motor vehicle theft around midnight across all days. 


#### Robbery 2006 to 2016

### Subset by crime type

dfCrime <- dfTen[(dfTen$`Primary Type` == "ROBBERY"), ]


### Districts

## Group by District
by_district <- na.omit(dfCrime) %>% group_by(Year, District) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_district, aes(x = Year, y = Total, group = District, colour = as.factor(District))) +
  geom_line(size = 1.5) +
  guides(colour = guide_legend(title = "District")) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Numbers of Robbery per District") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Only District 1 experienced an increase in crime from 2006 to 2016 (7%).

by_district_spread <- by_district %>% spread(Year, Total)
by_district_spread$`Percent Change` <- round(as.numeric(unlist((by_district_spread[ , 3] - by_district_spread[ , 2]) / by_district_spread[ , 3] * 100)))
by_district_spread <- by_district_spread %>% arrange(`Percent Change`)
by_district_spread


### Community Areas

## Group by Community Area
by_community <- na.omit(dfCrime) %>% group_by(Year, `Community Area`) %>% summarise(Total = n()) %>% arrange(Year)### Year by month

## Plot line graph
ggplot(data = by_community, aes(x = Year, y = Total, group = `Community Area`, colour = as.factor(`Community Area`))) +
  geom_line(size = 1) +
  scale_x_discrete(expand = c(-0.25, 0.25)) +
  ggtitle("Number of Crimes per Community Area") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

# 

by_community_spread <- by_community %>% spread(Year, Total)
by_community_spread$`Percent Change` <- round(as.numeric(unlist((by_community_spread[ , 3] - by_community_spread[ , 2]) / by_community_spread[ , 3] * 100)))
by_community_spread <- by_community_spread %>% arrange(`Percent Change`)
by_community_spread

# A number of Community Areas experienced an increase in robberies from 2006 to 2016 including
# Armour Square (39%), West Elsdon (37%) and Ohare (31%).


### Year by Month

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Year and Month")

## 2016
period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period2 <- ggplot(period_counts2, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Year and Month")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)

# Robberies peaks in Octover in 2006, yet it peaked in August for 2016. The weekday by hour heatmap is 
# similar across both years.


### Weekday by Hour

## 2006
period_counts1 <- na.omit(dfCrime) %>% filter(Year == "2006") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period1 <- ggplot(period_counts1, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Weekday and Hour")

## 2016
period_counts2 <- na.omit(dfCrime) %>% filter(Year == "2016") %>% group_by(Weekday, Hour) %>% summarise(Total = n())

gg_period2 <- ggplot(period_counts2, aes(Weekday, Hour, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Weekday and Hour")

## Plot both
grid.arrange(gg_period1, gg_period2, ncol = 2)


#### Crime from 2002 to 2004

# data entry/ record leeping/ change in practice or definitions.

## Subset 2002, 2003, 2004
dfEarly <- df[(df$Year %in% c("2002", "2003", "2004")), ]

## Year by Month
period_counts <- na.omit(dfEarly) %>% group_by(Year, Month) %>% summarise(Total = n())

gg_period <- ggplot(period_counts, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Number of Crimes by Year and Month")
gg_period

