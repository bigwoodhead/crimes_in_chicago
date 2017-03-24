##### Crimes in Chicago data exploration
# Using script from Umesh


library(highcharter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(lubridate)
library(xts)
library(maps)
library(ggmap)
library(gridExtra)
library(readr)

setwd("C:/Users/Muhsin Karim/Documents/Data/raw_data/kaggle/crimes_in_chicago")

chicagocrimes20122016 <- read_csv("Chicago_Crimes_2012_to_2017.csv")

chicagocrimes20122016 <- chicagocrimes20122016[chicagocrimes20122016$Year!='2017',]

### Working with Date and Time

chicagocrimes20122016$Day <- factor(day(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p")))
chicagocrimes20122016$Month <- factor(month(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
chicagocrimes20122016$Year <- factor(year(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p")))
chicagocrimes20122016$Weekday <- factor(wday(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))

chicagocrimes20122016$Date <- as.Date(chicagocrimes20122016$Date, "%m/%d/%Y %I:%M:%S %p")

## Creating timeseries
by_Date <- na.omit(chicagocrimes20122016) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))

## Creating timeseries of arrests made
Arrests_by_Date <- na.omit(chicagocrimes20122016[chicagocrimes20122016$Arrest == 'True',]) %>% group_by(Date) %>% summarise(Total = n())
arrests_tseries <- xts(Arrests_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

## BY Location
by_location <- chicagocrimes20122016 %>% group_by(`Location Description`) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Primary Type
by_type <- chicagocrimes20122016 %>% group_by(`Primary Type`) %>% summarise(Total = n()) %>% arrange(desc(Total))


## By District
by_district <- chicagocrimes20122016 %>% group_by(District) %>% summarise(Total = n()) %>% arrange(desc(Total))


## By Primary Type
by_ward <- chicagocrimes20122016 %>% group_by(Ward) %>% summarise(Total = n()) %>% arrange(desc(Total))


## By FBI Code
by_fbi <- chicagocrimes20122016 %>% group_by(`FBI Code`) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Arrest
by_arrest <- chicagocrimes20122016 %>% group_by(Arrest) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Domestic
by_domestic <- chicagocrimes20122016 %>% group_by(Domestic) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Year
by_year <- chicagocrimes20122016 %>% group_by(Year) %>% summarise(Total = n()) %>% arrange(desc(Total))


## Lat and Long of Crimes
LatLonCounts <- as.data.frame(table(round(chicagocrimes20122016$Longitude,2), round(chicagocrimes20122016$Latitude,2)))
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))
LatLonCounts2 <- subset(LatLonCounts, Freq > 0)

## Lat and Long of Arrests
arrests_data <- na.omit(chicagocrimes20122016[chicagocrimes20122016$Arrest == 'True',])
LatLonArrestsCounts <- as.data.frame(table(round(arrests_data$Longitude,2), round(arrests_data$Latitude,2)))
LatLonArrestsCounts$Long <- as.numeric(as.character(LatLonArrestsCounts$Var1))
LatLonArrestsCounts$Lat <- as.numeric(as.character(LatLonArrestsCounts$Var2))
LatLonArrestsCounts2 <- subset(LatLonArrestsCounts, Freq > 0)


hchart(tseries, name = "Crimes") %>% 
  hc_add_series(arrests_tseries, name = "Arrests") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Times Series plot of Chicago Crimes and Arrests") %>%
  hc_legend(enabled = TRUE)


hchart(arrests_tseries) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Times Series plot of Arrests made in Chicago (2012-2016)") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px"))


arrests_count <- arrests_data %>% group_by(Year, Month) %>% summarise(Total = n())

arrests <- ggplot(arrests_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Arrests by Year and Month(2012-2016)")

crime_count <- chicagocrimes20122016 %>% group_by(Year, Month) %>% summarise(Total = n())

crimes <- ggplot(crime_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Crimes by Year and Month(2012-2016)")

grid.arrange(crimes, arrests, ncol = 2)


hchart(by_year, "column", hcaes(x = Year, y = Total, color = Year)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Number of Crimes by Year") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)
  

hchart(by_location[1:20,], "column", hcaes(x = `Location Description`, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Locations with most Crimes - Top 20") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


streets <- chicagocrimes20122016[chicagocrimes20122016$`Location Description`=="STREET",]
## Creating timeseries
streets_by_Date <- na.omit(streets) %>% group_by(Date) %>% summarise(Total = n())
streets_tseries <- xts(streets_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

residence <- chicagocrimes20122016[chicagocrimes20122016$`Location Description`=="RESIDENCE",]
## Creating timeseries
residence_by_Date <- na.omit(residence) %>% group_by(Date) %>% summarise(Total = n())
residence_tseries <- xts(residence_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

apartment <- chicagocrimes20122016[chicagocrimes20122016$`Location Description`=="APARTMENT",]
## Creating timeseries
apartment_by_Date <- na.omit(apartment) %>% group_by(Date) %>% summarise(Total = n())
apartment_tseries <- xts(apartment_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

sidewalk <- chicagocrimes20122016[chicagocrimes20122016$`Location Description`=="SIDEWALK",] 
## Creating timeseries
sidewalk_by_Date <- na.omit(sidewalk) %>% group_by(Date) %>% summarise(Total = n())
sidewalk_tseries <- xts(sidewalk_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

hchart(streets_tseries, name = "Streets") %>% 
  hc_add_series(residence_tseries, name = "Residence") %>% 
  hc_add_series(apartment_tseries, name = "Apartment") %>%
  hc_add_series(sidewalk_tseries, name = "Sidewalk") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Crimes in Streets/Residence/Apartment/Sidewalk") %>%
  hc_legend(enabled = TRUE)


hchart(by_type, "column", hcaes(`Primary Type`, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Crime Types") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


#### Let us see how crime numbers vary in top crimes

thefts <- chicagocrimes20122016[chicagocrimes20122016$`Primary Type`=="THEFT",] 
## Creating timeseries
thefts_by_Date <- na.omit(thefts) %>% group_by(Date) %>% summarise(Total = n())
thefts_tseries <- xts(thefts_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

battery <- chicagocrimes20122016[chicagocrimes20122016$`Primary Type`=="BATTERY",] 
## Creating timeseries
battery_by_Date <- na.omit(battery) %>% group_by(Date) %>% summarise(Total = n())
battery_tseries <- xts(battery_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

criminals <- chicagocrimes20122016[chicagocrimes20122016$`Primary Type`=="CRIMINAL DAMAGE",]
## Creating timeseries
criminals_by_Date <- na.omit(criminals) %>% group_by(Date) %>% summarise(Total = n())
criminals_tseries <- xts(criminals_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

narcotics <- chicagocrimes20122016[chicagocrimes20122016$`Primary Type`=="NARCOTICS",] 
## Creating timeseries
narcotics_by_Date <- na.omit(narcotics) %>% group_by(Date) %>% summarise(Total = n())
narcotics_tseries <- xts(narcotics_by_Date$Total, order.by=as.POSIXct(by_Date$Date))


hchart(thefts_tseries, name = "Thefts") %>% 
  hc_add_series(battery_tseries, name = "Battery") %>% 
  hc_add_series(criminals_tseries, name = "Criminal Damage") %>%
  hc_add_series(narcotics_tseries, name = "Narcotics") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Crimes in Thefts/Battery/Criminal Damage/Narcotics") %>%
  hc_legend(enabled = TRUE)


### How big is the increase in homicides in Chicago?

homicide <- chicagocrimes20122016[chicagocrimes20122016$`Primary Type`=="HOMICIDE",] 

homicide_year <-  homicide %>% group_by(Year) %>% summarise(Total = n())

hchart(homicide_year, "column", hcaes(Year, Total, color = Year)) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Homicide 2012-2016")  %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px"))


homicide_count <- homicide %>% group_by(Year, Month) %>% summarise(Total = n())

ggplot(homicide_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Homicides in Chicago (2012-2016)")
