##### Crimes in Chicago forecast
# Using script from Umesh


library(dplyr)
library(prophet) # Tool for producing high quality forecasts for time series data that has multiple seasonality with linear 
                  # or non-linear growth.
library(xts)
library(highcharter)
library(readr)


setwd("C:/Users/Muhsin Karim/Documents/Data/raw_data/kaggle/crimes_in_chicago")


chicagocrimes20012004 <- read_csv("Chicago_Crimes_2001_to_2004.csv")
chicagocrimes20052007 <- read_csv("Chicago_Crimes_2005_to_2007.csv")
chicagocrimes20082011 <- read_csv("Chicago_Crimes_2008_to_2011.csv")
chicagocrimes20122017 <- read_csv("Chicago_Crimes_2012_to_2017.csv")

chicagocrimes20012004 <- chicagocrimes20012004[, c('Date', 'ID')]
chicagocrimes20052007 <- chicagocrimes20052007[, c('Date', 'ID')]
chicagocrimes20082011 <- chicagocrimes20082011[, c('Date', 'ID')]
chicagocrimes20122015 <- chicagocrimes20122017[chicagocrimes20122017$Year %in% c('2012', '2013', '2014', '2015'), c('Date', 'ID')]

chicagocrimes2017 <- chicagocrimes20122017[chicagocrimes20122017$Year =='2017', c('Date', 'ID')]

#chicagocrimes <- rbind(chicagocrimes20012004, chicagocrimes20052007, chicagocrimes20082011, chicagocrimes20122016)

## Creating timeseries
chicagocrimes20122015$Date <- as.Date(chicagocrimes20122015$Date, "%m/%d/%Y %I:%M:%S %p")
by_Date <- na.omit(chicagocrimes20122015) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date)) # Time appeared

df <- chicagocrimes20122015 %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))
  # Logs: https://www.forbes.com/sites/naomirobbins/2012/01/19/when-should-i-use-logarithmic-scales-in-my-charts-and-graphs/#456f894f5e67

names(df) <- c("ds", "y")
df$ds <- factor(df$ds)


### Times Series plot of Chicago Crimes 2012-2016

hchart(tseries, name = "Crimes") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Times Series plot of Chicago Crimes") %>%
  hc_legend(enabled = TRUE)


#### Forecasting

### Fitting the model

m <- prophet(df)


### Creating dataframe with historical dates and future dates to forecast

future <- make_future_dataframe(m, periods = 365 * 4)
head(future)
tail(future)


### Forecasting by using predict method

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


### Basic plotting of the forecast

plot(m, forecast)  


### Plotting forecast broken down into trend, weekly and yearly

prophet_plot_components(m, forecast)
