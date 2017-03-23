##### Crimes in Chicago - Data wrangling exmaple for SuperDataScience

#### Libraries

## Load libraries
library(readr)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
library(ggplot2)


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
rm(df1, df2, df3, df4)


#### Format data

## Remove columns
df <- df %>%
  select(-X1,
         -IUCR,
         -`X Coordinate`,
         -`Y Coordinate`,
         -Location)

## Rename column
colnames(df)[colnames(df) == "Date"] <- "Datetime"


### Change data types

df$ID <- as.character(df$ID)


### Create datetimes

df$Datetime <- as.POSIXct(df$Datetime, format="%m/%d/%Y %I:%M:%S %p")
df$`Updated On` <- as.POSIXct(df$`Updated On`, format="%m/%d/%Y %I:%M:%S %p")


#### Create columns

### Create columns from datetime

## Month name
df$Month <- NA
df$Month <- months(df$Datetime)

## Month
df$`Month Value` <- NA
df$`Month Value` <- month(df$Datetime)

## Year Month
df$`Year Month` <- NA
df$`Year Month` <- paste(df$Year, sprintf("%02d", df$`Month Value`), sep = "-")

## Hour
df$Hour <- NA
df$Hour <- hour(df$Datetime)


#### Remove duplicates

## Create unique identifier
df$Identifier <- NA
df$Identifier <- paste(df$ID, df$`Case Number`, sep = "-")

## Remove duplicates
df <- df[order(df$`Updated On`, decreasing = T), ]
df <- df[-which(duplicated(df$Identifier)), ]


#### Crime rate per month per District

## Get crime counts for each Primary Type 
dfDistrict <- df %>%
  filter(!is.na(Datetime)) %>%
  filter(!is.na(District)) %>%
  filter(!is.na(`Primary Type`)) %>%
  group_by(`Year Month`, District, `Primary Type`) %>%
  summarise(Numerator = n())
dfDistrict$ID <- NA
dfDistrict$ID <- paste(dfDistrict$`Year Month`, dfDistrict$District, sep = " ")

## Get crime counts for each month
dfMonthly <- df %>%
  filter(!is.na(Datetime)) %>% # to match Python
  filter(!is.na(District)) %>%
  group_by(`Year Month`, District) %>%
  summarise(Denominator = n())
dfMonthly$ID <- NA
dfMonthly$ID <- paste(dfMonthly$`Year Month`, dfMonthly$District, sep = " ")
dfMonthly <- dfMonthly[ , c("ID", "Denominator")]

## Join to get numerator and denominator together
dfJoin <- left_join(dfDistrict, dfMonthly, by = "ID")
dfJoin$`Crime Rate` <- NA
dfJoin$`Crime Rate` <- dfJoin$Numerator/dfJoin$Denominator * 100


#### Visualisation

## Prepare data frame for Theft from 2016
dfPrimaryType <- dfJoin[dfJoin$`Primary Type` == "THEFT", ]
dfPrimaryType <- dfPrimaryType[dfPrimaryType$`Year Month` > "2015-12", ]
dfPrimaryType$District <- as.factor(dfPrimaryType$District)


### Plots

## Crime frequency - theft
p <- ggplot(dfPrimaryType, aes(`Year Month`, Numerator, group=District, color=District)) + geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=0.5))
print(p)

## Crime rate - theft
q <- ggplot(dfPrimaryType, aes(`Year Month`, `Crime Rate`, group=District, color=District)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=0.5))
print(q)


#### Save tidy data

setwd("C:/Users/Muhsin Karim/Documents/Data/tidy_data/crimes_in_chicago")
write.csv(df, file = "tidy_R.csv", row.names = F)
save(df, file = "tidy.Rdata")
save(dfJoin, file = "join.Rdata")
