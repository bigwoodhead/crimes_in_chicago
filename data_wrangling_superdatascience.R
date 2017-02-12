##### Crimes in Chicago - Data wrangling exmaple

#### Libraries

library(readr)
suppressPackageStartupMessages(library(dplyr))
library(stringr) # install.packages("stringr", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
suppressPackageStartupMessages(library(lubridate))
library(ggraptR) # install.packages("ggraptR", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
  # devtools::install_github('cargomoose/raptR', force = TRUE)


#### Get source data

## Set working directory
#! Change to your working directory here !#
setwd("C:/Users/Muhsin Karim/Documents/Data/source_data/kaggle/crimes_in_chicago")

## Load in each file
df1 <- read_csv("Chicago_Crimes_2001_to_2004.csv")

# #! Use a subset of data when writing code !#
# df <- df1
# df <- df[1:1000, ]

df2 <- read_csv("Chicago_Crimes_2005_to_2007.csv")
df3 <- read_csv("Chicago_Crimes_2008_to_2011.csv")
df4 <- read_csv("Chicago_Crimes_2012_to_2017.csv")

## Bind rows
df <- rbind(df1, df2, df3, df4)
rm(df1, df2, df3, df4)


#### Format data

### Remove columns

df <- df %>%
  select(-X1,
         -IUCR,
         -`X Coordinate`,
         -`Y Coordinate`,
         -Location)


### Rename columns

colnames(df)[colnames(df) == "Date"] <- "Datetime"
# colnames(df)[colnames(df) == "Case Number"] <- "Case_Number"
# colnames(df)[colnames(df) == "Primary Type"] <- "Primary_Type"
# colnames(df)[colnames(df) == "Location Description"] <- "Location_Description"
# colnames(df)[colnames(df) == "Community Area"] <- "Community_Area"
# colnames(df)[colnames(df) == "FBI Code"] <- "FBI_Code"
# colnames(df)[colnames(df) == "Updated On"] <- "Updated_On"


## Remove duplicate rows
df <- df[-which(duplicated(df)), ]


### Change data types

## Character
df$ID <- as.character(df$ID)

## Datetime
am_pm <- str_sub(df$Datetime, -2, -1) # Record AM/PM
df$Datetime <- as.POSIXct(df$Datetime, format="%m/%d/%Y %H:%M:%S %p")

## Updated On
df$Updated_On <- as.POSIXct(df$Updated_On, format="%m/%d/%Y %H:%M:%S %p")


#### Create columns

### Create columns from datetime

## Month name
df$Month <- NA
df$Month <- months(df$Datetime)

## Month
df$Month_Value <- NA
df$Month_Value <- month(df$Datetime)

## Year_Month
df$Year_Month <- NA
df$Year_Month <- paste(df$Year, sprintf("%02d", df$Month_Value), sep = "-")

## Hour
df$Hour <- NA
df$Hour <- hour(df$Datetime)

# Apply 24 time if PM
df$Hour[which(am_pm == "PM")] <- df$Hour[which(am_pm == "PM")] + 12
df$Hour[which(df$Hour == 24)] <- 0 # Set midnight to 1 am to 0
df$Hour <- sprintf("%02d", df$Hour)


#### Create unique identifier
# Use ID and Case Number

## Create unique identifier
df$Identifier <- NA
df$Identifier <- paste(df$ID, df$Case_Number, sep = "-")

## Remove duplicate
df <- df[order(df$Updated_On, decreasing = T), ]
df <- df[-which(duplicated(df$Identifier)), ]


#### Crime rate per District

## Get crime counts for each Primary Type 
dfDistrict <- df %>%
  group_by(Year_Month, District, Primary_Type) %>%
  summarise(Num = n())
dfDistrict$ID <- NA
dfDistrict$ID <- paste(dfDistrict$Year_Month, dfDistrict$District, sep = " ")

## Get crime counts for each Month
dfMonthly <- df %>%
  group_by(Year_Month, District) %>%
  summarise(Den = n())
dfMonthly$ID <- NA
dfMonthly$ID <- paste(dfMonthly$Year_Month, dfMonthly$District, sep = " ")
dfMonthly <- dfMonthly[ , c("ID", "Den")]

## Join
dfDistrict <- left_join(dfDistrict, dfMonthly, by = "ID")
dfDistrict$Crime_Rate <- NA
dfDistrict$Crime_Rate <- dfDistrict$Num/dfDistrict$Den


#
#x <- dfDistrict[dfDistrict$Primary_Type == "NARCOTICS", ]
x <- as.data.frame(dfDistrict)
x <- x[ , c("Year_Month", "District", "Primary_Type", "Crime_Rate")]
x <- x[x$Primary_Type == "CRIM SEXUAL ASSAULT", ]
#x <- x[!is.na(x$ID), ]

# District 1
x <- x[x$District == 1, ]
x <- x[ , c("Year_Month", "Primary_Type", "Crime_Rate")]

#x$District <- as.factor(x$District)
x$Primary_Type <- as.factor(x$Primary_Type)


ggraptR()


p <- ggplot(x, aes(Year_Month, Crime_Rate)) + geom_point()
p + facet_wrap(~Primary_Type)
  
  
    


#### Aggregate

### Month

## By Month and Primary_Type

dfMonth_Primary_Type <- df %>%
  group_by(Month, Month_Value, Primary_Type) %>%
  summarise(Count = n())

dfMonth_Primary_Type <- dfMonth_Primary_Type[!is.na(dfMonth_Primary_Type$Month), ]

dfMonth_Primary_Type <- as.data.frame(dfMonth_Primary_Type)

dfMonth_Primary_Type <- dfMonth_Primary_Type[order(dfMonth_Primary_Type$Month_Value), ]


### By Hour and Primary_Type

dfHour_Primary_Type <- df %>%
  group_by(Hour, Primary_Type) %>%
  summarise(Count = n())

#dfHour_Primary_Type$Hour <- as.character(dfHour_Primary_Type$Hour)

dfHour_Primary_Type <- dfHour_Primary_Type[!is.na(dfHour_Primary_Type$Hour), ]

dfHour_Primary_Type <- as.data.frame(dfHour_Primary_Type)



#### Visualise

#df <- as.data.frame(df)
ggraptR()





#### Save data

setwd("C:/Users/Muhsin Karim/Documents/Data/tidy_data/crimes_in_chicago")
write.csv(df, file = "tidy.csv", row.names = F)
save(df, file = "tidy.Rdata")
