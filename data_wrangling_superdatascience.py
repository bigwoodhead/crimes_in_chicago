# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

##### Crimes in Chicago - Data wrangling exmaple

#### Libraries

import pandas as pd
import numpy as np
import os


#### Get source data

## Set working directory
#! Change to your working directory here !#
os.chdir('C:\\Users/Muhsin Karim\\Documents\\Data\\source_data\\kaggle\\crimes_in_chicago') # Section 5 Unit 1 Importing data into Python
os.getcwd()

## Load in each file
column_names = ['X1', 'ID', 'Case Number' ,'Date', 'Block', 
'IUCR', 'Primary Type', 'Description', 'Location Description', 
'Arrest', 'Domestic', 'Beat', 'District', 'Ward', 
'Community Area', 'FBI Code', 'X Coordinate', 'Y Coordinate', 'Year', 
'Updated On', 'Latitude', 'Longitude', 'Location'] 

df1 = pd.read_csv('Chicago_Crimes_2001_to_2004.csv', encoding='utf-8', names=column_names, skiprows=1)
#df = df1
df2 = pd.read_csv('Chicago_Crimes_2005_to_2007.csv', encoding='utf-8', names=column_names, skiprows=1)
df3 = pd.read_csv('Chicago_Crimes_2008_to_2011.csv', encoding='utf-8', names=column_names, skiprows=1)
df4 = pd.read_csv('Chicago_Crimes_2012_to_2017.csv', encoding='utf-8', names=column_names, skiprows=1)

## Bind rows
df = pd.concat([df1, df2, df3, df4])
del df1, df2, df3, df4


#### Format data

### Remove columns
df = df.drop(['X1', 'IUCR', 'X Coordinate', 'Y Coordinate', 'Location'], axis=1)


### Rename columns
df = df.rename(columns={"Date": "Datetime"})


## Change types
df['ID2'] = df['ID']
df['ID2'].astype('str')



pd.to_numeric(df['ID2'], errors='coerce')
str(round(df['ID'])).dt.rstrip('0').rstrip('.')

df['ID'] = df['ID'].astype(str)


df['Case Number'] = df['Case Number'].astype(str)
df['Datetime'] = df['Datetime'].astype(str)
df['Block'] = df['Block'].astype(str)
df['Primary Type'] = df['Primary Type'].astype(str)
df['Description'] = df['Description'].astype(str)
df['Location Description'] = df['Location Description'].astype(str)
df['Arrest'] = df['Arrest'].astype(str)
df['Domestic'] = df['Domestic'].astype(str)
df['Beat'] = pd.to_numeric(df['Beat'], errors='coerce')
df['District'] = pd.to_numeric(df['District'], errors='coerce')
df['Ward'] = pd.to_numeric(df['Ward'], errors='coerce')
df['Community Area'] = pd.to_numeric(df['Community Area'], errors='coerce')
df['FBI Code'] = df['FBI Code'].astype(str)
df['Year'] = pd.to_numeric(df['Year'], errors='coerce')
df['Updated On'] = df['Updated On'].astype(str)
df['Latitude'] = pd.to_numeric(df['Latitude'], errors='coerce')
df['Longitude'] = pd.to_numeric(df['Longitude'], errors='coerce')

## Remove duplicate rows
#x = df.ix[(df['ID']==4786321)] # Good example for post!!!

df = df.drop_duplicates()


### Change more data types

## Datetime
df_copy = df

#df['Datetime2'] = df['Datetime']
df['Datetime'] = pd.to_datetime(df['Datetime'], format="%m/%d/%Y %I:%M:%S %p", errors='coerce') # Will not work without coerce... R does this automatically

## Updated On
df['Updated On'] = pd.to_datetime(df['Updated On'], format="%m/%d/%Y %I:%M:%S %p", errors='coerce')


#### Create columns

### Create columns from datetime
import datetime

## Month name
df['Month'] = df['Datetime'].dt.strftime('%B')

## Month
df['Month_Value'] = df['Datetime'].dt.month

## Year_Month
df['Year_Month'] = df['Datetime'].dt.strftime('%Y-%m')

## Hour
df['Hour'] = df['Datetime'].dt.hour


#### Create unique identifier
# Use ID and Case Number

## Create unique identifier
df['ID'] + df['Case Number']

