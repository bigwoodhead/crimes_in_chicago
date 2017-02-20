# -*- coding: utf-8 -*-
"""
##### Crimes in Chicago - Data wrangling exmaple for SuperDataScience
"""

#### Libraries

## Load libraries
import os
import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt


#### Get raw data

## Set working directory
#! Change to your working directory here !#
os.chdir('C:\\Users/Muhsin Karim\\Documents\\Data\\raw_data\\kaggle\\crimes_in_chicago')

## Load in each file
column_names = ['X1', 'ID', 'Case Number' ,'Date', 'Block', 
'IUCR', 'Primary Type', 'Description', 'Location Description', 
'Arrest', 'Domestic', 'Beat', 'District', 'Ward', 
'Community Area', 'FBI Code', 'X Coordinate', 'Y Coordinate', 'Year', 
'Updated On', 'Latitude', 'Longitude', 'Location'] 

df1 = pd.read_csv('Chicago_Crimes_2001_to_2004.csv', encoding='utf-8', names=column_names, skiprows=1)
df2 = pd.read_csv('Chicago_Crimes_2005_to_2007.csv', encoding='utf-8', names=column_names, skiprows=1)
df3 = pd.read_csv('Chicago_Crimes_2008_to_2011.csv', encoding='utf-8', names=column_names, skiprows=1)
df4 = pd.read_csv('Chicago_Crimes_2012_to_2017.csv', encoding='utf-8', names=column_names, skiprows=1)

## Bind rows
df = pd.concat([df1, df2, df3, df4])
del df1, df2, df3, df4


#### Format data

### Remove columns
df = df.drop(['X1', 'IUCR', 'X Coordinate', 'Y Coordinate', 'Location'], axis=1)

## Rename columns
df = df.rename(columns={"Date": "Datetime"})


### Change data types

df['ID'] = df['ID'].astype('str')

## Remove decimals
# http://stackoverflow.com/questions/17950374/converting-a-column-within-pandas-dataframe-from-int-to-string
def trim_fraction(text):
    if '.0' in text:
        return text[:text.rfind('.0')]
    return text
df.ID = df.ID.apply(trim_fraction)

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


### Create datetimes

df['Datetime'] = pd.to_datetime(df['Datetime'], format="%m/%d/%Y %I:%M:%S %p", errors='coerce') # Will not work without coerce... R does this automatically
df['Updated On'] = pd.to_datetime(df['Updated On'], format="%m/%d/%Y %I:%M:%S %p", errors='coerce')


#### Create columns

### Create columns from datetime

## Month name
df['Month'] = df['Datetime'].dt.strftime('%B')

## Month
df['Month Value'] = df['Datetime'].dt.month

## Year Month
df['Year Month'] = df['Datetime'].dt.strftime('%Y-%m')

## Hour
df['Hour'] = df['Datetime'].dt.hour


#### Create unique identifier

## Create unique identifier
df['Identifier'] = df['ID'] + '-' + df['Case Number']

## Remove duplicate
df = df.sort(['Updated On'], ascending=False)
df = df.drop_duplicates('Identifier')


#### Crime rate per District

## Get crime counts for each Primary Type 
dfDistrict = df.groupby(['Year Month', 'District', 'Primary Type']).size().reset_index()
dfDistrict = dfDistrict.rename(columns={0:'Numerator'})
dfDistrict['ID'] = dfDistrict['Year Month'] + ' ' + dfDistrict['District'].astype(str)

## Get crime counts for each Month
dfMonth = df.groupby(['Year Month', 'District']).size().reset_index()
dfMonth = dfMonth.rename(columns={0:'Denominator'})
dfMonth['ID'] = dfMonth['Year Month'] + ' ' + dfMonth['District'].astype(str)
dfMonth = dfMonth[['ID', 'Denominator']]

## Join
dfJoin = pd.merge(dfDistrict, dfMonth, how='left')
dfJoin['Crime Rate'] = dfJoin['Numerator']/dfJoin['Denominator'] * 100


#### Visualisation

## Prepare data frame for Theft from 2016
dfPrimaryType = dfJoin[(dfJoin['Primary Type'] == "THEFT")].reset_index()
dfPrimaryType = dfPrimaryType[(dfPrimaryType['Year Month'] > "2015-12")]


### Plots

## Spread data frame

## Crime frequency - theft
dfCrimeFreq = dfPrimaryType.pivot(index='Year Month', columns='District')['Numerator']
dfCrimeFreq = dfCrimeFreq.reset_index()
x = list(range(dfCrimeFreq.shape[0]))
y = dfCrimeFreq.drop(['Year Month'], axis=1)
plt.plot(x, y)
xticks = dfPrimaryType['Year Month']
plt.xticks(x, xticks, rotation=45)
plt.show()

## Crime rate - theft
dfCrimeRate = dfPrimaryType.pivot(index='Year Month', columns='District')['Crime Rate']
dfCrimeRate = dfCrimeRate.reset_index()
x = list(range(dfCrimeRate.shape[0]))
y = dfCrimeRate.drop(['Year Month'], axis=1)
plt.plot(x, y)
xticks = dfPrimaryType['Year Month']
plt.xticks(x, xticks, rotation=45)
plt.show()


#### Save tidy data

os.chdir('C:\\Users\\Muhsin Karim\\Documents\\Data\\tidy_data\\crimes_in_chicago')   
df.to_csv("tidy_py.csv", index=False)
df.to_pickle("tidy.pkl") 
df.to_pickle("join.pkl")
