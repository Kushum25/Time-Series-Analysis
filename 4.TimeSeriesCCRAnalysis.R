#Kushum K C
#Date: April 16 2023

#required packages
library(ggplot2)
library(imputeTS)
library(forecast)
library(Kendall)
library(trend)
library(lubridate)

#Step1: Setting working directory
path <- 'D:\\OneDrive - Lamar University\\00Spring2023\\MachineLearning\\Assignment_5\\WD'
setwd(path)
getwd()

#Step 2:Read the csv file
data1 <- read.csv("4.J_17_Imputed.csv", header=TRUE, sep=",")
data2 <- read.csv("5.Comal_imputed.csv", header=TRUE, sep=",")
data3 <- read.csv("6.SanMarcos_Imputed.csv", header=TRUE, sep=",")

#Step 3: Convert the date column of data to a date object (year-Month-day format)
data1$Date <- as.Date(data1$Date, format = "%Y-%m-%d")
data2$Date <- as.Date(data2$Date, format = "%m/%d/%Y")  #date downloaded is in format of month/day/year
data3$Date <- as.Date(data3$Date, format = "%Y-%m-%d")

#Step 4: Generate a continuous time series data frame (x-axis) at regular interval(daily)
#Step 4.1: Set start and end dates
start_date <- as.Date("1932-11-12")
end_date <- as.Date("2023-04-14")

#Step4.2: Generate sequence of dates
date_continue <- seq(start_date, end_date, by = "day")

#step 4.3: Generate dataframe for continuous dates
Continue_df <- data.frame(date = date_continue)
colnames(Continue_df) <- c('Date')

#Step 5: Generating continuous dataframe having all days from 1932/11/12 to 2023/04/14 and downloaded data
Series1_df <- merge(Continue_df, data1, by="Date", all=TRUE)
Series2_df <- merge(Series1_df, data2, by="Date", all=TRUE)
Series_df <- merge(Series2_df, data3, by="Date", all=TRUE)
colnames(Series_df)

#Step 6: Cross Correlation among the three datasets

Ccf(Series_df$WaterLevel,Series_df$Discharge_C, main='CCF J-17Well water level and Comal Spring Discharge')

Ccf(Series_df$WaterLevel,Series_df$Discharge, main='CCF J-17Well water level and SanMarcos Spring Discharge')

Ccf(Series_df$Discharge_C,Series_df$Discharge, main='CCF Comal and SanMarcos Spring Discharge')



