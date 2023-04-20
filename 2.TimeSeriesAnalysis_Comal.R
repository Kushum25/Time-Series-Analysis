#Kushum K C
#Date: April 16 2023

#required packages
library(ggplot2)
library(imputeTS)
library(forecast)
library(Kendall)
library(trend)
library(lubridate)
library('tseries')

#Step1: Setting working directory
path <- 'D:\\OneDrive - Lamar University\\00Spring2023\\MachineLearning\\Assignment_5\\WD'
setwd(path)
getwd()

#Step 2:Read the csv file
data = read.csv("2.Comal.csv", header=TRUE, sep=",")
colnames(data)

#Step 3: Convert the date column of data to a date object (year-Month-day format)
data$date <- as.Date(data$datetime, format = "%m/%d/%Y")  #date downloaded is in format of month/day/year

#Step 4: Generate a continuous time series data frame (x-axis) at regular interval(daily)
#Step 4.1: Set start and end dates
start_date <- as.Date("1927-12-19")
end_date <- as.Date("2023-04-16")

#Step4.2: Generate sequence of dates
date_continue <- seq(start_date, end_date, by = "day")

#step 4.3: Generate dataframe for continuous dates
Continue_df <- data.frame(date = date_continue)

#Step 5: Generating continuous dataframe having all days from 1927/12/19 to 2023/04/16 and downloaded data
#Step 5.1: Combine continue_df with the data (downloaded data)
Series1_df <- merge(Continue_df, data, by="date", all=TRUE)
colnames(Series1_df)

#Step 5.2: Storing the necessary column in new dataframe for furthure filtering and analysis
Final_df <- data.frame(Series1_df$date, Series1_df$X136450_00060_00003)
colnames(Final_df) <- c('Date','Discharge')
summary(Final_df)

#Step 5.3: Plot for the series with missing data
ggplot(Final_df, aes(x = Date, y = Discharge)) +
  geom_line() +
  xlab("Date") +
  ylab("Discharge(cfs)")

#Step 6: Imputing/ Filling missing data in the dataframe
#Step 6.1: Apply Kalman filter to impute missing values
imputed_data <- na_kalman(Final_df)
colnames(imputed_data)
summary(imputed_data)
#write.csv(imputed_data, file = "5.Comal_imputed.csv", row.names = FALSE)

#Step 6.2: Plot for the imputed time series
ggplot(imputed_data, aes(x = Date, y = Discharge)) +
  geom_line() +
  xlab("Date") +
  ylab("Discharge(cfs)")

#Step 6.3: Auto correlation and Partial Auto-Correlation analysis of imputed data
Acf(imputed_data$Discharge, lag.max = 1095)   #Autocorrelation for 3 years (1095 days)
Pacf(imputed_data$Discharge, lag.max = 10)    #Partial Autocorrelation for 10 days


#Step 7: Rolling average (10days) for making the time series variability smooth
df <- imputed_data
df$RollingAvg <- rollmean(df$Discharge, k = 10, fill = NA, align = "right")
summary(df)

#Step 7.1: Plot for the rolling averaged data time series
ggplot(df, aes(x = Date, y = RollingAvg)) +
  geom_line() +
  xlab("Date") +
  ylab("Discharge(cfs)")

#Step 7.2: Auto correlation and Partial Auto-Correlation analysis of Rolling Averaged data
Acf(df$RollingAvg, lag.max = 1095)   #Autocorrelation for 3 years (1095 days)
Pacf(df$RollingAvg, lag.max = 10)    #Partial Autocorrelation for 10 days


#Step 8: Test in time series data
#Step 8.1: Decomposition of data
Series_data <- ts(imputed_data$Discharge, frequency = 300)
Decomp_data <- decompose(Series_data)
plot(Decomp_data)

#Step 8.2: Mann-Kendall trend test
MannKendall(df$Discharge)

#Step 8.3: Seasonally-adjusted Mann-Kendall Trend Test
Discharge_ts <- ts(df$Discharge, frequency = 12, start = c(year(df$Date[1]), month(df$Date[1])))
SeasonalMannKendall(Discharge_ts)

#Step 8.4: Augmented Dickey Filler test
adf.test(df$Discharge)

