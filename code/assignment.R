# Introduction to Data Science 11372
# Niraj hirachan (U3208903)

# Assignment 1 for Introduction to  Data science (Data Wrangling and Exploration)

# A program to analysis on the the weather data obtained from from Daily Weather Observations of Canberra, 
# Australian Capital Territory from October 2018 to October 2019, and extracting insight from it.					



#Setting the directory of the working file.
setwd("/Users/niraj/OneDrive - University of Canberra/IntroductionToDataScience(11372)/Assignment/assignment1/code/")


#checking whether the tidyverse and lubridate packages are installed or not
list.of.packages <- c("tidyverse","lubridate","plyr","dplyr")
check.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#installing the missing packages
if(length(check.packages)){
  install.packages(check.packages)
}

#load the  library tidyverse and lubridate
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)


###############################PART-A, Reading#################################
# Making a dataframe by combing all the 13 months data received from the csv file.

#defining the list of all the available csv files to load
list_of_csv_files <- c("data/201808.csv","data/201809.csv","data/201810.csv","data/201811.csv",
                       "data/201812.csv","data/201901.csv","data/201902.csv","data/201903.csv",
                       "data/201904.csv","data/201905.csv","data/201906.csv","data/201907.csv",
                       "data/201908.csv")


#defining our main dataframe where all the data will be concatenated
weather_data_table <- NULL;

#reading the csv files one by one and appending the read data to the main.data.frame
for (i in 1:length(list_of_csv_files)){
  
  #variable to temporarily hold the data extracted from the csv file.
  temp <- read_csv(list_of_csv_files[i], skip = 7)
  
  #concanating the data from current csv file to the main data frame
  weather_data_table <- rbind.fill(weather_data_table, temp)
  
}

assertthat::assert_that(nrow(problems(weather_data_table))==0, msg="There is problem in loading csv file, which you need to fix first")




############################Part B ##########################################
#####Code to cleaning the non-essential data

# row count in the weather data table.
weather_data_table_row_count <- nrow(weather_data_table)


#Question-1 

#Dropping the columns from the weather data table, which have no data at all (i.e. all the records in these variables are NAs)

# if number of na is equal to number of rows in data table then the column has no data
weather_data_table <- weather_data_table[,colSums(is.na(weather_data_table))<weather_data_table_row_count]


#Question-2
# Dropping the variables, which have few data (i.e. NAs values are more than 90% of number of records in these variables).
#if number of na is equal to the 90% of the total data table row count then the column has very few data
weather_data_table <- weather_data_table[,colSums(is.na(weather_data_table)) < (0.9 * weather_data_table_row_count)]

#Question-3 
#Changing the column names to have no spaces between the words and replace these spaces 
#with underscore the ‘_’ character.
names(weather_data_table) <- gsub("\\ ", "_", names(weather_data_table))

#Question-4 
#Changing the type of the column called “Date” from character to Date data type.
weather_data_table$Date = as.Date(weather_data_table$Date, "%d/%m/%Y")


#Question-5 
#month column added with month data from Date column
weather_data_table <- add_column(weather_data_table, Month=month(weather_data_table$Date), .after='Date')

 
#Question-6
#Convert month data type from character to Ordinal
weather_data_table$Month <- as.factor(weather_data_table$Month)


# #Question-7
# # replacing the NA value of the column with median value of the column
# 
# Extracting the index of numeric column from the weather_data_table
numeric_column_index <- unlist(lapply(weather_data_table, is.numeric)) 

#Extracting the numeric column from the weather data table
numeric_column_in_weather_data_table <- weather_data_table[,numeric_column_index]


# Extracting the numeric column from the weather table that contains NA
numeric_column_in_weather_data_table_with_na <- numeric_column_in_weather_data_table[,colSums(is.na(numeric_column_in_weather_data_table))>0]

#checking if there is no numeric column with NA
if(ncol(numeric_column_in_weather_data_table_with_na) > 0){
  #Extract the median values of the columns that has a NA values
  median_of_column_with_na <- apply(numeric_column_in_weather_data_table_with_na, 2, FUN = function(x){
    median(x,na.rm = TRUE)
  })
  
  
  #cheking all the column with NA data for NA value and replacing the NA with median value
  for( i in 1:length(numeric_column_in_weather_data_table_with_na)){
    indx <- is.na(numeric_column_in_weather_data_table_with_na[,i])
    numeric_column_in_weather_data_table_with_na[indx, i] <-  median_of_column_with_na[i]
    weather_data_table[names(numeric_column_in_weather_data_table_with_na[i])] <- numeric_column_in_weather_data_table_with_na[i]
  }
  
}


# ###################################### PART C #########################################

#Question 1: 

#Finding the Summary(min, 1st quater, median, mean, 3rd quarter, max) of the 
#Minimum_temperature, Minimum_temperature, 9am_Temperature, 3pm_Temperature and Speed_of_maximum_wind_gust_(km/h)


summary_vector_column <- c('Maximum_temperature','Minimum_temperature','9am_Temperature','3pm_Temperature','Speed_of_maximum_wind_gust_(km/h)')

print("Summary of Maximum_temperature, Minimum_temperature, 9am_Temperature, 3pm_Temperature and Speed_of_maximum_wind_gust_(km/h)")
for (i in 1:length(summary_vector_column)){
  print(summary(weather_data_table[summary_vector_column[i]]))
}



#Question 2:
#Finding the mean of minimum temprature by month

mean_min_temp_by_month <- weather_data_table %>% 
  group_by(Month) %>% 
  summarise(month= month.abb[first(Month)], `Number of Days`=n(), mean = mean(Minimum_temperature))


#Question 3 
#Finding the mean of maximum temperature by month

mean_max_temp_by_month <- weather_data_table %>% 
  group_by(Month) %>% 
  summarise( month=month.abb[first(Month)], mean = mean(Maximum_temperature)) 


#4 Extracting the mean speed of maximum wind gust by direction of maximum wind gust
mean_wind_max_speed_by_direction<- weather_data_table %>% 
                              group_by(`Direction_of_maximum_wind_gust`) %>% 
                              summarise(`Mean speed of gust of wind per direction` = mean(`Speed_of_maximum_wind_gust_(km/h)`))


#5 Finding the month with highest rain fall
month_with_highest_rainfall <- weather_data_table %>% 
                               group_by(Month) %>% 
                               summarise(`Rainfall_(mm)`=sum(`Rainfall_(mm)`)) %>% 
                               summarise(max_month =  month.abb[which.max(`Rainfall_(mm)`)] )


#6 Checking for the dry months

check_month_with_no_rainfall <- function(){
  #declaring variable counter and indices to count the total number of dry month and index/month in which dry month falls
  counter = 0
  indices = NULL
  
  #getting the table of all the months with the total rain fall observed
  month_with_no_rainfall <- weather_data_table %>% 
    group_by(Month) %>% 
    summarise( count=n(), `Rainfall_(mm)`=sum(`Rainfall_(mm)`)) 
  
 
  #checking if any month observed 0mm rainfall
  for (i in 1:length(month_with_no_rainfall$`Rainfall_(mm)`)){
    if(month_with_no_rainfall$`Rainfall_(mm)`[i] == 0){
      counter = counter + 1
      indices[counter] <-  i
    }
  }
  
  
  if(counter > 0){
    print("The following months were the driest")
    for(i in 1:counter){
      print(month.abb[indices[i]])
    }
  }else{
    print("There were no dry months")
  }
}

check_month_with_no_rainfall()



 
#7Checking the max humidity level in the last year

month_with_highest_humidity <- weather_data_table %>% 
  filter(Date> as.Date("2019-01-01")) %>% 
  group_by(Month) %>% 
  summarise( count=n(),`average_relative_humidity_(%)` = (mean(`9am_relative_humidity_(%)`) + mean(`3pm_relative_humidity_(%)`))/2) %>% 
  summarise("Month with Highest Humidity" = month.abb[which.max(`average_relative_humidity_(%)`)])


    
