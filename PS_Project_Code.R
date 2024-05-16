#-----------------------------------------------------------------------------------------------------------------------------
#INSTALLING REQUIRED PACKAGES
#-----------------------------------------------------------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")

library(tidyverse) #Used data manipulation and visualization with a consistent design philosophy.
library(lubridate) #Used for working with dates and times, providing functions for parsing and manipulating date-time data.
library(dplyr)     #Has functions for filtering, transforming, and summarizing data frames.
library(ggplot2)   #Enables the creation of customizable and publication-quality data visualizations.
library(tidyr)     #Used for reshaping data frames, converting between wide and long formats.
library(here)      #Helps manage file paths in a platform-independent way for consistent file access.
library(skimr)     #Generates summary and descriptive statistics for data frames, offering quick data overviews.
library(janitor)   #Simplifies data cleaning tasks, including column renaming and data frame tidying.

#-----------------------------------------------------------------------------------------------------------------------------
#READING THE DATASETS AND UNDERSTANDING ITS STRUCTURE
#-----------------------------------------------------------------------------------------------------------------------------
Activity <- read.csv("D:/college/SEM 7/Probability & Statistics/project/dailyActivity_merged.csv")
head (Activity)
names(Activity) #gives column names
str(Activity) #gives structure of dataset

Calories <- read.csv("D:/college/SEM 7/Probability & Statistics/project/dailyCalories_merged.csv")
head (Calories)
names(Calories)
str(Calories)

Intensities <- read.csv("D:/college/SEM 7/Probability & Statistics/project/dailyIntensities_merged.csv")
head (Intensities)
names(Intensities)
str(Intensities)

Sleep <- read.csv("D:/college/SEM 7/Probability & Statistics/project/sleepDay_merged.csv")
head (Sleep)
names(Sleep)
str(Sleep)

Heartrate <- read.csv("D:/college/SEM 7/Probability & Statistics/project/heartrate_seconds_merged.csv")
head (Heartrate)
names(Heartrate)
str(Heartrate)

Sleep <- read.csv("D:/college/SEM 7/Probability & Statistics/project/sleepDay_merged.csv")
head (Sleep)
names(Sleep)

Weight <- read.csv("D:/college/SEM 7/Probability & Statistics/project/weightLogInfo_merged.csv")
head (Weight)
names(Weight)
str(Weight)

View(Activity)
View(Calories)
View(Intensities)
View(Heartrate)
View(Sleep)
View(Weight)

#-----------------------------------------------------------------------------------------------------------------------------
#DATA CLEANING
#-----------------------------------------------------------------------------------------------------------------------------
#REMOVES DUPLICATE VALUES
Sleep%>%distinct()

#THERE ARE TOO MANY MISSING VALUES IN FAT COLUMN SO WE REMOVE IT
Weight$Fat<-NULL

head(Weight)
glimpse(Weight)

#POSIXct - Store date, time, and fractional seconds.
#WE ARE USING ONE SINGLE FORMAT FOR DATE, TIME SO CONVERTING MISCILLANEOUS FROMATS TO REQUIRED FORMAT
Activity$ActivityDate=as.POSIXct(Activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone()) 
Activity$date <- format(Activity$ActivityDate, format = "%m/%d/%y")
Activity$ActivityDate=as.Date(Activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
Activity$date=as.Date(Activity$date, format="%m/%d/%Y")

Intensities$ActivityDay=as.Date(Intensities$ActivityDay, format="%m/%d/%Y", tz=Sys.timezone())

Sleep$SleepDay=as.POSIXct(Sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
Sleep$date <- format(Sleep$SleepDay, format = "%m/%d/%y")
Sleep$date=as.Date(Sleep$date, "% m/% d/% y")


#What’s total number of recorded users in the data sets? checking each dataset 
Activity %>%
summarise(Activity_participants = n_distinct(Activity$Id))

n_distinct(Calories$Id)

n_distinct(Intensities$Id)

n_distinct(Heartrate$Id)

n_distinct(Sleep$Id)

n_distinct(Weight$Id)

#There are 33 participant the data sets for activity, calories, and intensities. 
#24 individuals made up the Sleep data. 
#14 individuals in the Heartrate study, and only 8 people in the weight study. 


#SINCE HEART RATE AND WEIGHT HAS LESSER PARTICIPANTS DATA WE WILL ONLY FOCUS OUR ANALYSIS ON
#Activity, Calories, Intensities, and Sleep.

#-----------------------------------------------------------------------------------------------------------------------------
#DATA ANALYSIS 
#-----------------------------------------------------------------------------------------------------------------------------
#THIS GIVES US THE SUMMARY FOR EACH OF THE COLUMNS MENTIONED FOR EACH DATASET
Activity %>%
select(TotalSteps, TotalDistance, SedentaryMinutes, Calories) %>%
summary()

Intensities %>%
select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
summary()

Calories %>%
select(Calories) %>%
summary()

Sleep %>%
select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
summary()

#MERGING DATASETS FOR EASIER VISUALIZATION
Combined_data_inner <- merge(Sleep, Activity, by="Id")
n_distinct(Combined_data_inner$Id)

Combined_data_outer <- merge(Sleep, Activity, by="Id", all = TRUE)
n_distinct(Combined_data_outer$Id)

#-----------------------------------------------------------------------------------------------------------------------------
#DATA VISUALIZATION
#DETERMINING CORRELATION PATTERNS
#-----------------------------------------------------------------------------------------------------------------------------

#CORRELATION BTW TotalSteps and SedentaryMinutes
ggplot(data=Activity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point() + geom_smooth() + labs(title = "TotalSteps vs SedentaryMinutes") 

cor(Activity$TotalSteps, Activity$SedentaryMinutes)

#CORRELATION BTW TotalMinutesAsleep and  TotalTimeInBed
ggplot(data=Sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()+ geom_smooth() + labs(title = "TotalMinutesAsleep vs TotalTimeInBed")

cor(Sleep$TotalMinutesAsleep, Sleep$TotalTimeInBed)

#CORRELATION BTW Total Steps and Calories
ggplot(data=Activity, aes(x=TotalSteps, y=Calories)) +geom_point() + geom_smooth() + labs(title="Total Steps vs Calories")

cor(Activity$TotalSteps, Activity$Calories)


#RELATION BTW Time and Active Intensity
Intensities$ActiveIntensity <- (Intensities$VeryActiveMinutes)/60

Combined_data <- merge(Weight, Intensities, by="Id", all=TRUE)
Combined_data$time <- format(Combined_data$Date, format = "%H:%M:%S")

ggplot(data=Combined_data, aes(x=time, y=ActiveIntensity)) + geom_histogram(stat = "identity", fill='green') +theme(axis.text.x = element_text(angle = 90)) + labs(title="Total very Active Intensity vs. Time")

#-----------------------X---------------------------X-----------------------X-----------------------X----------------------------







