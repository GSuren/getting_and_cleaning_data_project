## Coursera: Johns Hopkins University: Getting and Cleaning Data Course Project
## Gijsbert Suren
## 20180106

## Requirements:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

################################################################################################################
################################################################################################################

install.packages('dplyr')
library(dplyr)

## Steps:
## 1 - Clean workspace

rm(list=ls(all=TRUE))

## 2 - Set working directory - please change this to desired directory

setwd('C:\\Users\\G.Suren\\Google Drive\\Data Science\\Getting and Cleaning Data\\Project')

## 3 - Download and unzip data

zipFileName <- 'getdata%2Fprojectfiles%2FUCI HAR Dataset.zip'

if(!file.exists(zipFileName)){
  zipFileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  download.file(zipFileURL, destfile = zipFileName, method = "wininet")
}
if(!file.exists("UCI HAR Dataset")){
  unzip(zipFileName)
}

## 4 - Load data into data tables

activities <- read.table("UCI HAR Dataset\\activity_labels.txt", header=FALSE, as.is = TRUE)
features <- read.table("UCI HAR Dataset\\features.txt", header=FALSE, as.is = TRUE)

trainingSubjects <- read.table("UCI HAR Dataset\\train\\subject_train.txt", header=FALSE)
trainingValues <- read.table("UCI HAR Dataset\\train\\X_train.txt", header=FALSE)
trainingActivities <- read.table("UCI HAR Dataset\\train\\y_train.txt", header=FALSE)

testSubjects <- read.table("UCI HAR Dataset\\test\\subject_test.txt", header=FALSE)
testValues <- read.table("UCI HAR Dataset\\test\\X_test.txt", header=FALSE)
testActivities <- read.table("UCI HAR Dataset\\test\\y_test.txt", header=FALSE)

## 5 - Set Column names

colnames(activities) <- c('activity_id', 'activity_name')
colnames(features) <- c('feature_id', 'feature_name')

colnames(trainingSubjects) <- 'subject'
colnames(trainingValues) <- features$feature_name
colnames(trainingActivities) <- 'activity'

colnames(testSubjects) <- 'subject'
colnames(testValues) <- features$feature_name
colnames(testActivities) <- 'activity'

## 6 - Make training and test data tables with the same structure
## then create a single table containing both data-sets

trainingSet <- cbind(trainingSubjects, trainingValues, trainingActivities)
rm(trainingSubjects, trainingValues, trainingActivities)

testSet <- cbind(testSubjects, testValues, testActivities)
rm(testSubjects, testValues, testActivities)

completeSet <- rbind(trainingSet, testSet)
rm(trainingSet, testSet)

## 7 - Create a logical vector that provides column names containing: subject, activity_id, mean(), std()
## Then only keep those columns of the dataset

names(completeSet)
relevant_columns <- grepl("subject|activity|mean\\(\\)|std\\(\\)", colnames(completeSet))
colnames(completeSet[,relevant_columns]) 

completeSet <- completeSet[,relevant_columns]

## 8 - Rename columns with descriptive variable names

columnNames <- colnames(completeSet)
columnNames

columnNames <- gsub("[\\(\\)-]", "", columnNames)

columnNames <- gsub("^t", "timeDomain", columnNames)
columnNames <- gsub("^f", "frequencyDomain", columnNames)

columnNames <- gsub("mean", "Mean", columnNames)
columnNames <- gsub("std", "StandardDeviation", columnNames)

columnNames <- gsub("Acc", "Accelerometer", columnNames)
columnNames <- gsub("Gyro", "Gyroscope", columnNames)
columnNames <- gsub("Mag", "Magnitude", columnNames)

colnames(completeSet) <- columnNames

## 9 - Set activity name

completeSet$activity <- merge(completeSet, activities, by.x = 'activity', by.y = 'activity_id', all = FALSE)$activity_name

## 10 - Make tidy data-sets
## reference: https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

completeSet$activity <- as.factor(completeSet$activity)
completeSet$subject <- as.factor(completeSet$subject)

tidyDataSet <- completeSet %>% 
                  group_by(subject, activity) %>% 
                  summarize_all(funs(mean))

write.table(tidy, "tidy_data_set.txt", row.names = FALSE, sep = '\t', quote = FALSE)
