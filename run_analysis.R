## COURSERA GETTING AND CLEANING DATA PROJECT 2
## Create one R script called run_analysis.R that does the following
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for 
##      each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##      with the average of each variable for each activity and each subject.

## CREATE DIRECTORY AND DOWNLOAD/UNZIP FILES
if(!file.exists("./Prog2")){dir.create("./Prog2")}
dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset_url, "./Prog2/accel.zip")
date()
# "Mon Feb 16 15:19:01 2015"
unzip("./Prog2/accel.zip", exdir = "./Prog2")
list.files("./Prog2")
setwd("Prog2/UCI HAR Dataset")

## ----------------------------------------------------------------------------
## 1. Merges the training and the test sets to create one data set.
## HEADER DATA
activity_labels <- read.table("activity_labels.txt", header=FALSE)
features <- read.table("features.txt",header=FALSE)

## TEST DATA
subject_test <- read.table("test/subject_test.txt", header=FALSE)
x_test <- read.table("test/x_test.txt", header=FALSE)
y_test <- read.table("test/y_test.txt", header=FALSE)
test <- cbind(subject_test,y_test,x_test)

## TRAIN DATA
subject_train <- read.table("train/subject_train.txt", header=FALSE)
x_train <- read.table("train/x_train.txt", header=FALSE)
y_train <- read.table("train/y_train.txt", header=FALSE)
train <- cbind(subject_train,y_train,x_train)

## MERGE TRAINING AND TEST & CREATE COLUMN NAMES
merged <- rbind(test,train)
features_labels = as.character(features[,2])
colnames(merged) <- c("Subject_ID","Activity_ID",features_labels)

## -----------------------------------------------------------------------------
## 2. Extracts only the measurements on the mean and standard deviation for 
##      each measurement. 
## FIND WHICH LABELS HAVE "mean" OR "std" IN THE NAME
keep <- grep("-mean\\(\\)|-std\\(\\)", features_labels)+2 ## ADD TWO TO ACCOUNT FOR SUBJECT AND ACTIVITY ID COLUMNS
keep <- c(1,2,keep)
## CREATE FINAL TABLE
final_data <- merged[,keep]

## -----------------------------------------------------------------------------
## 3. Use descriptive activity names to name the activities in the data set
colnames(activity_labels)  = c("Activity_ID","Activity_Name")
final_data = merge(final_data,activity_labels,by="Activity_ID",all.x=TRUE)
## MOVE ACTIVITY NAME NEXT TO ACTIVITY ID
final_data <- final_data[c(1,2,69,3:68)]
final_data <- final_data[c(2,1,3:69)]

## -----------------------------------------------------------------------------
## 4. Appropriately label the data set with descriptive activity names
colnames(final_data) <- gsub("\\(|\\)", "", names(final_data))
colnames(final_data) <- gsub("mean","Mean",names(final_data))
colnames(final_data) <- gsub("std","StdDev",names(final_data))
colnames(final_data) <- gsub("^(t)","Time",names(final_data))
colnames(final_data) <- gsub("^(f)","Freq",names(final_data))

## -----------------------------------------------------------------------------
## 5. Create a second, independent tidy data set with the average of each 
##      variable for each activity and each subject
install.packages("plyr")
library(plyr)
final_table <- ddply(final_data, .(Subject_ID, Activity_ID), .fun=function(x){ colMeans(x[,-c(1:3)]) })

## EXPORT FOR ASSIGNMENT
write.table(final_table, "tidy_data.txt", row.names=FALSE, sep='\t')
