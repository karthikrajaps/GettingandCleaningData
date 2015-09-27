setwd("D:/Personal/Coursera/Data Science/Getting and Cleaning Data/Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
## Opening Library
library("dplyr")

## Reading Datasets
X_train <- read.table("./train/X_train.txt")
X_test <- read.table("./test/X_test.txt")
y_train <- read.table("./train/y_train.txt")
y_test <- read.table("./test/y_test.txt")
y_Label <- rbind(y_train,y_test)
subject_train <- read.table("./train/subject_train.txt")
subject_test <- read.table("./test/subject_test.txt")
subject_id <- rbind(subject_train, subject_test)

## Merging Data
merged_data <- rbind(X_train, X_test)
features <- read.table("features.txt")
Act_Label <- read.table("activity_labels.txt")

## Description of Variables
colnames(merged_data) <- features[,2]

## Retrieving mean and Std related variables
Index1 <- grep("mean", colnames(merged_data))
Index2 <- grep("Mean", colnames(merged_data))
Index3 <- grep("std", colnames(merged_data))
Index <- sort(c(Index1, Index2, Index3))

Subset_Data <- merged_data[,Index]
y_Label <- t(y_Label)

for (i in 1:6) {
y_Label <- gsub(Act_Label[i,1],as.character(Act_Label[i,2]),y_Label)
}

## Adding Activity_Name into Data

Subset_Data <- as.numeric(Subset_Data)
Subset_Data <- cbind(y_Label, subject_id, Subset_Data)
colnames(Subset_Data)[1] <- "Activity_Name"
colnames(Subset_Data)[2] <- "Subject_ID"
Subset_Data1 <- cbind(Subset_Data, rbind(y_train,y_test)) 
## Create a tidy data

Tidy_Data <- Subset_Data
Tidy_Data <- Tidy_Data[-c(1:10299),]
for (i in 1:30) {
NewData <- filter(Subset_Data1,Subject_ID == i)
for (j in 1:6) {
    NewData1 <- filter(NewData, V1 == j) 
    NewData2 <- NewData1[,3:88]
    Temp <- colMeans(NewData2)
    TempRow <- cbind(as.character(Act_Label[j,2]), i, t(Temp))
    Tidy_Data <- rbind(Tidy_Data,TempRow)
    }
}
colnames(Tidy_Data)[1] <- "Activity_Name"
colnames(Tidy_Data)[2] <- "Subject_ID"

write.table(Tidy_Data, file = "TidyData.txt", row.name = FALSE)