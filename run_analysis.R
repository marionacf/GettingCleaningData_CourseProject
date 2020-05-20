# LIBRARY

library(data.table)
library(dplyr)


# DOWNLOAD DATA

filename <- "CourseProject.zip"

# Check if archieve already exists and download

if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="curl")
}  

# Check if folder exists and unzip

if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}


# check the zip file

files <- list.files("D:/moia4/Documents/Cursos/Data_science_2020/GettingCleaningData/UCI HAR Dataset",recursive=TRUE)
files


#READ FILES

# Read training datasets
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=F)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header=F)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=F)

# Read test datasets
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=F)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header=F)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=F)

# Read feature vector
features <- read.table("./UCI HAR Dataset/features.txt", header=F)

# Read activity labels
activityLabels = read.table("./UCI HAR Dataset/activity_labels.txt", header=F)


# ASSIGN VARIABLE NAMES

colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

colnames(activityLabels) <- c("activityID", "activityType")


# 1.MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATASET

x <- rbind(x_train ,x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
finaldataset <- cbind(subject, x, y)


# 2. EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT

colNames <- colnames(finaldataset)

mean_and_std <- (grepl("activityID", colNames) |
                         grepl("subjectID", colNames) |
                         grepl("mean..", colNames) |
                         grepl("std...", colNames))

# Subset all the true values

setforMeanandStd <- finaldataset[ , mean_and_std == TRUE]


# 3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATASET

setWithActivityNames = merge(setforMeanandStd, activityLabels, by='activityID', all.x=TRUE)

setWithActivityNames$activityID <- activityLabels[,2][match(setWithActivityNames$activityID, activityLabels[,1])] 



# 4. APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES 

names(setWithActivityNames)[1] = "Activity"
names(setWithActivityNames)<-gsub("Acc", "Accelerometer", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Gyro", "Gyroscope", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("BodyBody", "Body", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Mag", "Magnitude", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("^t", "Time", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("^f", "Frequency", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("tBody", "TimeBody", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("-mean()", "Mean", names(setWithActivityNames), ignore.case = TRUE)
names(setWithActivityNames)<-gsub("-std()", "STD", names(setWithActivityNames), ignore.case = TRUE)
names(setWithActivityNames)<-gsub("-freq()", "Frequency", names(setWithActivityNames), ignore.case = TRUE)
names(setWithActivityNames)<-gsub("angle", "Angle", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("gravity", "Gravity", names(setWithActivityNames))


# 5. FROM THE DATASET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY DATASET WITH THE AVERAGE OF EACH VARIABLE FOR EACH aCTIVITY AND EACH SUBJECT

# Make a second tidy data set

FinalData <- setWithActivityNames %>% 
        group_by(subjectID, Activity) %>% 
        summarise_all(funs(mean))

# Write second tidy data set into a txt file

write.table(FinalData, "tidydata.txt", row.names = FALSE)
