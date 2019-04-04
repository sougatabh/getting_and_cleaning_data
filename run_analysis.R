library(data.table)
library("reshape2")

#### Download the Data ###
fileName <- "UCIdata.zip"
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Dataset"

# File download verification. If file does not exist, download to working directory.
if(!file.exists(fileName)){
  download.file(url,fileName, mode = "wb") 
}

# Unzip the file if not exists
if(!file.exists(dir)){
  unzip(fileName, files = NULL, exdir=".")
}


#Read the files

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")


activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  


## Merge the training and test data set.
# 1. Merges the training and the test sets to create one data set.
merged_ds <- rbind(X_train,X_test)
#head(merged_ds)

#Extract only mean()|std only 
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std <- grep("mean()|std()", features[, 2])

merged_ds <- merged_ds[,mean_std]
#head(merged_ds)

# 4. Appropriately labels the data set with descriptive variable names.
clean_feature_name <-sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(merged_ds) <-clean_feature_name[mean_std]

#Merge train and test subject

merge_subject <- rbind(subject_train, subject_test)
names(merge_subject) <- 'subject'
merge_activity <- rbind(y_train, y_test)
names(merge_activity) <- 'activity'


#Merge subject activity and dataset

merged_ds<- cbind(merge_subject,merge_activity, merged_ds)


# 3. Uses descriptive activity names to name the activities in the data set
# group the activity column of dataSet, re-name lable of levels with activity_levels, 
#and apply it to dataSet.

act_group <- factor(ds$activity)
levels(act_group) <- activity_labels[,2]
ds$activity <- act_group

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

base_ds <- melt(merged_ds,(id.vars=c("subject","activity")))
second_independent_ds <- dcast(base_ds, subject + activity ~ variable, mean)
names(second_independent_ds)[-c(1:2)] <- paste("[mean of]" , names(second_independent_ds)[-c(1:2)] )
write.table(second_independent_ds, "tidy_data.txt", sep = ",")

write.table(second_independent_ds, "tidy_data.txt", sep = ",",row.names = FALSE)




                    
                    

