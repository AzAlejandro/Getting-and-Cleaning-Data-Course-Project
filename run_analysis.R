#Load libraries

library(tidyr)
library(dplyr)


#Get data from the link provided

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <- paste("./data", "/", "data.zip", sep = "")
dir.create("data")
download.file(url = URL, destfile = filename)
unzip(zipfile = filename, exdir = "./data")


#Get Directories

x_test_dir <- "./data/UCI HAR Dataset/test/X_test.txt"
x_train_dir <- "./data/UCI HAR Dataset/train/X_train.txt"
y_test_dir <- "./data/UCI HAR Dataset/test/y_test.txt"
y_train_dir <- "./data/UCI HAR Dataset/train/y_train.txt"
s_test_dir <- "./data/UCI HAR Dataset/test/subject_test.txt"
s_train_dir <- "./data/UCI HAR Dataset/train/subject_train.txt"
features_dir <- "./data/UCI HAR Dataset/features.txt"
activity_labels_dir <- "./data/UCI HAR Dataset/activity_labels.txt" 
  

#Read all the data and store in variables


x_test <- read.table(x_test_dir, sep = "")
x_train <- read.table(x_train_dir, sep = "")
y_test <- read.table(y_test_dir, sep = "")
y_train <- read.table(y_train_dir, sep = "")
s_test <- read.table(s_test_dir, sep = "")
s_train <- read.table(s_train_dir, sep = "")
features <- read.table(features_dir, sep = "")
activity_labels <- read.table(activity_labels_dir, sep = "")


#Merge test and train data

X_merged <- rbind(x_test,x_train)
y_merged <- rbind(y_test,y_train)
s_merged <- rbind(s_test,s_train)


#Get better features names and filter just mean and std names.


filter_regex <- grep("mean+|std+", features$V2)
filter_regex2 <- grep(".Freq", features$V2)
filter_regex <- filter_regex[!(grep("mean+|std+", features$V2) %in% grep(".Freq", features$V2))]
sort_by_regex <- features[filter_regex, 2]
sort_by_regex <- gsub("-mean", "Mean", sort_by_regex)
sort_by_regex <- gsub("-std", "Std", sort_by_regex)
sort_by_regex <- gsub("[-()]", "", sort_by_regex)


#Filter data on values and merge with Users and Activity data

X_merged <- X_merged[filter_regex]
data_merged <- cbind(s_merged, y_merged, X_merged)
colnames(data_merged) <- c("User", "Activity", sort_by_regex)
data_merged$Activity <- as.factor(data_merged$Activity)
levels(data_merged$Activity) <- activity_labels$V2


#Get tiddy dataframe

gathered_data <- gather(data_merged, key, value, -User,-Activity)


#Get mean of values by Users, Activity, and feature

final_data <- gathered_data %>% 
  group_by(User, Activity, key) %>%   
  summarise(media = mean(value)) %>% 
  spread(key, media)


#Write data as txt to upload in git

write.table(final_data, "./average_dataset.txt", row.names = FALSE)
