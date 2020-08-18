# Getting-and-Cleaning-Data-Course-Project

## Script Function

run_analysis.R script do:

1) Read all dataframes(Train and test data, subjects, activities and features)
2) Merge data by type (Values, Colnames and subject row)
3) Filter features with regex "mean+|std+" to get only ones with mean and std word in the name
4) Filter train and test values previously merged on point 2 with positions get with the regex on point 3
5) Merged all data (Values, Colnames and subject row)
6) Get a tidy dataframe
7) Creates a second, independent tidy data set with the average of each variable for each activity and each subject
8) Write a txt with the data obtained on point 7

## Average Dataset

average_dataset.txt contains the project data, including test and training data. The table contains data of the means calculated for each feature. These features are the ones that contain mean and std in their calculation.

## Codebook

codebook.md contains information on the data and variables used for the project.
