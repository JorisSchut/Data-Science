#######################################
# This script is to take the UCI HAR (Human Activity Recognition 
# using smart phones) Dataset and do the following things:
# 
# The assignment reads as follows:
# You should create one R script called run_analysis.R that does the following. 
#  1. Merges the training and the test sets to create one data set.
#  2. Extracts only the measurements on the mean and standard deviation for 
#     each measurement. 
#  3. Uses descriptive activity names to name the activities in the data set
#  4. Appropriately labels the data set with descriptive variable names. 
#  5. From the data set in step 4, creates a second, independent tidy data 
#     set with the average of each variable for each activity and each subject.
#
# It is assumed this script is situated in the same directory as the Samsung data
# and the working directory is already set.
#########################################

#Download the file (if not already done)
if (!file.exists("phonedata.zip")) {
  url  = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  dest = "phonedata.zip"
  meth = "internal"
  quit = TRUE
  mode = "wb"
  download.file(url, dest, meth, quit, mode)
  #Works on tested operating system (Windows 7). Please change values if needed.
  unzip("phonedata.zip")
} 

# Load the required libraries
library(dplyr)
library(stringr)
library(tidyr)

### Step 1
# Read in training data sets, apply initial column names and combine them into a
#combined variable

features <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
xtrn <- read.table("./UCI HAR Dataset/train/X_train.txt", nrows = 7352)
ytrn <- read.table("./UCI HAR Dataset/train/y_train.txt")
strn <- read.table("./UCI HAR Dataset/train/subject_train.txt")

colnames(xtrn) <- features$V2
colnames(ytrn) <- "Activity"
colnames(strn) <- "Subject"

train <- cbind(strn, ytrn, xtrn)

# Read in test data sets, apply initial column names and combine them into a
#combined variable

xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", nrows = 2947)
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
stest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

colnames(xtest) <- features$V2
colnames(ytest) <- "Activity"
colnames(stest) <- "Subject"

test <- cbind(stest, ytest, xtest)

# Combine both the train and test data sets into a single data frame (df)
df <- rbind(test, train)
print("step 1 complete")

### Step 2
# Filter columns to just the activity, subject, and measurement columns
#with mean and std (standard deviation) and call data frame skinny

keepnames <- grepl("mean|std|Subject|Activity", colnames(df)) & 
             !grepl("meanFreq", colnames(df))

skinny <- df[, keepnames]
print("step 2 complete")

### Step 3
# Change Activities from numeric to names
# Load in activity names file and add appropriate column names prior to join

activity_names <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
colnames(activity_names) <- c("Activity", "ActDesc")

# Join activity names with skinny by Activity in each data frame
skinny <- tbl_df(skinny)
skinny <- inner_join(skinny, activity_names)

# Reorder columns to move new activity names to first column 
# and remove and rename Description as activity

skinny <- skinny[,c(69, 1:68)]
colnames(skinny)[1] <- "Activity"

print("step 3 complete")
### Step 4
# Rename variable names in skinny to make more sense and separate by "-"

pracnames <- names(skinny)

pracnames <- str_replace_all(pracnames, "Acc", "-acceleration-")
pracnames <- str_replace_all(pracnames, "Gyro", "-gyroscope-")
pracnames <- str_replace_all(pracnames, "Mag", "-magnitude")
pracnames <- str_replace_all(pracnames, "\\(\\)", "")
pracnames <- str_replace_all(pracnames, "^t", "time-")
pracnames <- str_replace_all(pracnames, "^f", "frequency-")
pracnames <- str_replace_all(pracnames, "tBody", "time-body-")
pracnames <- str_replace_all(pracnames, "BodyBody", "body")
pracnames <- str_replace_all(pracnames, "--", "-")
pracnames <- tolower(pracnames)

names(skinny) <- pracnames
print("step 4 complete")

### Step 5
# Use tidyr to gather measurements into tidy data (1 line per observation)

ttidy <- skinny %>% gather(sensor, Value, 4:69)

# Use dplyr to summarize by subject and activity
ttidy <- ttidy %>% group_by(subject, activity)%>%
summarize(mean = mean(Value))

# Write out tidy data set to file

write.table(ttidy, "tidy_data.txt", row.name = FALSE)
print("step 5 complete")