

# Load in the required libraries
library(dplyr)
library(stringr)
library(tidyr)

### Step 1
# Read in training data sets and apply initial column names
features <- read.table("./data/UCI HAR Dataset/features.txt", 
                       stringsAsFactors = FALSE)
xtrn <- read.table("./data/UCI HAR Dataset/train/X_train.txt", nrows = 7352)
colnames(xtrn) <- features$V2
ytrn <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
colnames(ytrn) <- "Activity"
strn <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
colnames(strn) <- "Subject"
train <- cbind(strn, ytrn, xtrn)

# Read in test data sets and apply initial column names
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", nrows = 2947)
colnames(xtest) <- features$V2
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
colnames(ytest) <- "Activity"
stest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
colnames(stest) <- "Subject"
test <- cbind(stest, ytest, xtest)

# Combine both the train and test data sets into a single data frame
df <- rbind(test, train)

### Step 2
# Filter columns to just the activity, subject, and measurement columns
#          with mean and std (standard deviation) and call data frame skinny
keepnames <- grepl("mean|std|Subject|Activity", colnames(df)) & 
  !grepl("meanFreq", colnames(df))
skinny <- df[, keepnames]

### Step 3
# Change Activities from numeric to names
# Load in activity names file and add appropriate column names prior to join
activity_names <- read.table("./data/UCI HAR Dataset/activity_labels.txt", 
                             stringsAsFactors = FALSE)
colnames(activity_names) <- c("Activity", "ActDesc")
# Join activity names with skinny by Activity in each data frame
skinny <- tbl_df(skinny)
skinny <- inner_join(skinny, activity_names)
# Reorder columns to move new activity names to first column 
#   and remove and rename Description as activity
skinny <- skinny[,c(69, 2:68)]
colnames(skinny)[1] <- "Activity"

### Step 4
# Rename variable names in skinny to make more sense and separate by "-"
pracnames <- names(skinny)
# Change Acc to acceleration
pracnames <- str_replace_all(pracnames, "Acc", "-acceleration-")
# Change Gyro to gyroscope
pracnames <- str_replace_all(pracnames, "Gyro", "-gyroscope-")
# Change Mag to magnitude
pracnames <- str_replace_all(pracnames, "Mag", "-magnitude")
# Remove () 
pracnames <- str_replace_all(pracnames, "\\(\\)", "")
# Change t at the beginning to time
pracnames <- str_replace_all(pracnames, "^t", "time-")
# Change f at the beginning to frequency
pracnames <- str_replace_all(pracnames, "^f", "frequency-")
# Replace tBody with time-body
pracnames <- str_replace_all(pracnames, "tBody", "time-body-")
# Replace BodyBody with body
pracnames <- str_replace_all(pracnames, "BodyBody", "body")
# Clean up by removing any "--" and change to "-"
pracnames <- str_replace_all(pracnames, "--", "-")
# Make all lower case
pracnames <- tolower(pracnames)
# Apply them to skinny data frame
names(skinny) <- pracnames

### Step 5
# Use tidyr to gather measurements into tidy data (1 line per observation)
ttidy <- skinny %>%
  gather(sensor, Value, 3:68)

# Use dplyr to summarize by subject and activity
ttidy <- ttidy %>%
  group_by(subject, activity)%>%
  summarize(mean = mean(Value))

# Write out tidy data set to file
write.csv(ttidy, "tidy_data.csv", row.names = FALSE)
write.table(ttidy, "tidy_data.txt", row.name = FALSE)