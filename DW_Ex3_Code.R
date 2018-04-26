library("dplyr")
library("tidyr")
library("matrixStats")

# load the original data

# X_test is the test set. It has 561 columns which correspond to the featurs
X_test <- read.table("X_test.txt")

# y_test contains the test labels.  This is a number which corresponds to an activity
# the legend for the mapping of the number to the activity is in activity_labels
y_test <- read.table("y_test.txt")

# X_train is the training set. It has 561 columns which correspond to the features
X_train <- read.table("X_train.txt")

# y_train contains the test labels.  This is a number which corresponds to an activity
# the legend for the mapping of the number to the activity is in activity_labels
y_train <- read.table("y_train.txt")

# features is a list of all 561 features. Features will be the column headers 
features <- read.table("features.txt")

# subject_test gives the volunteer number associated with a particular row in the training set (i.e., X_train) 
subject_train <- read.table("subject_train.txt")

# activity_labels contains the mapping of numbers (class labels) with their activity name
activity_labels <- read.table("activity_labels.txt")

# subject_test gives the volunteer number associated with a particular row in the test set (i.e., X_test)
subject_test <- read.table("subject_test.txt")

str(features)
# features is a dataframe.  The factors which we want to use as column names are in V2

# Start building the clean training dataset

clean_train <- X_train

# Add column names to clean training set
colnames(clean_train) <- features$V2

View(clean_train)



# TASK: Calculate the mean for each measurement in the training set
#       and place that mean in a new column within the clean training set


# Select the columns in the clean training set which contain the mean

# Column names are not unique when try to select columns which contain the mean
# Make column names of training set unique
# temp_train will be used to perform the mean calculations so that the original column names from features remain unchanged


temp_train <- clean_train
valid_column_names <- make.names(names = names(temp_train), unique = TRUE, allow_ = TRUE)
names(temp_train) <- valid_column_names

# Find the columns which have the means
mean_cols <- select(temp_train, contains("mean"))
str(mean_cols)

# Calculate the means of each measurement (i.e, the mean of the rows)
# Create a new column to hold the means
calc_mean <- rowMeans(mean_cols)
str(calc_mean)

calc_mean <- as.vector(calc_mean)

# Add the calculated means column to the clean training set

clean_train <- cbind(clean_train, calc_mean)

# TASK: Calculate the standard deviation for each measurement in the training set
#       and place that standard deviation in a new column within the clean training set


# Select the columns in the clean training set which contain the standard deviation

# Column names are not unique when try to select columns which contain the standard deviation
# Make column names of training set unique
# temp_train will be used to perform the standard deviation calculations so that the original column names from features remain unchanged

temp_train <- clean_train
valid_column_names <- make.names(names = names(temp_train), unique = TRUE, allow_ = TRUE)
names(temp_train) <- valid_column_names

# Find the columns which have the standard deviations
std_cols <- select(temp_train, contains("std"))
str(std_cols)

# Calculate the standard deviations of each measurement (i.e, the std of the rows)
# std_cols is not a matrix. Need to convert so the rowSds function will work
# Create a new column to hold the standard deviations


calc_std <- rowSds(as.matrix(std_cols, nrow = length(X_train$V1), ncol = length(std_cols), byrow = TRUE))
str(calc_std)

calc_std <- as.vector(calc_std)

# Add the calculated standard deviation column to the clean training set

clean_train <- cbind(clean_train, calc_std)

# Make a function to assign the activity name to its corresponding number

assign_activity <- function(x){
 
  if (x == 1){
    x = "WALKING"
  } else {
    if (x == 2){
      x = "WALKING_UPSTAIRS"
    } else {
      if (x == 3){
        x = "WALKING_DOWNSTAIRS"
      } else {
        if (x == 4){
          x = "SITTING"
        } else {
          if (x == 5) {
            x = "STANDING"
          } else {
            if (x == 6){
              x = "LAYING"
            } else {
              x = " "
            }
          }
        }
      }
    }
  } 
}



# Make a vector that will hold the activity name

ActivityName <- as.vector(y_train$V1)
ActivityName <- sapply(ActivityName, assign_activity, simplify = TRUE)


# Add ActivityName vector to clean training set

clean_train <- cbind(ActivityName, clean_train)

# Add activity labels vector to clean training set

ActivityLabel <- y_train
clean_train <- cbind(ActivityLabel, clean_train)

# Rename the activity label column from V1 to ActivityLabel
colnames(clean_train)[1] <- "ActivityLabel"

# Add a column to the clean dataset to indicate whether observation is from training set or test set
# Values will be either "train" or "test"
# Label that column Group

Group <- as.vector(rep("train", length(X_train$V1)))

clean_train <- cbind(Group, clean_train)

# Add a column to the clean training set that indicates which volunteer performed the activity

str(subject_train)

clean_train <- cbind(subject_train, clean_train) 

# Give that column the name "Volunteer"
colnames(clean_train)[1] <- "Volunteer"

# Clean training set is complete
# Now build the clean test set

# Start building the clean test dataset

clean_test <- X_test

# Add column names to clean test set
colnames(clean_test) <- features$V2

# TASK: Calculate the mean for each measurement in the test set
#       and place that mean in a new column within the clean test set


# Select the columns in the clean test set which contain the mean

# Column names are not unique when try to select columns which contain the mean
# Make column names of test set unique
# temp_test will be used to perform the mean calculations so that the original column names from features remain unchanged


temp_test <- clean_test
valid_column_names <- make.names(names = names(temp_test), unique = TRUE, allow_ = TRUE)
names(temp_test) <- valid_column_names

# Find the columns which have the means
mean_cols <- select(temp_test, contains("mean"))
str(mean_cols)

# Calculate the means of each measurement (i.e, the mean of the rows)
# Create a new column to hold the means
calc_mean <- rowMeans(mean_cols)
str(calc_mean)

calc_mean <- as.vector(calc_mean)

# Add the calculated means column to the clean test set

clean_test <- cbind(clean_test, calc_mean)

# TASK: Calculate the standard deviation for each measurement in the test set
#       and place that standard deviation in a new column within the clean test set


# Select the columns in the clean test set which contain the standard deviation

# Column names are not unique when try to select columns which contain the standard deviation
# Make column names of test set unique
# temp_test will be used to perform the standard deviation calculations so that the original column names from features remain unchanged

temp_test <- clean_test
valid_column_names <- make.names(names = names(temp_test), unique = TRUE, allow_ = TRUE)
names(temp_test) <- valid_column_names

# Find the columns which have the standard deviations
std_cols <- select(temp_test, contains("std"))
str(std_cols)

# Calculate the standard deviations of each measurement (i.e, the std of the rows)
# std_cols is not a matrix. Need to convert so the rowSds function will work
# Create a new column to hold the standard deviations


calc_std <- rowSds(as.matrix(std_cols, nrow = length(X_test$V1), ncol = length(std_cols), byrow = TRUE))
str(calc_std)

calc_std <- as.vector(calc_std)

# Add the calculated standard deviation column to the clean test set

clean_test <- cbind(clean_test, calc_std)

# Make a vector that will hold the activity name

ActivityName <- as.vector(y_test$V1)
ActivityName <- sapply(ActivityName, assign_activity, simplify = TRUE)

# Add ActivityName vector to clean test set

clean_test <- cbind(ActivityName, clean_test)

# Add activity labels vector to clean test set

ActivityLabel <- y_test
clean_test <- cbind(ActivityLabel, clean_test)

# Rename the activity label column from V1 to ActivityLabel
colnames(clean_test)[1] <- "ActivityLabel"

# Add a column to the clean dataset to indicate whether observation is from training set or test set
# Values will be either "train" or "test"
# Label that column Group

Group <- as.vector(rep("test", length(X_test$V1)))

clean_test <- cbind(Group, clean_test)

# Add a column to the clean test set that indicates which volunteer performed the activity

str(subject_test)

clean_test <- cbind(subject_test, clean_test)

# Give that column the name "Volunteer"
colnames(clean_test)[1] <- "Volunteer"

# Clean test set is complete

# combine clean_train and clean_test to get clean_total

clean_total <- rbind(clean_train, clean_test)

# Store entire clean data set in a file X_test_train.txt
write.table(clean_total, file = "X_test_train.txt", row.names = FALSE, col.names = colnames(clean_total))

            


  
  
