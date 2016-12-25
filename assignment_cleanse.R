# 
# assignment_cleanse.R
#
# Source R script that documents the process of tidying data for the Getting and Cleaning Data Coursera Course Assignment
#

# Note to Peer Reviewer
# If you wish to read the resultant tidy_summary.csv file you can use the following R command
# data <- read.csv("tidy_summary.csv)   # assuming your current working directory is set correctly


# Assumptions:
# - .zip file has been extracted into the working directory as is
# - Raw contents has not been uploaded onto the github repository due to size
#
# - Initial investigation shows that values are all numeric

#
# Required Function Definition
# appendAverageToColumnHeading
# Appends the word "average" to a string. Will also capitalise the first letter and strip parentheses and hyphens
#
# Example: appendAverageToColumnHeading("test-Heading()") will return "averageTestHeading" 
#
appendAverageToColumnHeading <- function(heading){
        # Obtain the first character, to capitalise
        first <- substr(heading,1,1)
        new_heading <- sub(first, toupper(first), heading)
        
        # Strip parentheses and hyphens
        new_heading <- gsub("\\(\\)", "", new_heading)
        new_heading <- gsub("-", "", new_heading) 
        
        # Add "Average" to the front of the column name
        new_heading <- paste("Average",new_heading, sep = "")
        
        new_heading # return value
}


############# Start Data Cleansing Below ###################

#
# Build the complete training set
#
train <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/train/X_train.txt", quote="\"", comment.char="")
train_activity <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/train/y_train.txt", quote="\"", comment.char="")
train_subject <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/train/subject_train.txt", quote="\"", comment.char="")
dim(train)      # check the dimensions of what was read in as a sanity check
dim(train_activity)     # check the dimensions of what was read in as a sanity check
dim(train_subject)      # check the dimensions of what was read in as a sanity check
train_complete <- cbind(train, train_activity, train_subject)   # Merge the training data with the appropriate label


#
# Build the complete testing set
#
test <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/test/X_test.txt", quote="\"", comment.char="")
test_activity <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/test/y_test.txt", quote="\"", comment.char="")
test_subject <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/test/subject_test.txt", quote="\"", comment.char="")
dim(test)       # check the dimensions of what was read in as a sanity check
dim(test_activity)      # check the dimensions of what was read in as a sanity check
dim(test_subject)       # check the dimensions of what was read in as a sanity check
test_complete <- cbind(test, test_activity, test_subject)       # Merge the training data with the appropriate label


#
# Merge the training and testing sets and assign the column names
#
features <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/features.txt", quote="\"", comment.char="")
activity_feature <- data.frame(V1 = 562, V2 = "activityId")
subject_feature <- data.frame(V1 = 563, V2 = "subjectId")
features <- rbind(features, activity_feature, subject_feature)
dim(features)   # check to see if dimensions of features matches that of the combined data set

library(dplyr)
all_data <- rbind(train_complete, test_complete)
names(all_data) <- features[,2]
dim(all_data)

all_data <- all_data[, !duplicated(colnames(all_data))]  # remove the duplicated columns
filtered_data <- select(tbl_df(all_data), matches("mean|std|activityId|subjectId"))    # get only mean, std, and activityLabel columns
dim(filtered_data)      # inspect
names(filtered_data)    # inspect
#rm("test","test_complete","test_label","train","train_complete","train_label")  # memory cleanup



#
# Apply activity names to the activities
#
activity <- read.table("~/Coursera/DS Specialisation/3 - Get Clean Data/Assignment/activity_labels.txt", quote="\"", comment.char="")
names(activity) <- c("activityId", "activityName")
tidy_data <- merge(filtered_data, activity, by = "activityId") %>% select(-activityId)      # replace the activity id with the activity name
dim(tidy_data)
names(tidy_data)
#View(tidy_data) # Visualise data


#
# Create new table to get the average of each variable by subject and visualise the end result, adjust headings
#
tidy_summary <- group_by(tidy_data, subjectId, activityName) %>% summarise_each(funs(mean), -activityName, -subjectId)

# Apply updaed headings to reflect average
headings <- names(tidy_summary)
for(i in seq_along(headings)){  #append the word Average to all measurements
        if(!headings[i] == "subjectId" & !headings[i] == "activityName"){
                headings[i] = appendAverageToColumnHeading(headings[i]) # Prepend Average as averages are provided
        }
}

headings <- gsub("\\(\\)", "", headings)        # replace parentheses
headings <- gsub("-", "", headings)             # replace dashes
names(tidy_summary) <- headings                 # replace data frame headings with updated headings

#View(tidy_summary)     # visualise data

#
# Write summary output to a file (.csv)
#
write.csv(x = tidy_summary, file = "tidy_summary.csv", quote = TRUE, row.names = FALSE)    # output data

############# End Data Cleansing ###################


# Check tidy data principles
#       1. Each variable forms a column -> check
#       2. Each observation forms a row -> check
#       3. Each observational unit forms a table -> check
#


# TODO: Build a code book
# TODO: Load everything onto github