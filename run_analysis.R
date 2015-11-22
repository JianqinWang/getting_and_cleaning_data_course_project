# 1. Merges the training and the tests sets to create one data set.

#setting the work directory
setwd("E:/Fall 2015/Coursera/Getting and Cleaning Data/UCI HAR Dataset")

#reading in the files that will be combined
features <- read.table("./features.txt", header = FALSE)
activity_labels <- read.table("./activity_labels.txt", header = FALSE)
subject_train <- read.table("./train/subject_train.txt", header = FALSE)
x_train <- read.table("./train/X_train.txt", header = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE)
subject_test <- read.table("./test/subject_test.txt", header = FALSE)
x_test <- read.table("./test/X_test.txt", header = FALSE)
y_test <- read.table("./test/y_test.txt", header = FALSE)


#assigning column names for imported data
colnames(activity_labels) = c("activityID", "activityType")
colnames(subject_train) = "subjectID"
colnames(x_train) = features[,2]
colnames(y_train) = "activityID"
colnames(subject_test) = "subjectID"
colnames(x_test) = features[,2]
colnames(y_test) = "activityID"


# combine the data for the three training sets
training_data = cbind(y_train,subject_train, x_train)

# combine the data for the three test sets
test_data = cbind(y_test,subject_test, x_test)


# combine the training and data sets into one complete sets
final_data = rbind(training_data,test_data)

#combine col_names for final_data, so that vaules can be chosen for mean and std. deviation
col_names  = colnames(final_data)

# 2. Extracts only the measrements on the mean and standard deviaion for each measurement. 

# create a logical_Vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logical_vector = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names))

# Subset final_data table based on the logicalVector to keep only desired columns
final_data = final_data[logical_vector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the final_data set with the acitivityType table to include descriptive activity names
final_data = merge(final_data,activity_labels,by='activityID',all.x=TRUE)

# Updating the col_names vector to include the new column names after merge
col_names  = colnames(final_data)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(col_names)) 
{
  col_names[i] = gsub("\\()","",col_names[i])
  col_names[i] = gsub("-std$","StdDev",col_names[i])
  col_names[i] = gsub("-mean","Mean",col_names[i])
  col_names[i] = gsub("^(t)","time",col_names[i])
  col_names[i] = gsub("^(f)","freq",col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
};

# Reassigning the new descriptive column names to the final_data set
colnames(final_data) = col_names

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, final_dataNoActivityType without the activity_ype column
final_data_no_activity_type  = final_data[,names(final_data) != 'activityType']

# Summarizing the final_dataNoActivityType table to include just the mean of each variable for each activity and each subject
neat_data   = aggregate(final_data_no_activity_type[,names(final_data_no_activity_type) != c('activityID','subjectID')],by=list(activityID=final_data_no_activity_type$activityID,subjectID = final_data_no_activity_type$subjectID),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
neat_data   = merge(neat_data,activity_labels,by='activityID',all.x=TRUE)

# Export the tidyData set 
write.table(neat_data, './tidydata.txt',row.names=FALSE,sep='\t')