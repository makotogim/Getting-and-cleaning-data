## 1. Merges the training and the test sets to create one data set

# Extract data from test folder
datatest1 <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
datatest2 <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")

# Extrac data from train folder
datatrain1 <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
datatrain2 <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")
# combine data from X and Y files
Xdata <- rbind(datatest1,datatrain1, make.row.names = FALSE)
Ydata <- rbind(datatest2,datatrain2, make.row.names = FALSE)
Subject_data <- rbind(subjecttest,subjecttrain, make.row.names = FALSE)
colnames(Ydata) <- "Activity_num"

# Include the names on the features.txt file and creates column names for Y data files and Subject data
temp <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
colnames(Xdata) <- temp[,2]
colnames(Ydata) <- "Activity_num"
colnames(Subject_data) <- "Subject"

#Merge both datasets.
XYdata <- cbind(Subject_data,Ydata,Xdata)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement?
# Find the columns of mean and std values
namepattern <- ("Subject|Activity_num|mean|std")
mean_std <- grep(namepattern, names(XYdata), ignore.case = TRUE, perl = FALSE, value = FALSE, fixed = FALSE)

# Create a dataset only with the mean and data deviation measurements
Activitydata <- XYdata[,mean_std]


## 3. Uses descriptive activity names to name the activities in the data set
# load activity names data
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")
colnames(activity_labels) <- c("Activity_num", "Activity_names")
# Find activity id and create a new column with the activity name
Activitydata <- merge(activity_labels, Activitydata,by ="Activity_num", all.x = TRUE)

## 4. Appropriately labels the data set with descriptive variable names
names(Activitydata)<-gsub("std()", "Standard_deviation", names(Activitydata))
names(Activitydata)<-gsub("mean()", "Mean", names(Activitydata))
names(Activitydata)<-gsub("^t", "Time", names(Activitydata))
names(Activitydata)<-gsub("^f", "Frequency", names(Activitydata))
names(Activitydata)<-gsub("Acc", "Accelerometer", names(Activitydata))
names(Activitydata)<-gsub("Gyro", "Gyroscope", names(Activitydata))
names(Activitydata)<-gsub("Mag", "Magnitude", names(Activitydata))
names(Activitydata)<-gsub("BodyBody", "Body", names(Activitydata))

 
## 5. From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.
Activity_tidy <- ddply(Activitydata, .(Activity_names,Subject), numcolwise(mean))

write.table(Activity_tidy, "./tidy_data.txt", sep = "\t",row.names = FALSE)


