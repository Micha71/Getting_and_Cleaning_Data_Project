## Download the project raw data and extract the zipfile

if (!file.exists("data")) {dir.create("data")}
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/CourseraProjectRawData.zip")

list.files("./data")

# "CourseraProjectRawData.zip"

dateDownloaded <- date()
dateDownloaded

# "Sun May 03 16:44:15 2015"

# unzip the CourseraProjectRawData.zip file and extract the files to the "data" directory, i.e. UCI HAR Dataset

unzip("./data/CourseraProjectRawData.zip", exdir = "./data")

## Read the data files

Xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
str(Xtrain);View(Xtrain)

ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
str(ytrain);View(ytrain)

subjecttrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
str(subjecttrain);View(subjecttrain)

Xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
str(Xtest);View(Xtest)

ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
str(ytest);View(ytest)

subjecttest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
str(subjecttest);View(subjecttest)

features <- read.table("./data/UCI HAR Dataset/features.txt")
str(features);View(features)

activitylabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
str(activitylabels);View(activitylabels)

# Understanding the allocation of subjects to either the training set either the test set

unique(subjecttrain); unique(subjecttest)

# Understanding the frequency of logged activities

table(ytrain);table(ytest)

## Step 1: Merge the training and test set to create one data set

# Firstly vertically bind the training and test set for the features, the activities and the subjects

Xdata <- rbind(Xtrain, Xtest)
str(Xdata);View(Xdata)

Ydata <- rbind(ytrain, ytest)
str(Ydata);View(Ydata)

Subjectdata <- rbind(subjecttrain, subjecttest)
str(Subjectdata);View(Subjectdata)

# Secondly horizontally bind the subject, the activity and the feature variables and observations

Data <- cbind(Subjectdata, Ydata, Xdata)
str(Data);View(Data)

# Determine object size in MB of merged data

object.size(Data) / 2^20

## Step2: Extract only the measurements on the mean and standard deviation for each measurement.

# We only extract the variables which explicitly have the name 'mean()', 'meanFreq()' or 'std()' inside.

library(dplyr)

indexmean <- grep("mean()",features$V2)
indexstd <- grep("std()", features$V2)
extractedfeatures <- features[c(indexmean,indexstd),]

# sort the selected features list by number(V1) in ascending order

arrangedfeatures <- arrange(extractedfeatures, V1)

# now we subset the 'Data' dataframe, with the arranged, extracted featurelist

# arrangedfeatures$V1 lists al the extracted feature numbers
# subsequently we add these to the subject and activity variables which are the first two columns in the 'Data' dataframe
# this gives us finally the new dataframe with extracted variables: 'ExtractedData'

indexarrangedfeatures <- 2 + arrangedfeatures$V1
ExtractedData <- Data[ ,c(1,2,indexarrangedfeatures)]
head(ExtractedData)

## Step 3: Use descriptive activity names to name the activities in the data set.

# We exchange the activity numbers with the activity labels.

MutatedData <- ExtractedData %>%
                mutate(V1.1 = activitylabels$V2[V1.1])
View(MutatedData)

## Step 4: Appropriately label the data set with descriptive variable names.

View(arrangedfeatures)

# We first copy the dataset 'MutatedData' to a new dataset 'LabelledData' to avoid overwriting the original one.
# Then we import the arranged and selected feature list into the column names.

labelledData <- MutatedData
names(labelledData) <- c("subject", "activity", as.character(arrangedfeatures$V2))
str(labelledData);View(labelledData)


## Step 5: From the data set in step 4, create a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

library(tidyr)

# We first group the dataframe per subject and per activity. Subsequently we calculate the mean value per feature.
# Finally we collapse the dataframe wherein the 'feature' column becomes the variables and the previous feature columns become values.

tidyData <- labelledData %>%
                group_by(subject, activity) %>%
                summarise_each(funs(mean)) %>%
                gather(feature, mean, -c(subject,activity)) %>%
                arrange(subject, activity)

str(tidyData);View(tidyData)

# write a tidy data text file

write.table(tidyData,"tidyData.txt", sep = "\t", row.name = FALSE)

# one can easily read the file by means of the function call head(read.table("tidyData.txt", header = TRUE))
                
