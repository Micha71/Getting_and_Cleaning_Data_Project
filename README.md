# README
# Coursera Getting and Cleaning Data Project

## Micha Bouts, Tuesday June 2nd 2015

Adding some content to illustrate git / github integration from Windows 10
Tuesday March 1st 2016

## Introduction

This README Markdown file explains how the R-script works which was developed and tested on a Windows 7 environment with RStudio and R version 3.1.2.When we refer to 'the R-script' we mean the script filename 'run_analysis.R'. The explanation below details the reasoning behind the set-up of this R-script.

## The raw data is courtesy to www.smartlab.ws

Human Activity Recognition Using Smartphones Dataset
Version 1.0

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit? degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws


Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

## Download the project raw data and extract the zipfile

First we test whether a 'data' directory is already available. If not then we create one. This 'data' directory is the landing spot where we download the zipped data file to.

````
if (!file.exists("data")) {dir.create("data")}
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/CourseraProjectRawData.zip")

list.files("./data")
````

The zipped raw data is downloaded on Sun May 03 16:44:15 2015. 

````
dateDownloaded <- date()
dateDownloaded
````

Subsequently the unzip command extracts the UCI HAR Dataset files to the data directory. The file directory structure is the same as in the original zipped raw data.

````
unzip("./data/CourseraProjectRawData.zip", exdir = "./data")
````
## Read the data files

The following data files are read into a set of dataframes:

- Xtrain is the dataframe for the X_train.txt file
- ytrain is the dataframe for the y_train.txt file
- subjecttrain is the dataframe for the subject_train.txt file
- Xtest is the dataframe for the X_test.txt file
- ytest is the dataframe for the y_test.txt file
- subjecttest is the dataframe for the subject_test.txt file
- features is the dataframe for the features.txt file
- activitylabels is the dataframe for the activity_labels.txt file

Each dataframe is verified by means of its structure and by viewing it in spreadsheet format.

````
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
````
Furthermore, it is helpful to understand how the subjects are allocated to both the train and test sets.
And to understand the frequency of logged activities.

````
unique(subjecttrain); unique(subjecttest)
table(ytrain);table(ytest)
````
## Extract a dataset with only the mean and standard deviation variables for each measurement

We'll go through the following process steps to achieve this goal:

- Step 1: Merge the training and test set to create one data set;
- Step 2: Extract only the measurements on the mean and standard deviation for each measurement;
- Step 3: Use descriptive activity names to name the activities in the data set;
- Step 4: Appropriatelly label the data set with descriptive variable names;
- Step 5: From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject;

### Step 1: Merge the training and test set to create one data set

We first vertically connect the data for the feature list(Xtrain and Xtest), for the activities(ytrain and ytest) and for the subjects(subjecttrain and subjecttest).

- Xdata is the vertically connected feature list;
- Ydata is the vertically connected activity list;
- Subjectdata is the vertically connected subject list;

````
Xdata <- rbind(Xtrain, Xtest)
str(Xdata);View(Xdata)

Ydata <- rbind(ytrain, ytest)
str(Ydata);View(Ydata)

Subjectdata <- rbind(subjecttrain, subjecttest)
str(Subjectdata);View(Subjectdata)
````
Secondly, we horizontally integrate the subject, activity and feature variables and observations.

````
Data <- cbind(Subjectdata, Ydata, Xdata)
str(Data);View(Data)
````

And we verify the object size of the merged data expressed in megabytes.

````
object.size(Data) / 2^20
````
### Step 2: Extract only the measurements on the mean and standard deviation for each measurement;

Let's assume that by mean we understand the following syntax:

- mean();
- meanFreq();

For standard deviation we assume the syntax:

- std();

The dplyr library is loaded to extract the relevant features.

````
library(dplyr)
````

We create an index for the mean and standard deviation features to facilitate the extraction.

````
indexmean <- grep("mean()",features$V2)
indexstd <- grep("std()", features$V2)
extractedfeatures <- features[c(indexmean,indexstd),]
````
Next we sort the selected feature list by number(variable V1) in ascending order

````
arrangedfeatures <- arrange(extractedfeatures, V1)
````

Finally we create the ultimate extracted dataset 'ExtractedData' which is a combination of the following variables:

- subjects: column 1 in the 'Data' dataframe
- activities: column 2 in the 'Data' dataframe
- features: column 2 + elements 'V1' from the 'arrangedfeatures' dataframe

```
indexarrangedfeatures <- 2 + arrangedfeatures$V1
ExtractedData <- Data[ ,c(1,2,indexarrangedfeatures)]
head(ExtractedData)
```

### Step 3: Use descriptive activity names to name the activities in the data set

Activity numbers are exchanged with the activity labels as pointed out in the 'activitylabels' dataframe.

````
MutatedData <- ExtractedData %>%
                mutate(V1.1 = activitylabels$V2[V1.1])
View(MutatedData)
````

### Step 4: Appropriatelly label the data set with descriptive variable names

We first copy the 'MutatedData' dataset to a new 'LabelledData' dataset to avoid overwriting the original one.
Then we import the arranged and selected feature list into the column names. To do so we first convert from a factor to a character vector.

````
labelledData <- MutatedData
names(labelledData) <- c("subject", "activity", as.character(arrangedfeatures$V2))
str(labelledData);View(labelledData)
````
### Step 5: From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject

Loading the 'tidyr' library

````
library(tidyr)
````

We first group the dataframe per subject and secondly per activity as a tie-breaker. Subsequently we calculate the mean value per feature. Next we collapse the dataframe wherein the 'feature' column becomes the variables and the previously listed feature columns become values. The latter is achieved by means of the gather function. Finally the dataset is sorted by subject and by activity, in that order.

````
tidyData <- labelledData %>%
                group_by(subject, activity) %>%
                summarise_each(funs(mean)) %>%
                gather(feature, mean, -c(subject,activity)) %>%
                arrange(subject, activity)

str(tidyData);View(tidyData)
`````

## Write the tidy dataset

At the end of the R-script a line of code is included which writes the 'tidyData' dataset to a tab-separated text file.

````
write.table(tidyData,"tidyData.txt", sep = "\t", row.name = FALSE)
````

## Read the tidy dataset

One can easily read the tiday dataset 'tidyData' into the R-console or RStudio by means of the following command:

```
head(read.table("tidyData.txt", header = TRUE))
```


