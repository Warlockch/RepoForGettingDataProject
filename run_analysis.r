```
##  Function used to read the "X_train.txt",to avoid errors when using fread.
MyReadFileFunction <- function(f) {
    df <- read.table(f)
    dt <- data.table(df)
}
##  Function used to find the 
grepthis <- function(regex) {
    grepl(regex, dt$feature)
}
## load the reshape2 package
library("data.table")
library("reshape2")

## get the path where the UCI HAR dataset stores
path <- getwd()
pathIn <- file.path(path, "UCI HAR Dataset")
## read the subjct    Y    X   data
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

dtTrain <- MyReadFileFunction(file.path(pathIn, "train", "X_train.txt"))
dtTest <- MyReadFileFunction(file.path(pathIn, "test", "X_test.txt"))
## Merge the data of subject_train.txt and subject_test.txt
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
## change the column names from V1 to subject
setnames(dtSubject, "V1", "subjectid")
## Merge the data of Y_train.txt and Y_test.txt
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
## change the column names from V1 to activityNum
setnames(dtActivity, "V1", "activityNum")
## Merge the data of X_train.txt and X_test.txt
dt <- rbind(dtTrain, dtTest)

## Merges the training and the test sets to create one data set
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
setkey(dt, subjectid, activityNum)

## read the features
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
## find the mean and std data in these dtFeatures
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
## form the right format to match with the data in dt
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
##select specific columns in the dataset dt
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]
## read activity names
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
## merge the ActivityNames into dt
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
setkey(dt, subjectid, activityNum, activityName)
## use melt to get reshape the dt dataset
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
    all.x = TRUE)
## change the type of activity and feature to factor
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
    "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

setkey(dt, subjectid, activity, featDomain, featAcceleration, featInstrument, 
    featJerk, featMagnitude, featVariable, featAxis)
TidyData <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
write.table(TidyData,"TidyData.txt",row.name=FALSE)
```
