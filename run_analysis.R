library(dplyr)

runAnalysis <- function() {

    #read column names
    featureNames <- read.table("UCI HAR Dataset//features.txt")$V2
    
    #read test data
    testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names="subject")
    testY <- read.table("UCI HAR Dataset/test/y_test.txt", col.names="activity")
    testX <- read.table("UCI HAR Dataset/test/X_test.txt", col.names=featureNames)
    test <- cbind(testSubject, testY, testX)
    
    #read training data
    trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names="subject")
    trainY <- read.table("UCI HAR Dataset/train/y_train.txt", col.names="activity")
    trainX <- read.table("UCI HAR Dataset/train/X_train.txt", col.names=featureNames)
    train <- cbind(trainSubject, trainY, trainX)
    
    #combine test and training
    data <- rbind(test, train)  
    
    #replace dots in colum names
    colnames(data) <- gsub(colnames(data), pattern="\\.+", replacement="_")
    colnames(data) <- gsub(colnames(data), pattern="_+$", replacement="")    
    
    #set subject as factor
    data$subject <- as.factor(data$subject)
    
    #set activity as factor and use labels
    data$activity <- as.factor(data$activity)
    activityLabels <- read.table("UCI HAR Dataset//activity_labels.txt")$V2
    levels(data$activity) <- activityLabels  
    
    #extract mean and std
    data <- select(data, subject, activity, contains("_mean"), contains("_std"), -contains("_meanFreq"))
    
    #build second data set with mean values grouped by subject/activity
    data2 <- group_by(data, subject, activity)
    data2 <- summarise_each(data2, funs(mean))
    return(data2)
}