run_analysis <- function(){
	
	fileNameY_test = "UCI HAR Dataset\\test\\y_test.txt"
	fileNameX_test = "UCI HAR Dataset\\test\\X_test.txt"
	fileNameSubjecttest = "UCI HAR Dataset\\test\\subject_test.txt"
	
	fileNameX_train = "UCI HAR Dataset\\train\\X_train.txt"
	fileNameY_train = "UCI HAR Dataset\\train\\y_train.txt"
	fileNameSubject_train = "UCI HAR Dataset\\train\\subject_train.txt"

	fileName_features = "UCI HAR Dataset\\features.txt"
	fileName_activityType = "UCI HAR Dataset\\activity_labels.txt"

	trainSet_X <- read.table(fileNameX_train, header=FALSE)
	trainSet_Y <- read.table(fileNameY_train, header=FALSE)
	set_activityType <- read.table(fileName_activityType, header=FALSE)
	trainSet_subject <- read.table(fileNameSubject_train, header=FALSE)

	testSet_subject <- read.table(fileNameSubjecttest, header=FALSE)
	testSet_X <- read.table(fileNameX_test, header=FALSE)
	testSet_Y <- read.table(fileNameY_test, header=FALSE)

##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################
	train_data <- cbind(trainSet_X, trainSet_Y, trainSet_subject)
	test_data <- cbind(testSet_X, testSet_Y, testSet_subject)
	data <- rbind(train_data,test_data)

############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################
	features <- read.table(fileName_features, header=FALSE, stringsAsFactors = FALSE)
	meanStdFeatures <- features[grep("mean|std", features[,2]),]
	meanStdDataSet <- data[,meanStdFeatures[,1]]

	subjectTrainSet <- read.table(fileNameSubject_train, header=FALSE)
	subjectTestSet <- read.table(fileNameSubjecttest, header=FALSE)
	trainSet_subject <- rbind(subjectTrainSet, subjectTestSet)

	activityTrainSet <- read.table (fileNameY_train, header = FALSE)
	activityTestSet <- read.table (fileNameY_test, header = FALSE)
	set_activityType <- rbind(activityTrainSet, activityTestSet)

	meanStdDataSet <- cbind(meanStdDataSet, set_activityType)
	meanStdDataSet <- cbind(meanStdDataSet, trainSet_subject)
	names(meanStdDataSet) <- c(meanStdFeatures[,2], "Activity", "Subject")

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################
	library(plyr)
	activitiesIds <- meanStdDataSet[,"Activity"]
	activitiesFactors <- as.factor(activitiesIds)
	meanStdDataSet[,"Activity"] = activitiesFactors

###########################################################################
# 4. Appropriately labels the data set with descriptive activity names.
###########################################################################
	names(meanStdDataSet) <- c(meanStdFeatures[,2], "Activity", "Subject")

######################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################
	install.packages("data.table")
	library(data.table)	
	tidyDataTable <- data.table(meanStdDataSet)
	avgTidyDataTable <- tidyDataTable[, lapply(.SD,mean), by=c("Activity","Subject")]
	newColNames = sapply(names(avgTidyDataTable)[-(1:2)], function(name) paste("mean(", name, ")", sep=""))
	setnames(avgTidyDataTable, names(avgTidyDataTable), c("Activity", "Subject", newColNames))
	write.csv(avgTidyDataTable, file="MeasureAvgTidySet.txt", row.names = FALSE)

}