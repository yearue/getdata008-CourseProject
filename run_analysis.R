library(data.table)

# Check and create a new working directory
if(!file.exists("UCI HAR Dataset")){dir.create("UCI HAR Dataset")}
setwd("./UCI HAR Dataset")

#Download data to the new working directory
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
download.file(fileUrl, destfile = "rawdata.zip")
unzip("rawdata.zip")

#Read data to R
testData <- read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
testDataAct <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
testDataSub <- read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
trainDataAct <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
trainDataSub <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)

#Read the description and name the data with descriptive activities
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE,colClasses="character")
features <- read.table("./UCI HAR Dataset/features.txt",header=FALSE,colClasses="character")

testDataAct$V1 <- factor(testDataAct$V1,levels=activityLabels$V1,labels=activityLabels$V2)
trainDataAct$V1 <- factor(trainDataAct$V1,levels=activityLabels$V1,labels=activityLabels$V2)
colnames(testData)<-features$V2
colnames(trainData)<-features$V2
colnames(testDataAct)<-c("Activity")
colnames(trainDataAct)<-c("Activity")
colnames(testDataSub)<-c("Subject")
colnames(trainDataSub)<-c("Subject")

#Find the mean and sd columns
meanCol <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
sdCol <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
featuresMS<-cbind(features, meanCol, sdCol)
feaMS<-featuresMS[which(meanCol==TRUE|sdCol==TRUE),]
meanstd<-feaMS$V2
meanstd<-as.vector(meanstd)
testData1<-testData[,meanstd]
trainData1<-trainData[,meanstd]

#Merge all data to one dataset
testDataMerge<-cbind(testDataAct,testDataSub,testData1)
trainDataMerge<-cbind(trainDataAct,trainDataSub,trainData1)
allDataMerge<-rbind(testDataMerge,trainDataMerge)

#Calculate mean by Activity and Subject
tidy2 = aggregate(allDataMerge[,3:66], by=list(activity=allDataMerge$Activity, subject=allDataMerge$Subject), FUN=mean, na.action=na.omit)
write.table(tidy2, file="tidy2.txt",sep=",",row.names = FALSE)