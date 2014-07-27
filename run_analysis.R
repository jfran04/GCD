run_analysis <- function() {
  ##Set the working Directory
  setwd("C:/Users/Jon/Documents/Data Scientist/03 - Getting & Cleaning Data/Projects/UCI HAR Dataset")
  
  ##Read in features table.  This is the column names
  features <-read.table("./features.txt", header=FALSE)
  features <- t(features$V2)
  
  ##Read in Main Data  (X Data)
  X_test <-read.table("./test/X_test.txt", header=FALSE)
  X_train <-read.table("./train/X_train.txt", header=FALSE)
  X_data=rbind(X_test,X_train)
  X_data <- setNames(X_data,features)
  
  ##Read in Y Data
  y_test <-read.table("./test/y_test.txt", header=FALSE)
  y_train <-read.table("./train/y_train.txt", header=FALSE)
  y_data <- rbind(y_test,y_train)
  y_data <- setNames(y_data, c("label"))
  
  ##Read in Subject Data
  subject_test <-read.table("./test/subject_test.txt", header=FALSE)
  subject_train <-read.table("./train/subject_train.txt", header=FALSE)
  subject_data <- rbind(subject_test,subject_train)
  subject_data <- setNames(subject_data, c("subject"))
  
  ## Combine X, Y and Subject Data
  combined_data <- cbind(y_data, subject_data, X_data)
  
  ## extract columns that measure mean or stdev
  cd_test_exact <- combined_data[,c('label','subject','tBodyAcc-mean()-X', 'tBodyAcc-mean()-Y', 'tBodyAcc-mean()-Z', 'tBodyAcc-std()-X', 'tBodyAcc-std()-Y', 'tBodyAcc-std()-Z', 'tGravityAcc-mean()-X', 'tGravityAcc-mean()-Y', 'tGravityAcc-mean()-Z', 'tGravityAcc-std()-X', 'tGravityAcc-std()-Y', 'tGravityAcc-std()-Z', 'tBodyAccJerk-mean()-X', 'tBodyAccJerk-mean()-Y', 'tBodyAccJerk-mean()-Z', 'tBodyAccJerk-std()-X', 'tBodyAccJerk-std()-Y', 'tBodyAccJerk-std()-Z', 'tBodyGyro-mean()-X', 'tBodyGyro-mean()-Y', 'tBodyGyro-mean()-Z', 'tBodyGyro-std()-X', 'tBodyGyro-std()-Y', 'tBodyGyro-std()-Z', 'tBodyGyroJerk-mean()-X', 'tBodyGyroJerk-mean()-Y', 'tBodyGyroJerk-mean()-Z', 'tBodyGyroJerk-std()-X', 'tBodyGyroJerk-std()-Y', 'tBodyGyroJerk-std()-Z', 'tBodyAccMag-mean()', 'tBodyAccMag-std()', 'tGravityAccMag-mean()', 'tGravityAccMag-std()', 'tBodyAccJerkMag-mean()', 'tBodyAccJerkMag-std()', 'tBodyGyroMag-mean()', 'tBodyGyroMag-std()', 'tBodyGyroJerkMag-mean()', 'tBodyGyroJerkMag-std()', 'fBodyAcc-mean()-X', 'fBodyAcc-mean()-Y', 'fBodyAcc-mean()-Z', 'fBodyAcc-std()-X', 'fBodyAcc-std()-Y', 'fBodyAcc-std()-Z', 'fBodyAcc-meanFreq()-X', 'fBodyAcc-meanFreq()-Y', 'fBodyAcc-meanFreq()-Z', 'fBodyAccJerk-mean()-X', 'fBodyAccJerk-mean()-Y', 'fBodyAccJerk-mean()-Z', 'fBodyAccJerk-std()-X', 'fBodyAccJerk-std()-Y', 'fBodyAccJerk-std()-Z', 'fBodyAccJerk-meanFreq()-X', 'fBodyAccJerk-meanFreq()-Y', 'fBodyAccJerk-meanFreq()-Z', 'fBodyGyro-mean()-X', 'fBodyGyro-mean()-Y', 'fBodyGyro-mean()-Z', 'fBodyGyro-std()-X', 'fBodyGyro-std()-Y', 'fBodyGyro-std()-Z', 'fBodyGyro-meanFreq()-X', 'fBodyGyro-meanFreq()-Y', 'fBodyGyro-meanFreq()-Z', 'fBodyAccMag-mean()', 'fBodyAccMag-std()', 'fBodyAccMag-meanFreq()', 'fBodyBodyAccJerkMag-mean()', 'fBodyBodyAccJerkMag-std()', 'fBodyBodyAccJerkMag-meanFreq()', 'fBodyBodyGyroMag-mean()', 'fBodyBodyGyroMag-std()', 'fBodyBodyGyroMag-meanFreq()', 'fBodyBodyGyroJerkMag-mean()', 'fBodyBodyGyroJerkMag-std()', 'fBodyBodyGyroJerkMag-meanFreq()', 'angle(tBodyAccMean,gravity)', 'angle(tBodyAccJerkMean),gravityMean)', 'angle(tBodyGyroMean,gravityMean)', 'angle(tBodyGyroJerkMean,gravityMean)', 'angle(X,gravityMean)', 'angle(Y,gravityMean)', 'angle(Z,gravityMean)')]
  
  ## Apply activity names to the label codes
  cd_test_exact$label[which(cd_test_exact$label=="1")]<-"WALKING"
  cd_test_exact$label[which(cd_test_exact$label=="2")]<-"WALKING_UPSTAIRS"
  cd_test_exact$label[which(cd_test_exact$label=="3")]<-"WALKING_DOWNSTAIRS"
  cd_test_exact$label[which(cd_test_exact$label=="4")]<-"SITTING"
  cd_test_exact$label[which(cd_test_exact$label=="5")]<-"STANDING"
  cd_test_exact$label[which(cd_test_exact$label=="6")]<-"LAYING"
  
  ##Load the plyr package & Rename the variables (column headers)
  library(plyr)
  cd_test_exact <- rename(cd_test_exact, c("tBodyAcc-mean()-X"="tBodyAccMeanX", "tBodyAcc-mean()-Y"="tBodyAccMeanY", "tBodyAcc-mean()-Z"="tBodyAccMeanZ", "tBodyAcc-std()-X"="tBodyAccStdevX", "tBodyAcc-std()-Y"="tBodyAccStdevY", "tBodyAcc-std()-Z"="tBodyAccStdevZ", "tGravityAcc-mean()-X"="tGravityAccMeanX", "tGravityAcc-mean()-Y"="tGravityAccMeanY", "tGravityAcc-mean()-Z"="tGravityAccMeanZ", "tGravityAcc-std()-X"="tGravityAccStdevX", "tGravityAcc-std()-Y"="tGravityAccStdevY", "tGravityAcc-std()-Z"="tGravityAccStdevZ", "tBodyAccJerk-mean()-X"="tBodyAccJerkMeanX", "tBodyAccJerk-mean()-Y"="tBodyAccJerkMeanY", "tBodyAccJerk-mean()-Z"="tBodyAccJerkMeanZ", "tBodyAccJerk-std()-X"="tBodyAccJerkStdevX", "tBodyAccJerk-std()-Y"="tBodyAccJerkStdevY", "tBodyAccJerk-std()-Z"="tBodyAccJerkStdevZ", "tBodyGyro-mean()-X"="tBodyGyroMeanX", "tBodyGyro-mean()-Y"="tBodyGyroMeanY", "tBodyGyro-mean()-Z"="tBodyGyroMeanZ", "tBodyGyro-std()-X"="tBodyGyroStdevX", "tBodyGyro-std()-Y"="tBodyGyroStdevY", "tBodyGyro-std()-Z"="tBodyGyroStdevZ", "tBodyGyroJerk-mean()-X"="tBodyGyroJerkMeanX", "tBodyGyroJerk-mean()-Y"="tBodyGyroJerkMeanY", "tBodyGyroJerk-mean()-Z"="tBodyGyroJerkMeanZ", "tBodyGyroJerk-std()-X"="tBodyGyroJerkStdevX", "tBodyGyroJerk-std()-Y"="tBodyGyroJerkStdevY", "tBodyGyroJerk-std()-Z"="tBodyGyroJerkStdevZ", "tBodyAccMag-mean()"="tBodyAccMagMean", "tBodyAccMag-std()"="tBodyAccMagStdev", "tGravityAccMag-mean()"="tGravityAccMagMean", "tGravityAccMag-std()"="tGravityAccMagStdev", "tBodyAccJerkMag-mean()"="tBodyAccJerkMagMean", "tBodyAccJerkMag-std()"="tBodyAccJerkMagStdev", "tBodyGyroMag-mean()"="tBodyGyroMagMean", "tBodyGyroMag-std()"="tBodyGyroMagStdev", "tBodyGyroJerkMag-mean()"="tBodyGyroJerkMagMean", "tBodyGyroJerkMag-std()"="tBodyGyroJerkMagStdev", "fBodyAcc-mean()-X"="fBodyAccMeanX", "fBodyAcc-mean()-Y"="fBodyAccMeanY", "fBodyAcc-mean()-Z"="fBodyAccMeanZ", "fBodyAcc-std()-X"="fBodyAccStdevX", "fBodyAcc-std()-Y"="fBodyAccStdevY", "fBodyAcc-std()-Z"="fBodyAccStdevZ", "fBodyAcc-meanFreq()-X"="fBodyAccMeanFreqX", "fBodyAcc-meanFreq()-Y"="fBodyAccMeanFreqY", "fBodyAcc-meanFreq()-Z"="fBodyAccMeanFreqZ", "fBodyAccJerk-mean()-X"="fBodyAccJerkMeanX", "fBodyAccJerk-mean()-Y"="fBodyAccJerkMeanY", "fBodyAccJerk-mean()-Z"="fBodyAccJerkMeanZ", "fBodyAccJerk-std()-X"="fBodyAccJerkStdevX", "fBodyAccJerk-std()-Y"="fBodyAccJerkStdevY", "fBodyAccJerk-std()-Z"="fBodyAccJerkStdevZ", "fBodyAccJerk-meanFreq()-X"="fBodyAccJerkMeanFreqX", "fBodyAccJerk-meanFreq()-Y"="fBodyAccJerkMeanFreqY", "fBodyAccJerk-meanFreq()-Z"="fBodyAccJerkMeanFreqZ", "fBodyGyro-mean()-X"="fBodyGyroMeanX", "fBodyGyro-mean()-Y"="fBodyGyroMeanY", "fBodyGyro-mean()-Z"="fBodyGyroMeanZ", "fBodyGyro-std()-X"="fBodyGyroStdevX", "fBodyGyro-std()-Y"="fBodyGyroStdevY", "fBodyGyro-std()-Z"="fBodyGyroStdevZ", "fBodyGyro-meanFreq()-X"="fBodyGyroMeanFreqX", "fBodyGyro-meanFreq()-Y"="fBodyGyroMeanFreqY", "fBodyGyro-meanFreq()-Z"="fBodyGyroMeanFreqZ", "fBodyAccMag-mean()"="fBodyAccMagMean", "fBodyAccMag-std()"="fBodyAccMagStdev", "fBodyAccMag-meanFreq()"="fBodyAccMagMeanFreq", "fBodyBodyAccJerkMag-mean()"="fBodyBodyAccJerkMagMean", "fBodyBodyAccJerkMag-std()"="fBodyBodyAccJerkMagStdev", "fBodyBodyAccJerkMag-meanFreq()"="fBodyBodyAccJerkMagMeanFreq", "fBodyBodyGyroMag-mean()"="fBodyBodyGyroMagMean", "fBodyBodyGyroMag-std()"="fBodyBodyGyroMagStdev", "fBodyBodyGyroMag-meanFreq()"="fBodyBodyGyroMagMeanFreq", "fBodyBodyGyroJerkMag-mean()"="fBodyBodyGyroJerkMagMean", "fBodyBodyGyroJerkMag-std()"="fBodyBodyGyroJerkMagStdev", "fBodyBodyGyroJerkMag-meanFreq()"="fBodyBodyGyroJerkMagMeanFreq", "angle(tBodyAccMean,gravity)"="angleTBodyAccMeanGravity", "angle(tBodyAccJerkMean),gravityMean)"="angleTBodyAccJerkMeanGravityMean", "angle(tBodyGyroMean,gravityMean)"="angleTBodyGyroMeanGravityMean", "angle(tBodyGyroJerkMean,gravityMean)"="angleTBodyGyroJerkMeanGravityMean)", "angle(X,gravityMean)"="angleXGravityMean", "angle(Y,gravityMean)"="angleYGravityMean", "angle(Z,gravityMean)"="angleZGravityMean"))
  
  ##Load the reshape2 package
  library(reshape2)
  
  ## set up data to calculate mean for each activity (label), subject
  cd_test_exact_analysis <- melt(cd_test_exact, id=c("label","subject"), measure.vars = c("tBodyAccMeanX", "tBodyAccMeanY", "tBodyAccMeanZ", "tBodyAccStdevX", "tBodyAccStdevY", "tBodyAccStdevZ", "tGravityAccMeanX", "tGravityAccMeanY", "tGravityAccMeanZ", "tGravityAccStdevX", "tGravityAccStdevY", "tGravityAccStdevZ", "tBodyAccJerkMeanX", "tBodyAccJerkMeanY", "tBodyAccJerkMeanZ", "tBodyAccJerkStdevX", "tBodyAccJerkStdevY", "tBodyAccJerkStdevZ", "tBodyGyroMeanX", "tBodyGyroMeanY", "tBodyGyroMeanZ", "tBodyGyroStdevX", "tBodyGyroStdevY", "tBodyGyroStdevZ", "tBodyGyroJerkMeanX", "tBodyGyroJerkMeanY", "tBodyGyroJerkMeanZ", "tBodyGyroJerkStdevX", "tBodyGyroJerkStdevY", "tBodyGyroJerkStdevZ", "tBodyAccMagMean", "tBodyAccMagStdev", "tGravityAccMagMean", "tGravityAccMagStdev", "tBodyAccJerkMagMean", "tBodyAccJerkMagStdev", "tBodyGyroMagMean", "tBodyGyroMagStdev", "tBodyGyroJerkMagMean", "tBodyGyroJerkMagStdev", "fBodyAccMeanX", "fBodyAccMeanY", "fBodyAccMeanZ", "fBodyAccStdevX", "fBodyAccStdevY", "fBodyAccStdevZ", "fBodyAccMeanFreqX", "fBodyAccMeanFreqY", "fBodyAccMeanFreqZ", "fBodyAccJerkMeanX", "fBodyAccJerkMeanY", "fBodyAccJerkMeanZ", "fBodyAccJerkStdevX", "fBodyAccJerkStdevY", "fBodyAccJerkStdevZ", "fBodyAccJerkMeanFreqX", "fBodyAccJerkMeanFreqY", "fBodyAccJerkMeanFreqZ", "fBodyGyroMeanX", "fBodyGyroMeanY", "fBodyGyroMeanZ", "fBodyGyroStdevX", "fBodyGyroStdevY", "fBodyGyroStdevZ", "fBodyGyroMeanFreqX", "fBodyGyroMeanFreqY", "fBodyGyroMeanFreqZ", "fBodyAccMagMean", "fBodyAccMagStdev", "fBodyAccMagMeanFreq", "fBodyBodyAccJerkMagMean", "fBodyBodyAccJerkMagStdev", "fBodyBodyAccJerkMagMeanFreq", "fBodyBodyGyroMagMean", "fBodyBodyGyroMagStdev", "fBodyBodyGyroMagMeanFreq", "fBodyBodyGyroJerkMagMean", "fBodyBodyGyroJerkMagStdev", "fBodyBodyGyroJerkMagMeanFreq", "angleTBodyAccMeanGravity", "angleTBodyAccJerkMeanGravityMean", "angleTBodyGyroMeanGravityMean", "angleTBodyGyroJerkMeanGravityMean)", "angleXGravityMean", "angleYGravityMean", "angleZGravityMean"))
  
  ##Calculate the mean per measurement 
  final_output <- dcast(cd_test_exact_analysis, label + subject ~ variable, mean)
}