{
  ##Read all the text files and store them as variables
  ##Assign the appropriate column names
  ##To start with read the x_train data file
  traindata <- read.table("./train/X_train.txt")
  
  ##Read the features data and make as columns for train data
  features <- read.table("features.txt")
  features_names <-  features[,2]
  features_names_transposed<-t(features_names)
  features_names_transposed
  colnames(traindata)<- features_names_transposed
  
  
  ##Read the activities from y_train data and label the columns
  
  train_activity_id <- read.table("./train/y_train.txt")
  colnames(train_activity_id) <- "activity_id"
  
  ##Read the subject_train data and label the columns
  train_subject_id <- read.table("./train/subject_train.txt")
  colnames(train_subject_id) <- "subject_id"
  
  ##get the total train data in to one file by using cbind
  totaltraindata<-cbind(train_subject_id,train_activity_id,traindata)
  
  ##Read the x_test data file
  testdata <- read.table("./test/X_test.txt")
  
  ##label the columns of test data from features as done in case of train data.
  colnames(testdata)<- features_names_transposed
  
  ##Read the activites from y_test data and label the columns
  
  test_activity_id <- read.table("./test/y_test.txt")
  colnames(test_activity_id) <- "activity_id"
  
  ##Read the subject_test data and label the columns
  
  test_subject_id <- read.table("./test/subject_test.txt")
  colnames(test_subject_id) <- "subject_id"
  
  
  
  ##get the total test data in to one file by using cbind
  
  totaltestdata<-cbind(test_subject_id, test_activity_id, testdata)
  
  
  train_test_merged<-rbind(totaltraindata, totaltestdata)
  
  ##Next step is optional to ensure if the data looks OK till now
  
  ##write.csv(train_test_merged, "./a1.csv")
  
  ##call reshape2 library to use "grep" function
  
  library(reshape2)
  
  ## extract all the columns with mean() and std() values
  
  mean_columns <- grep("mean",names(train_test_merged),ignore.case=TRUE)
  mean_column_names <- names(train_test_merged)[mean_columns]
  std_columns <- grep("std",names(train_test_merged),ignore.case=TRUE)
  std_column_names <- names(train_test_merged)[std_columns]
  mean_std_data <-train_test_merged[,c("subject_id","activity_id",mean_column_names,std_column_names)]
  
  ##optional step:look at the mean_std_data
  ##by wrtitng as csv
  ##write.csv(mean_std_data, "./b1.csv")
  
  ## Read activities from activity_labels file and label the aproppriate columns 
  activity_labels <- read.table("./activity_labels.txt",col.names=c("activity_id","activity_name"))
  
  
  ##Merge the activities datase with the mean and std values datase 
  
  mean_std_data_with_activity <- merge(activity_labels,mean_std_data,by.x="activity_id",by.y="activity_id",all=TRUE)
  
##optional step:look at the mean_std_data_with_activity
##by wrtitng as csv
 ##write.csv(mean_std_data_with_activity, "./c1.csv")

  ##Melt the dataset with the descriptive activity names for better handling
  melted_data <- melt(mean_std_data_with_activity,id=c("activity_id","activity_name","subject_id"))
  
  ##Cast the melted dataset according to  the average of each variable 
  ##for each activity and each subject
  meandata <- dcast(melted_data,activity_id + activity_name + subject_id ~ variable,mean)
  
  ## Create a file with the new tidy dataset
  ## optional step: look at meandata by writing as csvs
  ##write.csv(meandata,"./d1.csv")
  write.table(meandata,"./final_tidy_data.txt")
  
}