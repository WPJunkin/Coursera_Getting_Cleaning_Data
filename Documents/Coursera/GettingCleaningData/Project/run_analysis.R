# You should create one R script called run_analysis.R that does the following. 
  #1.Merges the training and the test sets to create one data set.
  #2.Extracts only the measurements on the mean and standard deviation for each measurement. 
  #3.Uses descriptive activity names to name the activities in the data set
  #4.Appropriately labels the data set with descriptive variable names. 
  #5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

setwd("C:\\Users\\William\\Documents\\Coursera\\GettingCleaningData\\Project\\UCI HAR Dataset\\")

#Read in all data
features<-read.table("features.txt",  header=F)
activity_labels<-read.table("activity_labels.txt",  header=F)
X_train<-read.table("train\\X_train.txt", header=F)
Y_train<-read.table("train\\Y_train.txt", header=F)
X_test<-read.table("test\\X_test.txt", header=F)
Y_test<-read.table("test\\Y_test.txt", header=F)

subject_train<-read.table("train\\subject_train.txt", header=F)
subject_test<-read.table("test\\subject_test.txt", header=F)

body_acc_x_train<-read.table("train\\Inertial Signals\\body_acc_x_train.txt", header=F)
body_acc_y_train<-read.table("train\\Inertial Signals\\body_acc_y_train.txt", header=F)
body_acc_z_train<-read.table("train\\Inertial Signals\\body_acc_z_train.txt", header=F)

body_gyro_x_train<-read.table("train\\Inertial Signals\\body_gyro_x_train.txt", header=F)
body_gyro_y_train<-read.table("train\\Inertial Signals\\body_gyro_y_train.txt", header=F)
body_gyro_z_train<-read.table("train\\Inertial Signals\\body_gyro_z_train.txt", header=F)

total_acc_x_train<-read.table("train\\Inertial Signals\\total_acc_x_train.txt", header=F)
total_acc_y_train<-read.table("train\\Inertial Signals\\total_acc_y_train.txt", header=F)
total_acc_z_train<-read.table("train\\Inertial Signals\\total_acc_z_train.txt", header=F)

body_acc_x_test<-read.table("test\\Inertial Signals\\body_acc_x_test.txt", header=F)
body_acc_y_test<-read.table("test\\Inertial Signals\\body_acc_y_test.txt", header=F)
body_acc_z_test<-read.table("test\\Inertial Signals\\body_acc_z_test.txt", header=F)

body_gyro_x_test<-read.table("test\\Inertial Signals\\body_gyro_x_test.txt", header=F)
body_gyro_y_test<-read.table("test\\Inertial Signals\\body_gyro_y_test.txt", header=F)
body_gyro_z_test<-read.table("test\\Inertial Signals\\body_gyro_z_test.txt", header=F)

total_acc_x_test<-read.table("test\\Inertial Signals\\total_acc_x_test.txt", header=F)
total_acc_y_test<-read.table("test\\Inertial Signals\\total_acc_y_test.txt", header=F)
total_acc_z_test<-read.table("test\\Inertial Signals\\total_acc_z_test.txt", header=F)

#check out the dimensions to aid in understanding
dim(body_gyro_x_train)
dim(body_acc_y_test)
dim(body_acc_z_train)
dim(body_acc_x_train)
dim(X_train)
dim(Y_train)
dim(X_test)
dim(Y_test)
dim(subject_train)
dim(subject_test)
dim(features)

#start processing--I began working on it before reading full instructions. I found it easier
#to name things first as it helped me to combine them, etc.
colnames(X_train)<-features[,2]
colnames(X_test)<-features[,2]
colnames(subject_train)<-c("Subject_ID")
colnames(subject_test)<-c("Subject_ID")

X_train<-cbind(subject_train,X_train)
X_test<-cbind(subject_test,X_test)

#label this as training
X_train<-cbind(rep("Train",nrow(X_train)),X_train)
colnames(X_train)[1]<-c("Source")

#label this as test
X_test<-cbind(rep("Test",nrow(X_test)),X_test)
colnames(X_test)[1]<-c("Source")

#more labeling
colnames(Y_train)<-c("Activity_Label_ID")
colnames(Y_test)<-c("Activity_Label_ID")
table(Y_train)
head(X_train)

colnames(activity_labels)<-c("Activity_Label_ID", "Activity_Desc")

#Now, merge by activity, note cannot just cbind them
Y_train<-merge(Y_train,activity_labels,  by.x="Activity_Label_ID", by.y="Activity_Label_ID",all=T)
head(Y_train)

Y_test<-merge(Y_test,activity_labels,  by.x="Activity_Label_ID", by.y="Activity_Label_ID",all=T)
head(Y_test)


#put the activity labels in there
X_train<-cbind(X_train[,1:2],Y_train,X_train[,3:ncol(X_train)])
X_test<-cbind(X_test[,1:2],Y_test,X_test[,3:ncol(X_test)])

#Now put them all together
final1<-rbind(X_train,X_test)


#Now just get the mean and std columns
str(final1)
c1<-colnames(final1)[grep("mean",colnames(final1))]
c2<-colnames(final1)[grep("std",colnames(final1))]

cn<-c("Source", "Subject_ID", "Activity_Label_ID", "Activity_Desc" , c1,c2)
final2<-final1[,cn]
#Check
dim(final1)
dim(final2)
colnames(final2)

#Now, let's clean it up a little more, remove brackets, pluses, minuses, etc. 
colnames(final2)<-sub('[(]+[)]', "", colnames(final2))
colnames(final2)<-gsub('-', "_", colnames(final2))

str(final2)

#Now to take the means by subject and activty. aggregate() is an easy way to do this
final3 <- aggregate(final2[, -(1:4)], list(subject=final2$Subject_ID, 
                        activity=final2$Activity_Desc), mean, na.rm=TRUE)
dim(final3)

#now save it w/o headers per instructions
write.table(final3, file="final_stats.txt", row.names=FALSE)

#I do this one on my own just for QA
write.table(final3, file="final_stats.csv", row.names=T, sep=",")

