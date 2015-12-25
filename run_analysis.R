#######################################################################################################
# This is a R code to demonstrate the steps in creating tidy dataset.
# It is a part of the Cleaning Data assignment.
#
# The Samsung dataset is used, and multiple files having the required information are
# merged and processed to generate a summary report for a tidy dataset.
########################################################################################################
#
########################################################################################################
#Get the current working directory (In the current directory the datasets are stored.)
########################################################################################################

CurrDir<-getwd()

########################################################################################################
#The files are stored in sub-directories test and train respectively.
# Get the data stored in train sub-directory first.
########################################################################################################

Train.data<- read.table(paste(CurrDir,"/train/X_train.txt",sep=""))
Train.Activity<-read.table(paste(CurrDir,"/train/y_train.txt",sep=""))
Train.Subject<-read.table(paste(CurrDir,"/train/subject_train.txt",sep=""))

########################################################################################################
#Get the data stored in test sub-directory next
########################################################################################################

Test.data<- read.table(paste(CurrDir,"/test/X_test.txt",sep=""))
Test.Activity<-read.table(paste(CurrDir,"/test/y_test.txt",sep=""))
Test.Subject<-read.table(paste(CurrDir,"/test/subject_test.txt",sep=""))

########################################################################################################
#Now read the labels for the features and activities descriptors.
########################################################################################################

Labels.Data<-read.table(paste(CurrDir,"/features.txt",sep=""))
Activity.Desc<-read.table(paste(CurrDir,"/activity_labels.txt",sep=""))

########################################################################################################
# Now we have all the Raw data stored.
# Tidy Data Code begins.

#1. First assign the features labels to names of variables in the Test 
#  Training datasets. (Train.data & Test.data)
########################################################################################################

names(Train.data)<- Labels.Data[,2]
names(Test.data)<- Labels.Data[,2]

names(Train.Activity)<- "Activity" 
names(Test.Activity)<- "Activity" 

names(Train.Subject)<- "Subject" 
names(Test.Subject)<- "Subject" 
names(Activity.Desc)<- c("Activity","ActivityDescription")

library(dplyr)

########################################################################################################
#Now select the columns that have the mean and standard Deviation only.
########################################################################################################

Vec.Mean.Test<- grep("-mean()",names(Test.data),fixed=TRUE)
Vec.Std.Test<- grep("-std()",names(Test.data),fixed=TRUE)
Subset.Test <- Test.data[,c(Vec.Mean.Test,Vec.Std.Test)]

########################################################################################################
#Just to be sure, although both the datasets have columns 
# ordered in the same order
########################################################################################################

Vec.Mean.Train<- grep("-mean()",names(Train.data),fixed=TRUE)
Vec.Std.Train<- grep("-std()",names(Train.data),fixed=TRUE)
Subset.Train <- Train.data[,c(Vec.Mean.Train,Vec.Std.Train)]

########################################################################################################
# Now column bind the data, Activity and subject - Test and Train
########################################################################################################

Train.Dataset<- cbind(Subset.Train,Train.Subject,Train.Activity)
Test.Dataset<- cbind(Subset.Test,Test.Subject,Test.Activity)

########################################################################################################
#Merge Activity description into the Train and Test Datasets. This will
# now have the description of the activity instead of merely coded number
# so that it is readable.
########################################################################################################

Merged.Test.Activity<-merge(Test.Dataset,Activity.Desc,by="Activity",sort=FALSE)
Merged.Train.Activity<-merge(Train.Dataset,Activity.Desc,by="Activity",sort=FALSE)

########################################################################################################
# Now that we have the Descriptive activity in the training and test datasets, rbind them to create
# a single dataset.  This is the final tidy dataset
########################################################################################################

Final.Dataset <- rbind(Merged.Train.Activity,Merged.Test.Activity)

########################################################################################################
#Now summarize the date by Subject and Activity description, and save the output in text file
########################################################################################################

Summary.Table<- (Final.Dataset%>%
                      group_by(Subject,ActivityDescription) %>%
                      summarise_each(funs( mean)))

print(Summary.Table)

write.table(arrange(Summary.Table,Subject,Activity),"./run_analysis.txt",sep=" ",row.name=FALSE) 

########################################################################################################
