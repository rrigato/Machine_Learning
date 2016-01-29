#################################################################
#Merging the dataset so that they have one row per unique id
#
#
#
##################################################################
#importing the datasets that were provided by Airbnb
train <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\train_users_2.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\test_users.csv")
sessions <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\sessions.csv")
countries <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\countries.csv")
age_gender_bkts <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\age_gender_bkts.csv")



#################################################################
#
#
#
#
################################################################


	#initialize the matrix	
	 mtest2  = train
	mtest2[,17:390] = 0
	mtest2[,1:4] = train[,1:4]
	mtest2 = rename(mtest2, c('V1' = 'id', 'V2' = 'location',
	 'V3' = 'fault_severity', 'V4' = 'severity_type'))


#gets the unique 386 log_feature names as strings
feature_name = as.character(log_feature[!duplicated(log_feature[,2]),2]) 
for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+4 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 4] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the log_feature 
#variable name
for(z in 1:nrow(log_feature))
{
	#gets the row in train where the id corresponds to the id in log_feature
	train_row  = which(train$id == log_feature$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == log_feature$log_feature[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtestm
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + log_feature$volume[z]
	}
}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtest2[,5:390]) ==sum(log_feature[log_feature$id %in% train$id,3])

#set train equal to mtest2
train = mtest2



