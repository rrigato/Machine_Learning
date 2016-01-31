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
	 mtrain2  = train
	mtrain2[,17:376] = 0

	#gets the observations of sessions which user_id are in train
	trainSes = sessions[which(sessions$user_id %in% train$id),]


	#validation for trainSes
	nrow(trainSes) == length(which(sessions$user_id %in% train$id))
	ncol(trainSes) == ncol(sessions)
	

	
	length(which(sessions$user_id %in% test$id)) + length(which(sessions$user_id %in% train$id))


#gets the unique 360 sessions Action names as strings
feature_name = as.character(sessions[!duplicated(sessions[,2]),2]) 
for(i in 1:360)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+16 cause the first 16 columns are 
	colnames(mtrain2)[i + 16] = 
	as.character(feature_name[i])
}
ncol(mtrain2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$action)  != colnames(mtrain2)[17:376])

train_row = 1
column_num = 1



actionName = colnames(mtrain2)

#puts the volume into the observation corresponding to the sessions
#variable name
for(z in 1:nrow(trainSes))
{
	#gets the row in train where the id corresponds to the id in 
	#sessions
	if (train$id[train_row] == as.character(trainSes$user_id[z]))
	{
		#getting the column which corresponds to 'action x' 
		column_num = which( actionName == as.character(trainSes$action[z]))

	}else{
	
		train_row  = which(train$id == as.character(trainSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(trainSes$action[z]))
	}

	mtrain2[train_row,column_num] = mtrain2[train_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtrain2[,17:377]) ==length(sessions[sessions$user_id %in% train$id,2])

#set train equal to mtrain2
train = mtrain2

write.csv(train, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainAction2.csv')








	#initialize the matrix	
	 mtest2  = test
	mtest2[,16:375] = 0

	#gets the observations of sessions which user_id are in test
	testSes = sessions[which(sessions$user_id %in% test$id),]


	#validation for testSes
	nrow(testSes) == length(which(sessions$user_id %in% test$id))
	ncol(testSes) == ncol(sessions)
	
	
	length(which(sessions$user_id %in% test$id)) + length(which(sessions$user_id %in% test$id))


#gets the unique 360 sessions Action names as strings
feature_name = as.character(sessions[!duplicated(sessions[,2]),2]) 
for(i in 1:360)
{

	#gets the value of each unique action
	#then uses those as a column name
	#starts at i+15 cause the first 15 columns are 
	colnames(mtest2)[i + 15] = 
	as.character(feature_name[i])
}
ncol(mtest2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$action)  != colnames(mtest2)[16:375])

test_row = 1
column_num = 1



actionName = colnames(mtest2)

#puts the volume into the observation corresponding to the sessions
#variable name
for(z in 1:nrow(testSes))
{
	#gets the row in test where the id corresponds to the id in 
	#sessions
	if (test$id[test_row] == as.character(testSes$user_id[z]))
	{
		#getting the column which corresponds to 'action x' 
		column_num = which( actionName == as.character(testSes$action[z]))

	}else{
	
		test_row  = which(test$id == as.character(testSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(testSes$action[z]))
	}

	mtest2[test_row,column_num] = mtest2[test_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for test observations
sum(mtest2[,17:377]) ==length(sessions[sessions$user_id %in% test$id,2])

#set test equal to mtest2
test = mtest2

write.csv(test, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testAction2.csv')

















