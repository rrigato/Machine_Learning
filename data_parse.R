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
sum(mtest2[,16:375]) ==length(sessions[sessions$user_id %in% test$id,2])

#set test equal to mtest2
test = mtest2

write.csv(test, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testAction2.csv')




##############################################################################
#Adding action type to train
#
#
#
###############################################################################


	#initialize the matrix	
	 mtrain2  = as.data.frame(matrix(nrow = nrow(train),
					ncol = 12))
	mtrain2[,2:12] = 0
	mtrain2[,1] = train[,1]
	mtrain2 = rename(mtrain2, c('V1' = 'id'))
	#gets the observations of sessions which user_id are in train
	trainSes = sessions[which(sessions$user_id %in% train$id),]


	#validation for trainSes
	nrow(trainSes) == length(which(sessions$user_id %in% train$id))
	ncol(trainSes) == ncol(sessions)
	

	
	length(which(sessions$user_id %in% test$id)) + length(which(sessions$user_id %in% train$id))


#gets the unique 11 sessions action_type names as strings
feature_name = as.character(sessions[!duplicated(sessions[,3]),3]) 
for(i in 1:11)
{

	#gets the value of each unique action_name
	#then uses those as a column name
	#starts at i+1 cause the first 1 columns are 
	colnames(mtrain2)[i + 1] = 
	as.character(feature_name[i])
}
ncol(mtrain2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$action_type)  != colnames(mtrain2)[2:12])

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
		column_num = which( actionName == as.character(trainSes$action_type[z]))

	}else{
	
		train_row  = which(train$id == as.character(trainSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(trainSes$action_type[z]))
	}

	mtrain2[train_row,column_num] = mtrain2[train_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtrain2[,2:12]) ==length(sessions[sessions$user_id %in% train$id,3])
sum(mtrain2[,1] != train[,1])


write.csv(mtrain2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainActionType.csv')












	#initialize the matrix	
	 mtest2  = as.data.frame(matrix(nrow = nrow(test),
					ncol = 12))
	mtest2[,2:12] = 0
	mtest2[,1] = test[,1]
	mtest2 = rename(mtest2, c('V1' = 'id'))
	#gets the observations of sessions which user_id are in test
	testSes = sessions[which(sessions$user_id %in% test$id),]


	#validation for testSes
	nrow(testSes) == length(which(sessions$user_id %in% test$id))
	ncol(testSes) == ncol(sessions)
	

	
	
#gets the unique 11 sessions action_type names as strings
feature_name = as.character(sessions[!duplicated(sessions[,3]),3]) 
for(i in 1:11)
{

	#gets the value of each unique action_name
	#then uses those as a column name
	#starts at i+1 cause the first 1 columns are 
	colnames(mtest2)[i + 1] = 
	as.character(feature_name[i])
}
ncol(mtest2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$action_type)  != colnames(mtest2)[2:12])

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
		column_num = which( actionName == as.character(testSes$action_type[z]))

	}else{
	
		test_row  = which(test$id == as.character(testSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(testSes$action_type[z]))
	}

	mtest2[test_row,column_num] = mtest2[test_row,column_num] + 1

}









#tests to make sure the sum of the volume is equal to the sum of the volume
#for test observations
sum(mtest2[,2:12]) ==length(sessions[sessions$user_id %in% test$id,3])
sum(mtest2[,1] != test[,1])


write.csv(mtest2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testActionType.csv')










##############################################################################
#Adding action type to train
#
#
#
###############################################################################


	#initialize the matrix	
	 mtrain2  = as.data.frame(matrix(nrow = nrow(train),
					ncol = 157))
	mtrain2[,2:157] = 0
	mtrain2[,1] = train[,1]
	mtrain2 = rename(mtrain2, c('V1' = 'id'))
	#gets the observations of sessions which user_id are in train
	trainSes = sessions[which(sessions$user_id %in% train$id),]


	#validation for trainSes
	nrow(trainSes) == length(which(sessions$user_id %in% train$id))
	ncol(trainSes) == ncol(sessions)
	


#gets the unique 156 sessions action_detail names as strings
feature_name = as.character(sessions[!duplicated(sessions[,4]),4]) 
for(i in 1:156)
{

	#gets the value of each unique action_name
	#then uses those as a column name
	#starts at i+1 cause the first 1 columns are 
	colnames(mtrain2)[i + 1] = 
	as.character(feature_name[i])
}
ncol(mtrain2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$action_detail)  != colnames(mtrain2)[2:157])

train_row = 1
column_num = 1



actionName = colnames(mtrain2)

#puts the action_detail into the observation corresponding to the sessions
#variable name
for(z in 1:nrow(trainSes))
{
	#gets the row in train where the id corresponds to the id in 
	#sessions
	if (train$id[train_row] == as.character(trainSes$user_id[z]))
	{
		#getting the column which corresponds to 'action x' 
		column_num = which( actionName == as.character(trainSes$action_detail[z]))

	}else{
	
		train_row  = which(train$id == as.character(trainSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(trainSes$action_detail[z]))
	}

	mtrain2[train_row,column_num] = mtrain2[train_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtrain2[,2:157]) ==length(sessions[sessions$user_id %in% train$id,4])
sum(mtrain2[,1] != train[,1])


write.csv(mtrain2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainActionDetail.csv')










	#initialize the matrix	
	 mtest2  = as.data.frame(matrix(nrow = nrow(test),
					ncol = 157))
	mtest2[,2:157] = 0
	mtest2[,1] = test[,1]
	mtest2 = rename(mtest2, c('V1' = 'id'))
	#gets the observations of sessions which user_id are in test
	testSes = sessions[which(sessions$user_id %in% test$id),]


	#validation for testSes
	nrow(testSes) == length(which(sessions$user_id %in% test$id))
	ncol(testSes) == ncol(sessions)
	


#gets the unique 156 sessions action_detail names as strings
feature_name = as.character(sessions[!duplicated(sessions[,4]),4]) 
for(i in 1:156)
{

	#gets the value of each unique action_name
	#then uses those as a column name
	#starts at i+1 cause the first 1 columns are 
	colnames(mtest2)[i + 1] = 
	as.character(feature_name[i])
}
ncol(mtest2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$action_detail)  != colnames(mtest2)[2:157])

test_row = 1
column_num = 1



actionName = colnames(mtest2)

#puts the action_detail into the observation corresponding to the sessions
#variable name
for(z in 1:nrow(testSes))
{
	#gets the row in test where the id corresponds to the id in 
	#sessions
	if (test$id[test_row] == as.character(testSes$user_id[z]))
	{
		#getting the column which corresponds to 'action x' 
		column_num = which( actionName == as.character(testSes$action_detail[z]))

	}else{
	
		test_row  = which(test$id == as.character(testSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(testSes$action_detail[z]))
	}

	mtest2[test_row,column_num] = mtest2[test_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for test observations
sum(mtest2[,2:157]) ==length(sessions[sessions$user_id %in% test$id,4])
sum(mtest2[,1] != test[,1])


write.csv(mtest2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testActionDetail.csv')












##############################################################################
#Adding device_type to train
#
#
#
###############################################################################


	#initialize the matrix	
	 mtrain2  = as.data.frame(matrix(nrow = nrow(train),
					ncol = 15))
	mtrain2[,2:15] = 0
	mtrain2[,1] = train[,1]
	mtrain2 = rename(mtrain2, c('V1' = 'id'))
	#gets the observations of sessions which user_id are in train
	trainSes = sessions[which(sessions$user_id %in% train$id),]


	#validation for trainSes
	nrow(trainSes) == length(which(sessions$user_id %in% train$id))
	ncol(trainSes) == ncol(sessions)
	


#gets the unique 14 sessions action_detail names as strings
feature_name = as.character(sessions[!duplicated(sessions[,5]),5]) 
for(i in 1:14)
{

	#gets the value of each unique action_name
	#then uses those as a column name
	#starts at i+1 cause the first 1 columns are 
	colnames(mtrain2)[i + 1] = 
	as.character(feature_name[i])
}
ncol(mtrain2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$device_type)  != colnames(mtrain2)[2:15])

train_row = 1
column_num = 1



actionName = colnames(mtrain2)

#puts the action_detail into the observation corresponding to the sessions
#variable name
for(z in 1:nrow(trainSes))
{
	#gets the row in train where the id corresponds to the id in 
	#sessions
	if (train$id[train_row] == as.character(trainSes$user_id[z]))
	{
		#getting the column which corresponds to 'action x' 
		column_num = which( actionName == as.character(trainSes$device_type[z]))

	}else{
	
		train_row  = which(train$id == as.character(trainSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(trainSes$device_type[z]))
	}

	mtrain2[train_row,column_num] = mtrain2[train_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtrain2[,2:157]) ==length(sessions[sessions$user_id %in% train$id,4])
sum(mtrain2[,1] != train[,1])


write.csv(mtrain2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainActionDetail.csv')


























############################################################################
#	manipulate age/gender prior
#
#
#
#
#
############################################################################

AGPrior = as.data.frame(matrix(nrow = nrow(age_gender_bkts)*5,
					ncol = ncol(age_gender_bkts) + 1))


AGPrior[,1:3] = as.character(AGPrior[,1:3])
z = 1;
for( i in 1:nrow(age_gender_bkts))
{

	pos = regexpr('-', age_gender_bkts$age_bucket[i])
	if (pos == -1)
	{
		for(d in (100:104))
		{
			AGPrior[z,1:5]= age_gender_bkts[i,1:5];
			AGPrior[z,ncol(age_gender_bkts) + 1] = d;
			z = z + 1;
		}
		
	}else if (pos ==2)
	{
		start = as.numeric(substr(age_gender_bkts$age_bucket[i],1,1));
		end = as.numeric(substr(age_gender_bkts$age_bucket[i],3,3));
		for(d in (start:end))
		{

			AGPrior[z,ncol(age_gender_bkts) + 1] = d;
			AGPrior[z,1:5]= age_gender_bkts[i,1:5];
			
			z = z + 1;
		}

	}else if (pos ==3)
	{
		start = as.numeric(substr(age_gender_bkts$age_bucket[i],1,2));
		end = as.numeric(substr(age_gender_bkts$age_bucket[i],4,5));
		for(d in (start:end))
		{
			AGPrior[z,1:5]= age_gender_bkts[i,1:5];
			AGPrior[z,ncol(age_gender_bkts) + 1] = d;
			z = z + 1;
		}

	}


}






















