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



library(plyr)
library(sqldf)

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
sum(mtrain2[,2:15]) ==length(sessions[sessions$user_id %in% train$id,5])
sum(mtrain2[,1] != train[,1])


write.csv(mtrain2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainDeviceType.csv')














	#initialize the matrix	
	 mtest2  = as.data.frame(matrix(nrow = nrow(test),
					ncol = 15))
	mtest2[,2:15] = 0
	mtest2[,1] = test[,1]
	mtest2 = rename(mtest2, c('V1' = 'id'))
	#gets the observations of sessions which user_id are in test
	testSes = sessions[which(sessions$user_id %in% test$id),]


	#validation for testSes
	nrow(testSes) == length(which(sessions$user_id %in% test$id))
	ncol(testSes) == ncol(sessions)
	


#gets the unique 14 sessions action_detail names as strings
feature_name = as.character(sessions[!duplicated(sessions[,5]),5]) 
for(i in 1:14)
{

	#gets the value of each unique action_name
	#then uses those as a column name
	#starts at i+1 cause the first 1 columns are 
	colnames(mtest2)[i + 1] = 
	as.character(feature_name[i])
}
ncol(mtest2)
#checks to make sure column names is equal to number of unique sessions$actions
sum(unique(sessions$device_type)  != colnames(mtest2)[2:15])

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
		column_num = which( actionName == as.character(testSes$device_type[z]))

	}else{
	
		test_row  = which(test$id == as.character(testSes$user_id[z]))
		
		#getting the column which corresponds to 'action x' 
		column_num = which(actionName == as.character(testSes$device_type[z]))
	}

	mtest2[test_row,column_num] = mtest2[test_row,column_num] + 1

}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for test observations
sum(mtest2[,2:15]) ==length(sessions[sessions$user_id %in% test$id,5])
sum(mtest2[,1] != test[,1])


write.csv(mtest2, row.names = FALSE,
 'C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testDeviceType.csv')













############################################################################
#	manipulate age/gender prior
#
#
#
#
#
############################################################################



#initializing my prior distribution data frame
AGPrior = as.data.frame(matrix(nrow = nrow(age_gender_bkts)*5,
					ncol = 4))


AGPrior = rename(AGPrior, c("V1" = "country_destination", "V2" = "gender",
					"V3" = "population_in_thousands",
					"V4" = "age"))



#z keeps track of the row in AGPrior
z = 1;
for( i in 1:nrow(age_gender_bkts))
{

	#gets the position of where the dash is
	#ex: 5-9, pos = 2
	#ex: 15-19, pos = 3
	#ex: 100+, pos = -1
	#statements are exectued depending on where the pos is

	pos = regexpr('-', age_gender_bkts$age_bucket[i])[[1]]

	if (pos == -1)
	{

		#iterates over ages 100 to 104
		for(d in (100:104))
		{
			#places country in first column of AGPrior
			AGPrior[z,1] = as.character(age_gender_bkts[i,2])

			#places gender in second column of AGPrior
			AGPrior[z,2] = as.character(age_gender_bkts[i,3])
	
			#places population in third column of AGPrior
			AGPrior[z,3] = age_gender_bkts[i,4]

			#places the age in fourth column
			AGPrior[z,4] = d;
			
			#increments the row of AGPrior
			z = z + 1;
		}
		
	}else if (pos ==2)
	{


		#Start and end of the substring for the age bracket
		#ex: 5-9 has start = 5 and end = 9
		start = as.numeric(substr(age_gender_bkts$age_bucket[i],1,1));
		end = as.numeric(substr(age_gender_bkts$age_bucket[i],3,3));


		for(d in (start:end))
		{

			#places country in first column of AGPrior
			AGPrior[z,1] = as.character(age_gender_bkts[i,2])

			#places gender in second column of AGPrior
			AGPrior[z,2] = as.character(age_gender_bkts[i,3])
	
			#places population in third column of AGPrior
			AGPrior[z,3] = age_gender_bkts[i,4]

			#places the age in fourth column
			AGPrior[z,4] = d;
			
			#increments the row of AGPrior
			z = z + 1;

		}

	}else if (pos ==3)
	{

		#Start and end of the substring for the age bracket
		#ex: 95-99 has start = 95 and end = 99
		start = as.numeric(substr(age_gender_bkts$age_bucket[i],1,2));
		end = as.numeric(substr(age_gender_bkts$age_bucket[i],4,5));


		#makes sure each age in the range has a match
		for(d in (start:end))
		{

			#places country in first column of AGPrior
			AGPrior[z,1] = as.character(age_gender_bkts[i,2])

			#places gender in second column of AGPrior
			AGPrior[z,2] = as.character(age_gender_bkts[i,3])
	
			#places population in third column of AGPrior
			AGPrior[z,3] = age_gender_bkts[i,4]

			#places the age in fourth column
			AGPrior[z,4] = d;
			
			#increments the row of AGPrior
			z = z + 1;
		}

	}


}


sum(is.na(AGPrior))

nrow(age_gender_bkts)*5 == nrow(AGPrior)




#merging the prior with train


#initialize the data frame
genderTrain = as.data.frame(matrix(nrow = nrow(train),
					ncol = 13))


genderTrain = rename(genderTrain, c("V1" = "id", "V2" = "NDF",
					"V3" = "other",
					"V4" = "US",
					
					"V5" = "FR", "V6" = "CA", "V7" = "GB",
					"V8" = "ES", "V9" = "IT", "V10" = "PT",
					"V11" = "NL", "V12" = "DE", "V13" = "AU"))


#add train id and 0 out nas 
genderTrain[,1] = train[,1]

genderTrain[,2:13] = 0




names = colnames(genderTrain)[4:13]
for (i in 1:nrow(train))
{
	gender = as.character(train$gender[i])

	if (gender == "MALE" || gender == "FEMALE")
	{
		for (z in 1:9)
		{
			temp = names[z]
			sqldf("select population_in_thousands from	
				age_gender_bkts where
				country_destination = temp;")
			
		}
	}
}






















