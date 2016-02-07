##################################################################################
#	Airbnb script to predict where a customer will book
#
#	Attempt 3
#
#
#
#
#
#
#
#
###################################################################################
install.packages('microbenchmark')
install.packages('compiler')

library(compiler)

#used for testing speed of a function
library(microbenchmark)



library(class)



library(foreign)
#for examining classification and regression trees
library(caret)

#for neuralnetwork analysis
library(neuralnet)

#write to an xlsx file
library(xlsx)
#xgboost
library(DiagrammeR)
library(Ckmeans.1d.dp)
library(xgboost)
library(methods)
library(data.table)
library(magrittr)


#manipulating strings
library(stringr)

library(vcd)
library(plyr)
library(stats)

#sql queries
library(sqldf)

library(MASS)

#decision trees
library(tree)

library(ISLR)
#randomforests
library(randomForest)

library(foreign)
library(nnet)

#naive bayes
library(e1071)

#general boosting models
library(gbm)

#importing the datasets that were provided by Airbnb
train <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainAction2.csv")
trainType <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainActionType.csv")
trainActionDetail <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainActionDetail.csv")
trainDevice <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainDeviceType.csv")



test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testAction2.csv")
testType <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testActionType.csv")
testActionDetail <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testActionDetail.csv")
testDevice <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testDeviceType.csv")


sessions <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\sessions.csv")
countries <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\countries.csv")
age_gender_bkts <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\age_gender_bkts.csv")



train = merge(train, trainType, by = 'id')
train = merge(train, trainActionDetail, by = 'id')
train = merge(train, trainDevice, by = 'id')


test = merge(test, testType, by = 'id')
test = merge(test, testActionDetail, by = 'id')
test = merge(test, testDevice, by = 'id')


################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################




#edit The percentage of the dataset in the train2 and test2, used to build a model 
size_of_train = floor(.8*nrow(train))
ran_num_test = 1:nrow(train)

#gets random numbers for train2 using a sample
ran_num_train = sample(1:nrow(train), size_of_train)

#numbers not randomly selected for train2 are included in test2
#this command gets the numbers not in ran_num_train
ran_num_test = ran_num_test[(!(ran_num_test %in% ran_num_train)) == TRUE]
train2 = train[ran_num_train,]
test2 = train[ran_num_test,]


#######################################################################################
#	.823141 NDCG for 5 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#	.8304302 NDCG for 125 trees .1 shrinkage .8 train .2 test interaction.depth = 3
#	.8308948 NDCG for 200 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#
#	.8284771 NDCG for 100 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#	includes 'Action' variable from sessions
#
#	.8288097 NDCG for 100 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#	includes 'Action' and 'ActionDetail' variable from sessions
#
#
#	.8292822 NDCG for 100 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#	includes 'Action' and 'ActionDetail' and 'browser' variable from sessions
#
#
#
#
#
###############################################################################

remove = numeric()
z = 1
for( i in 17:ncol(train2))
{
	if (sum(train2[,i]) < 150)
	{
		remove[z] = i
		z = z + 1
	}
}
train3 = train2[,-c(1,2,4, remove)]
length(remove)
length(remove) + 3 + ncol(train3) length(remove2) == ncol(train)

#have to take out 1,2,4(id, date_first_booked, date_create_account) 
# and action variables that have no variation
bTree = gbm(country_destination ~. , distribution = "multinomial",
		 n.trees = 100, shrinkage = .1,
		interaction.depth =2,  data = train3 )

test3 = test2[,-c(1,2,4, remove)]
bTreeP = predict(bTree, newdata=test3, n.trees = 300, type="response")
bTreeP = as.data.frame(bTreeP)
head(bTreeP)



outputFrame = as.data.frame(matrix(nrow=nrow(test2), ncol = 6))

outputFrame = rename(outputFrame, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame[,1] = test2[,1]


#The gsub function finds a pattern for a vector and replaces that pattern
for(i in 1:nrow(test2))
{
	outputFrame[i,2:6] =  gsub(pattern = ".100", 
	replace = "", x = colnames(sort(bTreeP[i,1:12], decreasing=TRUE))[1:5])

}
head(outputFrame)


#falidating output for outputFrame
sum(is.na(outputFrame))
nrow(outputFrame) == nrow(test2)
nrow(outputFrame) * ncol(outputFrame) == nrow(test2) * 6




microbenchmark(NDCG(outputFrame), times = 1 )







colSums(Filter(is.numeric, train))



##############################################################################################
#
#randomForest
#
#
#
#
############################################################################################


train3 = train2[,-c(1, 2, 4, 214, 223, 224, 225,
		228, 274, 286, 297, 308:313, 314:322,  323:331, 332:345,346:360)]

#turns train3 into a numeric matrix
train3 = data.matrix(train3)

randomForest(factor(country_destination) ~. ,data = train3)





##############################################################################################
#testing speed and accuracy of matrix versus a data frame
# for same machine learning technique
#
#
#	.8216727 NDCG  train2/train3 as a data frame	 n.trees = 5, shrinkage = .1,
#		interaction.depth =2,  data = train3
#	
#	gbm has to be a data.frame
#
#	
#
##############################################################################################





train3 = train2[,-c(1, 2, 4, 214, 223, 224, 225,
		228, 274, 286, 297, 308:313, 314:322,  323:331, 332:345,346:360)]

#turns train3 into a numeric matrix
train3 = data.matrix(train3)
 
#have to take out 1,2,4(id, date_first_booked, date_create_account) 
# and action variables that have no variation
bTree = gbm(country_destination ~. , distribution = "multinomial",
		 n.trees = 5, shrinkage = .1,
		interaction.depth =2,  data = train3 )


test3 = test2[,-c(1, 2, 4, 214, 223, 224, 225,
		228, 274, 286, 297, 308:313, 314:322,  323:331, 332:345,346:360)]

test3 = data.matrix(test3)
bTreeP = predict(bTree, newdata=test3, n.trees = 300, type="response")
bTreeP = as.data.frame(bTreeP)
head(bTreeP)



outputFrame = as.data.frame(matrix(nrow=nrow(test2), ncol = 6))

outputFrame = rename(outputFrame, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame[,1] = test2[,1]


#The gsub function finds a pattern for a vector and replaces that pattern
for(i in 1:nrow(test2))
{
	outputFrame[i,2:6] =  gsub(pattern = ".5", 
	replace = "", x = colnames(sort(bTreeP[i,1:12], decreasing=TRUE))[1:5])

}
head(outputFrame)


#falidating output for outputFrame
sum(is.na(outputFrame))
nrow(outputFrame) == nrow(test2)
nrow(outputFrame) * ncol(outputFrame) == nrow(test2) * 6




microbenchmark(NDCG(outputFrame), times = 1 )






#######################################################################################
#xgboost
#
#
#.8297489 NDCG for 35 rounds
#.8310828 NDCG for 100 rounds 254 variables
#.8080378 NDCG for 250 rounds 254 variables
#.8326883 NDCG for 150 rounds all variables
#.8194414 NDCG for 200 rounds 367 variables
#.8337553 NDCG for 100 rounds no variables with no observations
#######################################################################################
test3 = test2

remove2 = which(colnames(train2) %in% temp$Feature)
remove = numeric()
z = 1
for( i in 17:ncol(train2))
{
	if (sum(train2[,i]) < 1)
	{
		remove[z] = i
		z = z + 1
	}
}


#saves the outcome variable into a seperate vector
train2_response = train2[,16]
test3_response = test3[,16]





#stores the ids in a vector 
train2id = train2[,1]
test3id = test2[,1]


#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames
length(train2id) == nrow(train2)
length(test3id) == nrow(test3)







#1 is removed because it is an id
#2 is removed cause it has the same information as timestamp_first_active
#4 is removed cause we don't have date_first_booking for observations that we are modeling
#16 is removed because it is what we are trying to predict
#remove is a vector with all columns that have less than 150 observations.
#remove2 is feature engineering
train2 = train2[,-c(1,2,4, 16, remove, remove2)]
test3 = test3[,-c(1,2,4, 16, remove, remove2)]


length(remove) + 4 + ncol(train2) +length(remove2) == ncol(train)

length(train2_response) == nrow(train2)
length(test3_response) == nrow(test3)

train2 = data.matrix(train2)
test3 = data.matrix(test3)
train2Matrix = train2


test3Matrix = test3


#Turns the observations which have NA for age into -1
train2Matrix[which(is.na(train2Matrix[,3])),3] = -1
test3Matrix[which(is.na(test3Matrix[,3])),3] = -1





#turn train2response into numeric

train3_response = numeric(length(train2_response))


#vectorized solution
train3_response[which(train2_response == 'US')] = 0
train3_response[which(train2_response == 'NDF')] = 1
train3_response[which(train2_response == 'other')] = 2
train3_response[which(train2_response == 'AU')] = 3
train3_response[which(train2_response == 'ES')] = 4
train3_response[which(train2_response == 'IT')] = 5
train3_response[which(train2_response == 'GB')] = 6
train3_response[which(train2_response == 'FR')] = 7
train3_response[which(train2_response == 'CA')] = 8
train3_response[which(train2_response == 'DE')] = 9
train3_response[which(train2_response == 'NL')] = 10
train3_response[which(train2_response == 'PT')] = 11

class(train3_response)
length(train2_response == 'GB') == length(train3_response == 6)
train2_response = train3_response

#cross_validation parameters
#make sure to change the number of classes
numberOfClasses = 12
param = list( "objective" = "multi:softprob",
		"eval_metric" = "mlogloss",
		"num_class" = numberOfClasses
		)
cv.nround <- 250
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround)

#test for optimal nround
bst.cv[which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean)

#actual xgboost
bst = xgboost(param=param, data = train2Matrix, label = train2_response,
		gamma = .1, eta = .1, nrounds=nround,
		subsample = .75, max_delta_step = 15)







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst); importance_matrix

# Nice graph for importance
xgb.plot.importance(importance_matrix[1:100,])



#the predictions are in a nrow(test3)*4 long vector
#bstPred[1:12] is the probability of each country
#for the first observation of test2
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=13))
outputFrame = rename(outputFrame, c("X1" = "id", "X2" = "US", 
		"X3" = "NDF","X4" = "other", "X5"="AU", "X6" = "ES", "X7" = "IT",
		"X8" = "GB", "X9" = "FR", "X10" = "CA", "X11" = "DE",
		"X12" = "NL", "X13" = "PT")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
outputFrame[,1] = test2[,1]
#test to make sure ids are the same
sum(outputFrame[,1] != test2[,1])
z_element = 1

#puts the probabilities in the outputFrame
for (i in 1:nrow(test2))
{
	for (z in 1:12)
	{
		#the ith row of outputFrame is given observation z_element
		#probability of occuring from bstPred
		#column z+1 since id is in column 1
		outputFrame[i,z+1] = bstPred[z_element]
		z_element = z_element + 1
	}
}






#The gsub function finds a pattern for a vector and replaces that pattern
for(i in 1:nrow(test2))
{
	outputFrame[i,2:6] =   colnames(sort(outputFrame[i,2:13], decreasing=TRUE))[1:5]

}
head(outputFrame)


#falidating output for outputFrame
sum(is.na(outputFrame))
nrow(outputFrame) == nrow(test2)
nrow(outputFrame) * ncol(outputFrame) == nrow(test2) * 13




microbenchmark(NDCG(outputFrame), times = 1 )







###################################################################################
#	Normalized Discounted Cumulative Gain
#
# if you guess in the first position you get 1.0, second = .63, 3rd = .5,
#	fourth = .43, fifth = .386
#
#
#	#the data_frame must have id in the first column, country_1 in second column,
#	... country_5 in the sixth column
#
#
#
###################################################################################
NDCG <- function(data_frame){
	
	#Cumulative Gain total
	total = 0
	


	temp = numeric(nrow(test2))
	#outer while should iterate nrow(data_frame) /5 times
	for (i in 1:nrow(data_frame) )
	{


		
		score = 0

		#determining which position the score is, should be one to five
		score =  which(data_frame[i,] == as.character(test2$country_destination[i]))[1]
		score = score - 1
		#if score is zero you will end up dividing by zero so you only want to divide if 
		#score is not zero
		if (!is.na(score)){
			total = total + 1/log2(1+score)
			temp[i] = score
			
		}

			
	}



	total = total / nrow(data_frame) 
	print("The normalized Discounted Cumulative Gain is:");
	print(total);
	return(temp);



}






















