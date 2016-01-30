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
train <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\trainAction.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\test_users.csv")
sessions <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\sessions.csv")
countries <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\countries.csv")
age_gender_bkts <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\age_gender_bkts.csv")




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
#
#
#
#
#
###############################################################################
bTree = gbm(fault_severity ~. -id, distribution = "multinomial", n.trees = 300, shrinkage = .1,
		interaction.depth =2,  data = train2)




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
	


	temp = numeric(nrow(test3))
	#outer while should iterate nrow(data_frame) /5 times
	for (i in 1:nrow(data_frame) )
	{


		
		score = 0

		#determining which position the score is, should be one to five
		score =  which(data_frame[i,] == as.character(test3$country_destination[i]))[1]
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






















