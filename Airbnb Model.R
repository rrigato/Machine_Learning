##################################################################################
#	Airbnb script to predict where a customer will book
#
#
#
#
#
#
#
#
#
#
###################################################################################

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

#importing the datasets that were provided by Telstra
train <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\train_users_2.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\test_users.csv")
sessions <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\sessions.csv")
countries <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\countries.csv")
age_gender_bkts <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\age_gender_bkts.csv")













data_frame = as.data.frame(matrix(nrow=10, ncol = 2))
data_frame = rename(data_frame, c("V1" = "id", "V2" = "country")) 
data_frame[1:5,1] = 'gxn3p5htnn'
data_frame[1:5,2] = 'NDF'
data_frame[6:10,1] = 'jh95kwisub'
data_frame[6:10,2] = 'NDF'


data_frame2 = as.data.frame(matrix(nrow=nrow(train), ncol = 2))
data_frame2 = rename(data_frame2, c("V1" = "id", "V2" = "country")) 
data_frame2[,1] = train$id
sum(!is.na(data_frame2[,1]))
data_frame2[,2] = 'NDF'

NDCG(data_frame)
###################################################################################
#	Normalized Discounted Cumulative Gain
#
# if you guess in the first position you get 1.0, second = .63, 3rd = .5,
#	fourth = .43, fifth = .386
#
#
#	#the data_frame that is provided must have 5 predicitions of id, country in
#	order of importance
#
#
#
###################################################################################
NDCG <- function(data_frame){
	
	#Cumulative Gain total
	total = 0

	#counter for how many rows
	i = 1
	
	#this is going to hold the country destination predictions from most to least likely
	temp = numeric(5)

	#outer while should iterate nrow(data_frame) /5 times
	while (i < nrow(data_frame) )
	{

		#going to be the row in train where the correct destination is
		row_num = 0

		#gets the row_num from train that matches the id corresponding
		#to the predictions in temp
		row_num = (which(train[,1] == data_frame[i,1])[1] )


		#input validation on what values row_num can be
		if ((row_num < 1) || row_num > nrow(train) )
		{
			print("Error: unable to find row number of correct solution")
			print("row_num:");
			print(row_num);
			return();
		}


		#setting temp as the five predicted countries.		
		temp = data_frame[i:(i+4),2]
		
		score = 0
		#determining which position the score is, should be one to five
		score = (which(temp == train$country_destination[row_num]) [1])		
		
		#if score is zero you will end up dividing by zero so you only want to divide if 
		#score is not zero
		if (score >0)
		{
			total = total + 1/log2(1+score)
		}
		#increment i by 5 since there are 5 predictions
		i = i + 5
	
	}



	total = total / (nrow(data_frame) / 5)
	print("The normalized Discounted Cumulative Gain is:");
	print(total);
	return(total);



}
























