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








################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################




#edit The percentage of the dataset in the train2 and test2, used to build a model 
size_of_train = floor(.5*nrow(train))
ran_num_test = 1:nrow(train)

#gets random numbers for train2 using a sample
ran_num_train = sample(1:nrow(train), size_of_train)

#numbers not randomly selected for train2 are included in test2
#this command gets the numbers not in ran_num_train
ran_num_test = ran_num_test[(!(ran_num_test %in% ran_num_train)) == TRUE]
train2 = train[ran_num_train,]
test2 = train[ran_num_test,]







###########################################################################################
#	NaiveBayes algorithm implementation
#
#
#
#
#
#
#
############################################################################################

#naive bayes
NBmod = naiveBayes(country_destination~. -id -date_account_created, data = train2)
summary(NBmod)
#predict the probability of each country 
#based on Naive Bayes from train2 for test2
NB.pred = predict(NBmod, newdata = test2, type = "raw")

#initializing the output dataframe
outputFrame = as.data.frame(matrix(nrow=nrow(test2), ncol = 6))

outputFrame = rename(outputFrame, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame[,1] = test2[,1]


for(i in 1:nrow(test2))
{
outputFrame[i,2:6] = rownames(as.data.frame(sort(NB.pred[i,], decreasing=TRUE)))[1:5]
}
head(outputFrame)


#falidating output for outputFrame
sum(is.na(outputFrame))
nrow(outputFrame) == nrow(test2)
nrow(outputFrame) * ncol(outputFrame) == nrow(test2) * 6


system.time(NDCG(outputFrame))




########################################################################################
#
#	testing to make sure function works
#
#
#
#########################################################################################


data_frame = as.data.frame(matrix(nrow=2, ncol = 6))
data_frame = rename(data_frame, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))
data_frame[1,1] = 'gxn3p5htnn'
data_frame[1,2:6] = 'NDF'
data_frame[2,1] = 'jh95kwisub'
data_frame[2,2:6] = 'NDF'


system.time(NDCG(data_frame))
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
	



	#outer while should iterate nrow(data_frame) /5 times
	for (i in 1:nrow(data_frame) )
	{

		#going to be the row in train where the correct destination is
		row_num = 0

		#gets the row_num from train that matches the id corresponding
		#to the predictions in temp
		row_num = (which(train[,1] == data_frame[i,1])[1] )


		#input validation on what values row_num can be
		if (row_num < 1  )
		{
			print("Error: unable to find row number of correct solution")
			print("row_num:");
			print(row_num);
			return();
		}


		
		score = 0
		z = 2
		#determining which position the score is, should be one to five
		while(z < 7)
		{
			#if the of the predicted data_frame is equal to the actual country 
			#then that position minus 1 is used for the score 
			#in the continued loss function 
			if(data_frame[i,z] == train$country_destination[row_num])
			{
				score = z -1
				z = 7
			}	
			z = z + 1 	
		}

		#if score is zero you will end up dividing by zero so you only want to divide if 
		#score is not zero
		if (score >0){
			total = total + 1/log2(1+score)
		}

			
	}



	total = total / nrow(data_frame) 
	print("The normalized Discounted Cumulative Gain is:");
	print(total);
	return(total);



}
























