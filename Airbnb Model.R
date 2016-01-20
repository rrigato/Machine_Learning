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
size_of_train = floor(.9*nrow(train))
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
#	3426.36 seconds, .8299452 NDCG, 90% train, 10% test
#country_destination~ date_account_created + date_first_booking
#		+ timestamp_first_active  .7910587 NDCG, 90% train, 10% test
#
#
#
#
#
#
############################################################################################

#naive bayes
NBmod = naiveBayes(country_destination~ date_account_created + date_first_booking
		+ timestamp_first_active   , data = train2)
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

NB.frame = as.data.frame(NB.pred)
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

#####################################################################################
#GBM model
#
#300 trees interaction 4 shrinkage = .1 .823
#n.trees = 1000, shrinkage = .01, interaction.depth =3,
#n.trees = 500, shrinkage = .01, interaction.depth =6, .817 NDCG
#
#
#
#
#######################################################################################

bTree = gbm(country_destination ~ gender + age + signup_method
		+ signup_flow + language + affiliate_channel + 
		affiliate_provider + first_affiliate_tracked + signup_app
		+ first_device_type + first_browser ,	distribution = "multinomial", 
		n.trees = 300, shrinkage = .1, interaction.depth =2,  data = train2)
bTreeP = predict(bTree, newdata=test2, n.trees = 1000, type="response")
 head(as.data.frame(bTreeP[,1:12,1]))
bTreeP = as.data.frame(bTreeP[,1:12,1])
head(bTreeP)



#initializing the output dataframe
outputFrame2 = as.data.frame(matrix(nrow=nrow(test2), ncol = 6))

outputFrame2 = rename(outputFrame2, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame2[,1] = test2[,1]


for(i in 1:nrow(test2))
{
outputFrame2[i,2:6] = colnames(as.data.frame(sort(bTreeP[i,], decreasing=TRUE)))[1:5]
}
head(outputFrame2)


#falidating output for outputFrame2
sum(is.na(outputFrame2))
nrow(outputFrame2) == nrow(test2)
nrow(outputFrame2) * ncol(outputFrame2) == nrow(test2) * 6


system.time(NDCG(outputFrame2))



################################################################################
#
#	Ensemble method: simple average of Naive Bayes and GBM
#
#
#
#
#
################################################################################
 
#difference of 12 probabilities from Naive Bayes and GBM
str(as.data.frame(abs(bTreeP - NB.frame)))

#gives the differences of the two methods greater than an arbitrary probability
sum(as.data.frame(abs(bTreeP - NB.frame)) > .5) 


#averaging the predicted probabilities
str(as.data.frame((bTreeP + NB.frame) / 2))

EnsembleFrame = as.data.frame((bTreeP + NB.frame) / 2)




#initializing the output dataframe
outputFrame3 = as.data.frame(matrix(nrow=nrow(test2), ncol = 6))

outputFrame3 = rename(outputFrame3, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame3[,1] = test2[,1]



#gets the top 5 countries by probability and places them in the outputFrame
for(i in 1:nrow(test2))
{
outputFrame3[i,2:6] = colnames(as.data.frame(sort(EnsembleFrame[i,], decreasing=TRUE)))[1:5]
}
head(outputFrame3)




#falidating output for outputFrame3
sum(is.na(outputFrame3))
nrow(outputFrame3) == nrow(test2)
nrow(outputFrame3) * ncol(outputFrame3) == nrow(test2) * 6


system.time(NDCG(outputFrame3))












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
data_frame[1,1] = '2cji4lpjr3'
data_frame[1,2:6] = 'NA'
data_frame[1,2] = 'NDF'
data_frame[2,1] = 'jtm5s5dhwx'
data_frame[2,2] = 'NDF'


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
		row_num = (which(test2[,1] == data_frame[i,1])[1] )


		#input validation on what values row_num can be
#		if (row_num < 1  )
#		{
#			print("Error: unable to find row number of correct solution")
#			print("row_num:");
#			print(row_num);
#			return();
#		}


		
		score = 0

		#determining which position the score is, should be one to five
		score =  which(data_frame[i,] == as.character(test2$country_destination[row_num]))[1]
		score = score - 1
		#if score is zero you will end up dividing by zero so you only want to divide if 
		#score is not zero
		if (!is.na(score)){
			total = total + 1/log2(1+score)
		}

			
	}



	total = total / nrow(data_frame) 
	print("The normalized Discounted Cumulative Gain is:");
	print(total);
	return(total);



}










train[row_num,16]

train$country_destination[row_num]
correct = train[,row_num] [1]
train[row_num,16]

head(train)







