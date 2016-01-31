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


test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\testAction2.csv")
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
#	.823141 NDCG for 5 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#	.8304302 NDCG for 125 trees .1 shrinkage .8 train .2 test interaction.depth = 3
#	.8308948 NDCG for 200 trees .1 shrinkage .8 train .2 test interaction.depth = 2
#
#
#
###############################################################################


train3 = train2[,-c(1, 2, 4, 214, 223, 224, 225,
		228, 274, 286, 297, 308:313, 314:322,  323:331, 332:345,346:360)]
#have to take out 1,2,4(id, date_first_booked, date_create_account) 
# and action variables that have no variation
bTree = gbm(country_destination ~. , distribution = "multinomial",
		 n.trees = 200, shrinkage = .1,
		interaction.depth =2,  data = train3 )

test3 = test2[,-c(1, 2, 4, 214, 223, 224, 225,
		228, 274, 286, 297, 308:313, 314:322,  323:331, 332:345,346:360)]
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
	outputFrame[i,2:6] =  gsub(pattern = ".200", 
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






















