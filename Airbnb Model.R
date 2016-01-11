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













test = as.data.frame(matrix(nrow=10, ncol = 2))
test = rename(test, c("V1" = "id", "V2" = "country")) 
test[1:5,1] = '5uwns89zht'
test[1:5,2] = 'NDF'
test[6:10,1] = 'jtl0dijy2j'
test[6:10,2] = 'NDF'
test = test[,-c(3,4,5)]
test
###################################################################################
#	Normalized Discounted Cumulative Gain
#
# if you guess in the first position you get 1.0, second = .63, 3rd = .5,
#	fourth = .43, fifth = .386
#
#
#
#
#
#
###################################################################################
NDCG <- function(data_frame){

		









}
























