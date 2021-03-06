##################################################################################
#	Airbnb script to predict where a customer will book
#
#	Attempt 2
#
#
#
#
#
#
#
#
###################################################################################
install.packages("class")
#used for the knn algorithm
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
train <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\train_users_2.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\test_users.csv")
sessions <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\sessions.csv")
countries <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\countries.csv")
age_gender_bkts <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\age_gender_bkts.csv")




train = merge(train , sessions, by.x='id', by.y = 'user_id')
test = merge(test , sessions, by.x='id', by.y = 'user_id', all.x = TRUE)


#turning dates in training and test sets into numeric values

train[,2] = as.numeric(train[,2])
train[,4] = as.numeric(train[,4])

test[,2] = as.numeric(test[,2])
test[,4] = as.numeric(test[,4])


################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################




#edit The percentage of the dataset in the train2 and test2, used to build a model 
size_of_train = floor(.7*nrow(train))
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
NBmod = naiveBayes(country_destination~. -id -date_account_created , data = train2)
summary(NBmod)
#predict the probability of each country 
#based on Naive Bayes from train2 for test2
NB.pred = predict(NBmod, newdata = test2, type = "raw")

NB.frame = as.data.frame(NB.pred)




NB.frame[,13] = test2[,1]
NB.frame = rename(NB.frame, c('V13' = 'id'))

#average probabilities by id
NB.frame[,1] = ave(NB.frame$AU, NB.frame$id, FUN=mean)
NB.frame[,2] = ave(NB.frame$CA,NB.frame$id, FUN=mean)
NB.frame[,3] = ave(NB.frame$DE, NB.frame$id, FUN=mean)
NB.frame[,4] = ave(NB.frame$ES,NB.frame$id, FUN=mean)
NB.frame[,5] = ave(NB.frame$FR, NB.frame$id, FUN=mean)
NB.frame[,6] = ave(NB.frame$GB,NB.frame$id, FUN=mean)
NB.frame[,7] = ave(NB.frame$IT, NB.frame$id, FUN=mean)
NB.frame[,8] = ave(NB.frame$NDF,NB.frame$id, FUN=mean)
NB.frame[,9] = ave(NB.frame$NL, NB.frame$id, FUN=mean)
NB.frame[,10] = ave(NB.frame$other,NB.frame$id, FUN=mean)
NB.frame[,11] = ave(NB.frame$PT,NB.frame$id, FUN=mean)
NB.frame[,12] = ave(NB.frame$US, NB.frame$id, FUN=mean)


#remove duplicate ids
NB.frame2 = NB.frame[!duplicated(NB.frame$id),]

#initialize test3 for NDCG
test3 = test2[!duplicated(test2$id),]


#initializing the output dataframe
outputFrame = as.data.frame(matrix(nrow=nrow(test3), ncol = 6))

outputFrame = rename(outputFrame, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame[,1] = test3[,1]


for(i in 1:nrow(test3))
{
outputFrame[i,2:6] = colnames(sort(NB.frame2[i,1:12], decreasing=TRUE))[1:5]
}
head(outputFrame)


#falidating output for outputFrame
sum(is.na(outputFrame))
nrow(outputFrame) == nrow(test3)
nrow(outputFrame) * ncol(outputFrame) == nrow(test3) * 6


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
#.7538923 for 50 tree gbm
#######################################################################################

bTree = gbm(country_destination ~ gender + age + signup_method
		+ signup_flow + language + affiliate_channel + 
		affiliate_provider + first_affiliate_tracked + signup_app
		+ first_device_type + first_browser + timestamp_first_active
		+ action_type + device_type + secs_elapsed + action + action_detail
		,	distribution = "multinomial", 
		n.trees = 50, shrinkage = .1, interaction.depth =2,  data = train2)
bTreeP = predict(bTree, newdata=test2, n.trees = 1000, type="response")

bTreeP = as.data.frame(bTreeP[,1:12,1])
head(bTreeP)



bTreeP[,13] = test2[,1]
bTreeP = rename(bTreeP, c('V13' = 'id'))

#average probabilities by id
bTreeP[,1] = ave(bTreeP$AU, bTreeP$id, FUN=mean)
bTreeP[,2] = ave(bTreeP$CA,bTreeP$id, FUN=mean)
bTreeP[,3] = ave(bTreeP$DE, bTreeP$id, FUN=mean)
bTreeP[,4] = ave(bTreeP$ES,bTreeP$id, FUN=mean)
bTreeP[,5] = ave(bTreeP$FR, bTreeP$id, FUN=mean)
bTreeP[,6] = ave(bTreeP$GB,bTreeP$id, FUN=mean)
bTreeP[,7] = ave(bTreeP$IT, bTreeP$id, FUN=mean)
bTreeP[,8] = ave(bTreeP$NDF,bTreeP$id, FUN=mean)
bTreeP[,9] = ave(bTreeP$NL, bTreeP$id, FUN=mean)
bTreeP[,10] = ave(bTreeP$other,bTreeP$id, FUN=mean)
bTreeP[,11] = ave(bTreeP$PT,bTreeP$id, FUN=mean)
bTreeP[,12] = ave(bTreeP$US, bTreeP$id, FUN=mean)



#remove duplicate ids
bTreeP2 = bTreeP[!duplicated(bTreeP$id),]

#initialize test3 for NDCG
test3 = test2[!duplicated(test2$id),]



#initializing the output dataframe
outputFrame2 = as.data.frame(matrix(nrow=nrow(test3), ncol = 6))

outputFrame2 = rename(outputFrame2, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame2[,1] = test3[,1]


for(i in 1:nrow(test3))
{
outputFrame2[i,2:6] = colnames(sort(bTreeP2[i,1:12], decreasing=TRUE))[1:5]
}
head(outputFrame2)


#falidating output for outputFrame2
sum(is.na(outputFrame2))
nrow(outputFrame2) == nrow(test3)
nrow(outputFrame2) * ncol(outputFrame2) == nrow(test3) * 6


system.time(NDCG(outputFrame2))




################################################################################
#	
#	randomForest
#
#	NDCG: .8074963
#	date_account_created and date_first_booking must be numeric 
#	with those two as numeric NDCG: .9236131!!!
#	problem: date_first_booking has no observations in test
#	.8118565 with no date_first_booking
#################################################################################
ranOut = randomForest(as.factor(country_destination) ~ gender + signup_method
		+ signup_flow + language + affiliate_channel + 
		affiliate_provider  + first_affiliate_tracked + signup_app
		+ first_device_type + first_browser + date_account_created 
		+ timestamp_first_active,  importance = TRUE,
				ntrees = 300, data=train2)

ranPred = predict(ranOut, newdata = test2,type = 'prob')

ranPred = as.data.frame(ranPred)


#initializing the output dataframe
outputFrame4 = as.data.frame(matrix(nrow=nrow(test2), ncol = 6))

outputFrame4 = rename(outputFrame4, c("V1" = "id", "V2" = "country1",
				"V3" = "country2", "V4" = "country3",
				"V5" = "country4", "V6" = "country5"))

outputFrame4[,1] = test2[,1]


for(i in 1:nrow(test2))
{
outputFrame4[i,2:6] = colnames(as.data.frame(sort(ranPred[i,], decreasing=TRUE)))[1:5]
}
head(outputFrame4)


#falidating output for outputFrame4
sum(is.na(outputFrame4))
nrow(outputFrame4) == nrow(test2)
nrow(outputFrame4) * ncol(outputFrame4) == nrow(test2) * 6


system.time(NDCG(outputFrame4))
varImpPlot(ranOut)



################################################################################
#
#	Ensemble method: .75 * NB + .25*GBM
#	country_destination~. -id NDCG = .837  
#	n.trees = 300, shrinkage = .1, interaction.depth =2 (GBM) 
#	NDCG = .8243965 +	 
##country_destination~ date_account_created + date_first_booking
#		+ timestamp_first_active  . NDCG, 90% train, 10% test
#	using simple average of the probabilities output from those two models
#	Average NDCG = .8458347
#
#	Average NDCG = .8472761 for .75 GBM + .25 NB
#	Average NDCG = .8476111 for .35 GBM + .65 NB
################################################################################
 
#difference of 12 probabilities from Naive Bayes and GBM
str(as.data.frame(abs(bTreeP - NB.frame)))

#gives the differences of the two methods greater than an arbitrary probability
sum(as.data.frame(abs(bTreeP - NB.frame)) > .4) 


#averaging the predicted probabilities
str(as.data.frame((bTreeP + NB.frame) / 2))

EnsembleFrame = as.data.frame((.3 * bTreeP +  .65 * NB.frame + .05 * ranPred) )




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

#gives the observations where none of the 5 predictions counted
 missed = NDCG(outputFrame3)
print("Proportion where actual country was not in prediction:")
print(length(which(missed==0))/nrow(test2))
summary(test2[which(missed==0),])

#gives the proportion of gender where the algorithm did not predict 
table(test2[which(missed==0),5])/ length(which(missed==0))


table(test2[,5])/ nrow(test2)



#gives the proportion of gender where the algorithm did not predict 
table(test2[which(missed==0),16])/ length(which(missed==0))


table(test2[,16])/ nrow(test2)


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


