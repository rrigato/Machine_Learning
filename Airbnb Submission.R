########################################################################################
#Getting it into a submission after the model has been trained
#
#Needs to be in a csv of: 
#ID1, Country1 Pred name
#ID1, Country2 Pred name
##ID1, Country3 Pred name
#ID1, Country4 Pred name
#ID1, Country5 Pred name
#########################################################################################









################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#
##################################################################


test3id = test[,1]
test3 = test[,-c(1)]






test3Matrix = data.matrix(test3)





#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
outputFrame = data.frame(matrix(nrow= nrow(test), ncol=13))
outputFrame = rename(outputFrame, c("X1" = "id", "X2" = "US", 
		"X3" = "NDF","X4" = "other", "X5"="AU", "X6" = "ES", "X7" = "IT",
		"X8" = "GB", "X9" = "FR", "X10" = "CA", "X11" = "DE",
		"X12" = "NL", "X13" = "PT")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
outputFrame[,1] = test[,1]
#test to make sure ids are the same
sum(outputFrame[,1] != test[,1])
z_element = 1

#puts the probabilities in the outputFrame
for (i in 1:nrow(test))
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
for(i in 1:nrow(test))
{
	outputFrame[i,2:6] =   colnames(sort(outputFrame[i,2:13], decreasing=TRUE))[1:5]

}









outputFrame2 = data.frame(matrix(nrow= nrow(test)*5, ncol=2))

total = 1
for(i in 1:nrow(test))
{
	for (z in 1:5)
	{
		outputFrame2[total, 1] = outputFrame$id[i] ;
		outputFrame2[total,2] = outputFrame[i,z];
		total = total + 1;
	}
}



Result1 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\Results.csv")


Result2 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\Results2.csv")


Difference = abs((Result1[,2:4] - outputFrame[,2:4]))
#write the data frame to an excel file
write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Kaggle Airbnb/Results2.xlsx')

write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Kaggle Airbnb/Results3.xlsx')







