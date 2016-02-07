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
#	Turning the output which has five columns as predictions,
#into 5 rows with the same unique id.
#
#
##################################################################


test3id = test[,1]
test3 = test[,-c(1,2,4, 16, remove, remove2)]







test3Matrix = data.matrix(test3)


#Turns the observations which have NA for age into -1
test3Matrix[which(is.na(test3Matrix[,3])),3] = -1




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



outputFrame2 =  data.frame(matrix(nrow= nrow(test), ncol=6))

outputFrame2 = rename(outputFrame2, c("X1" = "id", "X2" = "C1", 
		"X3" = "C2","X4" = "C3", "X5"="C4", "X6" = "C5")) 

outputFrame2[,1] = outputFrame[,1]
#The gsub function finds a pattern for a vector and replaces that pattern
for(i in 1:nrow(test))
{
	outputFrame2[i,2:6] =   colnames(sort(outputFrame[i,2:13], decreasing=TRUE))[1:5]

}








#initializing an outputFrame to use it needs to have 5* nrow(test) rows
# and the first column is for id and the second is for country.
outputFrame3 = data.frame(matrix(nrow= (nrow(test)*5), ncol=2))
outputFrame3 = rename(outputFrame3, c('X1'= 'id', 'X2' = 'country'))


#iterates once for each row in outputframe/nrow(test)
#the goal of these two loops is to turn the a row with one id and 5 column prediction
#variables in columns 2:6, into a 5 rows with the id in var 1 and the country in var 2
#starting with the highest probability country first

#total starts the count for the outputFrame3 row
total = 1
for(i in 1:nrow(test))
{

#goes from 2:6 because id is in the first row of outputFrame2
#need to account for that 
	for (z in 2:6)
	{
		#total is the row of outputFrame3 being filled
		#puts the id in column 1
		#need to cast the id as a character cause it gives that level garbage
		outputFrame3[total, 1] = as.character(outputFrame2$id[i]) ;
		
		#puts the country prediction in column 2
		outputFrame3[total,2] = outputFrame2[i,z];
		
		#increments the counter for outputFrame3
		total = total + 1;
	}
}



Result1 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\Results.csv")


Result2 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Airbnb\\Results2.csv")


Difference = abs((Result1[,2:4] - outputFrame[,2:4]))
#write the data frame to an excel file
write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Kaggle Airbnb/Results2.xlsx')

write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Kaggle Airbnb/Results3.xlsx')







