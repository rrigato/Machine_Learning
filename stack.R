install.packages('caretEnsemble')
install.packages('bnclassify', dependencies =  c("Depends", "Suggests"))
install.packages('e1071', dependencies =  c("Depends", "Suggests"))
install.packages('ggplot2')
library(caretEnsemble)
library(ggplot2)

models <- caretList(
x=iris[1:50,1:2],
y=iris[1:50,3],
trControl=trainControl(method='cv'),
methodList=c('rpart', 'glm')
)

HC = (as.data.frame(iris))



models <- caretList(
	x = HC[1:140,1:2],
	y = HC[1:140,5],
	trControl=trainControl(method='cv', savePredictions=TRUE),
	methodList = c('nb', 'multinom')
)

caretStack(models, method='glm')


