install.packages('caretEnsemble')
install.packages('caretEnsemble', dependencies =  c("Depends", "Suggests"))
install.packages('e1071', dependencies =  c("Depends", "Suggests"))
install.packages('ggplot2')
install.packages('h2o')
install.packages('bnlearn')
library(bnlearn)
library(caretEnsemble)
library(ggplot2)
library(h2o)

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
	trControl=trainControl(method='cv'),
	methodList = c('nb')
)

caretStack(models, method='glm')


##########################################################################
#
#	attempt two using h2o
#
#
#
###########################################################################
localH2O <- h2o.init()
setwd("C:\\Users\\Randy\\Downloads")
train <- h2o.importFile(localH2O, path = "h2otrain.csv")


####################################################################################
#	bayesian networks
#
#
#
#
####################################################################################

data(learning.test)


# learn the network structure.
res = gs(learning.test)
# set the direction of the only undirected arc, A - B.
res = set.arc(res, "A", "B")
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, learning.test)
# replace the parameters of the node B.
new.cpt = matrix(c(0.1, 0.2, 0.3, 0.2, 0.5, 0.6, 0.7, 0.3, 0.1),
byrow = TRUE, ncol = 3,
dimnames = list(B = c("a", "b", "c"), A = c("a", "b", "c")))
fitted$B = as.table(new.cpt)
# the network structure is still the same.
all.equal(res, bn.net(fitted))
# learn the network structure.
res = hc(gaussian.test)
# estimate the parameters of the Bayesian network.
fitted = bn.fit(res, gaussian.test)
# replace the parameters of the node F.
fitted$F = list(coef = c(1, 2, 3, 4, 5), sd = 3)
# set again the original parameters
fitted$F = lm(F ~ A + D + E + G, data = gaussian.test)
# discrete Bayesian network from expert knowledge.
net = model2network("[A][B][C|A:B]")
cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
cptB = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("GOOD", "BAD")))
cptC = c(0.5, 0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8)
dim(cptC) = c(2, 2, 2)
dimnames(cptC) = list("C" = c("TRUE", "FALSE"), "A" = c("LOW", "HIGH"),
"B" = c("GOOD", "BAD"))
cfit = custom.fit(net, dist = list(A = cptA, B = cptB, C = cptC))
# for ordinal nodes it is nearly the same.
cfit = custom.fit(net, dist = list(A = cptA, B = cptB, C = cptC),
ordinal = c("A", "B"))
# Gaussian Bayesian network from expert knowledge.
distA = list(coef = c("(Intercept)" = 2), sd = 1)
distB = list(coef = c("(Intercept)" = 1), sd = 1.5)
distC = list(coef = c("(Intercept)" = 0.5, "A" = 0.75, "B" = 1.32), sd = 0.4)
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))
# conditional Gaussian Bayesian network from expert knowledge.
cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
distB = list(coef = c("(Intercept)" = 1), sd = 1.5)
distC = list(coef = matrix(c(1.2, 2.3, 3.4, 4.5), ncol = 2,
dimnames = list(c("(Intercept)", "B"), NULL)),
sd = c(0.3, 0.6))
cgfit = custom.fit(net, dist = list(A = cptA, B = distB, C = distC))



#####################################################################################
#	multi level classification using svm
#
#
#
#
#####################################################################################

library( 'e1071' )
data( iris )
model <- svm( iris$Species~., iris )
res <- predict( model, newdata=iris )

















