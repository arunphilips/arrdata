#Installing kernlab package and getting Spam dataset - http://www.inside-r.org/packages/cran/kernlab/docs/spam
install.packages("kernlab") 
library(kernlab)
data(spam)
str(spam[,1:5])

set.seed(100) #Same results produced each rep
trainIndicator = rbinom(4601,size=1,prob = 0.5) #create int vector of 4601 values of values 0 and 1, probability 0.5 for each
table(trainIndicator)
str(trainIndicator)

trainSpam=spam[trainIndicator==1,] #split 1s into trainset
testSpam=spam[trainIndicator==0,] #split 0s into testset

names(trainSpam) #names of the columns

table(trainSpam$type)
table(testSpam$type)

plot(log10(trainSpam$capitalAve+1)~trainSpam$type) #Capital used Average (Y axis) vs Result (spam/ham)

plot(log10(trainSpam[,1:4]+1)) #plot of variables (factors) with each other

hCluster=hclust(dist(t(trainSpam[,1:57]))) #cluster dendogram
plot(hCluster)

hCluster1=hclust(dist(t(log(trainSpam[,1:55]+1)))) #log10 + 1 of factors
plot(hCluster1)

trainSpam$numType <- as.numeric(trainSpam$type)-1 #create a generalized linear model, find least cvError for which variable.
costfunction<-function(x,y) sum(x!=(y>0.5))
cvError <- rep(NA,55)
library(boot)
for(i in 1:55){
  lmFormula = reformulate(names(trainSpam[i]), response = "numType")
  glmFit = glm(lmFormula,family="binomial",data=trainSpam)
  cvError[i]=cv.glm(trainSpam,glmFit, costfunction,2)$delta[2]
}

names(trainSpam[which.min(cvError)]) #which variable has minumum cvError?
                 
predictionModel=glm(numType~charDollar,family="binomial",data=trainSpam)
#using best group model (no. of dollar signs)

predictionTest=predict(predictionModel,testSpam) #make predictions on test set
predictedSpam=rep("nonspam",dim(testSpam)[1]) #New vector that stores whether spam/nonspam

predictedSpam[predictionModel$fitted>0.5]="spam" #greater than 0.5 probability, is classified spam

table(predictedSpam,testSpam$type) # a 22 percent error rate in classifications
                  