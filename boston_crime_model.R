library(MASS)
attach(Boston)
##initialize array to store the converted response variable
crime_level<-array(0,dim(Boston)[1])

##classify crime levels
for (i in 1:dim(Boston)[1])
  
{
  ifelse(crim[i] < 1,crime_level[i]<-"low_crime",crime_level[i]<-"high_crime")

}

##make sure the response is viewed as a factor
crime_level<-factor(crime_level)

##check coding scheme. By default, the levels are arranged in alphabetical order and the first in order becomes the reference class. 
contrasts(crime_level)

#Add to dataframe crime level
Boston$crime_level=crime_level


set.seed(199)

##split data into two equal parts
sample<-sample.int(nrow(Boston), floor(.50*nrow(Boston)), replace = F)
train<-Boston[sample, ]
test<-Boston[-sample, ]

##fit model using training data

model_train<-glm(crime_level ~ indus + nox + rad + tax + lstat + medv, family="binomial", data=train)
summary(model_train)
##generate ROC curve
library(ROCR)
preds<-predict(model_train,newdata=test, type="response")
rates<-prediction(preds, test$crime_level)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Boston Crime data")
lines(x = c(0,1), y = c(0,1), col="red")

#Auc 
auc<-performance(rates, measure = "auc")
auc

# true/false positive table
##cutoff of 0.5
table(test$crime_level, preds>0.5)

##counts for test data
table(test$crime_level)

