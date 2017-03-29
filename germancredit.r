#GERMAN CREDIT RISK MODEL

#LOADING DATA INTO THE R

Credit_score <- read.csv("C:/Users/SIDDHI/Documents/Capstone/myproject/german_credit.txt",header=TRUE ,sep = ",")
Credit_score

head(Credit_score)

Credit_score$Creditability<-as.factor(Credit_score$Creditability)

# TO CHECK THE STRUCTURE

str(Credit_score)
is.na(Credit_score)

# creating training and testing dataset.

s1=sample(1:nrow(Credit_score),round(0.80*nrow(Credit_score)))
train= Credit_score[s1,]
test= Credit_score[-s1,]

#creating logistic model 
logistic_model= glm(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+ Length.of.current.employment+Sex...Marital.Status,family=binomial,data=train)
logistic_model

prediction1= predict(logistic_model,test,type = "response")
prediction1
plot(prediction1)
prediction1 <- ifelse(prediction1 > 0.60, 1, 0)

#library(caret)
con<- confusionMatrix(prediction1,test$Creditability)
con

#logistic regression with all the variables.
logistic_model1<- glm(Creditability~., train,family = binomial)
logistic_model1
plot(logistic_model1)

prediction2<- predict(logistic_model1,test, type = "response")
prediction2
plot(prediction2)
prediction2<-ifelse(prediction1>0.7,1,0)
prediction2

con1<-confusionMatrix(prediction2,test$Creditability)
con1


#Consider now some regression tree (on all covariates)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
deci<-rpart(Creditability~Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+Purpose+ Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Guarantors+Duration.in.Current.address+Most.valuable.available.asset+ Age..years.+Concurrent.Credits+Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone+Foreign.Worker, data = train, method = "class",control = rpart.control(minsplit = 50, minbucket = 20, maxdepth = 10, cp = 0.005 ))
deci

prp(deci)

prediction3<- predict(deci,test,type="class")
prediction3

con3<- confusionMatrix(prediction3,test$Creditability)
con3

library(randomForest)

forest1 <- randomForest(Creditability~., train, method = "response",controls=cforest_control(mtry=2, mincriterion=0))
forest1

prediction4<- predict(forest1,test,type = "response")
prediction4

con4<- confusionMatrix(prediction4,test$Creditability)
con4


