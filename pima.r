#Pima Indians diabete dataset 
setwd("C:/Users/SIDDHI/Documents/Capstone/myproject")
PID<-read.csv("pima.csv",header = TRUE,stringsAsFactors = TRUE)
PID
head(PID)

#To remove the first column.
PID<-PID[-1]
PID
head(PID)
str(PID)
summary(PID)
anyNA(PID)


#scaling of data

minmax<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Apply the minmax function to all the columns in our data
scale1<-as.data.frame(lapply(PID[1:9],minmax))
scale1
head(scale1)

#TO create subset.

class1<-subset(scale1,scale1$Diabet==1)
class1
head(class1)

class0<-subset(scale1,scale1$Diabet==0)
class0
head(class0)


#Splitting of data in train and test 
sample1<-sample(1:nrow(class1),round(0.70*nrow(class1)))
sample1

sample2<-sample(1:nrow(class0),round(0.70*nrow(class0)))
sample2

train1<-class1[sample1,]
train1
train0<-class0[sample2,]
train0

test1<-class1[-sample1,]
test1
test0<-class0[-sample2,]
test0

train<-rbind(train1,train0)
train
head(train)

test<-rbind(test1,test0)
test
head(test)


#Fit the model
library(rpart)
fit<- rpart(Diabet~ NPG+PGL+DIA+TSF+INS+BMI+DPF+AGE,data=train,method = "class")
fit

#We would then consider whether the tree could be simplified by pruning and make use of the plotcp function:
plotcp(fit)# visualize cross-validation results 

#Once the amount of pruning has been determined from this graph or by looking at the output from the printcp function
printcp(fit)## display the results 

summary(fit)

#Plot

library(rpart.plot)

#The prune function is used to simplify the tree based on a cp identified from the graph or printed output threshold.
fit2<-prune(fit,cp=0.015)
fit2

#The classification tree can be visualised with the plot function and then the text function adds labels to the graph
plot(fit2,uniform = TRUE)
text(fit2,use.n = TRUE,cex=0.75)

#predict our model
pred<-predict(fit,test,type = "class")
pred

#confusionmatrix
library(caret)
con<-confusionMatrix(pred,test$Diabet)
con

