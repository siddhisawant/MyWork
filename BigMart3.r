traincsv <- read.csv("C:/Users/SIDDHI/Documents/Capstone/myproject/Programs/BIGMART3/Train_UWu5bXk.csv",na.strings = "")

testcsv <- read.csv("C:/Users/SIDDHI/Documents/Capstone/myproject/Programs/BIGMART3/Test_u94Q5KV.csv",na.strings = "")

#### To check the missing values
sum(is.na(traincsv))

str(traincsv)

summary(traincsv)

### To find the names of the columns having NA values
colSums(is.na(traincsv))==0

###To find total Na's in Item_weight & Outlet_Size
sum(is.na(traincsv$Item_Weight))
sum(is.na(traincsv$Outlet_Size))

###To impute NA's
traincsv$Item_Weight[is.na(traincsv$Item_Weight)] = mean(traincsv$Item_Weight, na.rm=TRUE)


#train <- Bigdata[!is.na(Bigdata$Outlet_Size),]
#test <- Bigdata[is.na(Bigdata$Outlet_Size),]

plot(traincsv$Outlet_Size)
traincsv$Outlet_Size[is.na(traincsv$Outlet_Size)] <- "Medium"

###To check if any missing value exist after imputation.
sum(is.na(traincsv))

#To deal with outliers.
library(outliers)
boxplot(traincsv$Item_Outlet_Sales)
boxplot(traincsv$Item_Weight)
boxplot(traincsv$Item_MRP)
boxplot(traincsv$Outlet_Establishment_Year)

outlier1<-outlier(traincsv$Item_Outlet_Sales,logical=TRUE)
findoutliers<- which(outlier1==TRUE,arr.ind = TRUE)

#Removing outliers

NBigdata<- traincsv[-findoutliers,]
nrow(NBigdata)

#We take a look at the number of Item_Fat_Content so far
levels(NBigdata$Item_Fat_Content)
library(plyr)
NBigdata$Item_Fat_Content <- revalue(NBigdata$Item_Fat_Content,c("LF"="Low Fat","low fat"="Low Fat","reg"="Regular"))
NBigdata$Item_Fat_Content <- ifelse(NBigdata$Item_Fat_Content=="Low Fat",1,NBigdata$Item_Fat_Content)
NBigdata$Item_Fat_Content <- ifelse(NBigdata$Item_Fat_Content=="Regular",2,NBigdata$Item_Fat_Content)

#We take a look at the number of Item_Type so far
levels(NBigdata$Item_Type)

NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Baking Goods",0,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Breads",1,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Breakfast",2,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Canned",3,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Dairy",4,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Frozen Foods",5,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Fruits and Vegetables",6,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Hard Drinks",7,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type==" Health and Hygiene",8,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Household",9,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Meat ",10,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type==" Others",11,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Seafood",12,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type==" Snack Foods",13,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Soft Drinks",14,NBigdata$Item_Type)
NBigdata$Item_Type<- ifelse(NBigdata$Item_Type=="Starchy Foods",15,NBigdata$Item_Type)

#We take a look at the number of Outlet_Size so far
levels(NBigdata$Outlet_Size)

NBigdata$Outlet_Size<- ifelse(NBigdata$Outlet_Size=="High",0,NBigdata$Outlet_Size)
NBigdata$Outlet_Size<- ifelse(NBigdata$Outlet_Size=="Medium",1,NBigdata$Outlet_Size)
NBigdata$Outlet_Size<- ifelse(NBigdata$Outlet_Size=="Small",2,NBigdata$Outlet_Size)

#Outlet_Location_Type
levels(NBigdata$Outlet_Location_Type)

NBigdata$Outlet_Location_Type<- ifelse(NBigdata$Outlet_Location_Type=="Tier 1",1,NBigdata$Outlet_Location_Type)
NBigdata$Outlet_Location_Type<- ifelse(NBigdata$Outlet_Location_Type=="Tier 2",2,NBigdata$Outlet_Location_Type)
NBigdata$Outlet_Location_Type<- ifelse(NBigdata$Outlet_Location_Type=="Tier 3",3,NBigdata$Outlet_Location_Type)

#Outlet_Type
levels(NBigdata$Outlet_Type)

NBigdata$Outlet_Type<- ifelse(NBigdata$Outlet_Type==" Grocery Store",0,NBigdata$Outlet_Type)
NBigdata$Outlet_Type<- ifelse(NBigdata$Outlet_Type=="Supermarket Type1",1,NBigdata$Outlet_Type)
NBigdata$Outlet_Type<- ifelse(NBigdata$Outlet_Type=="Supermarket Type2",2,NBigdata$Outlet_Type)
NBigdata$Outlet_Type<- ifelse(NBigdata$Outlet_Type=="Supermarket Type3",3,NBigdata$Outlet_Type)

## Scaling of data
minmax<- function(x){
  (x-min(x))/(max(x)-min(x))
}

NBigdata$Item_Weight <- minmax(NBigdata$Item_Weight)
NBigdata$Item_Visibility <- minmax(NBigdata$Item_Visibility)
NBigdata$Item_MRP <- minmax(NBigdata$Item_MRP)
NBigdata$Outlet_Establishment_Year <- minmax(NBigdata$Outlet_Establishment_Year)


#### To check the missing values
sum(is.na(testcsv))

str(testcsv)

summary(testcsv)

### To find the names of the columns having NA values
colSums(is.na(testcsv))==0

###To find total Na's in Item_weight & Outlet_Size
sum(is.na(testcsv$Item_Weight))
sum(is.na(testcsv$Outlet_Size))

###To impute NA's
testcsv$Item_Weight[is.na(testcsv$Item_Weight)] = mean(testcsv$Item_Weight, na.rm=TRUE)

plot(testcsv$Outlet_Size)
testcsv$Outlet_Size[is.na(testcsv$Outlet_Size)] <- "Medium"

###To check if any missing value exist after imputation.
sum(is.na(testcsv))

#To deal with outliers.
library(outliers)
boxplot(testcsv$Item_Weight)
boxplot(testcsv$Item_MRP)
boxplot(testcsv$Outlet_Establishment_Year)

test <- testcsv

#We take a look at the number of Item_Fat_Content so far
levels(test$Item_Fat_Content)
library(plyr)
test$Item_Fat_Content <- revalue(test$Item_Fat_Content,c("LF"="Low Fat","low fat"="Low Fat","reg"="Regular"))
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content=="Low Fat",1,test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content=="Regular",2,test$Item_Fat_Content)

#We take a look at the number of Item_Type so far
levels(test$Item_Type)

test$Item_Type<- ifelse(test$Item_Type=="Baking Goods",0,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Breads",1,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Breakfast",2,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Canned",3,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Dairy",4,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Frozen Foods",5,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Fruits and Vegetables",6,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Hard Drinks",7,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type==" Health and Hygiene",8,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Household",9,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Meat ",10,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type==" Others",11,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Seafood",12,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type==" Snack Foods",13,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Soft Drinks",14,test$Item_Type)
test$Item_Type<- ifelse(test$Item_Type=="Starchy Foods",15,test$Item_Type)
View(test)

#We take a look at the number of Outlet_Size so far
levels(test$Outlet_Size)

test$Outlet_Size<- ifelse(test$Outlet_Size=="High",0,test$Outlet_Size)
test$Outlet_Size<- ifelse(test$Outlet_Size=="Medium",1,test$Outlet_Size)
test$Outlet_Size<- ifelse(test$Outlet_Size=="Small",2,NBigdata$Outlet_Size)

#Outlet_Location_Type
levels(test$Outlet_Location_Type)

test$Outlet_Location_Type<- ifelse(test$Outlet_Location_Type=="Tier 1",1,test$Outlet_Location_Type)
test$Outlet_Location_Type<- ifelse(test$Outlet_Location_Type=="Tier 2",2,test$Outlet_Location_Type)
test$Outlet_Location_Type<- ifelse(test$Outlet_Location_Type=="Tier 3",3,test$Outlet_Location_Type)

#Outlet_Type
levels(test$Outlet_Type)

test$Outlet_Type<- ifelse(test$Outlet_Type==" Grocery Store",0,test$Outlet_Type)
test$Outlet_Type<- ifelse(test$Outlet_Type=="Supermarket Type1",1,test$Outlet_Type)
test$Outlet_Type<- ifelse(test$Outlet_Type=="Supermarket Type2",2,test$Outlet_Type)
test$Outlet_Type<- ifelse(test$Outlet_Type=="Supermarket Type3",3,test$Outlet_Type)


## Scaling of data
minmax<- function(x){
  (x-min(x))/(max(x)-min(x))
}

test$Item_Weight <- minmax(test$Item_Weight)
test$Item_Visibility <- minmax(test$Item_Visibility)
test$Item_MRP <- minmax(test$Item_MRP)
test$Outlet_Establishment_Year <- minmax(test$Outlet_Establishment_Year)

library(rpart)
library(rpart.plot)
fit<- rpart(NBigdata$Item_Outlet_Sales~., data = NBigdata,method = "anova")
summary(fit)

## predicting the output
pred <- predict(fit,test)

pred1 <- as.data.frame(pred)
View(pred1)

##Submission file
sub <- cbind(as.vector(test$Item_Identifier),as.vector(test$Outlet_Identifier),pred1$pred)

dimnames(sub)=list(c(),c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales"))

BigMartOutput <- as.data.frame(sub)

class(BigMartOutput)
View(BigMartOutput)

write.csv(BigMartOutput,"BigMartprediction.csv",row.names = FALSE)

###Xgboost
library(xgboost)
fit1 <- xgboost(data = data.matrix(NBigdata[,-12]), 
                label = NBigdata[,12], 
                eta = 0.1,
                max_depth = 15, 
                nround=25)
pred2 <- predict(fit1,data.matrix(test))

ppred2 <- as.data.frame(pred2)

##Submission file
sub2 <- cbind(as.vector(test$Item_Identifier),as.vector(test$Outlet_Identifier),ppred2$pred)

dimnames(sub2)=list(c(),c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales"))

BigMartOutput1 <- as.data.frame(sub2)

class(BigMartOutput1)
View(BigMartOutput1)

write.csv(BigMartOutput1,"BigMartprediction1.csv",row.names = FALSE)





