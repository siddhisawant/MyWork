setwd("C:/Users/SIDDHI/Documents/Capstone/myproject")

#reading the file into R
lungcap<- read.csv("LungCapData.csv")

head(lungcap)

#converting categorical variables into factors.
lungcap$Smoke=as.factor(lungcap$Smoke)
lungcap$Gender=as.factor(lungcap$Gender)
lungcap$Caesarean=as.factor(lungcap$Caesarean)

#To check the structure of the data.
str(lungcap)

#Plotting the lungcap.
plot(lungcap)

#plotting histogram 
hist(lungcap$ï..LungCap)
hist(lungcap$Height)

#plotting box plot

boxplot(lungcap)

#To deal with outliers.
library(outliers)

outlier1<-outlier(lungcap$ï..LungCap,logical=TRUE)
findoutliers<- which(outlier1==TRUE,arr.ind = TRUE)

#Removing outliers

newlungcap<- lungcap[-findoutliers,]
nrow(newlungcap)


#splitting of data
sam<- sample(1:nrow(newlungcap),round(0.80*nrow(newlungcap)))
train<- newlungcap[sam,]
test<-newlungcap[-sam,]

#To build a linear model

model<- lm(ï..LungCap~., train)
model

plot(model)

#prediction of the model

predictlungcap<- predict(model,test)
predictlungcap

rmse<-sqrt(mean((test$ï..LungCap-predictlungcap)^2))
rmse




