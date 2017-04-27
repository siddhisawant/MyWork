library(rpart)
data<-read.csv("C:/Users/prati/Downloads/Telco-Customer-Churn.csv")
head(data)
data <- data[-1]
anyNA(data)
which(is.na(data),T) #how many NA values with row and column number
data <- na.omit(data) #removing NA values

class1<-subset(data,data$Churn=="Yes")
class0<-subset(data,data$Churn=="No")

s1<-sample(1:nrow(class1),floor(0.7*nrow(class1)))
s0<-sample(1:nrow(class0),floor(0.7*nrow(class0)))

train1<-class1[s1,]
train0<-class0[s0,]

test1<-class1[-s1,]
test0<-class0[-s0,]
train<-rbind(train1,train0)
test<-rbind(test1,test0)
head(train)
head(test)

fit <- rpart(Churn ~ gender+ SeniorCitizen+ Partner+ Dependents+ PhoneService+ MultipleLines+ InternetService+ OnlineSecurity+ OnlineBackup+ DeviceProtection+ TechSupport+ StreamingTV+ StreamingMovies+ Contract+ PaperlessBilling+ PaymentMethod+ MonthlyCharges+ TotalCharges, method = "class", train, control = rpart.control(minsplit = 100, cp = 0.001))
fit

fit$cptable

fit1=prune(fit,0.006498471)
fit1

pred<-predict(fit1,test,type = "class")



library(caret)
ConMat<-confusionMatrix(pred,test$Churn)
ConMat
