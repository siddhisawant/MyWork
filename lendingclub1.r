#set working directory
setwd("C:/Users/SIDDHI/Documents/Capstone/myproject/Aditya Birla")

#loading the file into R.
lendingclub <- read.csv("loan.csv")

#Checking the dimensions of the data. 
dim(lendingclub)

#Removing current status.
lendingclub1 <- lendingclub[which(lendingclub$loan_status != "Current" ),] 

dim(lendingclub1)

## Check for missing data

# Identifies total no. of missing values
sum(is.na(lendingclub1))

#Checking structure of the dataset.
str(lendingclub1)


#Identifies column names with missing data
names(lendingclub1[, !complete.cases(t(lendingclub1))]) 

#We drop features that contain too many NA values. We can list the raito of NA value for each feature
a = sort(sapply(lendingclub1, function(x) sum(length(which(is.na(x)))))/nrow(lendingclub1),decreasing = TRUE)

#Drop features that have more than 50% missing and take a look at what features that are still missing:
discard_column = names(a[a>0.5])
lendingclub1 = (lendingclub1[,!(names(lendingclub1) %in% discard_column)])

#take a look at what features that are still missing
a = sort(sapply(lendingclub1, function(x) sum(length(which(is.na(x)))))/nrow(lendingclub1),decreasing = TRUE)
a[a>0]

#impute missing values with the median value. 
library(caret)
median_impute_model = preProcess(lendingclub1[names(a)],method="medianImpute")
lendingclub1 = predict(median_impute_model,lendingclub1)

#Let's re-check the columns left, there should be no feature with NA value
sort(sapply(lendingclub1, function(x) sum(length(which(is.na(x)))))/nrow(lendingclub1),decreasing = TRUE)


##EXPLORATARY ANALYSIS

#Loan amount Distribution based on Grades assigned by Lending Club
library(ggplot2)
ggplot(lendingclub1, aes(loan_amnt, col = grade)) + geom_histogram(bins = 50) + facet_grid(grade ~ .)
#Those with higher grades (A, B, C and D) have received more loans compared to those with lower grades (E, F and G).

##Exploring interest rates based on Grades assigned by Lending Club
ggplot(lendingclub1, aes(int_rate, fill = grade)) + geom_density() + facet_grid(grade ~ .)
#Grades are assigned based on risk, and so interest rates go up as the risk goes up.

##Total loan issued over the years [2007 - 2015]
library(lubridate)
lendingclub1$issue_d <- dmy(paste0("01-",lendingclub1$issue_d))
lendingclub1$earliest_cr_line<-dmy(paste0("01-",lendingclub1$earliest_cr_line))

#Plot for issue_date and loan_amnt.
ggplot(lendingclub1, aes(issue_d, loan_amnt)) + geom_bar(stat = "identity")

##plot for Total loan amount for each loan status
ggplot(lendingclub1, aes(loan_status, loan_amnt, fill = loan_status)) + geom_bar(stat = "identity") + scale_x_discrete(breaks=NULL)


#We take a look at the number of each loan_status so far
library(dplyr)
library(stringr)
lendingclub1 %>% group_by(loan_status) %>% summarise(count=n())

#Creating a new columns in the dataset
Badloans<- c("Charged Off","Default","Does not meet the credit policy. Status:Charged Off",
                             "In Grace Period"," Late (16-30 days)"," Late (31-120 days)")
lendingclub1$GoodBadloans<- ifelse(lendingclub1$loan_status %in% Badloans,0,
                                  ifelse(lendingclub1$loan_status=="",NA,1))
lendingclub1$GoodBadloans<- as.factor(lendingclub1$GoodBadloans)

lendingclub1 <- na.omit(lendingclub1)
sum(is.na(lendingclub1$GoodBadloans))

#plot for Goodloan and charged of loan
ggplot(lendingclub1, aes(GoodBadloans, loan_amnt, fill = GoodBadloans)) + geom_bar(stat = "identity")


#plot for GoodBadloans and the Grades
ggplot(lendingclub1, aes(sub_grade, loan_amnt, fill = GoodBadloans)) + geom_bar(position="fill",stat = "identity") 
#as the grade goes down, the proportion of the badloan loan increases.


#plot for loan_amnt and purpose
ggplot(lendingclub1,aes(purpose,loan_amnt,fill=GoodBadloans))+geom_bar(position = "fill", stat = "identity")+ theme(axis.text.x=element_text(size=8, angle = 90))

# Convert character employment length to numeric variable
lendingclub1$emp_length <- ifelse(lendingclub1$emp_length == "10+", 10, lendingclub1$emp_length)
lendingclub1$emp_length <- ifelse(lendingclub1$emp_length == "< 1", 0.5, lendingclub1$emp_length)
lendingclub1$emp_length <- as.numeric(lendingclub1$emp_length)

#categorize own&mortage:1 remaining0
home <- c("OWN","MORTAGE")

lendingclub1$home_ownership <- ifelse(lendingclub1$home_ownership %in% home, 1,
                                     ifelse(lendingclub1$home_ownership=="", NA, 0))
lendingclub1$home_ownership<- as.factor(lendingclub1$home_ownership)

#Removing Irrelevant Columns

Removecolumns<- c("pymnt_plan", 
                  "delinq_2yrs",
                  "inq_last_6mths",
                  "pub_rec",
                  "initial_list_status",
                  "out_prncp",
                  "out_prncp_inv",
                  "total_pymnt_inv",
                  "total_rec_prncp",
                  "total_rec_int",
                  "total_rec_late_fee",
                  "recoveries",
                  "collection_recovery_fee",
                  "last_pymnt_amnt",
                  "collections_12_mths_ex_med",
                  "policy_code",
                  "application_type",
                  "acc_now_delinq",
                  "tot_cur_bal",
                  "tot_coll_amt",
                  "total_rev_hi_lim",
                  "total_pymnt",
                  "verification_status_joint",
                  "url",
                  "desc",
                  "last_pymnt_d",
                  "next_pymnt_d",
                  "last_credit_pull_d",
                  "title",
                  "zip_code",
                  "addr_state",
                  "emp_title",
                  "loan_status",
                  "grade",
                  "funded_amnt",
                  "funded_amnt_inv")

newlendclub = (lendingclub1[,!(names(lendingclub1) %in% Removecolumns)])
dim(newlendclub)
summary(newlendclub)

#Removing nas 
newlendclub <- newlendclub[complete.cases(newlendclub), ]
 str(newlendclub)
 newlendclub <- na.omit(newlendclub)
summary(newlendclub)

#Removing columns
newlendclub<- newlendclub[-1:-2]

#Scaling
minmax<- function(x){
  (x-min(x))/(max(x)-min(x))
}

newlendclub$loan_amnt <- minmax(newlendclub$loan_amnt)
newlendclub$int_rate <- minmax(newlendclub$int_rate)
newlendclub$installment<- minmax(newlendclub$installment)
newlendclub$emp_length<- minmax(newlendclub$emp_length)
newlendclub$annual_inc<- minmax(newlendclub$annual_inc)
newlendclub$dti <- minmax(newlendclub$dti)
newlendclub$open_acc<- minmax(newlendclub$open_acc)
newlendclub$revol_bal <- minmax(newlendclub$revol_bal)
newlendclub$revol_util<- minmax(newlendclub$revol_util)
newlendclub$total_acc<- minmax(newlendclub$total_acc)


####spliiting of data#####
set.seed(123)
class1<- subset(newlendclub,newlendclub$GoodBadloans==1)
class2<- subset(newlendclub,newlendclub$GoodBadloans==0)

s1<- sample(1:nrow(class1),round(0.80*nrow(class1)))
s2<- sample(1:nrow(class2),round(0.80*nrow(class2)))

train0<- class1[s1,]
train1<- class2[s2,]

test0<- class1[-s1,]
test1<- class2[-s2,]

train<- rbind(train0,train1)
test<- rbind(test0,test1)
str(train)

#Building model.

log1<- glm(GoodBadloans~.,data=train,family = binomial("logit"))
predict1<- predict(log1,test,type = "response")
predict1


library(caret)
library(ROCR)

ppp <- ifelse(predict1>=0.9,1,0)
ppp
pr <- prediction(predict1, test$GoodBadloans)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="green",lwd=2,main="ROC curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="red")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


step(log1)

log2<-  glm(formula = GoodBadloans ~ loan_amnt + term + int_rate + installment + 
              sub_grade + emp_length + annual_inc + verification_status + 
              issue_d + purpose + dti + earliest_cr_line + revol_bal + 
              revol_util + total_acc, family = binomial("logit"), data = train)

predict2<- predict(log2,test,type = "response")
predict2


library(caret)
library(ROCR)

ppp1 <- ifelse(predict2>=0.9,1,0)
ppp1
pr1 <- prediction(predict2, test$GoodBadloans)
prf1 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf1,col="green",lwd=2,main="ROC curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="red")

auc <- performance(pr1, measure = "auc")
auc <- auc@y.values[[1]]
auc




