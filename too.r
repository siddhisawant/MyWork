setwd("C:/Users/SIDDHI/Documents/tookitaki/test data")

#####Loading data into R####
raw_datatrain<- read.csv("raw_data_70_new.csv")
raw_datatest<- read.csv("raw_data_30_new.csv")
rawdatabind<- rbind(raw_datatrain,raw_datatest)

####Checking for dimensions of the data####
dim(rawdatabind)

#####Let's take look at the strc of the data####
str(rawdatabind)

#####Let's check for na in the data####
sum(is.na(rawdatabind))

#####find column names with the missing data####
misscol<- names(rawdatabind[,!complete.cases(t(rawdatabind))])

removecol<- sort(sapply(rawdatabind, function(x) sum(length(which(is.na(x)))))/nrow(rawdatabind),decreasing = TRUE)

#Drop features that have more than 50% missing and take a look at what features that are still missing:
discard_column = names(removecol[removecol>0.5])
rawdatabind = (rawdatabind[,!(names(rawdatabind) %in% discard_column)])


####Checking for na's again####
#take a look at what features that are still missing
misscol1<- names(rawdatabind[,!complete.cases(t(rawdatabind))])
sum(is.na(rawdatabind))
dim(rawdatabind)
rawdatabind<- na.omit(rawdatabind)
dim(rawdatabind)
#write.csv(rawdatabind,file = "rawdatabind.csv")
#drop<- c("feature_8","feature_10","feature_27","feature_9","feature_13","feature_18","feature_36","feature_37","feature_38","feature_51","feature_61","feature_73")
#rawdatabind1<- rawdatabind[,!names(rawdatabind) %in% drop]
#dim(rawdatabind1)
rawdatabind$Bad_label<- as.factor(rawdatabind$Bad_label)
str(rawdatabind)

#####Lets convert categorical numbers into factors#####
rawdatabind$feature_4<- as.factor(rawdatabind$feature_4)
rawdatabind$feature_14<- as.factor(rawdatabind$feature_14)
rawdatabind$feature_19<- as.factor(rawdatabind$feature_19)
rawdatabind$feature_25<- as.factor(rawdatabind$feature_25)
rawdatabind$feature_36<- as.factor(rawdatabind$feature_36)
rawdatabind$feature_31<- as.factor(rawdatabind$feature_31)
rawdatabind$feature_39<- as.factor(rawdatabind$feature_39)
rawdatabind$feature_40<- as.factor(rawdatabind$feature_40)
rawdatabind$feature_41<- as.factor(rawdatabind$feature_41)
rawdatabind$feature_55<- as.factor(rawdatabind$feature_55)
rawdatabind$feature_67<- as.factor(rawdatabind$feature_67)
rawdatabind$feature_76<- as.factor(rawdatabind$feature_76)


#####Using BorotuPackage for feature selection#####
library(Boruta)
boruta.train <- Boruta(Bad_label~., data = rawdatabind, doTrace = 2)
print(boruta.train)
plot(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
k <-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(k) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(k,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#####the time to take decision on tentative attributes#####
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
plot(final.boruta)
#####Let's obtain the list of confirmed attributes#####
datafinal<- getSelectedAttributes(final.boruta, withTentative = F)

#####We'll create a data frame of the final result derived from Boruta.#####
boruta.df <- attStats(final.boruta)


impfeature<- c("customer_no", "entry_time" , "feature_2",   "feature_3" ,  "feature_7" ,  "feature_12",
               "feature_14" , "feature_15" , "feature_16" , "feature_20" , "feature_22" , "feature_25",
               "feature_28" , "feature_29" , "feature_30",  "feature_33" , "feature_35",  "feature_41",
               "feature_43" , "feature_44" , "feature_47" , "feature_52" , "feature_56" , "feature_63",
               "feature_64" , "feature_65" , "feature_66" , "feature_67" , "feature_69" , "feature_71",
               "feature_77")
datafinal1<- rawdatabind[,names(rawdatabind) %in% impfeature]
dim(datafinal1)
datafinal1$Bad_label<- rawdatabind$Bad_label


######Enquiry data into R######
enqtrain<- read.csv("raw_enquiry_70_new.csv")
enqtest<- read.csv("raw_enquiry_30_new.csv")
enqdata<- rbind(enqtrain,enqtest)

missingcol<- names(enqdata[,!complete.cases(t(enqdata))])
str(enqdata)

enqdata$enq_amt<- ifelse(is.na(enqdata$enq_amt), mean(enqdata$enq_amt, na.rm=TRUE), enqdata$enq_amt)
enqdata$enq_purpose<- ifelse(is.na(enqdata$enq_purpose),mean(enqdata$enq_purpose,na.rm = TRUE),enqdata$enq_purpose)
sum(is.na(enqdata))
#enqtrain$mean_diff_opendt_eqdt<- (as.numeric(as.character(enqtrain$dt_opened))-as.numeric(as.character(enqtrain$enquiry_dt)))

enqdata$mean_diff_opendt_eqdt<- (as.numeric(enqdata$dt_opened)-as.numeric(enqdata$enquiry_dt))
enqdata<- enqdata[-4]
enqdata<- enqdata[-1]

#####Loading Account dataset in R #####
acctrain<- read.csv("raw_account_70_new.csv")
acctest<- read.csv("raw_account_30_new.csv")
accdata<- rbind(acctrain,acctest)

missingcol<- names(accdata[,!complete.cases(t(accdata))])
accdata$creditlimit<- ifelse(is.na(accdata$creditlimit),mean(accdata$creditlimit,na.rm = TRUE),accdata$creditlimit)
accdata$cashlimit<- ifelse(is.na(accdata$cashlimit),mean(accdata$cashlimit,na.rm = TRUE),accdata$cashlimit)

removecol<- sort(sapply(accdata, function(x) sum(length(which(is.na(x)))))/nrow(accdata),decreasing = TRUE)

#Drop features that have more than 50% missing and take a look at what features that are still missing:
discard_column = names(removecol[removecol>0.5])
accdata = (accdata[,!(names(accdata) %in% discard_column)])

####Checking for na's again####
#take a look at what features that are still missing
colnames(accdata)[colSums(is.na(accdata)) > 0]
sum(is.na(accdata))

accdata$high_credit_amt<- ifelse(is.na(accdata$high_credit_amt),mean(accdata$high_credit_amt,na.rm = TRUE),accdata$high_credit_amt)
dim(accdata)
#View(acctrain)
accdata$total_diff_lastpay_opendt<- (as.numeric(accdata$last_paymt_dt)-(as.numeric(accdata$opened_dt)))
accdata$currbal_credit_limit<- (accdata$cur_balance_amt)/(accdata$creditlimit)
accdata<- accdata[-6:-8]
accdata<- accdata[-13:-15]
accdata<- accdata[-1]
sum(is.na(accdata))


write.csv(accdata,file = "accdata.csv")
#class(accdata$paymenthistory2)
accdata<- read.csv("accdata.csv",na.strings = c("","NA"))
accdata<- na.omit(accdata)
accdata<- accdata[-1]
accdata<- accdata[-7]
library(mlbench)
library(caret)
# define the control using a random forest selection function
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(as.numeric(unlist(datafinal1[,1:30])), as.numeric(unlist(datafinal1[,31])), sizes=c(1:30), rfeControl=control)

#####Lets Merge the Data into one main tabel#####

library(base)

data1<- merge(datafinal1,accdata, by= "customer_no")
data2<- merge(data1,enqdata, by= "customer_no")

######Exploratory Analysis#####
library(ggplot2)
p1<- ggplot(data2,aes(Bad_label,acct_type,fill=Bad_label))+geom_bar(stat = "identity")
p1
p2<- ggplot(data2,aes(Bad_label,enq_amt,fill=Bad_label))+geom_bar(stat = "identity")
p2
p3<- ggplot(data2,aes(Bad_label,currbal_credit_limit))+geom_bar(stat = "identity")
p3
######Diving the huge data into part #####
part<- data2[1:20000,]


#####Selecting Important variables from maindata###
boruta.train1 <- Boruta(Bad_label~., data = part, doTrace = 2)
print(boruta.train1)
plot(boruta.train1)
plot(boruta.train1, xlab = "", xaxt = "n")
k <-lapply(1:ncol(boruta.train1$ImpHistory),function(i)
  boruta.train1$ImpHistory[is.finite(boruta.train1$ImpHistory[,i]),i])
names(k) <- colnames(boruta.train1$ImpHistory)
Labels <- sort(sapply(k,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train1$ImpHistory), cex.axis = 0.7)

#####the time to take decision on tentative attributes#####
final.boruta1 <- TentativeRoughFix(boruta.train1)
print(final.boruta1)
plot(final.boruta1)
#####Let's obtain the list of confirmed attributes#####
getimp<- getSelectedAttributes(final.boruta1, withTentative = F)

#####We'll create a data frame of the final result derived from Boruta.#####
boruta.imp <- attStats(final.boruta1)

#####Dropping unwanted variables#####
dropcol<- c("feature_14", "feature_33", "feature_52", "feature_67")
data2<- data2[,!names(data2) %in% dropcol]
#part<- part[-28]

cor(as.numeric(unlist(data2$feature_20)),as.numeric(unlist(data2$feature_47)))
cor(as.numeric(unlist(data2$feature_28)),as.numeric(unlist(data2$feature_43)))
data2<- data2[-17]
data2<- data2[-18]
cor(as.numeric(unlist(data2$entry_time)),as.numeric(unlist(data2$feature_2)))
data2<- data2[-3]
data2<- data2[-1]

library(CORElearn)
importance<- attrEval(Bad_label~.,data2,estimator="InfGain")
colnames(data2)
#####Removing Least important Variables#####
dropvar<- c("feature_15","feature_20","feature_35","feature_41","feature_56","feature_63",
            "feature_64","feature_69", "owner_indic" ,"paymt_str_dt","paymt_end_dt")
data2<- data2[,!names(data2) %in% dropvar]



library(caret)
# define the control using a random forest selection function
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(as.numeric(unlist(data2[,1:38])), as.numeric(unlist(datafinal1[,24])), sizes=c(1:38), rfeControl=control)
#part1<- data2[1:295790,]
#pd<- sample(1:nrow(part1),round(0.7*nrow(part1)))
#train1<- part1[pd,]
#test1<- part1[-pd,]
data<- data2
str(data2)
data2$entry_time<- as.numeric(data2$entry_time)
data2$feature_12<- as.numeric(data2$feature_12)
data2$feature_16<- as.numeric(data2$feature_16)
data2$feature_22<- as.numeric(data2$feature_22)
data2$feature_25<- as.numeric(data2$feature_25)
data2$feature_28<- as.numeric(data2$feature_28)
data2$feature_77<- as.numeric(data2$feature_77)
data2$upload_dt.x<- as.numeric(data2$upload_dt.x)
data2$reporting_dt<- as.numeric(data2$reporting_dt)
data2$paymenthistory1<- as.numeric(data2$paymenthistory1)
data2$paymenthistory2<- as.numeric(data2$paymenthistory2)
data2$upload_dt.y<- as.numeric(data2$upload_dt.y)
data2<- data2[-16]
library(outliers)
outlier1<-outlier(data2$enq_amt,logical=TRUE)
findoutliers<- which(outlier1==TRUE,arr.ind = TRUE)

outlier2<- outlier(data2$currbal_credit_limit,logical = TRUE)
findoutliers2<- which(outlier2==TRUE,arr.ind = TRUE)

#####Diving the dataset into train and test######
finaldata<-sample(1:nrow(data2),floor(0.7*nrow(data2)))
train<- data2[finaldata,]
test<- data2[-finaldata,]

#principal component analysis
pcdata <- prcomp(train, scale. = T)
names(pcdata)
dim(pcdata$x)
biplot(pcdata, scale = 0)
#compute standard deviation of each principal component
stddev <- pcdata$sdev

#compute variance
prvar <- stddev^2

#check variance of first 10 components
prvar[1:10]

#proportion of variance explained
propvarex <- prvar/sum(prvar)
propvarex[1:20]

#scree plot
plot(propvarex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(propvarex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")



