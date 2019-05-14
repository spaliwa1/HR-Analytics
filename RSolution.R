help.start()
?xlsx
getwd()
install.packages("ggplot2")
library(ggplot2)
install.packages("xlsx")
library(xlsx)
#load training data
trainHR<-read.csv("D:\\Kaggle\\HR Solution\\train_LZdllcl.csv",na.strings = "")
#load test or validation data
testHR<-read.csv("D:\\Kaggle\\HR Solution\\test_2umaH9m.csv",na.strings = "")
 # Data Exploration-train
dim(trainHR)
head(trainHR)
str(trainHR)
summary(trainHR$avg_training_score)
summary(trainHR)
hist(trainHR$awards_won.)
summary(trainHR$no_of_trainings)
class(trainHR$previous_year_rating)
ggplot(data=trainHR)+geom_point(mapping=aes(x=trainHR$is_promoted,y=trainHR$length_of_service))
# Data Exploration-test
summary(testHR)
#Data Preparation-Checking for missing value 
summary(trainHR$previous_year_rating)#This has 4124 missing values which is 7.52% of total data
summary(trainHR$department)# No missing value
summary(trainHR$region)# No missing value
summary(trainHR$education) #This has 2409 missing values which is 4.39% of total data
summary(trainHR$gender)# No missing value
summary(trainHR$recruitment_channel)# No missing value
summary(trainHR$no_of_trainings)# No missing value
summary(trainHR$age)# No missing value
summary(trainHR$length_of_service)# No missing value
summary(trainHR$KPIs_met..80.)# No missing value
summary(trainHR$awards_won.)# No missing value
summary(trainHR$avg_training_score)# No missing value
summary(trainHR$is_promoted)# No missing value
#Missing Value Treatment
#for education field-let's replace "NA's" with "Missing"
trainHR$education1<-ifelse(is.na(trainHR$education),"Missing",as.character(trainHR$education))
trainHR$education1<-as.factor(trainHR$education1)
trainHR<-trainHR[,-4]#removing the old education field
testHR$education1<-ifelse(is.na(testHR$education),"Missing",as.character(testHR$education))
testHR$education1<-as.factor(testHR$education1)
testHR<-testHR[,-4]#removing the old education field
#installing "Hmisc" package for imputaion of continuous variable"
install.packages("Hmisc")
library(Hmisc)
trainHR$previous_year_rating1<-with(trainHR,impute(previous_year_rating))
trainHR<-trainHR[,-8]
testHR$previous_year_rating1<-with(testHR,impute(previous_year_rating))
testHR<-testHR[,-8]
#Outlier Detection
boxplot(trainHR$no_of_trainings)# 9 entries potentiallly could be outliers
hist(trainHR$no_of_trainings)
boxplot(trainHR$age)# 6 entries potentiallly could be outliers
boxplot(trainHR$length_of_service)#1 entry potentiallly could be outliers
hist(trainHR$KPIs_met..80.)
boxplot(trainHR$avg_training_score)#No outlier
#Building first glm model using all variables
mod1<-glm(trainHR$is_promoted~.,data=trainHR[,-1],family = "binomial")
summary(mod1)
step(mod1,direction="both")
#let's create dummy variables for the categorical variables which proved significant for both train and test
trainHR$departmentFinance<-ifelse(trainHR$department=="Finance",1,0)
testHR$departmentFinance<-ifelse(testHR$department=="Finance",1,0)
trainHR$departmentHR<-ifelse(trainHR$department=="HR",1,0)
testHR$departmentHR<-ifelse(testHR$department=="HR",1,0)
trainHR$departmentLegal<-ifelse(trainHR$department=="Legal",1,0)
testHR$departmentLegal<-ifelse(testHR$department=="Legal",1,0)
trainHR$departmentOperations<-ifelse(trainHR$department=="Operations",1,0)
testHR$departmentOperations<-ifelse(testHR$department=="Operations",1,0)
trainHR$departmentProcurement<-ifelse(trainHR$department=="Procurement",1,0)
testHR$departmentProcurement<-ifelse(testHR$department=="Procurement",1,0)
trainHR$departmentRD<-ifelse(trainHR$department=="R&D",1,0)
testHR$departmentRD<-ifelse(testHR$department=="R&D",1,0)
trainHR$departmentSales<-ifelse(trainHR$department=="Sales & Marketing",1,0)
testHR$departmentSales<-ifelse(testHR$department=="Sales & Marketing",1,0)
trainHR$departmentTechnology<-ifelse(trainHR$department=="Technology",1,0)
testHR$departmentTechnology<-ifelse(testHR$department=="Technology",1,0)
trainHR$regionregion_17<-ifelse(trainHR$region=="region_17",1,0)
testHR$regionregion_17<-ifelse(testHR$region=="region_17",1,0)
trainHR$regionregion_22<-ifelse(trainHR$region=="region_22",1,0)
testHR$regionregion_22<-ifelse(testHR$region=="region_22",1,0)
trainHR$regionregion_23<-ifelse(trainHR$region=="region_23",1,0)
testHR$regionregion_23<-ifelse(testHR$region=="region_23",1,0)
trainHR$regionregion_25<-ifelse(trainHR$region=="region_25",1,0)
testHR$regionregion_25<-ifelse(testHR$region=="region_25",1,0)
trainHR$regionregion_29<-ifelse(trainHR$region=="region_29",1,0)
testHR$regionregion_29<-ifelse(testHR$region=="region_29",1,0)
trainHR$regionregion_32<-ifelse(trainHR$region=="region_32",1,0)
testHR$regionregion_32<-ifelse(testHR$region=="region_32",1,0)
trainHR$regionregion_34<-ifelse(trainHR$region=="region_34",1,0)
testHR$regionregion_34<-ifelse(testHR$region=="region_34",1,0)
trainHR$regionregion_4<-ifelse(trainHR$region=="region_4",1,0)
testHR$regionregion_4<-ifelse(testHR$region=="region_4",1,0)
trainHR$regionregion_7<-ifelse(trainHR$region=="region_7",1,0)
testHR$regionregion_7<-ifelse(testHR$region=="region_7",1,0)
trainHR$regionregion_9<-ifelse(trainHR$region=="region_9",1,0)
testHR$regionregion_9<-ifelse(testHR$region=="region_9",1,0)
trainHR$education1Master<-ifelse(trainHR$education1=="Master's & above",1,0)
testHR$education1Master<-ifelse(testHR$education1=="Master's & above",1,0)
trainHR$education1Missing<-ifelse(trainHR$education1=="Missing",1,0)
testHR$education1Missing<-ifelse(testHR$education1=="Missing",1,0)
#Adding dependent variable column in testHR
testHR$is_promoted<-NA
#splitting the trainHR into training and validation data
set.seed(200)
index<-sample(nrow(trainHR),.70*nrow(trainHR),replace=F)
train<-trainHR[index,]
val<-trainHR[-index,]
#Creating second model using only significant variables
mod2<-glm(trainHR$is_promoted~departmentFinance+education1Missing+education1Master+regionregion_9+regionregion_7+regionregion_4+regionregion_34
          +regionregion_32+regionregion_29+regionregion_25+regionregion_23+regionregion_22+regionregion_17
          +departmentTechnology+departmentSales+departmentHR+departmentRD+departmentProcurement
          +departmentOperations+departmentLegal+previous_year_rating1+avg_training_score+awards_won.
          +KPIs_met..80.+length_of_service+age+no_of_trainings,data=trainHR,family = "binomial")
summary(mod2)
mode3<-glm(train$is_promoted~departmentFinance+education1Master+regionregion_9+regionregion_7+regionregion_4
           +regionregion_29+regionregion_25+regionregion_23+regionregion_22+regionregion_17
           +departmentTechnology+departmentSales+departmentHR+departmentRD+departmentProcurement
           +departmentOperations+departmentLegal+previous_year_rating1+avg_training_score+awards_won.
           +KPIs_met..80.+length_of_service+age+no_of_trainings,data=train,family = "binomial")
summary(mode3)
pred<-predict(mode3,type="response",newdata=val)
head(pred)
tail(pred)
table(train$is_promoted)/nrow(train)
pred<-ifelse(pred>=0.0859377,1,0)
install.packages("irr")
install.packages("lpSolve")
library(irr)
install.packages("caret")
library(caret)
kappa2(data.frame(val$is_promoted,pred))
table(pred)
table(val$is_promoted)
u <- union(pred,val$is_promoted)
t <- table(factor(pred, u), factor(val$is_promoted, u))
confusionMatrix(t)
#plot ROC
install.packages("ROCR")
library(ROCR)
install.packages("Metrics")
library(Metrics)
pr <- prediction(pred,val$is_promoted)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) 
c<-auc(val$is_promoted,pred) #0.7815

#LET'S PREDICT 
pred1<-predict(mod2,type="response",newdata=testHR)
table(trainHR$is_promoted)/nrow(trainHR)
pred1<-ifelse(pred1>=0.08517005,1,0)
install.packages("irr")
install.packages("lpSolve")
library(irr)
install.packages("caret")
library(caret)
kappa2(data.frame(testHR$is_promoted,pred1))
table(pred1)
table(testHR$is_promoted)
v <- union(pred1,testHR$is_promoted)
m <- table(factor(pred1, v), factor(testHR$is_promoted, v))
confusionMatrix(m)
employee_id<-testHR$employee_id
output.df<-as.data.frame(employee_id)
output.df$is_promoted<-pred1
write.csv(output.df, file = "HR Analytics.csv", row.names = FALSE)