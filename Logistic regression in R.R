dataset = read.csv("C:/Users/Ashok/Downloads/R PROGRAM/2 logistic regression/diabetes.csv")
#diabetes pedigree function - likelihood score given that a patient has diabetes based on family history
View(dataset)

dim(dataset)
str(dataset)
#Except bmi and diabetes pedigree function, all are integer
dataset$Outcome<-as.factor(dataset$Outcome)
summary(dataset)
#all the variables distribution look ok. Insulin have mean and median fartherly placed. might have outliers

#univariate analysis of Age
summary(dataset$Age)
boxplot(dataset$Age)
outliers_age<-boxplot.stats(dataset$Age)$out
dataset$age_bin<-cut(dataset$Age,4,labels<-c("A","B","C","D"))
aggregate(dataset$Age,list(dataset$age_bin),FUN=min)
aggregate(dataset$Age,list(dataset$age_bin),FUN=max)
#age_bins are 21-36,37-51,52-66 and 67-81

#Univariate analysis of Pregnancies
summary(dataset$Pregnancies)
boxplot(dataset$Pregnancies)
boxplot.stats(dataset$Pregnancies)$out
?boxplot


#univariate analysis for Insulin
table(dataset$Insulin)
Insulin_Zero<-dataset[which(dataset$Insulin==0),]
nrow(Insulin_Zero)
(374/768)*100 #It is good to try to create 2 models - one for people whose insulin level is 0 and one for others
table(Insulin_Zero$age_bin)

nrow(unique(dataset)) # No duplicates
unique(is.na(dataset)) # No missing values
boxplot(dataset[-length(dataset)]) # All variables have outliers
#lets take no action now and see how the model would look like

#split the data - not efficient way
set.seed(3)
train<-sample(nrow(dataset),ceiling(nrow(dataset)*0.7))
test = (1:nrow(dataset))[-train]
dataset_train<- dataset[train,]
dataset_test<- dataset[test,]

#splitting the right way - stratified sampling using the target variable
require(caret)
set.seed(236)
train.rows<-createDataPartition(y=dataset$Outcome,p=0.7,list = FALSE)
dataset_train<-dataset[train.rows,]
dataset_test<-dataset[-train.rows,]

##Fitting the glm model
model_log1<-glm(Outcome~.,data=dataset_train,family = "binomial")
summary(model_log1) #AIC =515.14

#removing skinthickness
model_log2<-glm(Outcome~. -SkinThickness,data=dataset_train,family = "binomial")
summary(model_log2) #AIC = 513.36

#removing insulin
model_log3<-glm(Outcome~Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction+Age,data=dataset_train,family="binomial")
summary(model_log3) #AIC = 511.91

#removing Age
model_log4<-glm(Outcome~Pregnancies+Glucose+BMI+DiabetesPedigreeFunction+BloodPressure,data=dataset_train,family="binomial")
summary(model_log4) #AIC = 511.49 - Not much improvement

#Predict test set
Pred_test<-predict(model_log3,dataset_test,type="response")
Pred_test
dataset_test$Outcome

#Predict train set
Pred_train<-predict(model_log4,dataset_train,type="response")
Pred_train
dataset_train$Outcome

##Model metrics for test set#######
table(Predicted = Pred_train>0.5,Actual = dataset_train$Outcome)
#We need to reduce False positives = 34 and False negatives = 78
#accuracy = (TP+TN)/(TP+TN+FP+FN)


#Specificity = TN/(TN+FP)


#Sensitivity or recall = TP/(TP+FN)


#Precision = TP/(TP+FP)


#F1-Score = 2PR/(P+R)



#Plot ROC Curve
#install.packages("ROCR")
require(ROCR)
Roc_pred <- prediction(Pred_train,dataset_train$Outcome)
ROC_reference<- performance(Roc_pred,"tpr","fpr")
plot(ROC_reference,colorize = T, print.cutoffs.at=seq(0.1,by=0.1))

#Fine tuning the model using ROC Curve
table(Predicted = Pred_train>0.6,Actual = dataset_train$Outcome)
table(Predicted = Pred_train>0.5,Actual = dataset_train$Outcome)
table(Predicted = Pred_train>0.4,Actual = dataset_train$Outcome)
