dataset = read.csv("C:/Users/user/Downloads/Imarticus related/R programming/Class/2 Logistic_Regression/Dataset/diabetes.csv")
#diabetes pedigree function - likelihood score given that a patient has diabetes based on family history
View(dataset)
str(dataset)
dataset$Outcome<-as.factor(dataset$Outcome)

#Split the data into train and test
require(caret)
set.seed(236)
train.rows<-createDataPartition(y=dataset$Outcome,p=0.7,list = FALSE)
dataset_train<-dataset[train.rows,]
dataset_test<-dataset[-train.rows,]

###Create a Decision tree model#########
require(rpart)
dtree_model1<- rpart(formula = Outcome~.,data=dataset_train,method="class")

#Plot the tree
require(rpart.plot)
rpart.plot(x=dtree_model1)
#yesno = 2: Add Yes/No at each level
#type = 0: Draws a plot at each split and a node label at each leaf
rpart.plot(x=dtree_model1,yesno=2,type=0)
#extra=0, Removes all the unnecessary info
rpart.plot(x=dtree_model1,yesno=2,type=0,extra=0)

#Evaluation of test set
Predicted_test<-predict(object = dtree_model1,newdata = dataset_test,type = "class")

#CF Matrix
table(Predicted_test,dataset_test$Outcome)
cf<-confusionMatrix(data=Predicted_test,reference = dataset_test$Outcome)
cf
#accuracy
cf$overall["Accuracy"]

#Comparing Gini index and information gain
dtree_gini<-rpart(formula = Outcome~.,data=dataset_train,method="class",parms=list(split="gini"))
dtree_info<-rpart(formula = Outcome~.,data=dataset_train,method="class",parms=list(split="information"))
Predicted_test_gini<-predict(object = dtree_gini,newdata = dataset_test,type = "class")
Predicted_test_info<-predict(object = dtree_info,newdata = dataset_test,type = "class")
cf_gini<-confusionMatrix(Predicted_test_gini,dataset_test$Outcome)
accuracy_gini<-cf_gini$overall["Accuracy"]
cf_info<-confusionMatrix(Predicted_test_info,dataset_test$Outcome)
accuracy_info<-cf_info$overall["Accuracy"]

#####Pruning######

#Cost parameter plot
plotcp(dtree_gini)
print(dtree_gini$cptable)
cp_val_index<-which.min(dtree_gini$cptable[,"xerror"])
cp_val<-dtree_gini$cptable[cp_val_index,"CP"]

#pruning the tree based on cost parameter
dtree_prune<-prune(tree=dtree_gini,cp=cp_val)
rpart.plot(dtree_prune,yesno=2,type=0,extra=0)
Predicted_prune<-predict(object = dtree_prune,newdata = dataset_test,type = "class")
cf_prune<-confusionMatrix(Predicted_prune,dataset_test$Outcome)
accuracy_prune<-cf_prune$overall["Accuracy"] #increased the accuracy

#Grid search
#min num of obs that should exist in a node for split to be attempted
minsplit<-seq(1,20,1)
#Max depth for any node of the final tree
maxdepth<-seq(1,20,1)
grid<-expand.grid(minsplit=minsplit,maxdepth = maxdepth)

model_ct<-nrow(grid)

diabetes_models<-list()
for (i in 1:model_ct){
  minsplit = grid$minsplit[i]
  maxdepth = grid$maxdepth[i]
  diabetes_models[[i]]<-rpart(Outcome~.,dataset_train,method="class",minsplit=minsplit,maxdepth=maxdepth)
}

#Check any model
diabetes_models[[399]]


#capture accuracy
accuracy_models<-list()
for (i in 1:model_ct){
  Predicted_prune<-predict(object = diabetes_models[[i]],newdata = dataset_test,type = "class")
  cf_prune<-confusionMatrix(Predicted_prune,dataset_test$Outcome)
  accuracy_models[[i]]<-cf_prune$overall["accuracy"]
}

