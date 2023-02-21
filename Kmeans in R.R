drug<-read.csv("C:/Users/user/Downloads/Imarticus related/IBM WATSON/DRUG1n.csv")
summary(drug)

#Removing unnecessary variables
drug1<-drug[-c(1:4,7)] # Age could have been included but for demo and visualization purpose, only 2 input variables are used
summary(drug1)

normalize<-function(x){
  z<-(x-min(x))/(max(x)-min(x))
  return(z)
}

#Normalizing the drug1 dataset
drug2<-apply(drug1,2,normalize)
summary(drug2)

###OR####
#Standardizing the drug1 dataset
m<-apply(drug1,2,mean)
s<-apply(drug1,2,sd)
drug3<-scale(drug1,m,s)
?scale

###Fit Kmeans model########
km<-kmeans(drug3,3,iter.max = 100,nstart=1)
summary(km)
km$cluster
km$centers
km$withinss
km$tot.withinss
km$betweenss

###################################
#Creating a function to return sum of within sum of squares
km_fun<-function(k){
  km<-kmeans(drug3,k,iter.max = 100,nstart = 100)
  return(sum(km$withinss))
}

######################################
#Plotting Elbow Curve
a<-1:30
drug.withinss<-as.data.frame(sapply(a,km_fun))
drug.withinss$a<-a
names(drug.withinss)<-c("ss","a")
head(drug.withinss)
plot(drug.withinss$a,drug.withinss$ss,type="b")
#Plot shows that ideal k=4
###Fitting best model using the elbow curve approach
km1<-kmeans(drug3,4,iter.max = 100,nstart = 100)
km1$tot.withinss

#Plotting the clusters
require(cluster)
clusplot(drug3,km1$cluster)
km2<-kmeans(drug3,5,iter.max = 100,nstart = 100)
km2$tot.withinss
clusplot(drug3,km2$cluster,shade = TRUE,main="Cluster of Drugs",xlab = 'Sodium levels',ylab = 'Pottasium levels',lines = 0)
