library(readr)
crime=read.csv(file.choose())
summary(crime) #here we can see that thare are outlires since the mean median values are not same and they have huge difference in them
normalise=scale(crime[,2:5])
summary(normalise)

#elbow curve , here ,I am experimenting this data from 2 to 10 clusters
t<-NULL
for (i in 2:10) {
  t<- c(t,kmeans(normalise, centers = i)$tot.withinss)
}
t


#plotting 
plot(2:10,t,type="b",xlab = " No.of clusters",ylab = "Within groups sum of squares")
title("K-Means Clustering Elbow curve")

#k-means clusterring
fit<- kmeans(normalise,3)
str(fit)
fit$cluster
finaldata<-data.frame(fit$cluster,crime)
aggregate(crime[, 2:5], by = list(fit$cluster), FUN = mean)
