library(readr)
insurance=read.csv(file.choose())
summary(insurance)
normalise=scale(insurance)

#clustering
t=NULL
for (i in 2:6) {
  t=c(t,kmeans(normalise,centers = i)$tot.withinss)
}
t
#elbow curve
plot(2:6,t,type="b",xlab = "no of clusters",ylab = "total within ss")
title("Elbow curve")
#k-means
fit<-kmeans(normalise,3)
fit$cluster
finaldata<-data.frame(fit$cluster,insurance)
aggregate(insurance[,1:5],by =list(fit$cluster),FUN=mean)
