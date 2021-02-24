library(readxl)
airlines=read_xlsx(file.choose(),2)
summary(airlines)
normalise=scale(airlines[,2:12])
summary(normalise)

# clusterring
t=NULL
for (i in 2:7) {
  t=c(t,kmeans(normalise,centers = i)$tot.withinss)
  
}
t
#plotting the elbow curve

plot(2:7, t, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title("K-Means Clustering Elbow-Plot")

#k-means clusterring
fit<-kmeans(normalise,3)
fit$cluster
str(fit)
final<-data.frame(fit$cluster,airlines)
aggregate(airlines[,2:12] , by =list(fit$cluster), FUN = mean)
