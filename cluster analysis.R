# install.packages("PerformanceAnalytics")
# nstall.packages("NbClust")
library(NbClust)

library(PerformanceAnalytics)
library(readxl)
library(dplyr)
library(corrplot)
library(nbClust)
library(cluster)
library("cluster")


setwd("C:/Users/PC/demo_p")
getwd()
mydata = read_excel("normal.xlsx")
head(mydata)
names(mydata)
names(mydata)<-c("fid", "sido_code", "sido_name", "sigun_code", "sigun_name", "corona_i", "visitors_i", "rest_i", "trans_i", "accom_i", "outdoor_i", "index")
head(mydata)
summary(mydata)
mydata1 <- select(mydata, "corona_i", "visitors_i", "rest_i", "trans_i", "accom_i", "outdoor_i", "index")
head(mydata1)
pairs(mydata1)
apply(mydata1, 2, shapiro.test)
cor(mydata1)
plot(mydata1)
pairs(mydata1, panel = panel.smooth)
chart.Correlation(mydata1, histogram= TRUE, pch = 19)
mydata1.cor <- cor(mydata1)
mydata1.cor
par(mfrow = c(1,1))
corrplot(mydata1.cor, method ="ellipse")
plot(mydata1)

# cluster

access <- read_excel("access.xlsx")
head(access)
access_s<-select(access, -"...1" )
head(access_s)
access_s <- scale(access_s)
set.seed(123)
nc <- NbClust(access_s, 
              distance = "euclidean",
              method = "kmeans",
              min.nc = 2, max.nc = 4)
nc$Best.nc
table(nc$Best.nc[1,])

set.seed(123)
clustering.km <- kmeans(access_s, centers = 3,
                        nstart = 25)

clustering.km$size
clustering.km$cluster

# cluster_index

mydata2 <- select(mydata,  "corona_i", "visitors_i", "rest_i", "trans_i", "accom_i", "outdoor_i")
head(mydata2)
set.seed(123)
nc <- NbClust(mydata2, 
              distance = "euclidean",
              method = "kmeans",
              min.nc = 2, max.nc = 8)
nc$Best.nc
table(nc$Best.nc[1,])

set.seed(123)
clustering.km <- kmeans(mydata2_s, centers = 7,
                        nstart = 25)

clustering.km$size
clustering.km$cluster
cluster
par(mfrow = c(1,1))

# (mydata2, by = list(cluster = clustering.km$cluster), mean)

clusplot(x = mydata2, 
         clus = clustering.km$cluster,
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0, main = "cluster plot")

# cluster3
index2 = read_excel("index2.xlsx")
index_orgin <- select(index2, "시군구명칭2", "safety", "travel" )
index_orgin <-as.matrix(index_orgin)
rownames(index_orgin) <- index_orgin$시군구명칭2
View(index_orgin)
names(index2)[names(index2) == "시군구명칭2"] <- c("city_name")
index2 <- select(index2, "safety", "travel" )
summary(index2)
set.seed(123)
nc <- NbClust(index2, 
              distance = "euclidean",
              method = "kmeans",
              min.nc = 2, max.nc = 5)
nc$Best.nc
table(nc$Best.nc[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]))


# Visualize silhouhette information

set.seed(123)
km.res <- kmeans(index2, centers = 4)
sil <- silhouette(km.res$cluster, dist(index2))
fviz_silhouette(sil)



# 시각화
set.seed(123)
clustering.km <- kmeans(index2, centers = 4,
                        nstart = 25)



clustering.km$size
clustering.km$cluster
clustering.km$centers

index_cluster <-aggregate(index_origin, by=list(cluster = clustering.km$cluster), mean)
head(index_cluster)
index
par(mfrow = c(1,1))
clusplot(x = index2,
         clus = clustering.km$cluster,
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0, main = "cluster plot")
