install.packages("cluster")
install.packages("factoextra")
install.packages("flexclust")
install.packages("fpc")
install.packages("clustertend")
install.packages("ClusterR")

library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(tidyverse)


data_in <-
  read.csv(
    "cluster.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )

data_in %>%
  as_tibble() %>%
  mutate(
    population_total = XA19 + XA20 + XA21,
    workforce_ratio = XA20 / population_total
  ) -> data


data_full <- data %>%
  select(XA06, XA31, workforce_ratio) %>%
  rename(unemp_rate = XA06,
         salary = XA31)


data_unemp_sal <- data_full %>%
  select(-workforce_ratio)


###############################
dane <- data_in
dim(dane) # checking the dimensions of the dataset

# data selection
xxx <-
  dane[, c(25, 32)] #XA06–unemployment rate, XA31–salaries (Poland=100%)
dane$population.total <- dane$XA19 + dane$XA20 + dane$XA21
dane$workforce.ratio <- dane$XA20 / dane$population.total
names(dane)
yyy <- dane[, c(25, 32, 36)]
################################

# the same variables, different number of clusters and distance metrics
km1 <- eclust(xxx, "kmeans", hc_metric = "euclidean", k = 3)
fviz_cluster(km1, main = "kmeans / Euclidean")

km2 <- eclust(xxx, "kmeans", hc_metric = "manhattan", k = 4)
fviz_cluster(km2, main = "kmeans / Manhattan")



# one variable extra, different number of clusters and distance metrics
km1 <- eclust(yyy, "kmeans", hc_metric = "euclidean", k = 3)
fviz_cluster(km1, main = "kmeans / Euclidean")

km2 <- eclust(yyy, "kmeans", hc_metric = "manhattan", k = 4)
fviz_cluster(km2, main = "kmeans / Manhattan")

attributes(km1)


D<-daisy(xxx) # calculates the dissimilarity matrix, cluster:: package
plot(silhouette(km10$cluster, D), col=1:2, border=NA)


sil<-silhouette(km10$cluster, dist(xxx))
fviz_silhouette(sil)
# linia to średnia
# jeżeli większość klastra jest ponad średnią to klaster jest homogenous



###### Pam

pam1<-eclust(xxx, "pam", k=3) # factoextra::
fviz_silhouette(pam1)
fviz_cluster(pam1)

pam1$medoids
summary(pam1)



c(1:100) %>% map(function(x){
  xxx
}) %>% bind_rows() %>% as_tibble() -> xxx_big


pam3<-pam(xxx_big,3) 




#### TASK 1
km_list <- map(1:5, function(x) {
  eclust(xxx, "kmeans", hc_metric = "euclidean", k=x)
})


km.xxx.2<-eclust(xxx, "kmeans", hc_metric="euclidean",k=2)

km.xxx.3<-eclust(xxx, "kmeans", hc_metric="euclidean",k=3)

km.xxx.4<-eclust(xxx, "kmeans", hc_metric="euclidean",k=4)

km.xxx.5<-eclust(xxx, "kmeans", hc_metric="euclidean",k=5)




plot(km.xxx.2$centers, col="black", pch=17, cex=1.1, xlim=c(0,31), ylim=c(60,170))
points(km.xxx.3$centers, col="red", pch=18, cex=1)
points(km.xxx.4$centers, col="green", pch=16, cex=1)
points(km.xxx.5$centers, col="blue", pch=15, cex=1)
legend(20,160, 
       c("KMeans k=2","KMeans k=3", "KMeans k=4", "KMeans k=5"), 
       col=c("black","red","green", "blue"), 
       pch=c(17,18,16,15 ), bty="n")
points(xxx, col="grey20", pch=".")











pam.yyy.2 <- eclust(xxx, "pam", hc_metric="euclidean",k=2)


library(plotly)
library(dplyr)
plot_ly() %>%
  add_markers(x=km.yyy$centers$XA06,y=km.yyy$centers$XA31, z=km.yyy$centers$workforce.ratio,
              marker = list(size = 2, color = "blue"))


  

