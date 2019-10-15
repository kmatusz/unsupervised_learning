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



  

