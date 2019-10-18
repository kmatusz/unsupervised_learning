# Kmeans++
# Demonstracja złej inicjalizacji zwykłego kmeans
# Benchmarki szybkości i zajebistości zwykłego i z flexclust
# Efektywniejsza implementacja?

# Kod
# 1. worst case - prostokąt
# 2. Wygenerowanie małego datasetu - 3 klastry i puszczenie zwykłego kmeans milion razy
# 3. Porównanie klastrowania pod względem statystyk z idealnym wypadkiem
# 4. Na tym samym datasecie puszczenie kmeans++ z flexclust tyle samo razy i porównanie wyników
# 5. Benchmarki szybkości flexclust i zwykłego - porównanie na małym i dużym datasecie
# 6. Jeżeli szybkość będzie znacząco różna to implementacja w C++
library(tidyverse)
library(flexclust)
library(factoextra)

# tests ----
kmeans(cars, 2) -> a
factoextra::fviz_cluster(object = a, data = cars)

# funs ----
create_one_cluster <- function(center, size){
  center %>% map(function(x) {
    rnorm(size, mean = x)
  }) %>% 
    as.data.frame() -> out
  
  names(out) <- paste0("x", seq_along(names(out)))
  out
}


sample_clusters <- function(centers, size = 10){
  # centers - list
  centers %>% 
    map(create_one_cluster, size) %>%
    bind_rows(.id = "cluster_id")
}


kmeanspp <- function(x, k)
{
  x <- as.matrix(x)
  centers <- matrix(0, nrow=k, ncol=ncol(x))
  centers[1,] <- x[sample(1:nrow(x), 1), , drop=FALSE]
  target_point <- centers[1L,,drop=FALSE]
  
  d <- apply(x, MARGIN = 1, 
             function(x) sqrt(sum((x-target_point)^2)))
  for(l in 2:k){
    centers[l,] <- x[sample(1:nrow(x), 1, prob=d), , drop=FALSE]
    
    target_point <- centers[l,,drop=FALSE]
    
    arg2 <- apply(x, MARGIN = 1, 
                  function(x) sqrt(sum((x-target_point)^2)))
    d <- pmin(d, arg2)
    
    
    
  }
  centers
}


# fake data generation ----
cluster_centers <- list(
  c(1, 10),
  c(2, 3),
  c(5, 8)
)


sample_clusters(cluster_centers, 300) -> data_with_id



# show the data- plot and sample clustering ----
ggplot(data_with_id, aes(x=x1, y=x2, color=cluster_id))+
  geom_point()+
  theme_minimal()+
  coord_fixed()

data <- data_with_id %>% select(-cluster_id)

# sample clustering
kmeans(data, centers = cluster_centers, iter.max = 1) -> res
res %>% fviz_cluster(data)


# Bulk standard k means clustering
map(1:1000, function(x){
  kmeans(data, centers = 3)
}) -> bulk_results 

# save betweenss close to the best
betweenss_baseline <- res$betweenss

# saving cluster results to data frame
bulk_results %>%
  map_df(function(x){
    broom::glance(x)
  }) -> cluster_results 

cluster_results$cluster_object <- bulk_results










k = 3

map(1:1000, function(x){
  if(x %% 100 == 0) print(x) 
  kmeans(data, centers = kmeanspp(data, k))
}) -> bulk_results_pp



# saving cluster results to data frame
bulk_results_pp %>%
  map_df(function(x){
    broom::glance(x)
  }) -> cluster_results_pp

cluster_results_pp$cluster_object <- bulk_results_pp

# Analysis of results 

# show plot of betweenss - histogram
cluster_results %>% ggplot(aes(x=betweenss))+geom_histogram()

# Showing bad clusterings - number
cluster_results %>% filter(betweenss < 9000) # 140 (14%)

# Example plot
cluster_results %>% filter(betweenss < 9000) %>% 
  head(1) %>% 
  .$cluster_object %>%
  .[[1]] %>% 
  fviz_cluster(data)




# Analysis of results 

# show plot of betweenss - histogram
cluster_results_pp %>% ggplot(aes(x=betweenss))+geom_histogram()

# Showing bad clusterings - number
cluster_results_pp %>% filter(betweenss < 9000) 

# Example plot
cluster_results_pp %>% filter(betweenss < 3000) %>% 
  head(1) %>% 
  .$cluster_object %>%
  .[[1]] %>% 
  fviz_cluster(data)








