## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## ------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(factoextra)

set.seed(10)


## ------------------------------------------------------------------------
create_one_cluster <- function(center, size) {
  # Center - cluster center coordinates, given as list
  # Size - amount of points to create
  
  center %>% purrr::map(function(x) {
    rnorm(size, mean = x, sd = 1)
  }) %>%
    as.data.frame() -> out
  
  names(out) <- paste0("x", seq_along(names(out)))
  out
}


## ------------------------------------------------------------------------
create_one_cluster(list(1,2), 10) %>% 
  kable() %>% 
  kable_styling()


## ------------------------------------------------------------------------
create_clusters_from_centers <- function(centers, size = 10) {
  # centers - list
  centers %>%
    purrr::map(create_one_cluster, size) %>%
    dplyr::bind_rows(.id = "cluster_id")
}


## ------------------------------------------------------------------------
cluster_centers <- list(c(1, 10),
                        c(2, 3),
                        c(5, 8))

create_clusters_from_centers(cluster_centers, 100) -> data_with_id


## ------------------------------------------------------------------------
data_with_id %>%
  head() %>%
  kable() %>% 
  kable_styling()


## ------------------------------------------------------------------------
ggplot(data_with_id, aes(x = x1, y = x2, color = cluster_id)) +
  geom_point() +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  coord_fixed() +
  labs(color = "Cluster") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_y_continuous(breaks = seq(0, 12, 2))


## ------------------------------------------------------------------------
data <- data_with_id %>% select(-cluster_id)

kmeans(data, centers = 3) -> res
res %>% 
  fviz_cluster(data, geom = "point") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  coord_fixed() +
  labs(title = NULL)
  



## ------------------------------------------------------------------------
run <- function(data,
                niter = 1000,
                k = 3) {
  # Bulk standard k means clustering
  map(1:niter, function(x) {
      kmeans(data, centers = k)
  }) -> bulk_results
  
  # saving cluster results to data frame
  bulk_results %>%
    map_df(function(x) {
      broom::glance(x)
    }) -> cluster_results
  
  cluster_results$cluster_object <- bulk_results
  
  
  cluster_results
}



## ------------------------------------------------------------------------
bulk_results <- run(data, niter = 1000, k = 3)
bulk_results %>% 
  head(5) 



## ------------------------------------------------------------------------
ggplot(bulk_results, aes(x=tot.withinss))+ 
  geom_histogram(fill = "#377EB8") +
  theme_minimal(base_size = 13) +
  coord_fixed()


## ------------------------------------------------------------------------
bulk_results %>%
  filter(tot.withinss < 1000) %>%
  head(1) %>%
  .$cluster_object %>%
  .[[1]] -> bad_clustering

  fviz_cluster(bad_clustering, data = data, geom = "point") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    coord_fixed() +
    labs(title = NULL)


## ------------------------------------------------------------------------
kmeanspp <- function(x, k)
{
  x <- as.matrix(x)
  centers <- matrix(0, nrow = k, ncol = ncol(x))
  centers[1, ] <- x[sample(1:nrow(x), 1), , drop = FALSE] # Take first point at random
  target_point <- centers[1L, , drop = FALSE] 
  
  map(1:ncol(x), function(i){
      (x[,i] - target_point[,i]) ^ 2
    }) -> distances_list
    
    
  d <- sqrt(Reduce(`+`, distances_list))^2
  
  for (l in 2:k) {
    centers[l, ] <- x[sample(1:nrow(x), 1, prob = d), , drop = FALSE] # Select next point with probability weighted by distance from starting point
    
    target_point <- centers[l, , drop = FALSE]
    
    map(1:ncol(x), function(i){
      (x[,i] - target_point[,i]) ^ 2
    }) -> distances_list
    
    
    arg2 <- sqrt(Reduce(`+`, distances_list))^2
    d <- pmin(d, arg2)
    
    
    
    
    
  }
  centers
}


## ------------------------------------------------------------------------
kmeans(data, centers = kmeanspp(data, 3))


## ------------------------------------------------------------------------
load("results/batch_experiments.Rdata")

results_k10_n10_perfect <- kmeans(data_k10_n10, centers = data_k10_n10_centers)
results_k3_n2_perfect <- kmeans(data_k3_n2, centers = data_k3_n2_centers)



## ------------------------------------------------------------------------


d<-get_dist(data_k10_n10[sample(1:nrow(data_k10_n10), 100),], method = "euclidean")
fviz_dist(d, show_labels = FALSE)+ 
  labs(title = "Dissimilarity matrix",
       subtitle = "(n = 10, k= 10)") -> diss_k10_n10

d<-get_dist(data_k3_n2, method = "euclidean")
fviz_dist(d, show_labels = FALSE)+ 
  labs(
    title = NULL,
    subtitle = "(n = 2, k= 3)") -> diss_k3_n2


## ------------------------------------------------------------------------
cowplot::plot_grid(diss_k10_n10, diss_k3_n2)


## ------------------------------------------------------------------------

results_k10_n10 %>% 
  mutate(type = "standard") %>%
  rbind(results_k10_n10_pp %>% 
          mutate(type = "pp")) %>%
  mutate(type = factor(type, levels = c("standard", "pp"))) -> results_k10_n10_all

ggplot(results_k10_n10_all, aes(x = tot.withinss, fill = type, color = type)) +
         geom_histogram(alpha=0.5) +
  geom_vline(xintercept =  round(results_k10_n10_perfect$tot.withinss, 2),
             color = "red", linetype = "dashed") +
  facet_grid(rows = vars(type), scales = "free") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "Algorithm",
       x = "tot.withinss (x100000)") +
  guides(color = FALSE) +
  scale_x_continuous(labels = function(x) round(x/100000))



## ------------------------------------------------------------------------
results_k10_n10_all %>% 
  select(-cluster_object) %>%
  group_by(type) %>%
  summarise(
    min = min(tot.withinss),
    q25 = quantile(tot.withinss, 0.25),
    q50 = quantile(tot.withinss, 0.5),
    q75 = quantile(tot.withinss, 0.75),
    max = max(tot.withinss)
  ) %>% 
  arrange(desc(type)) %>%
  kable() %>%
  kable_styling()


## ------------------------------------------------------------------------
standard <- function(){kmeans(data, centers = 3)}
pp <- function(){kmeans(data, centers = kmeanspp(data, 3))}
microbenchmark::microbenchmark(
standard(),
pp()
)


