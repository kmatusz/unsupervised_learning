# Cluster generation


create_one_cluster <- function(center, size) {
  center %>% purrr::map(function(x) {
    rnorm(size, mean = x, sd = 1)
  }) %>%
    as.data.frame() -> out
  
  names(out) <- paste0("x", seq_along(names(out)))
  out
}


create_clusters_from_centers <- function(centers, size = 10) {
  # centers - list
  centers %>%
    map(create_one_cluster, size) %>%
    bind_rows(.id = "cluster_id")
}


generate_dataset <- function(k = 3,
                             dim_cnt = 2,
                             n_points = 100) {
  map(1:k,
      
      function(x) {
        runif(dim_cnt, min = 0, max = 500)
      }) -> centers
  
  CENTERS_GENERATED <<- centers
  
  create_clusters_from_centers(centers, n_points) %>%
    select(-cluster_id)
}


kmeanspp <- function(x, k)
{
  x <- as.matrix(x)
  centers <- matrix(0, nrow = k, ncol = ncol(x))
  centers[1,] <- x[sample(1:nrow(x), 1), , drop = FALSE]
  target_point <- centers[1L, , drop = FALSE]
  
  map(1:ncol(x), function(i) {
    (x[, i] - target_point[, i]) ^ 2
  }) -> distances_list
  
  
  d <- sqrt(Reduce(`+`, distances_list)) ^ 2
  
  for (l in 2:k) {
    centers[l,] <- x[sample(1:nrow(x), 1, prob = d), , drop = FALSE]
    
    target_point <- centers[l, , drop = FALSE]
    
    map(1:ncol(x), function(i) {
      (x[, i] - target_point[, i]) ^ 2
    }) -> distances_list
    
    
    arg2 <- sqrt(Reduce(`+`, distances_list)) ^ 2
    d <- pmin(d, arg2)
    
    
    
  }
  centers
}


run <- function(data,
                niter = 1000,
                k = 3,
                pp = FALSE) {
  # Bulk standard k means clustering
  map(1:niter, function(x) {
    if (x %% 100 == 0)
      print(x)
    if (!pp) {
      kmeans(data, centers = k)
      
    } else {
      kmeans(data, centers = kmeanspp(data, k))
    }
  }) -> bulk_results
  
  # saving cluster results to data frame
  bulk_results %>%
    map_df(function(x) {
      broom::glance(x)
    }) -> cluster_results
  
  cluster_results$cluster_object <- bulk_results
  
  
  cluster_results
}
