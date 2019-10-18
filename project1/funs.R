# funs ---
create_one_cluster <- function(center, size) {
  center %>% map(function(x) {
    rnorm(size, mean = x)
  }) %>%
    as.data.frame() -> out
  
  names(out) <- paste0("x", seq_along(names(out)))
  out
}


sample_clusters <- function(centers, size = 10) {
  # centers - list
  centers %>%
    map(create_one_cluster, size) %>%
    bind_rows(.id = "cluster_id")
}


kmeanspp <- function(x, k)
{
  x <- as.matrix(x)
  centers <- matrix(0, nrow = k, ncol = ncol(x))
  centers[1, ] <- x[sample(1:nrow(x), 1), , drop = FALSE]
  target_point <- centers[1L, , drop = FALSE]
  
  d <- apply(x, MARGIN = 1,
             function(x)
               (sqrt(sum((x - target_point) ^ 2)))^2)
  for (l in 2:k) {
    centers[l, ] <- x[sample(1:nrow(x), 1, prob = d), , drop = FALSE]
    
    target_point <- centers[l, , drop = FALSE]
    
    arg2 <- apply(x, MARGIN = 1,
                  function(x)
                    (sqrt(sum((x - target_point) ^ 2)))^2)
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




create_data <- function(cluster_centers, n=100){
  sample_clusters(cluster_centers, n) -> data_with_id
  data_with_id %>% select(-cluster_id)
}


generate_cluster_centers <- function(cnt, dim_cnt){
  map(1:cnt, function(x) {runif(dim_cnt, min = 0, max = 500)})
}

run_full_experiment <- function(data, pp = FALSE, niter = 1000){
  data %>%
    run(niter = niter, k = k, pp = pp)
}

generate_full_data <- function(k = 3, dim_cnt = 2, n_points = 100){
  generate_cluster_centers(k, dim_cnt) %>%
    create_data(n = n_points)
}



