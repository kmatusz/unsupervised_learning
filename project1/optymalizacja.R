kmeanspp <- function(x, k)
{
  x <- as.matrix(x)
  centers <- matrix(0, nrow=k, ncol=ncol(x))
  centers[1,] <- x[sample(1:nrow(x), 1), , drop=FALSE]
  target_point <- centers[1L,,drop=FALSE]
  
  d <- apply(x, MARGIN = 1, 
                   function(x) dist(rbind(x, target_point)))
  for(l in 2:k){
    centers[l,] <- x[sample(1:nrow(x), 1, prob=d), , drop=FALSE]
    
    target_point <- centers[l,,drop=FALSE]
    
    arg2 <- apply(x, MARGIN = 1, 
                        function(x) dist(rbind(x, target_point)))
    d <- pmin(d, arg2)
    
    
    
  }
  centers
}



cluster_centers <- tribble( ~ x, ~ y,
                            1, 10,
                            2, 3,
                            5, 8,
                            10, 20, 
                            10, 30,
                            2,5,
                            4,10,
                            1,1,
                            4,8,
                            10, 12
                            
                            )

x <- sample_clusters(cluster_centers, 100) %>% select(-cluster_id)



k <- 10

profvis::profvis(kmeanspp(x,k))
microbenchmark::microbenchmark(kmeanspp(x,k), times=3)
microbenchmark::microbenchmark(kmeanspp_fast(x,k), times=3)
# przyspieszenie 8 razy- od 800 do 100 ms



d <- apply(x, MARGIN = 1, 
           function(x) sqrt(sum((x-target_point)^2)))



x[1,] %>% dput
target_point %>% dput()

target_point_bench <- structure(c(1.47144492178536, 3.25424048127007), .Dim = 1:2)
x_bench <- c(x = 0.135464192175944, y = 11.6618270256801)

microbenchmark::microbenchmark(sqrt(sum((x_bench-target_point_bench)^2)))


library(Rcpp)





Rcpp::sourceCpp("c.cpp")

distC(target_point_bench, x_bench)









