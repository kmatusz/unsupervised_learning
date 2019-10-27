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

source("funs.R")

# tests ---
kmeans(cars, 2) -> a
factoextra::fviz_cluster(object = a, data = cars)


a <- run_full_experiment(pp = FALSE, niter = 100, k=10, dim_cnt = 2, n_points = 100)


# fake data generation ----
cluster_centers <- list(c(1, 10),
                        c(2, 3),
                        c(5, 8))

f(cluster_centers, 100)

cluster_centers %>%
  map(function(x) {
    x <- as.data.frame(x %>% t())
    #names(x) <- paste0("x", seq_along(x))
    x
  }) %>%
  bind_rows() -> cluster_centers_df

names(cluster_centers_df) <-
  paste0("x", seq_along(cluster_centers_df))
#c("cluster_id", paste0("x", 1:(ncol(cluster_centers_df)-1)))

cluster_centers_df

sample_clusters(cluster_centers, 100) -> data_with_id



# show the data- plot and sample clustering ----
ggplot(data_with_id, aes(x = x1, y = x2, color = cluster_id)) +
  geom_point() +
  theme_minimal() +
  coord_fixed()

data <- data_with_id %>% select(-cluster_id)

# sample clustering
kmeans(data, centers = cluster_centers_df, iter.max = 1) -> res
res %>% fviz_cluster(data)


cluster_results <- run(data, niter = 1000, k = 3, pp = FALSE)
cluster_results_pp <- run(data, niter = 1000, k = 3, pp = TRUE)


cluster_results


# Analysis of results ----

# show plot of betweenss - histogram
exp2 %>% ggplot(aes(x = betweenss)) + geom_histogram()

# Showing bad clusterings - number
cluster_results %>% filter(betweenss < 3000) # 140 (14%)

# Example plot
cluster_results %>% filter(betweenss < 3000) %>%
  head(1) %>%
  .$cluster_object %>%
  .[[1]] %>%
  fviz_cluster(data)




# Analysis of results

# show plot of betweenss - histogram
cluster_results_pp %>% ggplot(aes(x = betweenss)) + geom_histogram()

# Showing bad clusterings - number
cluster_results_pp %>% filter(betweenss < 3000)

# Example plot
cluster_results_pp %>% filter(betweenss < 3000) %>%
  head(1) %>%
  .$cluster_object %>%
  .[[1]] %>%
  fviz_cluster(data)
