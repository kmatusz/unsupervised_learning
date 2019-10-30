library(tidyverse)
library(factoextra)

source("funs.R")

set.seed(100)

data_k3_n2 <- generate_dataset(k=3, dim_cnt = 2, n_points = 100)

results_k3_n2 <- data_k3_n2 %>% run(niter = 1000, k = 3, pp = FALSE)
results_k3_n2_pp <- data_k3_n2 %>% run(niter = 1000, k = 3, pp = TRUE)

set.seed(100)
data_k10_n10 <- generate_dataset(k=10, dim_cnt = 10, n_points = 100)

results_k10_n10 <- data_k10_n10 %>% run(niter = 1000, k = 10, pp = FALSE)
results_k10_n10_pp <- data_k10_n10 %>% run(niter = 1000, k = 10, pp = TRUE)




save(data_k3_n2,
     data_k10_n10,
     results_k3_n2,
     results_k3_n2_pp,
     results_k10_n10,
     results_k10_n10_pp,
     file = "results/batch_experiments.Rdata")


load("results/batch_experiments.Rdata")
mean(results_k3_n2$tot.withinss)
mean(results_k3_n2_pp$tot.withinss)


mean(results_k10_n10$tot.withinss)
mean(results_k10_n10_pp$tot.withinss)



# mean(exp1$betweenss)
# mean(exp1_pp$betweenss)
# mean(exp2$betweenss)
# mean(exp2_pp$betweenss)
# 
# mean(exp2$beweenss) / mean(exp2_pp$betweenss)
# 
# 
# mean(exp1$tot.withinss)
# mean(exp1_pp$tot.withinss)
# median(exp2$tot.withinss)
# median(exp2_pp$tot.withinss)

