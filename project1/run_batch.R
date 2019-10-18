library(tidyverse)
library(flexclust)
library(factoextra)

source("funs.R")



set.seed(10)

data1 <- generate_full_data(k=3, dim_cnt = 2, n_points = 100)

exp1 <- data1 %>% run(niter = 1000, k = 3, pp = FALSE)
exp1_pp <- data1 %>% run(niter = 1000, k = 3, pp = TRUE)


data2 <- generate_full_data(k=10, dim_cnt = 10, n_points = 100)

exp2 <- data2 %>% run(niter = 1000, k = 10, pp = FALSE)
exp2_pp <- data2 %>% run(niter = 1000, k = 10, pp = TRUE)


exp2 <- run_full_experiment(pp = FALSE, niter = 1000, k=10, dim_cnt = 10, n_points = 100)
exp2_pp <- run_full_experiment(pp = TRUE, niter = 1000, k=10, dim_cnt = 10, n_points = 100)


save(exp1, exp1_pp,
     exp2, exp2_pp,
     exp3, exp3_pp,
     exp4, exp4_pp, file = "results/batch_experiments.Rdata")



mean(exp1$betweenss)
mean(exp1_pp$betweenss)
mean(exp2$betweenss)
mean(exp2_pp$betweenss)

mean(exp2$betweenss) / mean(exp2_pp$betweenss)


mean(exp1$tot.withinss)
mean(exp1_pp$tot.withinss)
median(exp2$tot.withinss)
median(exp2_pp$tot.withinss)

