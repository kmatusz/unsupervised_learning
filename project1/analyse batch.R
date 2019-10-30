load("results/batch_experiments.Rdata")


results_k10_n10 %>% 
  mutate(type = "standard") %>%
  rbind(results_k10_n10_pp %>% 
          mutate(type = "++")) -> results_k10_n10_all

ggplot(results_k10_n10_all, aes(x = tot.withinss, fill = type, color = type)) +
         geom_histogram(alpha=0.5) +
  facet_grid(rows = vars(type), scales = "free") +
  theme_minimal()


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
  arrange(desc(type))


d<-get_dist(data_k10_n10, method = "euclidean")
fviz_dist(d, show_labels = FALSE)+ labs(title = "Dissimilarity matrix (n = 10, k= 10)") -> diss_k10_n10
diss_k10_n10

d<-get_dist(data_k3_n2, method = "euclidean")
fviz_dist(d, show_labels = FALSE)+ labs(title = "Dissimilarity matrix (n = 2, k= 3)") -> diss_k3_n2
diss_k3_n2




