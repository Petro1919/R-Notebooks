library(data.table)
library(tidyverse)
library(factoextra)

data("USArrests")
?USArrests
UA <- scale(USArrests)

set.seed(101)

# Hierarchical clustering
hc <- hclust(dist(x = UA, method = 'euclidean'), method = 'complete')

fviz_dend(x = hc, k = 4) +
  geom_hline(yintercept = 3.5, linetype = "dashed")

# Hierarchical k-means clustering
hkmeans_cluster <- hkmeans(x = UA, 
                           k = 4, 
                           hc.metric = 'euclidean', 
                           hc.method = 'complete')

fviz_cluster(object = hkmeans_cluster)
