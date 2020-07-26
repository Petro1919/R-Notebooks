library(tidyverse)
library(ISLR)     # for dataset
library(cluster)  # for gower similarity and pam
library(scales)   # for rescaling data

set.seed(42)

data("College")
head(College)
?College

# Some feature engineering first
college_clean <- College %>%
  mutate(name = row.names(.),                             # new variable with row names
         accept_rate = Accept/Apps,                       # new variable with acceptation rate
         isElite = cut(Top10perc,                         # new categorycal variable
                       breaks = c(0, 50, 100),
                       labels = c("Not Elite", "Elite"),
                       include.lowest = TRUE)) %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, isElite)

head(college_clean)

college_scaled <- college_clean %>% 
  mutate_if(is.numeric, rescale)                          # scale data between 0 and 1



## Calculating dissimilarity matrix (Gower distance: between 0-1)
gower_dist <- daisy(college_scaled[, -1],
                    metric = "gower",
                    # type = list(logratio = 3),          # Column 3: logarithmic treatment if variable is not scaled
                    )

gower_mat <- as.matrix(gower_dist)
gower_mat[1:10, 1:10]

## Checking results with dataset
## Most similar
college_clean[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
## Most dissimilar
college_clean[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

## Medoids clustering algorithm (alternative to centroids K-means)
# example <- pam(x = gower_dist, diss = T, k = 5)

# Method to decide the optimum number of clusters
sil_width <- c()

for(i in 1:10){                                            # Testing from 1 to 10 clusters
  pam_fit <- pam(gower_dist,
                 diss = TRUE,                              # Using dissimilarity object
                 k = i)
  # Silhouette width: how similar an observation is to its own cluster 
  # compared its closest neighboring cluster.
  # From -1 to 1. The higher, the better
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",
     type = 'b')

# Implementation of the optimal clusters
pam_fit <- pam(gower_dist, 
               diss = TRUE, 
               k = which(sil_width == max(sil_width, na.rm = T)))

# Assign a cluster to each observation
college_cluster <- college_clean %>% 
  mutate(cluster = pam_fit$clustering)

# Creating a summary for each cluster
pam_results <- college_clean %>%
  select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(summary = summary(.))

pam_results$summary

# Identifying mediods
pam_fit$medoids
college_clean[pam_fit$medoids,]
