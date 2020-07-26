library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
# library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(MLmetrics)
library(AmesHousing)
library(tidyverse)

## First approach --------------------------------------------------------------
set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = 0.7)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

## 25 seconds
system.time({
  gbm.fit <- gbm(formula = Sale_Price ~ .,
                 distribution = 'gaussian',
                 data = ames_train,
                 n.trees = 5000,
                 interaction.depth = 1,
                 shrinkage = 0.001,
                 cv.folds = 5)
  
})         #WE SHOULD USE THESE  } TO PUT MANY CODE
summary(gbm.fit)
#SYSTEM TIME FUNCTION (CALCULATES THE TIME A PIECE OF CODE TAKES TO EXCECUTE!)


# Features importance
par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit, 
  cBars = 10,
  method = relative.influence,
  las = 2
)


# Train metrics
train_pred <- predict(gbm.fit, newdata = ames_train)
RMSE(y_pred = train_pred, y_true = ames_train$Sale_Price)
# Test metrics
test_pred <- predict(gbm.fit, newdata = ames_test)
RMSE(y_pred = test_pred, y_true = ames_test$Sale_Price)

# Improving hyperparameters (gridsearch) ---------------------------------------
hyper_grid <- expand.grid(
  shrinkage = c(.001, .01, .1),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(10, 15),
  # bag.fraction = c(.5, .8),
  optimal_trees = NA,               # a place to store results
  RMSE = NA                    # a place to store results
)

#WE HAVE EXPLANATION OF DIFFERENT HYPERPARAMETERS WITH GONZALO!


# Grid search ------------------------------------------------------------------
#randomize data
random_idx <- sample(1:nrow(ames_train), nrow(ames_train))
random_ames_train <- ames_train[random_idx,]

system.time({
  for(i in 1:nrow(hyper_grid)) {
    print(i)
    
    # reproducibility
    set.seed(123)
    
    # train model
    gbm.tune <- gbm(
      formula = Sale_Price ~ .,
      distribution = "gaussian",
      data = random_ames_train,
      n.trees = 5000,
      interaction.depth = hyper_grid$interaction.depth[i],
      shrinkage = hyper_grid$shrinkage[i],
      n.minobsinnode = hyper_grid$n.minobsinnode[i],
      # bag.fraction = hyper_grid$bag.fraction[i],
      train.fraction = 0.75,
      n.cores = NULL,
      verbose = FALSE
    )
    
    # add RMSE and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    # hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
    hyper_grid$RMSE[i] <- RMSE(y_pred = predict(gbm.tune, newdata = random_ames_train), y_true = random_ames_train$Sale_Price)
  }
})

## Sorting results
hyper_grid %>% 
  arrange(RMSE) %>% 
  print()

#we used arrange function from tydiverse! and according to this table, this combination of hyperparameters
#has the lowest error! 


# Optimal hyperparameters
system.time({
  gbm.fit.final <- gbm(
    formula = Sale_Price ~ .,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 1500,               ## 5000
    interaction.depth = 5,        ## 5
    shrinkage = 0.1,             ## 0.001
    n.minobsinnode = 15,          ## 15
    # bag.fraction = .5,          ## .5
    train.fraction = 0.75,
    n.cores = NULL,
    verbose = FALSE,
    cv.folds = 5
  )
})

# # maybe we can lose some accuracy and make the model work better!

# Train metrics
train_final_pred <- predict(gbm.fit.final, newdata = random_ames_train)
RMSE(y_pred = train_final_pred, y_true = random_ames_train$Sale_Price)
# Test metrics
test_final_pred <- predict(gbm.fit.final, newdata = ames_test)
RMSE(y_pred = test_final_pred, y_true = ames_test$Sale_Price)

# Features importance
par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence,
  las = 2
)