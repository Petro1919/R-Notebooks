# libraries needed
library(caret)
library(psych)

# read the data
data <- sat.act[complete.cases(sat.act),]
head(data)

inTrain <- createDataPartition(y = data$ACT,
                               p = 0.75,
                               list = F)

train <- data[inTrain,]
test <- data[-inTrain,]

data_ctrl <- trainControl(method = "cv", number = 5)

gbm_grid <- expand.grid(interaction.depth = c(1, 3, 5), 
                        n.trees = (0:50) * 50,
                        shrinkage = c(0.01, 0.001),
                        n.minobsinnode = 10)

#now we can automatically do "something" that we did 3 sessions ago 

## Available models in caret
names(getModelInfo())
available_models <- getModelInfo()


gbm_caret <- train(ACT ~ gender + age + SATV + SATQ,
                   data = train,
                   trControl = data_ctrl,
                   # preProc = c("center", "scale"),
                   method = "gbm",
                   tuneGrid = gbm_grid,
                   metric = 'RMSE')

#we are using train because caret allows us to put it as a parameter
#caret website

gbm_caret$finalModel
gbm_caret$resample
gbm_caret_results <- gbm_caret[['results']]
ggplot(gbm_caret)
#we pick a depth, a number of tree! and decide (why do we need)

#train decide best model (combination of features)
#validation different!=(not cross validation-->used in test to take diff parts of data at different..)
# test

#the plot (up) shows the error in the train dataset!
#(in ideal world we choose it with validation set) BUT NOT THIS EXAMPLE

#SHRINKAGE --> LEARNING RATE OF THE MODEL! similar to gradient test methodology! 

# chi learning rate w hek!! value to find the minimum! 
#if the learning rate is big--> jumps and I can't find the best solution
#if not --> I will surely find the solution but it will take a lot of time
#learning rate in this case helps me find how the error will be minimized!!



predictions_train <- predict(object = gbm_caret,
                       type = 'raw',
                       newdata = train)
RMSE(train$ACT, predictions_train)

predictions_test <- predict(object = gbm_caret,
                             type = 'raw',
                             newdata = test)
RMSE(test$ACT, predictions_test)

#wide data # long data chi hek! creating columns with primary and secondary
#unite!!! 
#we can combine 2 columns!!
