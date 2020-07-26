library(tree)
library(MASS)
library(ISLR)
library(MLmetrics)
library(tidyverse)

#to avoid a library running in the wrong order
#we can use a little trick!

## Regression ------------------------------------------------------------------
data("Boston")
head(Boston)

set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston) * 0.75)

tree_regr <- tree(formula = medv ~ .,
                   data = Boston,
                   subset = train,
                   split = "deviance")
#medv --> target!   formula is always target variable! +all the other factors that affect the 
#target variable, (that's why we have a dot after ~)IF NOT WE CAN PUT THE NAMES!
#formula chi target variable in the predictors!
#or specify if we want to give some variables
#we pass this vector to only get 75% of the rows!
#split --> we can change type!

summary(tree_regr)
tree_regr

dim(Boston)

# Plotting the tree
plot(x = tree_regr, 
     type = "proportional")
text(x = tree_regr, 
     splits = TRUE, 
     pretty = 0,
     cex = 0.8, 
     col = "firebrick")
#THAT HAS SOMETHING TO DO WITH LAST WEEK! 
# the things that we predicted (values of the model) are down! 
# < to the left and other to right!


# Predicting values
train_predict <- predict(tree_regr, newdata = Boston[train,])
RMSE(y_pred = train_predict, y_true = Boston[train, 'medv'])
# we got 3.6 thousands dollars are the difference between the predicted and the values!


# Pruning the tree
cv.tree(tree_regr, K = 10)
tree_pruning <- prune.tree(tree = tree_regr, best = 8)

# Predicting values with pruning
pruning_predict <- predict(tree_pruning, newdata = Boston[train,])
RMSE(y_pred = pruning_predict, y_true = Boston[train, 'medv'])

# Test
test_predict <- predict(tree_regr, newdata = Boston[-train,])
RMSE(y_pred = test_predict, y_true = Boston[-train, 'medv'])

# is this good? we will see?
#maybe in train we need crossvalidations!


## Classification --------------------------------------------------------------
data("Carseats")
head(Carseats)

train_class <- sample(1:nrow(Carseats), size = nrow(Carseats) * 0.75)

# Converting numeric to categoric
Carseats$high_sales <- as.factor(ifelse(test = Carseats$Sales > 8,
                                        yes = 'Yes',
                                        no = 'No'))

#the pupi:
#allows us to write code the same way that we think !! chi men awal session! 

#CHI FORMAT OF A DATE
#as.Date(spanish_date, format = '%d-%m-%Y')
#we can coose a format

Carseats_nosales <- Carseats %>% 
       select(-Sales)

#Carseats_nosales <- tidyverse:select(, data = Carseats, -Sales)

#xx <- select(,data=carseats, Advertising, Sales)  #select helps us to select and change order!


tree_class <- tree(formula = high_sales ~ ., 
                   data = Carseats_nosales,
                   subset = train_class
                   )
summary(tree_class)

# Plotting the tree
plot(x = tree_class, type = "proportional")
text(x = tree_class, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

# Predicting classes
train_class_predict <- predict(object = tree_class, 
                               newdata = Carseats_nosales[train_class,],
                               type = 'class'
                               )
table(train_class_predict, Carseats_nosales[train_class, 'high_sales'])
ConfusionMatrix(train_class_predict, Carseats_nosales[train_class, 'high_sales'])
Accuracy(train_class_predict, Carseats_nosales[train_class, 'high_sales'])
Precision(train_class_predict, Carseats_nosales[train_class, 'high_sales'])
Recall(train_class_predict, Carseats_nosales[train_class, 'high_sales'])

#he's gonna explain the g.. (the one from Gonzalo)


