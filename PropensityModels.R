# SESSION 2: INTRODUCTION TO PROPENSITY MODELS
source("C:/Users/User/Desktop/Term 2 MIBA/R/Session 2/descriptives.R")

# ========================================================================== # 
#  Data loading. Importing data from CSV file to data table.
# ========================================================================== # 

#chi regression categorical aw chi hek!

# Load a CSV file (with comma as a delimiter and point as decimal separator) containing loan data
library(data.table)
# install.packages("pROC")
library(pROC)
lending_data = fread(file = "C:/Users/User/Desktop/Term 2 MIBA/R/Session 2/lending_data.csv", 
                     header = TRUE, 
                     sep = ",",
                     stringsAsFactors = FALSE,
                     dec=".")



# ========================================================================== # 
#  Preliminary data exploration 
# ========================================================================== # 
# summary of variables types and basic statistical quantities
summary(lending_data)
# Ranking avg_default by purpose
lending_data[, .(avg_default = mean(default)), by = purpose][order(-avg_default)]


# What are the key drivers of default?
arcvi_descriptive(lending_data$loan_amnt, lending_data$default) #we take 2 variables
arcvi_descriptive(lending_data$grade, lending_data$default)  #helps us visualize the data and calculate proba of default of each quantile
arcvi_descriptive(lending_data$emp_length_years, lending_data$default) #chi number of cases!
arcvi_descriptive(lending_data$annual_inc, lending_data$default)  #we can check if there is something between loan amnt w proba of default!
arcvi_descriptive(lending_data$home_ownership, lending_data$default)
arcvi_descriptive(lending_data$delinq_2yrs, lending_data$default)
arcvi_descriptive(lending_data$purpose, lending_data$default)

# ============================================================================ #

# FEATURE ENGINEERING  #we are transforming how data is expressed.. we are maintaining and not cleaning
#we are changing the type (dummies, numeric)  

# ============================================================================ #
# How can we transform features to make them more powerful to predict default?
# Grade
lending_data$grade_num = as.numeric(as.factor(lending_data$grade))  #as.factor mheme!
# Employment
lending_data$is_emp = as.numeric(!is.na(lending_data$emp_length_years))
# Home ownership
lending_data$home_own_ord = 4
lending_data$home_own_ord[lending_data$home_ownership == "MORTGAGE"] = 1
lending_data$home_own_ord[lending_data$home_ownership == "OWN"] = 2
lending_data$home_own_ord[lending_data$home_ownership == "RENT"] = 3
# Purpose
lending_data$risk_purpose = 0
lending_data$risk_purpose[(lending_data$purpose %in% c('small_business','educational','moving'))] = 1
lending_data$risk_purpose[(lending_data$purpose %in% c('credit_card','major_purchase','car'))] = -1
arcvi_descriptive(lending_data$risk_purpose, lending_data$default)
# Delinq_2yrs
lending_data$delinq_2yrs_ind = as.numeric(lending_data$delinq_2yrs <=1)
lending_data$delinq_2yrs_ind[is.na(lending_data$delinq_2yrs_ind)] = 0
arcvi_descriptive(lending_data$delinq_2yrs_ind, lending_data$default)

# ========================================================================== # 
#  Introduction to propensity models
# ========================================================================== #
#we create a new dataset with the stuff we want to use!

model_data = lending_data[,.(default,grade_num,is_emp,home_own_ord,
                             annual_inc,delinq_2yrs_ind, risk_purpose)]

m0 = glm(default~., data = model_data, family = "binomial")
summary(m0)

m1 = step(m0)
summary(m1)

# Predictions
pred1 = predict(m1, newdata = model_data, type = "response")
# On a new data_set
newclient = data.table(grade_num = 1, is_emp = 0, home_own_ord = 2,
                       annual_inc = 90000, delinq_2yrs_ind = 0, risk_purpose = 0)
prednewclient = predict(m1, newdata = newclient, type = "response")
prednewclient

# ========================================================================== # 
#  Cross-validation
# ========================================================================== #
# 5 fold : Train-test split
cfolds = 5
index = sample(1:cfolds, nrow(model_data), replace = TRUE)
# To simplify, create a train with index 1:4, test with index 5
train = model_data[index %in% 1:4,]
test = model_data[index == 5,]


m0 = glm(default ~ ., data = train, family = "binomial")
m1 = glm(default ~ grade_num + is_emp + annual_inc + risk_purpose, data = train, family = "binomial")

# Predictions and AUC m0
pred_train0 = predict(m0, newdata = train, type = "response")
pred_test0 = predict(m0, newdata = test, type = "response")
auc(train$default, pred_train0)
auc(test$default, pred_test0)


# Predictions and AUC m1
pred_train1 = predict(m1, newdata = train, type = "response")
pred_test1 = predict(m1, newdata = test, type = "response")
auc(train$default, pred_train1)
auc(test$default, pred_test1)


auc_train = 0
auc_test = 0

for (i in 1:5){
  train = model_data[index !=i,]
  test = model_data[index == i,]
  m1 = glm(default~grade_num+is_emp+annual_inc+risk_purpose, data = train, family = "binomial")
  pred_train1 = predict(m1, newdata = train, type = "response")
  pred_test1 = predict(m1, newdata = test, type = "response")
  auc_train[i] = auc(train$default, pred_train1)
  auc_test[i] = auc(test$default, pred_test1)
}



## Top 20 ----------------------------------------------------------------------
hist(m1$fitted.values, breaks = 40)

train$probs <- m1$fitted.values

train$intervals <- cut(x = train$probs, quantile(train$probs), labels = c("very low", "low", "medium", "high"))

train[, .(avDefault = mean(default)), by = 'intervals']

# Extra -------------------------------------------------------------------

# l10 = lending_data[year(issue_date)==2010]
# l10[,.(int_rate = mean(int_rate),
#        default = mean(default),
#        profit_per_dollar = -mean(default)/2 + mean(int_rate)/100 -mean(default)*mean(int_rate)/100,
#        pct=.N/nrow(l10)),
#     by=grade][order(grade)]
# l10[,.(default = mean(default),int_rate = mean(int_rate),pct=.N/nrow(l10),loan_amnt=mean(loan_amnt)),
#     by=grade][order(grade)]
# 
# nrow(lending_data)
# min(lending_data$issue_date)
# max(lending_data$issue_date)
# sort(table(lending_data$purpose))/nrow(lending_data)
# mean(lending_data$default)
# 
# 
# lending_data[,.(profit_per_dollar =
#                   mean(default)/2 - 1 +
#                   (1 - mean(default))*(1+mean(int_rate)/100)),by=grade][order(grade)]
# 
# 
# 
# 
