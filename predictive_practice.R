library(dplyr)
library(tidyr)

library(caret)
library(glmnet)
library(gbm)
library(randomForest)
library(caTools)


# Bring in source data
ces = read.csv(file = 'data/CES22_Common.csv')
cols <- colnames(ces)
sort(cols)


# Look at different columns to figure out what we want to use
head(ces[, c('educ', 'birthyr')])


# Normalize samples
max(ces[, 'pid7'])
ces['pid7_normalized'] = ces[, 'pid7'] / max(ces[, 'pid7'])
head(ces[, 'pid7_normalized'])


# Build gradient boosting model with k-fold control and regularization
# Initial k-fold model
kfold_control <- trainControl(method='cv', number=5)

kfold_model <- train(pid7_normalized ~ educ + birthyr,
                     data=ces, method='lm', trControl=kfold_control)
kfold_model
