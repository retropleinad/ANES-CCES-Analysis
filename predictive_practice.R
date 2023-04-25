library(dplyr)
library(tidyr)

library(caret)
library(glmnet)
library(gbm)
library(randomForest)
library(caTools)
library(ecospat)


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


# Build cross-validated gradient and ridge models

# K-fold by itself
kfold_control <- trainControl(method='cv', number=5)

kfold_model <- train(pid7_normalized ~ educ + birthyr,
                     data=ces,
                     method='lm',
                     trControl=kfold_control)
kfold_model

# Gradient with k-fold
# https://www.rdocumentation.org/packages/gbm/versions/2.1.8.1/topics/gbm
gradient_model <- gbm(pid7_normalized ~ educ + birthyr,
                      data = ces,
                      distribution = 'bernoulli',
                      cv.folds = 10,
                      n.cores = 2,
                      n.trees = 500)
gradient_model
summary(gradient_model)

gradient_pred_y <- predict.gbm(gradient_model, ces[, c('educ', 'birthyr')])
gradient_pred_y

gradient_residuals <- ces[, 'pid7'] - gradient_pred_y
gradient_rmse <- sqrt(mean(gradient_residuals^2))
gradient_rmse

gradient_y_test_mean <- mean(ces[, 'pid7'])
gradient_total_sum_squares <- sum((ces[, 'pid7'] - gradient_y_test_mean)^2)
gradient_total_sum_squares

gradient_residual_sum_squares <- sum(gradient_residuals^2)
gradient_residual_sum_squares

gradient_rsq <- 1 - (gradient_residual_sum_squares / gradient_total_sum_squares)
gradient_rsq

# Cross-validated ridge regression
# https://www.rdocumentation.org/packages/glmnet/versions/4.1-7/topics/cv.glmnet
ridge_y <- ces$pid7
ridge_x <- data.matrix(ces[, c('educ', 'birthyr')])

ridge_model <- cv.glmnet(x = ridge_x,
                         y = ridge_y,
                         nfolds = 10,
                         alpha=0)
ridge_model
summary(ridge_model)

ridge_best_lambda <- kfold_ridge_model$lambda.min

ridge_y_predicted <- predict(ridge_model,
                             newx = ridge_x)

ridge_sst <- sum((ridge_x - mean(ridge_x))^2)
ridge_sst

ridge_sse <- sum((ridge_y_predicted - ridge_y)^2)
ridge_sse

ridge_rsq <- 1 - ridge_sse / ridge_sst
ridge_rsq

plot(ridge_model)
