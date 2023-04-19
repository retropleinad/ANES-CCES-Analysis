library(dplyr)
library(tidyr)

library(caret)
library(glmnet)
library(gbm)
library(randomForest)
library(caTools)


# Bring in source file and look at it
ces <- read.csv(file = 'data/CES22_Common.csv')
head(ces)
colnames(ces)


# K-Fold Cross-Validation
# https://www.statology.org/k-fold-cross-validation-in-r/
kfold_control <- trainControl(method='cv', number=5)

kfold_model <- train(pid7 ~ birthyr + gender4 + race,
                     data=ces, method='lm', trControl=kfold_control)

print(kfold_model)
kfold_model$finalModel
kfold_model$resample

# Holdout Set
# https://rpubs.com/anandjage/cvholdout
holdout_index <- createDataPartition(ces$pid7, p=0.8, list=FALSE)
head(holdout_index)
dim(holdout_index)

holdout_train <- ces[holdout_index,]
holdout_test <- ces[-holdout_index,]

dim(holdout_train)
dim(holdout_test)

holdout_model <- lm(pid7 ~ birthyr + gender4 + race,
                    data=holdout_train)
holdout_model

holdout_train$predicted <- predict(holdout_model, holdout_train)
holdout_train$residuals <- residuals(holdout_model)
head(holdout_train$predicted)
head(holdout_train$residuals)

holdout_test$predicted <- predict(holdout_model, holdout_test)
holdout_test$residuals <- holdout_test$pid7 - holdout_test$predicted
head(holdout_test$predicted)
head(holdout_train$residuals)

rmse_holdout_train <- sqrt(mean(holdout_train$residuals ** 2))
rmse_holdout_test <- sqrt(mean(holdout_test$residuals ** 2))
rmse_holdout_train
rmse_holdout_test
rmse_holdout_train / rmse_holdout_test


# Ridge Regression
# https://www.statology.org/ridge-regression-in-r/
ridge_dependent <- ces$pid7
ridge_independent <- data.matrix(ces[, c('birthyr', 'gender4', 'race')])

ridge_model <- glmnet(ridge_independent, ridge_dependent, alpha=0)
summary(ridge_model)

kfold_ridge_model <- cv.glmnet(ridge_independent, ridge_dependent, alpha=0)

ridge_best_lambda <- kfold_ridge_model$lambda.min
ridge_best_lambda

plot(kfold_ridge_model)

best_ridge_model <- glmnet(ridge_independent, ridge_dependent,
                           alpha=0, lambda=ridge_best_lambda)
coef(best_ridge_model)

plot(ridge_model, xvar='lambda')

ridge_y_predicted <- predict(best_ridge_model,
                             s=ridge_best_lambda,
                             newx=ridge_independent)

ridge_sst <- sum((ridge_dependent - mean(ridge_dependent))^2)
ridge_sse <- sum((ridge_y_predicted - ridge_dependent)^2)
ridge_rsq <- 1 - ridge_sse / ridge_sst
ridge_rsq


# Lasso Regression
# https://www.statology.org/lasso-regression-in-r/
lasso_y <- ces$pid7
lasso_x <- data.matrix(ces[, c('birthyr', 'gender4', 'race')])

lasso_kfold_model <- cv.glmnet(lasso_x, lasso_y, alpha=1)
lasso_best_lambda <- lasso_kfold_model$lambda.min
lasso_best_lambda

plot(lasso_kfold_model)

lasso_best_model <- glmnet(lasso_x, lasso_y, alpha=1, lambda=lasso_best_lambda)
coef(lasso_best_model)

lasso_y_predicted <- predict(lasso_best_model, s=lasso_best_lambda, newx=lasso_x)

lasso_sst <- sum((lasso_y - mean(lasso_y))^2)
lasso_sse <- sum((lasso_y_predicted - lasso_y)^2)
lasso_rsq <- 1 - lasso_sse / lasso_sst
lasso_rsq


# Gradient Boosting
# https://www.projectpro.io/recipes/apply-gradient-boosting-r-for-regression
head(ces)
summary(ces)
dim(ces)

gradient_partition <- createDataPartition(y=ces$pid7, p=.8, list=FALSE)
gradient_train <- ces[gradient_partition, ]
gradient_test <- ces[-gradient_partition, ]

gradient_test_x <- gradient_test[, c('birthyr', 'gender4', 'race')]
gradient_test_y <- gradient_test[, 'pid7']

gradient_model = gbm(pid7 ~ birthyr + gender4 + race,
                     data = gradient_train,
                     distribution = 'gaussian',
                     cv.folds = 10,
                     shrinkage = .01,
                     n.minobsinnode = 10,
                     n.trees = 500)

print(gradient_model)
summary(gradient_model)

gradient_pred_y <- predict.gbm(gradient_model, gradient_test_x)
gradient_pred_y

gradient_residuals <- gradient_test_y - gradient_pred_y
gradient_rmse <- sqrt(mean(gradient_residuals^2))
gradient_rmse

gradient_y_test_mean <- mean(gradient_test_y)
gradient_total_sum_squares <- sum((gradient_test_y - gradient_y_test_mean)^2)
gradient_total_sum_squares

gradient_residual_sum_squares <- sum(gradient_residuals^2)

gradient_rsq <- 1 - (gradient_residual_sum_squares / gradient_total_sum_squares)
gradient_rsq

gradient_x_axis <- 1:length(gradient_pred_y)
plot(gradient_x_axis, gradient_test_y, col='blue', pch=20, cex=.9)
lines(gradient_x_axis, gradient_pred_y, col='red', pch=20, cex=.9)


# Random Forest
# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9
dim(ces)

forest_sample <- sample.split(ces$pid7, SplitRatio=.74)
forest_train <- subset(ces, forest_sample == TRUE)
forest_test <- subset(ces, forest_sample == FALSE)

forest_model <- randomForest(pid7 ~ birthyr + gender4 + race,
                             data=forest_train)

forest_model
summary(forest_model)

forest_predict <- predict(forest_model,
                          newdata=forest_test[, c('birthyr', 'gender4', 'race')])
forest_predict

forest_predictions <- table(forest_test[, 'pid7'],
                            forest_predict)
forest_predictions
