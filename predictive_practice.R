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
                     data=ces, method='lm', trControl=kfold_control)
kfold_model

# Gradient with k-fold
# https://search.r-project.org/CRAN/refmans/ecospat/html/ecospat.cv.gbm.html
gradient_model <- gbm(pid7_normalized ~ educ + birthyr,
                      data = ces,
                      distribution = 'bernoulli',
                      cv.folds = 10,
                      n.cores = 2)

gradient_predict <- ecospat.cv.gbm(gbm.obj=gradient_model,
                                   ces,
                                   k=10,
                                   cv.lim=10,
                                   jack.knife=FALSE)

# Cross-validated ridge regression
# https://www.rdocumentation.org/packages/glmnet/versions/4.1-7/topics/cv.glmnet
ridge_model <- cv.glmnet(pid7_normalized ~ educ + birthyr,
                         data = ces,
                         alpha=0)

# Cross-validated
