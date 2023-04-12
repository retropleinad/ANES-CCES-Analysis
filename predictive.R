library(dplyr)
library(tidyr)
library(caret)


# Bring in source file and look at it
ces <- read.csv(file = 'data/CES22_Common.csv')
head(ces)
colnames(ces)


# K-Fold Cross-Validation
control <- trainControl(method='cv', number=5)

kfold_model <- train(pid7 ~ birthyr + gender4 + race,
               data=ces, method='lm', trControl=control)

print(kfold_model)
kfold_model$finalModel
kfold_model$resample

# Holdout Set
