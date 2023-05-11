library(dplyr)
library(gbm)


# Load data and select columns we're using
ces = read.csv(file = 'data/CES22_Common.csv')
cols <- colnames(ces)
sort(cols)

ces <- ces[, c('pid7', # Used to select only Democrats
               'CC22_430c', # y variable - amount donated
               'gender4',
               'race',
               'birthyr',
               'educ',
               'votereg'
               )
           ]


# Apply weights

# Apply weights by gender
gender_weights <- data.frame(
  survey_val = c(1, 2, 3, 4),
  gender_mapping = c('man', 'woman', 'non-binary', 'other'),
  gender_weight_coeff = c(.495, .505, .505, .505)
)
# Note: non-binary and other grouped with women so they're not removed

ces <- merge(x = ces, y = gender_weights,
             by.x = 'gender4', by.y = 'survey_val',
             all.x = TRUE)

# Apply weights by race
unique(ces[, 'race'])

race_weights <- data.frame(
  survey_val = c(1:8),
  race_mapping = c('white', 'black', 'hispanic', 'asian',
                   'native american','two or more', 'other', 'middle eastern'),
  race_weight_coeff = c(.593, .136, .189, .061, .013, .029, .003, .061)
)
# Note: middle eastern is included with asian

ces <- merge(x = ces, y = race_weights,
             by.x = 'race', by.y = 'survey_val',
             all.x = TRUE)

# Apply weights by age
ces['age_estimate'] <- 2023 - ces[, 'birthyr']
head(ces[, 'age_estimate'])
max(ces[, 'age_estimate']) # 98
min(ces[, 'age_estimate']) # 19

ces['age_category'] <- ifelse(ces[, 'age_estimate'] <= 44,
                              1,
                              ifelse(ces[, 'age_estimate'] <= 64, 2, 3)
)
head(ces[, 'age_category'])

age_weights <- data.frame(
  category = c(1:3),
  age_mapping = c('18 to 44',
                  '45 to 64',
                  '65 and over'),
  age_weight_coeff = c(.465, .326, .218)
)

ces <- merge(x = ces, y = age_weights,
             by.x = 'age_category', by.y = 'category',
             all.x = TRUE)

# Apply weights by education
educ_weights <- data.frame(
  survey_val = c(1:6),
  educ_mapping = c('Did not graduate from high school',
                   'High school graduate',
                   'Some college, but no degree (yet)',
                   '2-year college degree',
                   '4-year college degree',
                   'Postgraduate degree'),
  educ_weight_coeff = c(.089, .279, .149, .105, .235, .144)
)

ces <- merge(x = ces, y = educ_weights,
             by.x = 'educ', by.y = 'survey_val',
             all.x = TRUE)
head(ces)

# Combine weights into one column
ces['weight'] <- ces[, 'gender_weight_coeff'] *
                 ces[, 'race_weight_coeff'] *
                 ces[, 'age_weight_coeff'] *
                 ces[, 'educ_weight_coeff']


# Transform data for model

# Look only at Democrats who gave money
ces <- ces %>%
  filter(pid7 %in% (1:3) & !is.na(CC22_430c))

count(ces)


# Build gbm model to predict how much someone will give
gradient_model <- gbm(CC22_430c ~ gender4 + race + birthyr + educ + votereg,
                      data = ces,
                      weights = ces$weights,
                      cv.folds = 10,
                      shrinkage = .01,
                      n.minobsinnode = 10,
                      n.trees = 500).

gradient_model
summary(gradient_model)


# Look at accuracy of model
gradient_pred_y <- predict.gbm
