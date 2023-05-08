library(dplyr)
library(glmnet)


# Load data and select columns we're using
ces = read.csv(file = 'data/CES22_Common.csv')
cols <- colnames(ces)
sort(cols)

ces <- ces[, c('pid7',         # y variable
               'gender4',      # gender we use for weight
               'race',         # very categorical
               'birthyr',      # used for weight
               'educ',         # 1 is least, 6 is most
               'votereg',      # yes or no
               'region',       # census region
               'CC22_300a',    # tv news
               'CC22_300c',    # paper news
               'CC22_302',     # state of economic changes
               'CC22_303',     # household income change
               'CC22_307',     # do the police make you feel safe
               'CC22_306',     # covid vaccination status
               'CC22_320a',    # Biden approval
               'CC22_327a',    # m4a
               'CC22_333',     # climate change with least believing it is most serious
               'CC22_340a',    # self lib/con rating with 1 being most liberal
               'CC22_340c',    # Biden lib/con rating
               'CC22_340d',    # Trump lib/con rating
               'CC22_340e',    # Dem lib/con rating
               'CC22_340f',    # GOP lib/con rating
               'CC22_340g',    # SCOTUS lib/con rating
               'CC22_363',     # Plan to vote
               'pew_religimp', # religion most important 1 to least 4
               'pew_churatd',  # church attendance, 1 most 6 least
               'investor',     # 1 or 2 do you have stock
               'union',        # 1 in a union, 3 never in a union
               'ownhome',      # 1 own 2 rent
               'newsint',      # 1 follow the news to 4 never
               'CC22_401',     # voted in 2022 election with 1 no 5 did
               'CC22_430c',    # amount contributed to candidates
               'numchildren'   # number of children
               )
           ]


# Weight our data

# Apply weights by gender
unique(ces[, 'gender4'])

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

# Cross-validated ridge regression for all variables on pid7
y <- data.matrix(select(ces, pid7))
y

df_x <- select(ces, c(1:9), -pid7)
makeX(df_x, sparse=FALSE)
x <- data.matrix(df_x)

ridge_model <- cv.glmnet(x = x,
                         y = y,
                         nfolds = 10,
                         alpha=0)
ridge_model
summary(ridge_model)

best_lambda <- ridge_model$lambda.min

# Rebuild ridge model


# Look at model accuracy
y_predicted <- predict(ridge_model,
                       newx = x)

sst <- sum((x - mean(x))^2)
sst

sse <- sum((y_predicted - y)^2)
sse

rsq <- 1 - sse / sst
rsq

plot(ridge_model)
