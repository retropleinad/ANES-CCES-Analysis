library(dplyr)
library(tidyr)

# Bring in source file and look at it
ces <- read.csv(file = 'data/CES22_Common.csv')
head(ces)
colnames(ces)

# Calculate mean/median/mode off of birth year
class(ces[, 'birthyr'])

birthyr_mean <- mean(ces[, 'birthyr'])
birthyr_mean

birthyr_median <- median(ces[, 'birthyr'])
birthyr_median

mode <- function(vals) {
  uniq_values <- unique(vals)
  uniq_values[which.max(tabulate(match(vals, uniq_values)))]
}

birthyr_mode <- mode(ces[, 'birthyr'])
birthyr_mode

# T-Test: how does the mean birthyr compare to the national avg
national_mean_birthyr <- 1984

birthyr_ttest <- t.test(ces[, 'birthyr'], mu=national_mean_birthyr)
birthyr_ttest

# Linear Regression: Does age affect pid
linear_regression <- lm(ces[, 'pid7'] ~ ces[, 'birthyr'])
linear_regression
summary(linear_regression)

# Logistic Regression
logistic_regression <- glm(ces[, 'pid7'] / 8 ~ ces[, 'birthyr'],
                           family = binomial(link = "logit"))
logistic_regression
summary(logistic_regression)

# Interaction Terms: age, gender, race
