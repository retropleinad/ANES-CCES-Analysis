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

ttest_result <- "Low p-value and high t-value indicate mean not close to 1984"


# Linear Regression: Does age affect pid
linear_regression <- lm(ces[, 'pid7'] ~ ces[, 'birthyr'])
linear_regression
summary(linear_regression)
# https://towardsdatascience.com/understanding-linear-regression-output-in-r-7a9cbda948b3


# Interaction Terms: age, gender, race
interaction_lm <- lm(ces[, 'pid7'] ~ ces[, 'birthyr']
                     + ces[, 'gender4']
                     + ces[, 'race'])
interaction_lm
summary(interaction_lm)


# Logistic Regression
logistic_regression <- glm(ces[, 'pid7'] / 8 ~ ces[, 'birthyr'],
                           family = binomial(link = "logit"))
logistic_regression
summary(logistic_regression)
# https://towardsdatascience.com/simply-explained-logistic-regression-with-example-in-r-b919acb1d6b3


# Interaction Terms: age, gender, race
interaction_glm <- glm(pid7 / 8 ~ birthyr + gender4 + race,
                       data=ces,
                       family='binomial')
interaction_glm
summary(interaction_glm)


# Plot a graph
plot(ces[, 'pid7'], ces[, 'birthyr'])
