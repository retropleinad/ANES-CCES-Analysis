library(dplyr)
library(tidyr)

# Bring in source file and look at it
ces = read.csv(file = 'data/CES22_Common.csv')
head(ces)
colnames(ces)

