library(MASS)
library(dplyr)
library(e1071)
library(splitstackshape)

source("import.R")

set.seed(50)
crimes_pred <- summarise(group_by(crimes, OffenType, WeekDay, Hour, Beat), TO=sum(NrOffen))
sp <- sample(1:nrow(crimes_pred), as.integer(nrow(crimes_pred) * 0.7))
tr <- crimes_pred[sp, ]
ts <- crimes_pred[-sp, ]

# Tree Model 

tree <- rpartXse(TO ~ ., tr)
ps <- predict(tree,ts)
mae <- mean(abs(ps-ts$TO))