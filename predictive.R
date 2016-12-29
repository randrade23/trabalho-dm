library(MASS)
library(dplyr)
library(e1071)
library(splitstackshape)

source("import.R")

set.seed(1)
crimes_pred <- summarise(group_by(crimes, OffenType, Premise, WeekDay, Hour, Beat), TO=sum(NrOffen))
crimes_pred$TO <- factor(crimes_pred$TO)
sp <- sample(1:nrow(crimes_pred), as.integer(nrow(crimes_pred) * 0.7))
tr <- crimes_pred[sp, ]
ts <- crimes_pred[-sp, ]

# Naive Bayes

nb <- naiveBayes(TO ~., tr, laplace=1) 
mtrx <- table(predict(nb,ts), ts$TO)
(errn <- 1-sum(diag(mtrx))/sum(mtrx))