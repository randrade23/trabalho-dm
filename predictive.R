library(MASS)
library(dplyr)
library(e1071)

source("import.R")

set.seed(1)
crimes_pred <- crimes %>% expandRows("NrOffen") %>% dplyr::select(Day, OffenType, Premise, StreetName, BlockRange, Period, Beat, WeekDay)
sp <- sample(1:nrow(crimes_pred), as.integer(nrow(crimes_pred) * 0.7))
tr <- crimes_pred[sp, ]
ts <- crimes_pred[-sp, ]

nb <- naiveBayes(Beat ~., tr, laplace=1) 
mtrx <- table(predict(nb,ts), ts$Beat)
(errn <- 1-sum(diag(mtrx))/sum(mtrx))