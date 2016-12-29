# Importing and preprocessing data

library(gdata)
library(dplyr)
library(lubridate)
library(DMwR)

get_period <- function(x) {
  sapply(x, function(x) {
    if (x >= 8 & x < 12) {
      "morning"
    }
    else {
      if (x >= 12 & x < 19) {
        "afternoon"
      }
      else {
        "night"
      }
    }
  })
}

# Load the dataset
crimes <- read.xls("crime.xls", header=TRUE, na.strings=c("", "UNK", "-"))

# Remove crimes before 2014 (too old, not relevant)
crimes <- crimes[ymd(crimes$Date) >= ymd("2014-01-01"),]

# Create Period column
crimes <- mutate(crimes, Period = get_period(Hour))

# Split dates into day, month, year, weekday (this takes a while to process)
dates <- crimes$Date
parsedDates <- data.frame(Date = dates, Day=lubridate::day(dates), 
                         Month=lubridate::month(dates, label=TRUE), 
                         Year=lubridate::year(dates), WeekDay=lubridate::wday(dates, label=TRUE))
crimes <- data.frame(crimes, parsedDates)
crimes["Date.1"] <- NULL

# Try to complete lines with NA in important columns, delete invalid lines
crimesA <- crimes[,1:7]
crimesB <- crimes[,8:ncol(crimes)]
crimesA$Premise <- as.character(crimesA$Premise)
crimesA$Premise[is.na(crimesA$Premise)] <- "Other, Unknown or Not Listed"
crimesA$Premise <- factor(crimesA$Premise)
crimesA <- knnImputation(crimesA, k=10)
crimes <- cbind(crimesA, crimesB)
crimes <- crimes[!(grepl("^[0-9 ]*$", crimes$Offense.Type)),]

# Rename columns to friendlier names
colnames(crimes)[10] <- "NrOffen"
colnames(crimes)[3] <- "OffenType"
colnames(crimes)[8] <- "StrType"