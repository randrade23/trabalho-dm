# Importing and preprocessing data

library(gdata)
library(dplyr)
library(lubridate)

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
crimes <- read.xls("crime.xls", header=TRUE, na.strings=c("UNK", "-"))

# Create Period column
crimes <- mutate(crimes, Period = get_period(Hour))

# Split dates into day, month, year, weekday (this takes a while to process)
dates <- crimes$Date
parsedDates <- data.frame(Date = dates, Day=day(dates), 
                         Month=month(dates, label=TRUE), 
                         Year=year(dates), WeekDay=wday(dates, label=TRUE))
crimes <- merge(crimes, parsedDates, by='Date')