#import data
source("import.R")
library(ggplot2)
library(GGally)

### graphs
# crime frequency per month
crimesmonth <- select(crimes,  Month, Day, NrOffen) %>% group_by(Month, Day) %>% summarise(TotalOffenses = sum(NrOffen))
crimesmonth$Month <- factor(crimesmonth$Month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(crimesmonth, aes(x=Day,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + facet_wrap(~ Month) + ggtitle("Offenses by month and day")

# crime frequency per day of week (and hour)
crimesweek <- select(crimes, Hour, WeekDay, NrOffen) %>% group_by(WeekDay, Hour) %>% summarise(TotalOffenses = sum(NrOffen))
crimesweek$WeekDay <- factor(crimesweek$WeekDay, levels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))
ggplot(crimesweek, aes(x=WeekDay,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + ggtitle("Offenses by day of week and hour")
ggplot(crimesweek, aes(x=Hour,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + facet_wrap(~ WeekDay) + ggtitle("Offenses by day of week and hour")

# mean crimes per day
avgCrimesperDayWeek <- summarise(crimesweek, avgCrimes=mean(TotalOffenses))
ggplot(avgCrimesperDayWeek, aes(x = WeekDay, y=avgCrimes)) +geom_histogram(binwidth = 3,stat = "identity") + ggtitle("Average Crimes per Day of the week")

# crime type frequency
crimetype <- select(crimes, OffenType, NrOffen) %>% group_by(OffenType) %>% summarise(TotalOffenses = sum(NrOffen)) %>% arrange(desc(TotalOffenses))
ggplot(crimetype, aes(x=OffenType,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + ggtitle("Offenses by type")

# crime type frequency per hour per day of week
aux <- select(crimes, OffenType, WeekDay, NrOffen) %>% group_by(OffenType,WeekDay) %>% summarise(TO = sum(NrOffen))
typesday <- matrix(nrow=length(unique(aux$OffenType)), ncol=length(unique(aux$WeekDay))+1)
i <- 1
for (off in unique(aux$OffenType)) {
  # Number of occurences per weekday for the current offense
  tmpdf <- filter(aux, OffenType==off)
  for (wd in unique(aux$WeekDay)) { # If any weekday is missing (no occurrences) add entry with 0
    if (!(wd %in% unique(tmpdf$WeekDay))) {
      tmpdf[nrow(tmpdf)+1,] <- c(off, wd, 0)
    }
  }
  tmpdf <- arrange(tmpdf, WeekDay) # Sort by weekday
  print(tmpdf)
  vec <- c(off, tmpdf$TO) # Name of offense and occurences, ordered by weekday
  typesday[i,] <- vec # Add to dataframe
  i <- i + 1
}
typesday <- data.frame(typesday)
colnames(typesday) <- c("OffenseType","Sun","Mon","Tues","Wed","Thurs","Fri","Sat")
typesday$OffenseType <- factor(typesday$OffenseType)
ggparcoord(typesday,columns=2:8,groupColumn=1, scale = "uniminmax", title = "OffenseType by Day of Week")

#TOP 10 beats
beats <- select(crimes, Beat, NrOffen) %>% group_by(Beat) %>% summarise(TotalOffenses = sum(NrOffen)) %>% arrange(desc(TotalOffenses)) %>% head(10)
beats$Beat <- factor(beats$Beat, levels=arrange(beats, desc(TotalOffenses))$Beat)
ggplot(beats, aes(x=Beat,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + coord_flip() + ggtitle("TOP10 Beats")

#TOP 10 premises
premise <- select(crimes, Premise, NrOffen) %>% group_by(Premise) %>% summarise(TotalOffenses = sum(NrOffen)) %>% arrange(desc(TotalOffenses)) %>% head(10)
premise$Premise <- factor(premise$Premise, levels=arrange(premise, desc(TotalOffenses))$Premise)
ggplot(premise, aes(x=Premise,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + coord_flip() + ggtitle("TOP10 Premises")

#crimetype per top10 premise
crimepremise <- filter(crimes, Premise %in% premise$Premise) %>% group_by(Premise,OffenType) %>% summarise(TotalOffenses = sum(NrOffen))
ggplot(crimepremise, aes(x=OffenType,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + facet_wrap(~ Premise) + ggtitle("Offenses type by premise")

#dayWeek with more/less crimes 
temp <- summarise(crimesweek, AllOffenses = sum(TotalOffenses)) 
maxCrimesDay <- filter(temp, AllOffenses == max(AllOffenses))
minCrimesDay <- filter(temp, AllOffenses == min(AllOffenses))
ggplot(bind_rows(maxCrimesDay, minCrimesDay),aes(x=WeekDay, y=AllOffenses))+ geom_histogram(binwidth=3,stat="identity") + ggtitle("Day of the week with maximum and minimum crime occurrences")
