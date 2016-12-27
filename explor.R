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
ggplot(crimesweek, aes(x=WeekDay,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + ggtitle("Offenses by day of week")
ggplot(crimesweek, aes(x=Hour,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + facet_wrap(~ WeekDay) + ggtitle("Offenses by day of week and hour")

# mean crimes per day
avgCrimesperDayWeek <- summarise(crimesweek, avgCrimes=mean(TotalOffenses))
ggplot(avgCrimesperDayWeek, aes(x = WeekDay, y=avgCrimes)) +geom_histogram(binwidth = 3,stat = "identity") + ggtitle("Average Crimes per Day of the week")

# crime type frequency
crimetype <- select(crimes, OffenType, NrOffen) %>% group_by(OffenType) %>% summarise(TotalOffenses = sum(NrOffen)) %>% arrange(desc(TotalOffenses))
ggplot(crimetype, aes(x=OffenType,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + ggtitle("Offenses by type")

# crime type frequency per day of week
typesday <- select(crimes, OffenType, WeekDay, NrOffen) %>% group_by(OffenType,WeekDay) %>% summarise(TotalOffenses = sum(NrOffen))
ggplot(typesday, aes(x=WeekDay, y=TotalOffenses, colour=OffenType)) + geom_point()

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