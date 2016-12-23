setwd("~/Dropbox/FCUPiano/7ºsem/DM1/trabalho-dm")

#import data
source("import.R")
colnames(crimes)[10] <- "NrOffen"
library(ggplot2)

#number of crimes per period (week)

offensas <- select(crimes, Hour, WeekDay, NrOffen) %>% group_by(offensas, WeekDay, Hour) %>% summarise(offensas, TotalOffenses=sum(NrOffen))
offensas$WeekDay <- factor(offensas$WeekDay, levels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))
ggplot(offensas, aes(x=Hour,y=TotalOffenses)) + geom_histogram(binwidth=3,stat="identity") + facet_wrap(~ WeekDay) + ggtitle("Offenses by day of week and hour")
