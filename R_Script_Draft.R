install.packages("tidyverse")
library(tidyverse)

weightinfo <- read.csv(file='weightLogInfo_merged.csv')
head(weightinfo)
nrow(weightinfo)
n_distinct(weightinfo$Id)
result <- data.frame(stepsnarrow$Id)
print(unique(result)) 
n_distinct(stepsnarrow$Id)
head(weightinfo)
n_distinct(weightinfo$Id)
weightinfo %>%
  group_by(Id)%>%
  summarise(mean = mean(WeightKg), n=n())

ggplot(weightinfo, aes(x=Date, y=WeightKg, colour=Id)) + geom_point()
ggplot(subset(weightinfo, Id %in% c("6962181067", "8877689391"))) + geom_point(aes(x=Date, y=WeightKg, group=Id))

stepsnarrow %>%
  group_by(Id)%>%
  summarise(mean = mean(Steps), n=n())

ggplot(subset(stepsnarrow, Id %in% c("6962181067", "8877689391"))) + geom_point(aes(x=ActivityMinute, y=Steps, group=Id))

dailysteps <- read.csv(file='dailySteps_merged.csv')
head(dailysteps)
n_distinct(dailysteps$Id)
stepssum <- dailysteps %>%
    group_by(Id)%>%
    summarise(stepmean = mean(StepTotal), n3=n())
stepssum

ggplot(subset(dailysteps, Id %in% c("6962181067", "8877689391"))) + geom_point(aes(x=ActivityDay, y=StepTotal, colour=Id))

dailycal <- read.csv(file='dailyCalories_merged.csv')
head(dailycal)
dailysleep <- read.csv(file='sleepDay_merged.csv')
head(dailysleep)
n_distinct(dailycal$Id)
n_distinct(dailysleep$Id)
sleep <- dailysleep %>%
    select(Id, TotalMinutesAsleep, TotalTimeInBed) %>%
    mutate(
      sleepquality = TotalMinutesAsleep / TotalTimeInBed,
    )
head(sleep)
sleepsum <- sleep %>%
    group_by(Id)%>%
    summarise(sqmean = mean(sleepquality), n1=n())

calsum <- dailycal %>%
    group_by(Id)%>%
    summarise(calmean = mean(Calories), n2=n())

combined <- left_join(sleepsum, calsum, by='Id')
combined <- right_join(combined, stepssum, by='Id')
combined
ggplot(combined, aes(x=stepmean, y=sqmean)) + geom_point()