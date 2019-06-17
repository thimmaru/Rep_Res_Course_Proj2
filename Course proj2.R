setwd("C:\\Users\\TRUDR\\OneDrive - Monsanto\\Migrated from My PC\\Desktop\\Data\\Reprod.Res\\course_proj_2")
##Download data file
link <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url = link, destfile = "StormData")
##Read a file in table format
StormData <- read.csv(bzfile("StormData"),sep = ",",header=TRUE)

##Property damage estimates were entered as actual dollar amounts (the variable PROPDMG). But they were rounded to three significant digits, followed by an alphabetical character signifying the magnitude of the number, i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify magnitude include "K" for thousands, "M" for millions, and "B" for billions. So I created a new variable PROPDMGEXP2 and assigned conditionally "K" = 1000, "M" = 1000000, "B" = 1000000000, in other cases 1. These variables are multiplied in the next step.
table(StormData$PROPDMGEXP)
StormData$PROPDMGEXP2 <- 1
StormData$PROPDMGEXP2[which(StormData$PROPDMGEXP == "K")] <- 1000
StormData$PROPDMGEXP2[which(StormData$PROPDMGEXP == "M" | StormData$PROPDMGEXP == "m")] <- 1000000
StormData$PROPDMGEXP2[which(StormData$PROPDMGEXP == "B")] <- 1000000000

table(StormData$PROPDMGEXP2)

# install.packages("bindrcpp")
# install.packages("dplyr")
# library(dplyr)

# Which types of events are most harmful to population health?
StormData %>%
  select(FATALITIES, EVTYPE) %>%
  group_by(EVTYPE) %>%
  summarise(SumFATALITIES = sum(FATALITIES)) %>%
  top_n(n = 8, wt = SumFATALITIES) %>%
  ggplot(aes(y = SumFATALITIES, x = reorder(x = EVTYPE, X = SumFATALITIES), fill=EVTYPE))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  #geom_text(aes(label=SumFATALITIES), size = 4, hjust = 0.5, vjust = -0.1) +
  xlab(label = "") +
  ylab(label = "Death toll") +
  coord_flip() +
  theme_light()
# The event of occuring tornados has the most harmful effect as it caused the highest death toll follwed by Excessive heat & Flash flood respectively.Tornado 

#The second plot presents Injuries by Event type
StormData %>%
  select(INJURIES, EVTYPE) %>%
  group_by(EVTYPE) %>%
  summarise(SumINJURIES = sum(INJURIES)) %>%
  top_n(n = 8, wt = SumINJURIES) %>%
  ggplot(aes(y = SumINJURIES, x = reorder(x = EVTYPE, X = SumINJURIES), fill=EVTYPE))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  #geom_text(aes(label=SumINJURIES), size = 4, hjust = 0.5, vjust = -0.1) +
  xlab(label = "") +
  ylab(label = "INJURIES") +
  coord_flip() +
  theme_light()
## Tornados have the most harmful effect as they caused highest injuries to people. 

# Which types of events have the greatest economic consequences?
#This plot shows Property damage estimates by Event type
StormData %>%
  select(PROPDMG, PROPDMGEXP2, EVTYPE) %>%
  group_by(EVTYPE) %>%
  mutate(SumPROPDMGEXP = (PROPDMG * PROPDMGEXP2)) %>%
  summarise(SumPROPDMGEXP2 = sum(SumPROPDMGEXP)) %>%
  top_n(n = 8, wt = SumPROPDMGEXP2) %>%
  ggplot(aes(y = SumPROPDMGEXP2, x = reorder(x = EVTYPE, X = SumPROPDMGEXP2), fill=EVTYPE))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  #geom_text(aes(label=SumFATALITIES), size = 4, hjust = 0.5, vjust = -0.1) +
  xlab(label = "") +
  ylab(label = "Property damage estimates") +
  coord_flip() +
  theme_light()
#Floods caused highest economic damage followed by Hurricane and Tornados respectively

# Conclusion
Overall, floods caused highest economic damage followed by Hurricane and Tornados respectively. This may be because of the severity of the damage to properties. However, when it comes to harmful effect on people it was Tornados that caused the maximum effect in terms of injuries and amout of death toll it inflicted. 