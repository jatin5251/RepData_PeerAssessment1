df <- read.csv("/Users/jatin/Desktop/storm",header = TRUE,sep = ",",na.strings = "NA",
               stringsAsFactors = FALSE)

## ----cache=TRUE----------------------------------------------------------
df <- read.csv(bzfile("./Desktop/storm.bz2"))


## ------------------------------------------------------------------------
# number of unique event types
length(unique(df$EVTYPE))
# translate all letters to lowercase
event_types <- tolower(df$EVTYPE)
# replace all punct. characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))
# update the data frame
df$EVTYPE <- event_types


## ------------------------------------------------------------------------
library(dplyr)
casual <- df%>%group_by(EVTYPE)%>%summarize(fatalities = sum(FATALITIES),
                                            injuries = sum(INJURIES))
# Find events that caused most death and injury
fatal_events <- head(casual[order(casual$fatalities, decreasing = T), ], 10)
injury_events <- head(casual[order(casual$injuries, decreasing = T), ], 10)


## ------------------------------------------------------------------------
fatal_events[, c("EVTYPE", "fatalities")]


## ------------------------------------------------------------------------
injury_events[, c("EVTYPE", "injuries")]


## ------------------------------------------------------------------------
exp_transform <- function(e) {
  # h -> hundred, k -> thousand, m -> million, b -> billion
  if (e %in% c('h', 'H'))
    return(2)
  else if (e %in% c('k', 'K'))
    return(3)
  else if (e %in% c('m', 'M'))
    return(6)
  else if (e %in% c('b', 'B'))
    return(9)
  else if (!is.na(as.numeric(e))) # if a digit
    return(as.numeric(e))
  else if (e %in% c('', '-', '?', '+'))
    return(0)
  else {
    stop("Invalid exponent value.")
  }
}


## ----cache=TRUE----------------------------------------------------------
prop_dmg_exp <- sapply(df$PROPDMGEXP, FUN=exp_transform)
df$prop_dmg <- df$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(df$CROPDMGEXP, FUN=exp_transform)
df$crop_dmg <- df$CROPDMG * (10 ** crop_dmg_exp)


## ------------------------------------------------------------------------
# Compute the economic loss by event type
library(dplyr)

econ_loss <- df%>% group_by(EVTYPE)%>%summarize(prop_dmg = sum(prop_dmg),
                                               crop_dmg = sum(crop_dmg))
# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)


## ------------------------------------------------------------------------
prop_dmg_events[, c("EVTYPE", "prop_dmg")]


## ------------------------------------------------------------------------
crop_dmg_events[, c("EVTYPE", "crop_dmg")]


## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
# Set the levels in order
p1 <- ggplot(data=fatal_events,
             aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Total number of fatalities") +
  xlab("Event type") +
  theme(legend.position="none")

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  ylab("Total number of injuries") +
  xlab("Event type") +
  theme(legend.position="none")

grid.arrange(p1, p2, top="Top deadly weather events in the US (1950-2011)")


## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
# Set the levels in order
p1 <- ggplot(data=prop_dmg_events,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Event type") +
  ylab("Property damage in dollars (log-scale)") +
  theme(legend.position="none")

p2 <- ggplot(data=crop_dmg_events,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  xlab("Event type") +
  ylab("Crop damage in dollars") + 
  theme(legend.position="none")

grid.arrange(p1, p2, top="Weather costs to the US economy (1950-2011)")
