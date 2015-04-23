# Questions
# 
# Your data analysis must address the following questions:
#     
#     Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with 
# respect to population health?
# 
# Across the United States, which types of events have the greatest economic consequences?
# 
# Consider writing your report as if it were to be read by a government or municipal manager who might be responsible 
# for preparing for severe weather events and will need to prioritize resources for different types of events. However, 
# there is no need to make any specific recommendations in your report.
# Requirements
# 

# Title
## Your document should have a title that briefly summarizes your data analysis

# Synopsis 
## (Immediately after the title, there should be a synopsis which describes and summarizes your analysis in at most 10
##  complete sentences)

# Data Processing
## (which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your
## analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document.
## If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks)

# Results
## (your results are presented)

## 1. The analysis document must have at least one figure containing a plot.
## 2. Your analyis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but 
##    there cannot be more than three figures total.
## 3. You must show all your code for the work in your analysis document. This may make the document a bit verbose, but
##    that is okay. In general, you should ensure that echo = TRUE for every code chunk (this is the default setting in
##    knitr).
library(lubridate)
library(dplyr)
# data <- read.csv('repdata-data-StormData.csv.bz2')
# save(data, file = 'data.Rdata')
load('data.Rdata')

data <- data %>%
  group_by(EVTYPE) %>%
  summarize(FATALITIES = sum(FATALITIES),
            INJURIES = sum(INJURIES)) %>%
  arrange(-FATALITIES, -INJURIES)

# Replace row nums with existing value
rownames(data) <- data$REFNUM
data$REFNUM <- NULL

# Remove unused columns
data$COUNTY_END <- NULL
data$COUNTYENDN <- NULL

# Begin date/time (needs work)
data$BGN_DATE <- gsub('\\s0:00:00$', '', data$BGN_DATE)
data$BGN_TIME <- gsub('o|O', '0', data$BGN_TIME)
data$BGN_TIME <- gsub('^(\\d{3})$', '0\\1', data$BGN_TIME)
data$BGN_TIME <- gsub('^(\\d{2})(\\d{2})$', '\\1:\\2:00', data$BGN_TIME)
hour <- as.integer(substring(data$BGN_TIME, 1, 2))
hour <- hour %% 12
hour[grep('PM$', data$BGN_TIME)] <- hour[grep('PM$', data$BGN_TIME)] + 12
data$BGN_TIME <- paste(sprintf('%02d', hour), gsub('^\\d{2}', '', data$BGN_TIME), sep = '')
data$BGN_DATETIME <- mdy_hms(paste(data$BGN_DATE, data$BGN_TIME))
#fix this#######################
data[is.na(data$BGN_DATETIME), ]
################################
data$BGN_DATE <- NULL
data$BGN_TIME <- NULL

# End date/time (needs work)
data$END_DATE[data$END_DATE == ''] <- NA
data$END_DATE <- gsub('\\s0:00:00$', '', data$END_DATE)
data$END_TIME[data$END_TIME == ''] <- NA
#data$END_TIME <- gsub('^(\\d{3})$', '0\\1', data$END_TIME)
data$END_TIME <- gsub('^(\\d{2})(\\d{2})$', '\\1:\\2:00', data$END_TIME)
hour <- as.integer(substring(data$END_TIME, 1, 2))
hour <- hour %% 12
hour[grep('PM$', data$END_TIME)] <- hour[grep('PM$', data$END_TIME)] + 12
data$END_TIME <- paste(sprintf('%02d', hour), gsub('^\\d{2}', '', data$END_TIME), sep = '')
data$END_DATETIME <- mdy_hms(paste(data$END_DATE, data$END_TIME))

# States (needs work, inconsistent)
test <- unique(data[, c('STATE__', 'STATE')])
test <- test[order(test$STATE, test$STATE__), ]

# LATITUDE (needs work, MinMax: http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States)
unique(data$STATE[which(data$LATITUDE < 1854 & data$LATITUDE != 0)])

# LONGITUDE
min(data$LONGITUDE)
max(data$LONGITUDE)

names(data)
unique(data$ZONENAMES)

