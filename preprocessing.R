library(lubridate)

data_dir <- "data"
data <- read.csv(paste(data_dir,"MCI_2014_to_2017.csv",sep="/"), header = TRUE, sep = ",")

#remove any duplicate rows/entries
data <- subset(data, !duplicated(data$event_unique_id))

#remove columns that aren't useful/duplicates
#duplicates of other columns
data <- data[, -(1:4)]
#UCR codes - not used in this case
data <- data[, -(4:5)]
#ID number - not needed
data <- data[, -23]
#Hood ID - not needed
data <- data[, -19]

#formatting dates - remove garbage time values at the end
data$occurrencedate <- ymd(gsub("(.*)T.*", "\\1", data$occurrencedate))
data$reporteddate <- ymd(gsub("(.*)T.*", "\\1", data$reporteddate))

#removing whitespace from day of week
data$occurrencedayofweek <- as.factor(trimws(data$occurrencedayofweek, "b"))
data$reporteddayofweek <- as.factor(trimws(data$reporteddayofweek, "b"))

#missing data
colSums(is.na(data))
NAdata <- unique (unlist (lapply (data, function (x) which (is.na (x)))))

#imputing occurence dates from occurence date field
data$occurrenceyear[NAdata] <- year(data$occurrencedate[NAdata])
data$occurrencemonth[NAdata] <- month(data$occurrencedate[NAdata], label = TRUE, abbr = FALSE)
data$occurrenceday[NAdata] <- day(data$occurrencedate[NAdata])
data$occurrencedayofweek[NAdata] <- wday(data$occurrencedate[NAdata], label = TRUE, abbr = FALSE)
data$occurrencedayofyear[NAdata] <- yday(data$occurrencedate[NAdata])

#time between report and occurance date - do we need this for anything?
#data$daysbtwn <- data$reporteddate - data$occurrencedate

#change things to factors
data$reportedyear <- as.factor(data$reportedyear)
data$reportedday <- as.factor(data$reportedday)
data$reporteddayofyear <- as.factor(data$reporteddayofyear)
data$reportedhour <- as.factor(data$reportedhour)
data$occurrenceyear <- as.factor(data$occurrenceyear)
data$occurrenceday <- as.factor(data$occurrenceday)
data$occurrencedayofyear <- as.factor(data$occurrencedayofyear)
data$occurrencehour <- as.factor(data$occurrencehour)

#drop unused factor levels
data$occurrencedayofweek <- droplevels(data$occurrencedayofweek)
data$occurrencemonth <- droplevels(data$occurrencemonth)

#write CSV for use elsewhere
write.csv(data, file.choose())
