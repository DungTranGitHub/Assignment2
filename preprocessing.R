library(lubridate)

data_dir = "data"
data=read.csv(paste(data_dir,"MCI_2014_to_2017.csv",sep="/"), header = TRUE, sep = ",")

#remove any duplicate rows/entries
data <- subset(data, !duplicated(data$event_unique_id))

#remove columns that aren't useful/duplicates
#duplicates of other columns
data2 <- data[, -(1:4)]
#UCR codes - not used in this case
data2 <- data2[, -(4:5)]
#ID number - not needed
data2 <- data2[, -23]
#Hood ID - not needed
data2 <- data2[, -19]

#formatting dates - remove garbage time values at the end
data2$occurrencedate <- ymd(gsub("(.*)T.*", "\\1", data2$occurrencedate))
data2$reporteddate <- ymd(gsub("(.*)T.*", "\\1", data2$reporteddate))

#removing whitespace from day of week
data2$occurrencedayofweek <- as.factor(trimws(data2$occurrencedayofweek, "b"))
data2$reporteddayofweek <- as.factor(trimws(data2$reporteddayofweek, "b"))

#missing data
colSums(is.na(data2))
NAdata <- unique (unlist (lapply (data2, function (x) which (is.na (x)))))

#imputing occurence dates from occurence date field
data2$occurrenceyear[NAdata] <- year(data2$occurrencedate[NAdata])
data2$occurrencemonth[NAdata] <- month(data2$occurrencedate[NAdata], label = TRUE, abbr = FALSE)
data2$occurrenceday[NAdata] <- day(data2$occurrencedate[NAdata])
data2$occurrencedayofweek[NAdata] <- wday(data2$occurrencedate[NAdata], label = TRUE, abbr = FALSE)
data2$occurrencedayofyear[NAdata] <- yday(data2$occurrencedate[NAdata])

#time between report and occurance date - do we need this for anything?
#data2$daysbtwn <- data2$reporteddate - data2$occurrencedate

#change things to factors
data2$reportedyear <- as.factor(data2$reportedyear)
data2$reportedday <- as.factor(data2$reportedday)
data2$reporteddayofyear <- as.factor(data2$reporteddayofyear)
data2$reportedhour <- as.factor(data2$reportedhour)
data2$occurrenceyear <- as.factor(data2$occurrenceyear)
data2$occurrenceday <- as.factor(data2$occurrenceday)
data2$occurrencedayofyear <- as.factor(data2$occurrencedayofyear)
data2$occurrencehour <- as.factor(data2$occurrencehour)

#drop unused factor levels
data2$occurrencedayofweek <- droplevels(data2$occurrencedayofweek)
data2$occurrencemonth <- droplevels(data2$occurrencemonth)

#write CSV for use elsewhere
write.csv(data2, file.choose())
