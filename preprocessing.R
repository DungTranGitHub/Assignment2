library(lubridate)

data_dir <- "data"
data <- read.csv(paste(data_dir,"MCI_2014_to_2017.csv",sep="/"), header = TRUE, sep = ",")

#remove any duplicate rows/entries
data <- subset(data, !duplicated(data$event_unique_id))

#remove columns that aren't useful/duplicates
#duplicates of other columns
#UCR codes - not used in this case
#ID number - not needed
#Hood ID - not needed
data <- data[, !colnames(data) %in% c("X","Y","Index_","event_unique_id","ucr_code","ucr_ext","FID","Hood_ID")]

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
for(col in c("reportedyear","reportedday","reporteddayofyear","reportedhour","occurrenceyear","occurrenceday",
             "occurrencedayofyear","occurrencehour")) {
  data[,col] = as.factor(data[,col])
}

#drop unused factor levels
for(col in names(data)) {
  if(is.factor(data[,col])) {
    data[,col] = droplevels(data[,col])
  }
}

#write CSV for use elsewhere
#write.csv(data, file.choose())
