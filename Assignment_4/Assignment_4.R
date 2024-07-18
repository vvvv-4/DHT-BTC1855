# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 4

# Reviewer is advised to place this file on the same working directory as the ufo_subset.csv file

# We'll start by loading the necessary packages, which are the dplyr and lubridate packages
library(dplyr)
library(lubridate)

# Read the ufo_subset data into a dataframe
ufo <- read.csv("ufo_subset.csv")

# Check that the ufo dataset is of class dataframe
class(ufo)

# check for any structural issues in the variables within the ufo dataframe
# We'll see that two variables have incorrect structure, which are the datetime
# and the date_posted variables. Being a date, they should not be structured
# as a character
str(ufo)

# We can also see a summary of the data to inspect for any content issues 
# We can flag an issue with the duration.seconds variable, where the maximum
# value is 82800000
summary(ufo)

# Before continuing with any cleaning and analysis, we'll want to keep a copy of
# the orignial dataset

# Let's create a new dataset that we'll work on called ufo_work
ufo_work <- ufo

## convert datetime into date 
ufo_work$datetime <- ymd_hm(ufo_work$datetime)

str(ufo_work$datetime)

## convert date_posted into date

ufo_work$date_posted <- dmy(ufo_work$date_posted)

str(ufo_work$date_posted)

# converting date_posted into posixct
ufo_work$date_posted <- as.POSIXct(ufo_work$date_posted)

str(ufo_work$date_posted)

## change the duration.hours.min columns so that they report the hours and minutes based on the duration.seconds and make it consistent

ufo_work <- ufo_work %>%
  mutate(duration.hours.min = paste(floor(duration.seconds / 3600), 
                                    floor((duration.seconds %% 3600) / 60), 
                                    sep = ":"))

str(ufo_work)

# Identify and remove duplicate entries
ufo_work <- ufo_work %>%
  distinct()

# Remove rows with missing `country` values
# It seems like seconds 0.02 is problematic by looking at the comments
ufo_work <- ufo_work %>%
  filter(!is.na(country) & country != "") %>%
  filter(!is.na(shape) & shape != "") %>%
  filter(duration.seconds > 0.5)

str(ufo_work)

summary(ufo_work)


# Check for missing values in 'country', 'shape', and 'duration.seconds'
sum(is.na(ufo_work$country))
sum(is.na(ufo_work$shape))
sum(is.na(ufo_work$duration.seconds))

# Look at the distribution in duration.seconds using boxplot
boxplot(ufo_work$duration.seconds)

head(sort(ufo_work$duration.seconds, decreasing = T), 10)

# Taking only the 99th percentile
threshold <- quantile(ufo_work$duration.seconds, 0.99)
ufo_work <- ufo_work %>%
  filter(duration.seconds <= threshold)

# Visualize the distribution of duration.seconds
hist(ufo_work$duration.seconds, main = "Histogram of Duration Seconds", xlab = "Duration Seconds")

# Examine values at different percentiles
quantile(ufo_work$duration.seconds, probs = seq(0.8, 1, by = 0.01))

# Taking only the 90th percentile
threshold <- quantile(ufo_work$duration.seconds, 0.89)
ufo_work <- ufo_work %>%
  filter(duration.seconds <= threshold)

hist(ufo_work$duration.seconds, main = "Histogram of Duration Seconds", xlab = "Duration Seconds")

boxplot(ufo_work$duration.seconds)


# Now, look at the summaries for the three variables of interest
summary(ufo_work$country)
summary(ufo_work$shape)
summary(ufo_work$duration.seconds)

table(ufo_work$country, useNA = "ifany")

table(ufo_work$shape, useNA = "ifany")

# Standardize 'country' and 'shape' values to handle inconsistencies
ufo_work$country <- toupper(ufo_work$country)
ufo_work$shape <- tolower(ufo_work$shape)

# Recheck frequency tables for 'country' and 'shape'
table(ufo_work$country, useNA = "ifany")
table(ufo_work$shape, useNA = "ifany")

# It seems like comments with NUFORC Note indicate that the sighting might not
# be a UFO but is some other objects like planets, starts, birds, etc
# We should remove them too (those with nuforc in it)

ufo_hoax_removed <- ufo_work %>%
  filter(!(grepl("(?i)hoax", comments) & !grepl("(?i)this is not a hoax", comments)) &
           !grepl("(?i)NUFORC Note", comments))

# creating another column called report_delay, which is the time difference in 
# days, between the date of the sighting and the date it was reported.
ufo_hoax_removed <- ufo_hoax_removed %>%
  mutate(report_delay = as.numeric(difftime(date_posted, datetime, units = "days"))) %>%
  filter(report_delay >= 0)

average_report_delay <- ufo_hoax_removed %>%
  group_by(country) %>%
  summarize(average_report_delay = mean(report_delay))

average_report_delay

hist(ufo_hoax_removed$duration.seconds)

