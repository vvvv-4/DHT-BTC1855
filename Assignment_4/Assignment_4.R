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

# Fixing the structural issues
# First, we'll convert the datetime structure into a POSIXct which can store date
# and time in the correct format. Here, we specify year-month-day-hour-minute.
# This function is part of the lubridate package that we have loaded earlier.
ufo_work$datetime <- ymd_hm(ufo_work$datetime)

# We can confirm that we have changed the structure of the datetime variable
str(ufo_work$datetime)

# Similarly, we'll convert date_posted into a POSIXct structure to make it similar
# with the datetime variable's structure and format

# Since the date_posted values are in character, we'll convert it to the date structure
# first using the dmy function in the lubridate package. It will conveniently convert
# it to a year-month-day format
ufo_work$date_posted <- dmy(ufo_work$date_posted)

# Here, we'll see that it now has a date structure
str(ufo_work$date_posted)

# Now, we can convert date_posted into the POSIXct structure
ufo_work$date_posted <- as.POSIXct(ufo_work$date_posted)

# Again, confirming that it is now in the POSIXct structure
str(ufo_work$date_posted)

# Another notable issue with the dataset is that the duration.hours.min variable is
# not consistent across the rows. We can make them consistent by giving the hour:minutes
# based on the respective duration.seconds variable
ufo_work <- ufo_work %>%
  mutate(duration.hours.min = paste(floor(duration.seconds / 3600), 
                                    floor((duration.seconds %% 3600) / 60), 
                                    sep = ":"))

# We can do a final check of the structure before continuing on to content cleaning. I think
# we have fixed all the necessary issues in the structure
str(ufo_work)

# One common content issue is duplicate entries, so we can first remove them
ufo_work <- ufo_work %>%
  distinct()

# Another common content issue is missing values. Since we are interested in the "country,"
# "shape," and "duration.seconds" variables, we can look specifically at them
sum(is.na(ufo_work$country))
sum(is.na(ufo_work$shape))
sum(is.na(ufo_work$duration.seconds))

# Interestingly, they all result in 0 entries having NAs. However, by inspecting the
# data themselves, we do see missing values. Perhaps these missing values are empty
# strings. We can now check on those
sum(ufo_work$country == "")
sum(ufo_work$shape == "")
sum(ufo_work$duration.seconds == "")

# It is indeed true that the missing values are listed as empty strings. We can remove that
# It also appears that there are no missing values in the duration.seconds. However, we
# can see that there are several entries where the sightings are very brief (<0.5 s).
# I personally find them suspicious, so I would like to exclude them as well!
ufo_work <- ufo_work %>%
  filter(!is.na(country) & country != "") %>%
  filter(!is.na(shape) & shape != "") %>%
  filter(duration.seconds > 0.5)

# Again, we can inspect the structures and the summary of the content in each variables
str(ufo_work)
summary(ufo_work)

# As previously mentioned, looking at the summary, we can see that the maximum duration.seconds 
# values may be a little suspicious (82800000)

# We can look at the distribution in duration.seconds using boxplot to see any outliers
boxplot(ufo_work$duration.seconds)

# There appear to be quite a few outliers in this data set, specifically very high values
# We can look at the top 10 highest values
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

