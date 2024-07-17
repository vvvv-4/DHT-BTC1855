# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 4

library(dplyr)
# Read the ufo_subset data into a data frame
ufo <- read.csv("ufo_subset.csv")

class(ufo)

# check and correct structural issue

str(ufo)

summary(ufo)

## convert datetime into date 
library(lubridate)

ufo$datetime <- ymd_hm(ufo$datetime)

str(ufo$datetime)

## convert date_posted into date

ufo$date_posted <- dmy(ufo$date_posted)

str(ufo$date_posted)

# converting date_posted into posixct
ufo$date_posted <- as.POSIXct(ufo$date_posted)

str(ufo$date_posted)

## change the duration.hours.min columns so that they report the hours and minutes based on the duration.seconds and make it consistent

ufo <- ufo %>%
  mutate(duration.hours.min = paste(floor(duration.seconds / 3600), 
                                    floor((duration.seconds %% 3600) / 60), 
                                    sep = ":"))

str(ufo)

# Identify and remove duplicate entries
ufo_no_dupe <- ufo %>%
  distinct()

# Remove rows with missing `country` values
# It seems like seconds 0.02 is problematic by looking at the comments
ufo_no_dupe <- ufo_no_dupe %>%
  filter(!is.na(country) & country != "") %>%
  filter(!is.na(shape) & shape != "") %>%
  filter(duration.seconds != 0.02)

str(ufo_no_dupe)

summary(ufo_no_dupe)


# Check for missing values in 'country', 'shape', and 'duration.seconds'
sum(is.na(ufo_no_dupe$country))
sum(is.na(ufo_no_dupe$shape))
sum(is.na(ufo_no_dupe$`duration seconds`))

# Look at the distribution in duration.seconds using boxplot
boxplot(ufo_no_dupe$duration.seconds)

head(sort(ufo_no_dupe$duration.seconds, decreasing = T), 10)

# Taking only the 99th percentile
threshold <- quantile(ufo_no_dupe$duration.seconds, 0.99)
ufo_no_dupe <- ufo_no_dupe %>%
  filter(duration.seconds <= threshold)

# Visualize the distribution of duration.seconds
hist(ufo_no_dupe$duration.seconds, breaks = 100, main = "Histogram of Duration Seconds", xlab = "Duration Seconds")

# Examine values at different percentiles
quantile(ufo_no_dupe$duration.seconds, probs = seq(0.9, 1, by = 0.01))

# Taking only the 90th percentile
threshold <- quantile(ufo_no_dupe$duration.seconds, 0.90)
ufo_no_dupe <- ufo_no_dupe %>%
  filter(duration.seconds <= threshold)

hist(ufo_no_dupe$duration.seconds, breaks = 100, main = "Histogram of Duration Seconds", xlab = "Duration Seconds")

boxplot(ufo_no_dupe$duration.seconds)


# Now, look at the summaries for the three variables of interest

summary(ufo_no_dupe$country)
summary(ufo_no_dupe$shape)
summary(ufo_no_dupe$duration.seconds)

table(ufo_no_dupe$country, useNA = "ifany")

table(ufo_no_dupe$shape, useNA = "ifany")



# Standardize 'country' and 'shape' values to handle inconsistencies
ufo_no_dupe$country <- toupper(ufo_no_dupe$country)
ufo_no_dupe$shape <- tolower(ufo_no_dupe$shape)

# Recheck frequency tables for 'country' and 'shape'
table(ufo_no_dupe$country, useNA = "ifany")
table(ufo_no_dupe$shape, useNA = "ifany")





# Remove rows with missing '

#sum(ufo_no_dupe$shape == "")

#sum(is.na(ufo_no_dupe$shape))

#sum(ufo_no_dupe$duration.seconds <= 0.02)



#sum(is.na(ufo_no_dupe$duration.seconds))

# identify weird data
#summary(ufo_no_dupe)

#which(ufo$duration.seconds == 82800000)

#ufo[17253, ]





