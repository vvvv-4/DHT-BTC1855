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

ufo$date_posted <- ymd(ufo$date_posted)

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
ufo_no_dupe <- ufo_no_dupe %>%
  filter(!is.na(country) & country != "")


# identify missing
# identify missing country
sum(ufo_no_dupes$country == "")
# I want to remove these^ columns from my ufo_no_dupe data frame, help




# identify weird data
summary(ufo_no_dupe)

which(ufo$duration.seconds == 82800000)

ufo[17253, ]





