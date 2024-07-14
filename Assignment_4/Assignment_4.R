# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 4

# Read the ufo_subset data into a data frame
ufo <- read.csv("ufo_subset.csv")

class(ufo)

# check and correct structural issue

str(ufo)

summary(ufo)

## convert datetime into date 
library(lubridate)

ufo$datetime <- ymd_hm(ufo$datetime)

str(ufo)

## convert date_posted into date

ufo$date_posted <- dmy(ufo$date_posted)

str(ufo)

ufo$date_posted <- ymd(ufo$date_posted)
