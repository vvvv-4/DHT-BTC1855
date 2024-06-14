# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 2

# Prompt the user to enter a three digit positive number
number <- readline(prompt="Enter a three digit positive number: ")

# Check if the user input is numeric. If not, print an error message and quit. 
if (is.na(as.numeric(number)) || nchar(number) != 3 || as.numeric(number) <= 0) {
  stop("The input is either not numeric, not a three digit number, or not a positive number")
} else {
  print(paste("You have entered a three digit positive number: ", number))
}

