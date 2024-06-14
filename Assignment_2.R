# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 2

# Prompt the user to enter a three digit positive number
input <- readline(prompt="Enter a three digit positive number: ")

# Check if the user input is numeric. If not, print an error message and quit. 
if (is.na(as.numeric(input)) || nchar(input) != 3 || as.numeric(input) <= 0) {
  stop("The input is either not numeric, not a three digit number, or not a positive number")
} else {
  print(paste("You have entered a three digit positive number: ", input))
}

# Checking if the input is narcissistic
# Get the first, second, and third numbers from the user's input using the 
# substr() function and make that number numeric
first_num <- as.numeric(substr(input, 1, 1))
second_num <- as.numeric(substr(input, 2, 2))
third_num <- as.numeric(substr(input, 3, 3))

# Get the sum of the cubes of the first three numbers
sum_cubes <- (first_num)^3 + (second_num)^3 + (third_num)^3

# Finally, check if the sum_cubes is the same as the original input
if (sum_cubes == as.numeric(input)){
  print(paste(input, "is a narcissistic number"))
} else {
  print(paste(input, "is not an Armstrong number"))
}

