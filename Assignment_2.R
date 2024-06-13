# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 2

# Prompt the user to enter a three digit positive number
number <- readline(prompt="Enter a three digit positive number: ")

if (is.na(as.numeric(number))) {
  print(paste(number, "is not numeric"))
  stop()
}



if (is.numeric(number)) {
  print(paste(number, "is numeric"))
} else if (!is.numeric(number)) {
  print(paste(number, "is not numeric"))
  stop()
}
