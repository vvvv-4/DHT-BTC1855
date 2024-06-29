# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 3

# Assuming that the words_list file is in the same directory as this file
# Get the list of words from the words_list.txt file and make it into a vector
words <- readLines("words_list.txt")
words

# other method to read the text file
words2 <- read.delim("words_list.txt", header = FALSE)
words2
words2[[1]][1]

# Randomly select 1 word from the list of words
answer <- sample(words, 1)
answer

print(paste("The length of the answer word is", nchar(answer)))

# Initiate and report the number of tries the user has
tries <- 5
print(paste0("Guess this word. The number of tries you have is ", tries, ". Good Luck!"))

user_input <- function(){
  # Want the question to be asked as least once
  repeat{
    input <- readline(prompt = "Enter a single letter: ")
    # check if the input is a letter and a single character
    if (nchar(input) == 1 & grepl("^[a-zA-Z]$", input)){
      break
    } else {
      cat("Input is invalid. Please enter a single letter")
    }
  }
  return(tolower(input))
}

user_input()





