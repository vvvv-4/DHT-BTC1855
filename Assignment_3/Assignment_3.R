# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 3

# Assuming that the words_list file is in the same directory as this file
# Get the list of words from the words_list.txt file and make it into a vector
words <- readLines("words_list.txt")
#words

# other method to read the text file
#words2 <- read.delim("words_list.txt", header = FALSE)
#words2
#words2[[1]][1]

# Randomly select 1 word from the list of words
answer_key <- sample(words, 1)
answer_key <- unlist(strsplit(answer_key, ""))

print(paste("The length of the answer word is", length(answer_key)))

# Initiate and report the number of tries the user has
tries <- 5
print(paste0("Guess this word. The number of tries you have is ", tries, ". Good Luck!"))
user_answer <- rep("_", length(answer_key))

# Initialize an empty vector for the correct guesses
correct_letters <- vector() 

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

while (tries != 0) {
  cat("current progress: ", paste(user_answer, collapse = ""), "\n")
  cat("remaining tries: ", tries)
  
  # get input from user
  current <- user_input()
  
  if (current %in% answer_key) {
    cat("Nice, you have guessed a letter correctly!")
    
    # get the position of the guess
    positions <- which(current == answer_key)
    user_answer[positions] <- current
    cat("current progress: ", paste(user_answer, collapse = ""), "\n")
    correct_letters <- c(correct_letters, current)
  } else {
    tries = tries - 1
    cat("Oops, incorrect guess. You now have ", tries, "remaining")
  }
}



