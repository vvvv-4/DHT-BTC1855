# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 3

# Assuming that the words_list file is in the same directory as this file
# Get the list of words from the words_list.txt file and make it into a vector
words <- readLines("words_list.txt")

# Randomly select 1 word from the list of words
answer_key <- sample(words, 1)
answer_key <- unlist(strsplit(answer_key, ""))

# Provide user the length of the answer
print(paste("The length of the answer word is", length(answer_key)))

# Initiate and report the number of tries the user has
# Here, we set the number of tries the user have as 5. 
tries <- 5
print(paste0("Guess this word. The number of tries you have is ", tries, ". Good Luck!"))

# Create a display of the user's correct answer so far. This will begin as 
# underscores corresponding to the length of the answer key
user_answer <- rep("_", length(answer_key))

# Initialize an empty vector for the correct guesses
your_guesses <- vector() 

user_input <- function(your_guesses){
  # Want the question to be asked as least once
  repeat{
    input <- readline(prompt = "Enter a single letter: ")
    # check if the input is a letter and a single character
    if (input %in% your_guesses){
      cat("You have already guessed this. Please enter a different letter.", "\n")
    } else if (nchar(input) == 1 & grepl("^[a-zA-Z]$", input)) {
      break
    } else {
      cat("Input is invalid. Please enter a single letter", "\n")
    }
  }
  return(tolower(input))
}


while (tries != 0) {
  cat("current progress: ", paste(user_answer, collapse = ""), "\n")
  cat("your guesses so far are:", paste(your_guesses, collapse = ""), "\n")
  cat("remaining tries: ", tries, "\n")
  
  # get input from user
  current <- user_input(your_guesses)
  
  your_guesses <- sort(c(your_guesses, current))
  
  if (current %in% answer_key) {
    cat("Nice, you have guessed a letter correctly!", "\n")
    
    # get the position of the guess
    positions <- which(current == answer_key)
    user_answer[positions] <- current
    
    if (identical(user_answer, answer_key)) {
      cat("Congratulations! You have guessed the word correctly:", paste(answer_key, collapse = ""))
      break
    }
    
  } else {
    tries = tries - 1
    cat("Oops, incorrect guess.", "\n")
  }
}

if (tries == 0) {
  cat("You ran out of tries, you lose :(", "\n")
  cat("The correct answer is:", paste(answer_key, collapse = ""), "\n")
}

