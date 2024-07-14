# Mikael Gouwtama
# 1007128127
# BTC1855 Assignment 3

# Assuming that the words_list file is in the same directory as this file
# Get the list of words from the words_list.txt file and make it into a vector
words <- readLines("words_list.txt")

# Randomly select 1 word from the list of words
answer_key <- sample(words, 1)
# Splitting the answer key into a vector separating the individual letters would
# aid later in checking whether the user's guess is in the answer key 
answer_key <- unlist(strsplit(answer_key, ""))

# Provide user the length of the answer
print(paste("The length of the answer word is", length(answer_key)))

# Tell the user some additional rules
print(paste("You can guess a letter or a word in each round"))
print(paste("You'll lose a trial if you guessed a letter or word incorrectly"))

# Initiate and report the number of tries the user has
# Here, we set the number of tries the user have as 5. 
tries <- 5
print(paste0("Guess this word. The number of tries you have is ", tries, ". Good Luck!"))

# Create a display of the user's correct answer so far. This will begin as 
# underscores corresponding to the length of the answer key
user_answer <- rep("_", length(answer_key))

# Initialize an empty vector for the correct guesses
your_guesses <- vector() 

# Get the user input
user_input <- function(your_guesses) {
  # Ask the user to choose whether they want to guess a letter or a word
  repeat {
    input_1 <- readline(prompt = "Guess a letter or a word? Choose 1 for letter, 2 for word: ")
    # If user correctly entered either 1 or 2
    if (input_1 == "1" | input_1 == "2") {
      # If the user choose to guess a letter, ask the user to enter the letter
      if (input_1 == "1") {
        repeat {
          input_2 <- readline(prompt = "Enter a single letter: ")
          # Based on the letter input, do the following
          if (input_2 %in% your_guesses) {
            cat("You have already guessed this. Please enter a different letter.\n")
          } else if (nchar(input_2) == 1 & grepl("^[a-zA-Z]$", input_2)) {
            # Return a list specifying "letter" and the input value if the user 
            # correctly enter a single letter
            # Make the values lower case so that letter cases difference are ignored
            # This is one way to exit the repeat loop
            return(list(input_type = "letter", input_value = tolower(input_2)))
          } else {
            cat("Input is invalid. Please enter a single letter.\n")
          }
        }
      } else if (input_1 == "2") {
        # If the user wants to enter a word, record the word after making them
        # all lowercase in a list with the type "word"
        # This is a second way to exit the repeat loop
        input_3 <- readline(prompt = "Enter a word: ")
        return(list(input_type = "word", input_value = tolower(input_3)))
      }
    } else {
      # If the user incorrectly enters 1/2, ask it again and repeat the loop
      cat("Please input either 1 or 2. 1 for letter and 2 for word.\n")
    }
  }
}

# Main loop for the hangman
# A while loop is used such that the game keeps running until the user exhausted
# all 5 of tries
while (tries != 0) {
  # Show the user their current progress
  cat("Current progress: ", paste(user_answer, collapse = ""), "\n")
  # Show the user their guesses so far
  cat("Your guesses so far are: ", paste(your_guesses, collapse = ""), "\n")
  # Tell the user their remaining tries
  cat("Remaining tries: ", tries, "\n")
  
  # Get input from user by calling the user_input function
  user_input_result <- user_input(your_guesses)
  
  # Since the input is a list with the type and the values, we isolate them into
  # 2 variables, x and y
  x <- user_input_result$input_type
  y <- user_input_result$input_value
  
  # Looking at the input type, enter this if condition if the input is a letter
  if (x == "letter") {
    # Update the user's guesses so far with the input value
    your_guesses <- sort(c(your_guesses, y))
    
    # Continue to check whether the input value (the letter) is in the answer key
    # If it is in the answer key, continue with this if conditional loop 
    if (y %in% answer_key) {
      cat("Nice, you have guessed a letter correctly!\n")
      
      # Get the position of the guess
      positions <- which(y == answer_key)
      # Update the user answer (the underlines showing the word) with the correct
      # guess at its correct position
      user_answer[positions] <- y
      
      # If the user has guessed the last letter such that the user's answer matches
      # the answer key, end the game and congratulate them
      if (identical(user_answer, answer_key)) {
        cat("Congratulations! You have guessed the word correctly:", 
            paste(answer_key, collapse = ""))
        break
      }
      
    # If the input value (the letter) is not in the answer key, deduct 1 trial
    } else {
      tries <- tries - 1
      cat("Oops, incorrect guess.\n")
    }
  
  # if the user decide to guess the word directly, enter this else if condition
  } else if (x == "word") {
    # If they guessed the answer key correctly, end the game and congratulate them
    if (y == paste(answer_key, collapse = "")) {
      cat("Congratulations! You have guessed the word correctly:", 
          paste(answer_key, collapse = ""))
      break
    # If they guessed the word wrong, deduct 1 trial
    } else {
      tries <- tries - 1
      cat("Oops, incorrect guess.\n")
    }
  }
}

# if the user ran out of tries, tell them they lose as the main loop for the
# game will no longer run
if (tries == 0) {
  cat("You ran out of tries, you lose :(", "\n")
  cat("The correct answer is:", paste(answer_key, collapse = ""), "\n")
}

