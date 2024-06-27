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

