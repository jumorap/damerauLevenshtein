library(tidyverse)
library(dplyr)
library(ggplot2)

# Returns a matrix of minimum edit distance operations.
# Also returns the optimal alignment.
editDistDP <- function(str1, str2, m, n) {
  # Initialize the matrix dp that will store the minimum edit distance operations.
  dp <- matrix(rep(0, (n + 1) * (m + 1)), ncol=n + 1)

  # Fill d[][] in bottom up manner
  for (i in 0: m + 1) {
    for (j in 0: n + 1) {
      # First row of d[][] is 0
      if (i == 1) {
        dp[i, j] <- j - 1
      }
      # First column of d[][] is 0
      else if (j == 1) {
        dp[i, j] <- i - 1
      }
      # If last characters are same, ignore last char
      # and recur for remaining string
      else if (str_sub(str1, i - 1, i - 1) == str_sub(str2, j - 1, j - 1)) {
        dp[i, j] <- dp[i - 1, j - 1]
      }
      # If last character are different, consider all
      # possibilities and find minimum
      else {
        dp[i, j] <- min(dp[i, j - 1], dp[i - 1, j], dp[i - 1, j - 1]) + 1
      }
    }
  }
  # Return the edit distance matrix and the optimal alignment
  return(dp[m + 1, n + 1])
}

# Returns the optimal alignment of two strings using the edit distance matrix dp
getCombinationDistance <- function(words) {
  n <- length(words)
  data <- NULL
  # Iterate through all possible combinations of words and find the minimum edit distance
  for (i in 1: (n - 1)) {
    for (j in (i + 1): n) {
      # Get the minimum edit distance between the two words
      str1 <- words[i]
      str2 <- words[j]
      distance <- editDistDP(str1, str2, str_length(str1), str_length(str2))
      data <- append(data,distance)
    }
  }
  print(n)
  return(data)
}

# Plot the histogram of frequency of the minimum edit distance between words
setHist <- function(data) {
  hist(data)
}

# Get the average edit distance between the words
getAvarget <- function(data) {
  mean(data)
}

# Get the variance of the edit distance between the words
getVariance <- function(data) {
  var(data)
}

words <- c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")

data <- getCombinationDistance(words)

setHist(data)
getAvarget(data)
getVariance(data)
