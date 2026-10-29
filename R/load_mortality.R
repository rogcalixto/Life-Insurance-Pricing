library(readr)
library(dplyr)

#Load mortality data
mortality <- read_csv("sult.csv")

#Check import
head(mortality)

#Create Function to retrieve mortality rates
get_qx <- function(x ,mortality) {

  #Find the row corresponding to age x
  idx <- match(x, mortality$age)

  #Stop if the requested age is not in table
  if (is.na(idx)) {
    stop("Requested age is not available in mortality table")
  }

  #Return the mortality probability
  mortality$qx[idx]
}

#Create function for probability of surviving multiple years

survival_prob <- function(x, t, mortality) {

  # Surviving zero years has probability 1
  if (t == 0) {
    return(1)
  }

  # Ages whose annual survival probabilities are needed
  ages <- x + seq_len(t) - 1

  # Find those ages in the mortality table
  idx <- match(ages, mortality$age)

  if (anyNA(idx)) {
    stop("Mortality table does not cover the requested ages.")
  }

  # Multiply annual survival probabilities
  prod(1 - mortality$qx[idx])
}








