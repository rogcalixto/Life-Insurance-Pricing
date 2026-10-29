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

#Discount Factor Function 
discount_factor <- function(t, i) { 
  #Validate inputs
  if (any(t < 0) || any(!is.finite(t))) {
    stop("Time must be finite and non-negative.")
  }
  
  if (length(i) != 1 || !is.finite(i) || i <= -1) {
    stop("Interest rate must be a finite number greater than -1.")
  }
  
  #Present Value Factor
  1 / (1+i)^t
}