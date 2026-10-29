annuity_due_apv <- function(x, n, i, mortality) {
  
  if (length(n) != 1 || is.na(n) || 
    n < 1 || n != floor(n)) {
    stop("Term must be a positive whole number of years")
  }
  
  #Premium payments time 0, 1,..., (n-1)
  k <- 0:(n-1)
  
  #Probability of surviving to each premium payment 
  survival <- sapply(
    k, 
    function(t) survival_prob(x, t, mortality)
  )
  
  #Discount each possible premium payment 
  pv_factors <- discount_factor(k ,i)
  
  #Present Value of all expected premium payments 
  sum(pv_factors * survival)
  
  
}

#Whole Life Annuity Due APV 
#NOTE similar to WL Insurance, ages only go to 100 
#Assumptions are needed to make the true values more accurate 

whole_life_annuity_due_apv <- function(
    x, i, mortality, max_age = 100) {
  
  if (length(x) != 1 || is.na(x) ||
      x != floor(x)) {
    stop("Issue age must be a whole number.")
  }
  
  if (length(max_age) != 1 || is.na(max_age) ||
      max_age != floor(max_age) || max_age < x) {
    stop("Maximum age must be a whole number at or above issue age.")
  }
  
  ages <- x:max_age
  
  if (!all(ages %in% mortality$age)) {
    stop("Mortality table does not cover every age requested.")
  }
  
  k <- 0:(length(ages) - 1)
  
  survival <- sapply(
    k,
    function(t) survival_prob(x, t, mortality)
  )
  
  pv_factors <- discount_factor(k, i)
  
  sum(pv_factors * survival)
}

#Reusable Annuity Pricing 

whole_life_annuity_due_apv <- function(
    x, annuity_due_apv_value) {
  
  if (length(x) != 1 || is.na(x) ||
      x != floor(x)) {
    stop("Age must be a whole number.")
  }
  
  if (length(annuity_due_apv_value) != 1 ||
      !is.finite(annuity_due_apv_value) ||
      annuity_due_apv_value <= 0) {
    stop("Whole life annuity-due APV must be positive.")
  }
  
  annuity_due_apv_value
}

#Annuity Immediate APV 

annuity_immediate_apv <- function(x, n, i, mortality) {
  
  if (length(n) != 1 || is.na(n) ||
      n < 1 || n != floor(n)) {
    stop("Term must be a positive whole number of years.")
  }
  
  # Payments occur at times 1, 2, ..., n
  k <- seq_len(n)
  
  # Probability of surviving to each payment date
  survival <- sapply(
    k,
    function(t) survival_prob(x, t, mortality)
  )
  
  # Discount each payment to time 0
  pv_factors <- discount_factor(k, i)
  
  sum(pv_factors * survival)
}

#Whole Life Annuity Immediate 

whole_life_annuity_immediate_apv <- function(
    x, annuity_due_apv_value) {
  
  if (length(x) != 1 || is.na(x) ||
      x != floor(x)) {
    stop("Age must be a whole number.")
  }
  
  if (length(annuity_due_apv_value) != 1 ||
      !is.finite(annuity_due_apv_value) ||
      annuity_due_apv_value < 1) {
    stop("Whole life annuity-due APV must be at least 1.")
  }
  
  annuity_due_apv_value - 1
}

