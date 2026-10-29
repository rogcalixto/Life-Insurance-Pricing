#Pricing Term-Insurance 
term_insurance_apv <- function(x, n, i, mortality) {
  #Validate the policy term
  if(length(n) != 1 || is.na(n) ||
     n < 1 || n != floor(n)) {
    stop("Term must be a positive whole number of years.")
  }
  
  #Death years: 1 through n 
  k <- 0:(n-1)
  
  #Probability of surviving to the start of each year
  survival <- sapply(
    k, 
    function(t) survival_prob(x, t, mortality)
  )
  
  #Probability of death during each year
  death <- sapply(
    k, 
    function(t) get_qx(x+t, mortality)
  )
  
  #Discount death benefits to the end of each year
  pv_factors <- discount_factor(k+1, i)
  
  #APV per $1 of death benefits
  sum(pv_factors * survival * death)
}

# Calculating the Net Annual Premium Under the Equivalence Principal(EP)
net_annual_term_premium <- function(
    x, n, i, face_amount, mortality) {
  
  if (length(face_amount) !=1 ||
      !is.finite(face_amount) || 
      face_amount <= 0) {
    stop("Face amount must be a positive number.")
  }
  
  benefit_apv <- term_insurance_apv(x, n, i, mortality)
  
  premium_apv <- annuity_due_apv(x, n, i, mortality)
  
  face_amount * benefit_apv / premium_apv
}

#Whole Life Insurance Pricing 
#NOTE only values for ages up to 100 are included in life tables
#So pricing goes up to a max age of 100. 
#I would need further assumptions of lives past 100 to get more accurate results 

whole_life_insurance_apv <- function(x, i, mortality) {
  
  if (length(x) != 1 || is.na(x) ||
      x != floor(x)) {
    stop("Age must be a whole number.")
  }
  
  if (length(i) != 1 || !is.finite(i) || i <= -1) {
    stop("Interest rate must be greater than -1.")
  }
  
  ages <- mortality$age[mortality$age >= x]
  
  if (length(ages) == 0 || min(ages) != x) {
    stop("Issue age is not available in the mortality table.")
  }
  
  # Calculate the APV only across the available ages
  k <- 0:(length(ages) - 1)
  
  survival <- sapply(
    k,
    function(t) survival_prob(x, t, mortality)
  )
  
  death <- sapply(
    ages,
    function(age) get_qx(age, mortality)
  )
  
  pv_factors <- discount_factor(k + 1, i)
  
  sum(pv_factors * survival * death)
}

#Reusable Whole Life Premium Function 
whole_life_net_premium <- function(
    face_amount, insurance_apv, annuity_due_apv) {
  
  if (length(face_amount) != 1 ||
      !is.finite(face_amount) ||
      face_amount <= 0) {
    stop("Face amount must be positive.")
  }
  
  if (length(insurance_apv) != 1 ||
      !is.finite(insurance_apv) ||
      insurance_apv <= 0 ||
      insurance_apv > 1) {
    stop("Insurance APV must be between 0 and 1.")
  }
  
  if (length(annuity_due_apv) != 1 ||
      !is.finite(annuity_due_apv) ||
      annuity_due_apv <= 0) {
    stop("Annuity-due APV must be positive.")
  }
  
  face_amount * insurance_apv / annuity_due_apv
}

#Pure Endowment 

pure_endowment_apv <- function(x, n, i, mortality) {
  
  if (length(n) != 1 || is.na(n) ||
      n < 1 || n != floor(n)) {
    stop("Term must be a positive whole number of years.")
  }
  
  survival_to_term <- survival_prob(
    x = x,
    t = n,
    mortality = mortality
  )
  
  discount_factor(n, i) * survival_to_term
}

#Endowment Insurance 
#Term + Pure Endowment 

endowment_insurance_apv <- function(x, n, i, mortality) {
  
  term_apv <- term_insurance_apv(
    x = x,
    n = n,
    i = i,
    mortality = mortality
  )
  
  pure_endowment <- pure_endowment_apv(
    x = x,
    n = n,
    i = i,
    mortality = mortality
  )
  
  term_apv + pure_endowment
}

#Endowment Premium 
endowment_net_premium <- function(
    x, n, i, face_amount, mortality) {
  
  benefit_apv <- endowment_insurance_apv(
    x, n, i, mortality
  )
  
  premium_apv <- annuity_due_apv(
    x, n, i, mortality
  )
  
  face_amount * benefit_apv / premium_apv
}
