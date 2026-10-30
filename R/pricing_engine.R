#Pricing Engine

price_insurance <- function(
    product,
    x,
    i,
    face_amount,
    mortality,
    n = NULL,
    whole_life_Ax = NULL,
    whole_life_annuity_due = NULL) {
  
  product <- tolower(product)
  
  if (length(face_amount) != 1 ||
      !is.finite(face_amount) ||
      face_amount <= 0) {
    stop("Face amount must be positive.")
  }
  
  if (length(i) != 1 ||
      !is.finite(i) ||
      i <= -1) {
    stop("Interest rate must be greater than -1.")
  }
  
  if (product == "term") {
    
    if (is.null(n)) {
      stop("A term in years is required.")
    }
    
    benefit_apv <- term_insurance_apv(
      x, n, i, mortality
    )
    
    premium_apv <- annuity_due_apv(
      x, n, i, mortality
    )
    
  } else if (product == "endowment") {
    
    if (is.null(n)) {
      stop("A term in years is required.")
    }
    
    benefit_apv <- endowment_insurance_apv(
      x, n, i, mortality
    )
    
    premium_apv <- annuity_due_apv(
      x, n, i, mortality
    )
    
  } else if (product == "whole_life") {

    benefit_apv <- whole_life_insurance_apv(x, i, mortality)
    premium_apv <- whole_life_annuity_due_apv(x, i, mortality, max_age = 100)
    
  } else {
    stop("Product must be 'term', 'endowment', or 'whole_life'.")
  }
  
  net_premium <- face_amount * benefit_apv / premium_apv
  
  data.frame(
    product = product,
    issue_age = x,
    term_years = if (is.null(n)) NA_integer_ else n,
    interest_rate = i,
    face_amount = face_amount,
    benefit_apv = benefit_apv,
    premium_apv = premium_apv,
    net_annual_premium = net_premium
  )
}
