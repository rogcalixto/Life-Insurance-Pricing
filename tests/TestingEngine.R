#20 year term with 100,000 Face Amount (30)
term_quote <- price_insurance(
  product = "term",
  x = 30,
  n = 20,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality
)

print(term_quote)

#20 year Endowment with Face Amount 100,000 (30)
endowment_quote <- price_insurance(
  product = "endowment",
  x = 30,
  n = 20,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality
)

print(endowment_quote)

#Whole Life with Face Amount 100,000 (30)
whole_life_quote <- price_insurance(
  product = "whole_life",
  x = 30,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality,
  whole_life_Ax = 0.07698,
  whole_life_annuity_due = 19.3834
)

print(whole_life_quote)

#Final Price-Engine Tests

# Term insurance
stopifnot(term_quote$net_annual_premium > 0)

# Endowment insurance
stopifnot(endowment_quote$net_annual_premium > 0)

# Endowment includes a survival benefit, so its APV
# should exceed the term insurance APV for the same term.
stopifnot(
  endowment_quote$benefit_apv >
    term_quote$benefit_apv
)

# Whole life insurance
stopifnot(whole_life_quote$net_annual_premium > 0)

# Check equivalence principle for term insurance
stopifnot(
  isTRUE(all.equal(
    term_quote$net_annual_premium *
      term_quote$premium_apv,
    term_quote$face_amount *
      term_quote$benefit_apv,
    tolerance = 1e-8
  ))
)

# Check equivalence principle for endowment insurance
stopifnot(
  isTRUE(all.equal(
    endowment_quote$net_annual_premium *
      endowment_quote$premium_apv,
    endowment_quote$face_amount *
      endowment_quote$benefit_apv,
    tolerance = 1e-8
  ))
)

