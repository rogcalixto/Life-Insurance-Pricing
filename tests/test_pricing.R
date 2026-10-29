
# Automated tests for the life insurance pricing engine

# Load required functions and mortality data
source("R/survival_functions.R")
source("R/insurance_pricing.R")
source("R/annuity_pricing.R")
source("R/pricing_engine.R")

library(readr)

mortality <- read_csv("data/sult.csv", show_col_types = FALSE)

# Create sample quotes
term_quote <- price_insurance(
  product = "term",
  x = 30,
  n = 20,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality
)

endowment_quote <- price_insurance(
  product = "endowment",
  x = 30,
  n = 20,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality
)

whole_life_quote <- price_insurance(
  product = "whole_life",
  x = 30,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality,
  whole_life_Ax = 0.07698,
  whole_life_annuity_due = 19.3834
)

# Test 1: Premiums must be positive
stopifnot(term_quote$net_annual_premium > 0)
stopifnot(endowment_quote$net_annual_premium > 0)
stopifnot(whole_life_quote$net_annual_premium > 0)

# Test 2: Benefit APVs must be between 0 and 1
stopifnot(term_quote$benefit_apv > 0)
stopifnot(term_quote$benefit_apv <= 1)
stopifnot(endowment_quote$benefit_apv > 0)
stopifnot(endowment_quote$benefit_apv <= 1)

# Test 3: Endowment includes a survival benefit
stopifnot(
  endowment_quote$benefit_apv >
    term_quote$benefit_apv
)

# Test 4: Longer term should have a higher or equal
# term insurance APV for the same issue age and rate
term_10 <- term_insurance_apv(30, 10, 0.05, mortality)
term_20 <- term_insurance_apv(30, 20, 0.05, mortality)

stopifnot(term_20 >= term_10)

# Test 5: The net premium must satisfy the
# equivalence principle: premium APV = benefit APV
stopifnot(
  isTRUE(all.equal(
    term_quote$net_annual_premium *
      term_quote$premium_apv,
    term_quote$face_amount *
      term_quote$benefit_apv,
    tolerance = 1e-8
  ))
)

stopifnot(
  isTRUE(all.equal(
    endowment_quote$net_annual_premium *
      endowment_quote$premium_apv,
    endowment_quote$face_amount *
      endowment_quote$benefit_apv,
    tolerance = 1e-8
  ))
)

# Test 6: Annuity-due APV should exceed
# annuity-immediate APV for the same term
ann_due <- annuity_due_apv(30, 20, 0.05, mortality)
ann_immediate <- annuity_immediate_apv(
  30, 20, 0.05, mortality
)

stopifnot(ann_due > ann_immediate)

message("All pricing validation tests passed!")
