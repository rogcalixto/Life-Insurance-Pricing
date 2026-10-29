#Testing for Mortality Function 

#In range
get_qx(30, mortality)
#Out of range
get_qx(110, mortality)
get_qx(10, mortality)

#Testing Survival Function

#Valid Input
survival_prob(30, 5, mortality)
# Survival for zero years must equal 1
survival_prob(30, 0, mortality)
# Survival for one year must equal 1 - qx
survival_prob(30, 1, mortality)
1 - get_qx(30, mortality)
# Survival probabilities should decrease with duration
survival_prob(30, 5, mortality) <=
  survival_prob(30, 1, mortality)

#Testing for Discount factor function 

# Present value of $1 payable in one year at 5%
discount_factor(1, 0.05)
# Present value of $1 payable in ten years at 5%
discount_factor(10, 0.05)
# Present values at several durations
discount_factor(c(1, 5, 10, 20), 0.05)
#Testing Rate Sensitvity
rates <- c(0.02, 0.03, 0.04, 0.05)
data.frame(
  interest_rate = rates,
  pv_20_years = 1 / (1 + rates)^20
)

#Testing for Term Insurance Factor

term_apv <- term_insurance_apv(
  x = 30,
  n = 20,
  i = 0.05,
  mortality = mortality
)
term_apv
#Adding a death benefit amount 
face_amount <- 100000
term_benefit_apv <- face_amount * term_apv
term_benefit_apv

#More Testing for Term Insurance Factor

# APV should be positive
stopifnot(term_apv > 0)
# APV per $1 should not exceed 1
stopifnot(term_apv <= 1)
# A longer term should have an APV at least as large
# as a shorter term, under the same assumptions
term_10 <- term_insurance_apv(30, 10, 0.05, mortality)
term_20 <- term_insurance_apv(30, 20, 0.05, mortality)
stopifnot(term_20 >= term_10)
cat("10-year term APV:", term_10, "\n")
cat("20-year term APV:", term_20, "\n")

#Testing for Annuity Due Factor 

annuity_apv <- annuity_due_apv(
  x = 30,
  n = 20,
  i = 0.05,
  mortality = mortality
)
print(annuity_apv)
# Must be greater than zero but less than 20 for all payments
stopifnot(annuity_apv > 0)
stopifnot(annuity_apv <= 20)

#Testing for the Premium under EP 

annual_premium <- net_annual_term_premium(
  x = 30,
  n = 20,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality
)
print(annual_premium)
#Additional Tests for Premiums
stopifnot(annual_premium > 0)
benefit_value <- 100000 *
  term_insurance_apv(30, 20, 0.05, mortality)
premium_value <- annual_premium *
  annuity_due_apv(30, 20, 0.05, mortality)
stopifnot(
  isTRUE(all.equal(
    benefit_value,
    premium_value,
    tolerance = 1e-8
  ))
)

#Comparing published values to calculated values 

soa_annuity_20_age30 <- 13.0410
my_annuity_20_age30 <- annuity_due_apv(
  x = 30,
  n = 20,
  i = 0.05,
  mortality = mortality
)
# Allow a small difference from the benchmark
stopifnot(
  abs(my_annuity_20_age30 - soa_annuity_20_age30) < 0.001
)

#Testing for Partial Whole Life Insurance 

partial_whole_life_apv <- whole_life_insurance_apv(
  x = 30,
  i = 0.05,
  mortality = mortality
)
print(partial_whole_life_apv)
#Compare to the SOA values 
soa_whole_life_apv <- 0.07698
difference <- soa_whole_life_apv - partial_whole_life_apv
print(difference)
# The partial value should not exceed the full published value,
# allowing for small differences due to table rounding.
stopifnot(difference >= -0.00001)

#Testing Premium Function

premium_30 <- whole_life_net_premium(
  face_amount = 100000,
  insurance_apv = 0.07698,
  annuity_due_apv = 19.3834
)

print(premium_30)
stopifnot(premium_30 > 0)

#Testing Pure Endowment Insurance 

endowment_apv <- endowment_insurance_apv(
  x = 30,
  n = 20,
  i = 0.05,
  mortality = mortality
)
print(endowment_apv)
stopifnot(endowment_apv > 0)
stopifnot(endowment_apv <= 1)

#Testing the Premium for Endowment Insurance
endowment_premium <- endowment_net_premium(
  x = 30,
  n = 20,
  i = 0.05,
  face_amount = 100000,
  mortality = mortality
)

print(endowment_premium)

#Testing Reuseable Annuity Function 

whole_life_annuity_30 <- whole_life_annuity_due_apv(
  x = 30,
  annuity_due_apv_value = 19.3834
)
print(whole_life_annuity_30)
stopifnot(whole_life_annuity_30 > 0)

#Testing for Annuity Immediate 

annuity_immediate_20 <- annuity_immediate_apv(
  x = 30,
  n = 20,
  i = 0.05,
  mortality = mortality
)
print(annuity_immediate_20)
stopifnot(annuity_immediate_20 > 0)
stopifnot(annuity_immediate_20 < 20)


annuity_due_20 <- annuity_due_apv(
  x = 30,
  n = 20,
  i = 0.05,
  mortality = mortality
)

stopifnot(annuity_due_20 > annuity_immediate_20)

#Testing the Whole Life Annuity Immediate Function 

whole_life_annuity_immediate_30 <-
  whole_life_annuity_immediate_apv(
    x = 30,
    annuity_due_apv_value = 19.3834
  )

print(whole_life_annuity_immediate_30)

stopifnot(
  whole_life_annuity_immediate_30 > 0
)




