
# Load the pricing functions and mortality data
source("R/survival_functions.R")
source("R/insurance_pricing.R")
source("R/annuity_pricing.R")
source("R/pricing_engine.R")

library(readr)

mortality <- read_csv("data/sult.csv", show_col_types = FALSE)

# Set pricing assumptions
issue_age <- 30
interest_rate <- 0.05
face_amount <- 100000
term_years <- 20

# Price term insurance
term_quote <- price_insurance(
  product = "term",
  x = issue_age,
  n = term_years,
  i = interest_rate,
  face_amount = face_amount,
  mortality = mortality
)

# Price endowment insurance
endowment_quote <- price_insurance(
  product = "endowment",
  x = issue_age,
  n = term_years,
  i = interest_rate,
  face_amount = face_amount,
  mortality = mortality
)

# Price whole life insurance using supplied benchmark APVs
whole_life_quote <- price_insurance(
  product = "whole_life",
  x = issue_age,
  i = interest_rate,
  face_amount = face_amount,
  mortality = mortality,
  whole_life_Ax = 0.07698,
  whole_life_annuity_due = 19.3834
)

# Combine results into one comparison table
pricing_comparison <- rbind(
  term_quote,
  endowment_quote,
  whole_life_quote
)

# Display the results
pricing_comparison$net_annual_premium <- round(
  pricing_comparison$net_annual_premium, 2
)

print(pricing_comparison)


#Premium Comparision Chart

library(ggplot2)

# Create readable product labels
pricing_comparison$product_label <- c(
  "Term Life (20-Year)",
  "Endowment (20-Year)",
  "Whole Life"
)

# Create premium comparison chart
premium_chart <- ggplot(
  pricing_comparison,
  aes(
    x = reorder(product_label, net_annual_premium),
    y = net_annual_premium
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Annual Net Premium by Life Insurance Product",
    subtitle = "Issue age 30 | $100,000 benefit | 5% interest",
    x = NULL,
    y = "Annual Net Premium",
    caption = paste(
      "Whole life premium uses supplied benchmark APVs;",
      "all premiums exclude expenses and profit."
    )
  ) +
  theme_minimal()

print(premium_chart)


# Create the outputs folder if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

ggsave(
  filename = "outputs/premium_comparison.png",
  plot = premium_chart,
  width = 9,
  height = 5,
  dpi = 300
)

#Interest Rate Sensitivity Analysis

# Interest rate sensitivity analysis
interest_rates <- seq(0.01, 0.08, by = 0.01)

sensitivity_results <- data.frame()

for (rate in interest_rates) {

  quote <- price_insurance(
    product = "term",
    x = 30,
    n = 20,
    i = rate,
    face_amount = 100000,
    mortality = mortality
  )

  sensitivity_results <- rbind(
    sensitivity_results,
    data.frame(
      interest_rate = rate,
      annual_premium = quote$net_annual_premium
    )
  )
}

# Display results
print(sensitivity_results)


sensitivity_chart <- ggplot(
  sensitivity_results,
  aes(x = interest_rate, y = annual_premium)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Interest Rate Sensitivity: 20-Year Term Life",
    subtitle = "Age 30 | $100,000 death benefit",
    x = "Annual Effective Interest Rate",
    y = "Annual Net Premium"
  ) +
  theme_minimal()

print(sensitivity_chart)

ggsave(
  "outputs/interest_rate_sensitivity.png",
  plot = sensitivity_chart,
  width = 9,
  height = 5,
  dpi = 300
)

#Issue Age Analysis

# Issue age sensitivity analysis
issue_ages <- seq(20, 60, by = 5)

age_results <- data.frame()

for (age in issue_ages) {

  quote <- price_insurance(
    product = "term",
    x = age,
    n = 20,
    i = 0.05,
    face_amount = 100000,
    mortality = mortality
  )

  age_results <- rbind(
    age_results,
    data.frame(
      issue_age = age,
      annual_premium = quote$net_annual_premium
    )
  )
}

print(age_results)


age_chart <- ggplot(
  age_results,
  aes(x = issue_age, y = annual_premium)
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  scale_x_continuous(
    breaks = issue_ages
  ) +
  labs(
    title = "Effect of Issue Age on Term Life Premiums",
    subtitle = "20-year term | $100,000 benefit | 5% interest",
    x = "Issue Age",
    y = "Annual Net Premium"
  ) +
  theme_minimal()

print(age_chart)

ggsave(
  "outputs/issue_age_sensitivity.png",
  plot = age_chart,
  width = 9,
  height = 5,
  dpi = 300
)

