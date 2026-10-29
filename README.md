# Life Insurance Pricing and Sensitivity Analysis in R

## Overview

This project develops a life insurance pricing engine in R to calculate net annual premiums for term life, endowment, and whole life insurance. It applies actuarial present value calculations, mortality assumptions, and discounting to evaluate insurance benefits and premium payments.

The project demonstrates how actuarial mathematics and programming can support insurance pricing and financial analysis.

## Objectives

- Calculate net annual premiums for multiple life insurance products.
- Implement survival probabilities and actuarial present value functions.
- Analyze how interest rates and issue ages affect insurance premiums.
- Visualize pricing results using R.
- Validate calculations using automated tests and actuarial identities.

## Products

| Product | Description |
|---|---|
| Term life | Death benefit payable if death occurs during the specified term. |
| Endowment insurance | Death benefit during the term or a survival benefit at the end of the term. |
| Whole life | Lifetime death benefit, priced using supplied benchmark actuarial present values. |

## Methodology

The pricing engine applies the actuarial equivalence principle:

\[
P = \frac{B A}{\ddot{a}}
\]

where:

- \(P\) = annual net premium
- \(B\) = face amount
- \(A\) = actuarial present value of the insurance benefit per unit of coverage
- \(\ddot{a}\) = actuarial present value of the premium-paying annuity-due per unit of annual premium

The model uses annual effective interest rates, annual premium payments in advance while the insured is alive, and death benefits payable at the end of the year of death.

## Analysis

The project includes:

- **Product comparison:** Compares annual net premiums for different insurance products.
- **Interest rate sensitivity:** Examines the effect of interest rate assumptions on term insurance premiums.
- **Issue age sensitivity:** Examines how premiums vary by issue age.

Charts are saved in the `outputs/` directory.

## Tools

- R
- readr
- dplyr
- ggplot2
- Standard Ultimate Life Table mortality data

## Validation

Automated tests check premium positivity, expected actuarial present value bounds, relationships between insurance products, annuity timing, and the equivalence principle.

These checks establish internal consistency, but they do not independently verify every input or actuarial assumption.

## Assumptions and Limitations

- Single-life model with annual cash flows.
- Net premiums exclude expenses, commissions, profit margins, and other loadings.
- Mortality calculations are limited to the ages covered by the supplied data.
- Whole life quotes use supplied benchmark actuarial present values rather than a complete whole life calculation from the truncated mortality table.
- Benchmark values should be verified against authoritative sources before being treated as reference values.

This is an educational portfolio project, not a production pricing model.

## Project Structure

```text
life-insurance-pricing/
├── R/
├── data/
├── tests/
├── outputs/
├── life_pricing_analysis.R
├── setup_folders.R
├── README.md
└── life-insurance-pricing.Rproj
```
