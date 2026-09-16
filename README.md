# Longevity Risk Analysis in Life Insurance — A Lee-Carter Approach

## Overview

Bachelor's thesis in Actuarial Science (Università degli Studi di Trieste, DEAMS), analyzing longevity risk in life insurance and its impact on the actuarial value of a life annuity portfolio.

The Lee-Carter mortality model is fitted to Italian population data and used to quantify how uncertainty in future mortality translates into uncertainty in annuity valuation.

## Methodology

- **Mortality modelling**: from static to dynamic (stochastic) mortality projection, motivating the use of GAPC-family models
- **Lee-Carter model**: Poisson-distributed deaths, log-mortality decomposition into age effect, age-period interaction, and time-varying mortality index, estimated by maximum likelihood under standard identifiability constraints
- **Forecasting and simulation**: the period index modelled as an ARIMA(0,1,0) process, with parameter uncertainty assessed via semiparametric bootstrap
- **Longevity risk decomposition**: process risk, parameter risk, and model risk
- **Numerical application**: the model is fitted to the Italian population (1969-2019, ages 0-90) and applied to a portfolio of life annuities (entry age 65, annual payment €100, 2.5% interest rate, 25-year duration), assessing:
  - the coefficient of variation of the portfolio value as a function of portfolio size
  - the distribution of the annuity value across simulated mortality scenarios
  - how the annuity valuation would have evolved if calibrated on successive historical periods (1984-2019)

## Repository structure

- `code/` – R project and scripts: the Lee-Carter model implementation, plotting utilities, and a set of year-by-year analyses (`yearly-analysis/`) used to assess how the valuation changes when the model is calibrated on data up to each of those years
- `report/` – thesis document and defense presentation

## Tools

- R, Latex

## Author

Milla Campajola — Supervisor: Prof. Mario Marino
