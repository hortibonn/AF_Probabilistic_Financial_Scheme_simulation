# Decision Model used to compute the Results in:
#"Agroforestry adoption in Germany: using decision analysis to explore the impact of funding mechanisms on system profitability"

# Packages needed ####

#install.packages("decisionSupport")
# library(decisionSupport)

# function
# ------------------------------------------------------------
# Helper: calculate discounted outputs from a bottom-line vector
# ----------------------------------------------------------
# ------------------------------------------------------------
# Helper: calculate discounted outputs from a bottom-line vector
# ------------------------------------------------------------

calc_discount_outputs <- function(
    bottom_line_benefit,
    treeless_bottom_line_benefit,
    discount_rate_p
) {
  AF_NPV <- discount( bottom_line_benefit, discount_rate = discount_rate_p, calculate_NPV = TRUE)
  AF_cash_flow <- discount(bottom_line_benefit,discount_rate = discount_rate_p,calculate_NPV = FALSE )
  AF_cum_cash_flow <- cumsum(AF_cash_flow)
  
  Decision_benefit <- bottom_line_benefit - treeless_bottom_line_benefit
  NPV_decision <- discount( Decision_benefit, discount_rate = discount_rate_p, calculate_NPV = TRUE)
  CF_decision <- discount( Decision_benefit, discount_rate = discount_rate_p, calculate_NPV = FALSE )
  CumCF_decision <- cumsum(CF_decision)
  
  list(
    AF_NPV = AF_NPV,
    AF_cash_flow = AF_cash_flow,
    AF_cum_cash_flow = AF_cum_cash_flow,
    Decision_benefit = Decision_benefit,
    NPV_decision = NPV_decision,
    CF_decision = CF_decision,
    CumCF_decision = CumCF_decision
  )
}


calc_repayment_vector <- function(
    loan_draw,
    annual_repayment_amount_p,
    interest_rate_c,
    repayment_start_year_c,
    maturity_year_c,
    n_years_c
) {
  repayment_vector <- rep(0, n_years_c)
  
  if (loan_draw <= 0) {
    return(repayment_vector)
  }
  
  repayment_end_year <- min(maturity_year_c, n_years_c)
  repayment_start_year <- max(repayment_start_year_c, 1)
  
  if (repayment_start_year > repayment_end_year) {
    return(repayment_vector)
  }
  
  annual_repayment_amount_adj <- cummax( vv( annual_repayment_amount_p, 10, repayment_end_year, relative_trend = 10))
  
  payoff_function <- function(scale_factor) {
    balance <- loan_draw
    
    for (year in seq_len(repayment_end_year)) {
      repayment <- if (year >= repayment_start_year) {
        scale_factor * annual_repayment_amount_adj[year]
      } else {
        0
      }
      
      balance <- balance * (1 + interest_rate_c) - repayment
    }
    
    balance
  }
  
  lower_bound <- 0
  upper_bound <- max(loan_draw, 1e-6)
  
  while (payoff_function(upper_bound) > 0) {
    upper_bound <- upper_bound * 2
  }
  
  repayment_years <- repayment_end_year - repayment_start_year + 1
  
  scaling_factor <- uniroot( payoff_function,lower = lower_bound, upper = upper_bound * (1 + interest_rate_c)^repayment_years)$root
  
  repayment_vector[repayment_start_year:repayment_end_year] <-
    scaling_factor *
    annual_repayment_amount_adj[repayment_start_year:repayment_end_year]
  
  repayment_vector
}


calc_loan_scenario <- function(
    AF_total_investment_cost,
    AF_total_running_cost,
    AF_total_benefit,
    Treeless_bottom_line_benefit,
    discount_rate_p,
    loan_amount_c,
    annual_repayment_amount_p,
    interest_rate_c,
    repayment_start_year_c,
    maturity_year_c,
    n_years_c,
    farmer_own_capital_c = 0,
    eligible_investment_cost = NULL
) {
  if (is.null(eligible_investment_cost)) {
    eligible_investment_cost <- AF_total_investment_cost[1]
  }
  
  financing_gap <- max(eligible_investment_cost - farmer_own_capital_c, 0)
  
  loan_draw <- min(loan_amount_c, financing_gap, eligible_investment_cost)
  
  repayment_vector <- calc_repayment_vector(
    loan_draw = loan_draw,
    annual_repayment_amount_p = annual_repayment_amount_p,
    interest_rate_c = interest_rate_c,
    repayment_start_year_c = repayment_start_year_c,
    maturity_year_c = maturity_year_c,
    n_years_c = n_years_c
  )
  
  AF_total_investment_cost_adj <- AF_total_investment_cost
  AF_total_investment_cost_adj[1] <- max( AF_total_investment_cost_adj[1] - loan_draw, 0)
  
  AF_total_running_cost_adj <- AF_total_running_cost + repayment_vector
  AF_total_cost_adj <- AF_total_investment_cost_adj + AF_total_running_cost_adj
  
  AF_bottom_line_benefit_adj <- AF_total_benefit - AF_total_cost_adj
  
  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )
  
  list(
    has_financing = loan_draw > 0,
    loan_draw = loan_draw,
    loan_amount_requested = loan_amount_c,
    financing_gap = financing_gap,
    repayment_vector = repayment_vector,
    
    AF_total_investment_cost = AF_total_investment_cost_adj,
    AF_total_running_cost = AF_total_running_cost_adj,
    AF_total_benefit = AF_total_benefit,
    AF_total_cost = AF_total_cost_adj,
    AF_bottom_line_benefit = AF_bottom_line_benefit_adj,
    
    AF_NPV = discounted$AF_NPV,
    AF_cash_flow = discounted$AF_cash_flow,
    AF_cum_cash_flow = discounted$AF_cum_cash_flow,
    
    Decision_benefit = discounted$Decision_benefit,
    NPV_decision = discounted$NPV_decision,
    CF_decision = discounted$CF_decision,
    CumCF_decision = discounted$CumCF_decision
  )
}


calculate_financing_scenarios <- function(
    baseline_result,
    use_subsidies = FALSE,
    n_years_c,
    discount_rate_p,
    farmer_own_capital_c = 0,
    
    commercial_loan_amount_c,
    commercial_annual_repayment_amount_p,
    commercial_interest_rate_c,
    commercial_repayment_start_year_c,
    commercial_maturity_year_c,
    
    Dev_bank_loan_amount_c,
    Dev_bank_annual_repayment_amount_p,
    Dev_bank_interest_rate_c,
    Dev_bank_repayment_start_year_c,
    Dev_bank_maturity_year_c,
    
    impact_invst_fund_loan_c,
    impact_invst_annual_repayment_amount_p,
    impact_invst_fund_interest_rate_c,
    impact_invst_fund_repayment_start_year_c,
    impact_invst_bank_maturity_year_c
) {
  if (isTRUE(use_subsidies)) {
    AF_total_investment_cost <- baseline_result$AF_total_investment_cost_subs
    AF_total_running_cost <- baseline_result$AF_total_running_cost_subs
    AF_total_benefit <- baseline_result$AF_total_benefit_subs
  } else {
    AF_total_investment_cost <- baseline_result$AF_total_investment_cost
    AF_total_running_cost <- baseline_result$AF_total_running_cost
    AF_total_benefit <- baseline_result$AF_total_benefit
  }
  
  commercial <- calc_loan_scenario(
    AF_total_investment_cost = AF_total_investment_cost,
    AF_total_running_cost = AF_total_running_cost,
    AF_total_benefit = AF_total_benefit,
    Treeless_bottom_line_benefit = baseline_result$Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p,
    loan_amount_c = commercial_loan_amount_c,
    annual_repayment_amount_p = commercial_annual_repayment_amount_p,
    interest_rate_c = commercial_interest_rate_c,
    repayment_start_year_c = commercial_repayment_start_year_c,
    maturity_year_c = commercial_maturity_year_c,
    n_years_c = n_years_c,
    farmer_own_capital_c = farmer_own_capital_c
  )
  
  development_bank <- calc_loan_scenario(
    AF_total_investment_cost = AF_total_investment_cost,
    AF_total_running_cost = AF_total_running_cost,
    AF_total_benefit = AF_total_benefit,
    Treeless_bottom_line_benefit = baseline_result$Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p,
    loan_amount_c = Dev_bank_loan_amount_c,
    annual_repayment_amount_p = Dev_bank_annual_repayment_amount_p,
    interest_rate_c = Dev_bank_interest_rate_c,
    repayment_start_year_c = Dev_bank_repayment_start_year_c,
    maturity_year_c = Dev_bank_maturity_year_c,
    n_years_c = n_years_c,
    farmer_own_capital_c = farmer_own_capital_c
  )
  
  impact_investment <- calc_loan_scenario(
    AF_total_investment_cost = AF_total_investment_cost,
    AF_total_running_cost = AF_total_running_cost,
    AF_total_benefit = AF_total_benefit,
    Treeless_bottom_line_benefit = baseline_result$Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p,
    loan_amount_c = impact_invst_fund_loan_c,
    annual_repayment_amount_p = impact_invst_annual_repayment_amount_p,
    interest_rate_c = impact_invst_fund_interest_rate_c,
    repayment_start_year_c = impact_invst_fund_repayment_start_year_c,
    maturity_year_c = impact_invst_bank_maturity_year_c,
    n_years_c = n_years_c,
    farmer_own_capital_c = farmer_own_capital_c
  )
  
  list(
    commercial_loan = commercial,
    development_bank_loan = development_bank,
    impact_investment_loan = impact_investment
  )
}

