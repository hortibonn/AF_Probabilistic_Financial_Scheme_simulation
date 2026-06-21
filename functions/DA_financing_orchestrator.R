# ------------------------------------------------------------
# Scenario orchestrator
# ------------------------------------------------------------
# This script controls the order in which optional modules are applied.
# UI/server logic stays in app.R. app.R should collect user selections and pass
# them here as simple lists.
#
# Calculation order:
#   1. Baseline result from DA_baseline.R
#   2. Optional financing from DA_A_financing.R
#   3. Optional derisking from DA_B_derisking.R
#   4. Optional advisory support from DA_C_advisory.R
#   5. Optional market/value-chain access from DA_E_marketVC.R
#
# Each module receives the latest result as base_result/current_result and must
# return the standard fields needed by the next module:
#   AF_total_investment_cost, AF_total_running_cost, AF_total_benefit,
#   AF_total_cost, AF_bottom_line_benefit, AF_NPV, AF_cash_flow,
#   AF_cum_cash_flow, NPV_decision, CF_decision, CumCF_decision.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

get_input <- function(input_list, name, default = NULL) {
  input_list[[name]] %||% default
}

source_decision_modules <- function(functions_dir) {
  source(file.path(functions_dir, "DA_A_financing.R"), local = FALSE)
  source(file.path(functions_dir, "DA_B_derisking.R"), local = FALSE)
  source(file.path(functions_dir, "DA_C_advisory.R"), local = FALSE)
  source(file.path(functions_dir, "DA_E_marketVC.R"), local = FALSE)
}

select_baseline_variant <- function(
    baseline_result,
    use_subsidies = FALSE
) {
  result <- baseline_result

  if (isTRUE(use_subsidies)) {
    result$AF_NPV <- baseline_result$AF_NPV_subs %||% baseline_result$AF_NPV
    result$AF_cash_flow <- baseline_result$AF_cash_flow_subs %||%
      baseline_result$AF_cash_flow
    result$AF_cum_cash_flow <- baseline_result$AF_cum_cash_flow_subs %||%
      baseline_result$AF_cum_cash_flow

    result$AF_total_investment_cost <-
      baseline_result$AF_total_investment_cost_subs %||%
      baseline_result$AF_total_investment_cost
    result$AF_total_running_cost <-
      baseline_result$AF_total_running_cost_subs %||%
      baseline_result$AF_total_running_cost
    result$AF_total_benefit <-
      baseline_result$AF_total_benefit_subs %||%
      baseline_result$AF_total_benefit
    result$AF_total_cost <-
      baseline_result$AF_total_cost_subs %||% baseline_result$AF_total_cost
    result$AF_bottom_line_benefit <-
      baseline_result$AF_bottom_line_benefit_subs %||%
      baseline_result$AF_bottom_line_benefit

    result$scenario_type <- "baseline_with_subsidies"
  } else {
    result$scenario_type <- "baseline"
  }

  result$has_financing <- FALSE
  result$loan_draw <- 0
  result$repayment_vector <- rep(0, length(result$AF_cash_flow))
  result
}

apply_financing_step <- function(
    current_result,
    baseline_result,
    financing_inputs,
    n_years_c,
    discount_rate_p,
    use_subsidies = FALSE
) {
  financing_type <- get_input(financing_inputs, "financing_type", "none")

  if (is.null(financing_type) || financing_type == "none") {
    return(list(
      result = current_result,
      financing_scenarios = NULL,
      applied_option = NULL
    ))
  }

  financing_scenarios <- calculate_financing_scenarios(
    baseline_result = baseline_result,
    use_subsidies = use_subsidies,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p,
    farmer_own_capital_c = get_input(financing_inputs, "farmer_own_capital_c", 0),

    commercial_loan_amount_c = get_input(financing_inputs, "commercial_loan_amount_c", 0),
    commercial_annual_repayment_amount_p =
      get_input(financing_inputs, "commercial_annual_repayment_amount_p", 0),
    commercial_interest_rate_c = get_input(financing_inputs, "commercial_interest_rate_c", 0),
    commercial_repayment_start_year_c =
      get_input(financing_inputs, "commercial_repayment_start_year_c", 1),
    commercial_maturity_year_c = get_input(financing_inputs, "commercial_maturity_year_c", n_years_c),

    Dev_bank_loan_amount_c = get_input(financing_inputs, "Dev_bank_loan_amount_c", 0),
    Dev_bank_annual_repayment_amount_p =
      get_input(financing_inputs, "Dev_bank_annual_repayment_amount_p", 0),
    Dev_bank_interest_rate_c = get_input(financing_inputs, "Dev_bank_interest_rate_c", 0),
    Dev_bank_repayment_start_year_c =
      get_input(financing_inputs, "Dev_bank_repayment_start_year_c", 1),
    Dev_bank_maturity_year_c = get_input(financing_inputs, "Dev_bank_maturity_year_c", n_years_c),

    impact_invst_fund_loan_c = get_input(financing_inputs, "impact_invst_fund_loan_c", 0),
    impact_invst_annual_repayment_amount_p =
      get_input(financing_inputs, "impact_invst_annual_repayment_amount_p", 0),
    impact_invst_fund_interest_rate_c =
      get_input(financing_inputs, "impact_invst_fund_interest_rate_c", 0),
    impact_invst_fund_repayment_start_year_c =
      get_input(financing_inputs, "impact_invst_fund_repayment_start_year_c", 1),
    impact_invst_bank_maturity_year_c =
      get_input(financing_inputs, "impact_invst_bank_maturity_year_c", n_years_c)
  )

  selected_result <- switch(
    financing_type,
    commercial_loan = financing_scenarios$commercial_loan,
    development_bank_loan = financing_scenarios$development_bank_loan,
    impact_investment_loan = financing_scenarios$impact_investment_loan,
    stop("Unknown financing_type: ", financing_type)
  )

  selected_result$scenario_type <- financing_type

  list(
    result = selected_result,
    financing_scenarios = financing_scenarios,
    applied_option = financing_type
  )
}

apply_derisking_step <- function(
    current_result,
    baseline_result,
    derisking_inputs,
    n_years_c,
    discount_rate_p
) {
  use_guarantee <- isTRUE(get_input(derisking_inputs, "use_guarantee", FALSE))
  use_insurance <- isTRUE(get_input(derisking_inputs, "use_insurance", FALSE))

  if (!use_guarantee && !use_insurance) {
    return(list(result = current_result, applied_option = NULL))
  }

  result <- calculate_derisking_mechanisms(
    base_result = current_result,
    Treeless_bottom_line_benefit = baseline_result$Treeless_bottom_line_benefit,
    Apple_yield_reduction_due_to_weather = get_input(
      list(x = baseline_result$Apple_yield_reduction_due_to_weather),
      "x",
      rep(0, n_years_c)
    ),
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p,

    use_guarantee = use_guarantee,
    use_insurance = use_insurance,

    guarantee_cover_rate_c = get_input(derisking_inputs, "guarantee_cover_rate_c", 0),
    guarantee_default_loss_rate_c =
      get_input(derisking_inputs, "guarantee_default_loss_rate_c", 100),
    guarantee_fee_rate_c = get_input(derisking_inputs, "guarantee_fee_rate_c", 0),
    guarantee_fee_paid_by_farmer =
      isTRUE(get_input(derisking_inputs, "guarantee_fee_paid_by_farmer", FALSE)),

    insurance_cover_rate_c = get_input(derisking_inputs, "insurance_cover_rate_c", 0),
    insurance_payout_amount_c = get_input(derisking_inputs, "insurance_payout_amount_c", 0),
    insurance_annual_premium_c =
      get_input(derisking_inputs, "insurance_annual_premium_c", 0),
    insurance_annual_premium_surcharge_c =
      get_input(derisking_inputs, "insurance_annual_premium_surcharge_c", 0)
  )

  result$scenario_type <- "derisked"

  list(
    result = result,
    applied_option = paste(
      c(if (use_guarantee) "guarantee_fund", if (use_insurance) "insurance"),
      collapse = "+"
    )
  )
}

apply_advisory_step <- function(
    current_result,
    baseline_result,
    advisory_inputs,
    n_years_c,
    discount_rate_p
) {
  use_advisory <- isTRUE(get_input(advisory_inputs, "use_advisory", FALSE))

  if (!use_advisory) {
    return(list(result = current_result, applied_option = NULL))
  }

  result <- calculate_advisory_support(
    base_result = current_result,
    Treeless_bottom_line_benefit = baseline_result$Treeless_bottom_line_benefit,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p,

    use_advisory = use_advisory,
    advisory_support_types = get_input(advisory_inputs, "advisory_support_types", character(0)),

    consultation_cost = get_input(
      advisory_inputs,
      "consultation_cost",
      baseline_result$consultation_cost %||% rep(0, n_years_c)
    ),
    machinery_cost = get_input(
      advisory_inputs,
      "machinery_cost",
      baseline_result$machinery_cost %||% rep(0, n_years_c)
    ),
    labour_cost = get_input(
      advisory_inputs,
      "labour_cost",
      baseline_result$labour_cost %||% rep(0, n_years_c)
    ),
    digital_tool_subscription = get_input(
      advisory_inputs,
      "digital_tool_subscription",
      baseline_result$digital_tool_subscription %||% rep(0, n_years_c)
    ),

    organisation_nominal_fee_c = get_input(advisory_inputs, "organisation_nominal_fee_c", 0),
    organisation_consultation_reduction_perc_c =
      get_input(advisory_inputs, "organisation_consultation_reduction_perc_c", 0),

    cooperative_machinery_reduction_perc_c =
      get_input(advisory_inputs, "cooperative_machinery_reduction_perc_c", 0),
    cooperative_labour_reduction_perc_c =
      get_input(advisory_inputs, "cooperative_labour_reduction_perc_c", 0),
    cooperative_nominal_fee_c = get_input(advisory_inputs, "cooperative_nominal_fee_c", 0),

    digital_tool_subscription_discount_amount_c =
      get_input(advisory_inputs, "digital_tool_subscription_discount_amount_c", 0)
  )

  result$scenario_type <- "advisory_adjusted"

  list(result = result, applied_option = "advisory_support")
}

apply_market_step <- function(
    current_result,
    baseline_result,
    market_inputs,
    n_years_c,
    discount_rate_p
) {
  use_market_access <- isTRUE(get_input(market_inputs, "use_market_access", FALSE))

  if (!use_market_access) {
    return(list(result = current_result, applied_option = NULL))
  }

  result <- calculate_market_value_chain_access(
    base_result = current_result,
    Treeless_bottom_line_benefit = baseline_result$Treeless_bottom_line_benefit,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p,

    use_market_access = use_market_access,
    market_access_types = get_input(market_inputs, "market_access_types", character(0)),

    Table_apple_yield = get_input(market_inputs, "Table_apple_yield", baseline_result$Table_apple_yield),
    B_qual_table_apple_yield =
      get_input(market_inputs, "B_qual_table_apple_yield", baseline_result$B_qual_table_apple_yield),
    Juice_apple_yield = get_input(market_inputs, "Juice_apple_yield", baseline_result$Juice_apple_yield),
    AF_maize_yield = get_input(market_inputs, "AF_maize_yield", baseline_result$AF_maize_yield),
    AF_wheat_yield = get_input(market_inputs, "AF_wheat_yield", baseline_result$AF_wheat_yield),
    AF_barley_yield = get_input(market_inputs, "AF_barley_yield", baseline_result$AF_barley_yield),
    AF_rapeseed_yield = get_input(market_inputs, "AF_rapeseed_yield", baseline_result$AF_rapeseed_yield),

    table_apple_market_price =
      get_input(market_inputs, "table_apple_market_price", baseline_result$table_apple_price_market),
    bqual_apple_market_price =
      get_input(market_inputs, "bqual_apple_market_price", baseline_result$B_qual_apple_price_market),
    juice_apple_market_price =
      get_input(market_inputs, "juice_apple_market_price", baseline_result$Juice_apple_price_market),
    maize_market_price = get_input(market_inputs, "maize_market_price", baseline_result$maize_value_p),
    wheat_market_price = get_input(market_inputs, "wheat_market_price", baseline_result$wheat_value_p),
    barley_market_price = get_input(market_inputs, "barley_market_price", baseline_result$barley_value_p),
    rapeseed_market_price = get_input(market_inputs, "rapeseed_market_price", baseline_result$rapeseed_value_p),

    price_guarantee_type = get_input(market_inputs, "price_guarantee_type", "floor"),
    price_guarantee_share_c = get_input(market_inputs, "price_guarantee_share_c", 0),
    table_apple_guaranteed_price_c =
      get_input(market_inputs, "table_apple_guaranteed_price_c", 0),
    bqual_apple_guaranteed_price_c =
      get_input(market_inputs, "bqual_apple_guaranteed_price_c", 0),
    juice_apple_guaranteed_price_c =
      get_input(market_inputs, "juice_apple_guaranteed_price_c", 0),
    maize_guaranteed_price_c = get_input(market_inputs, "maize_guaranteed_price_c", 0),
    wheat_guaranteed_price_c = get_input(market_inputs, "wheat_guaranteed_price_c", 0),
    barley_guaranteed_price_c = get_input(market_inputs, "barley_guaranteed_price_c", 0),
    rapeseed_guaranteed_price_c = get_input(market_inputs, "rapeseed_guaranteed_price_c", 0),

    price_premium_share_c = get_input(market_inputs, "price_premium_share_c", 0),
    table_apple_price_premium_c = get_input(market_inputs, "table_apple_price_premium_c", 0),
    bqual_apple_price_premium_c = get_input(market_inputs, "bqual_apple_price_premium_c", 0),
    juice_apple_price_premium_c = get_input(market_inputs, "juice_apple_price_premium_c", 0),
    maize_price_premium_c = get_input(market_inputs, "maize_price_premium_c", 0),
    wheat_price_premium_c = get_input(market_inputs, "wheat_price_premium_c", 0),
    barley_price_premium_c = get_input(market_inputs, "barley_price_premium_c", 0),
    rapeseed_price_premium_c = get_input(market_inputs, "rapeseed_price_premium_c", 0)
  )

  result$scenario_type <- "market_adjusted"

  list(result = result, applied_option = "market_value_chain_access")
}

run_selected_scenario <- function(
    baseline_result,
    n_years_c,
    discount_rate_p,
    use_subsidies = FALSE,
    financing_inputs = list(financing_type = "none"),
    derisking_inputs = list(use_guarantee = FALSE, use_insurance = FALSE),
    advisory_inputs = list(use_advisory = FALSE),
    market_inputs = list(use_market_access = FALSE)
) {
  current_result <- select_baseline_variant(
    baseline_result = baseline_result,
    use_subsidies = use_subsidies
  )

  applied_options <- character(0)

  financing_step <- apply_financing_step(
    current_result = current_result,
    baseline_result = baseline_result,
    financing_inputs = financing_inputs,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p,
    use_subsidies = use_subsidies
  )
  current_result <- financing_step$result
  if (!is.null(financing_step$applied_option)) {
    applied_options <- c(applied_options, financing_step$applied_option)
  }

  derisking_step <- apply_derisking_step(
    current_result = current_result,
    baseline_result = baseline_result,
    derisking_inputs = derisking_inputs,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p
  )
  current_result <- derisking_step$result
  if (!is.null(derisking_step$applied_option) && nzchar(derisking_step$applied_option)) {
    applied_options <- c(applied_options, derisking_step$applied_option)
  }

  advisory_step <- apply_advisory_step(
    current_result = current_result,
    baseline_result = baseline_result,
    advisory_inputs = advisory_inputs,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p
  )
  current_result <- advisory_step$result
  if (!is.null(advisory_step$applied_option)) {
    applied_options <- c(applied_options, advisory_step$applied_option)
  }

  market_step <- apply_market_step(
    current_result = current_result,
    baseline_result = baseline_result,
    market_inputs = market_inputs,
    n_years_c = n_years_c,
    discount_rate_p = discount_rate_p
  )
  current_result <- market_step$result
  if (!is.null(market_step$applied_option)) {
    applied_options <- c(applied_options, market_step$applied_option)
  }

  current_result$scenario_type <- if (length(applied_options) == 0) {
    current_result$scenario_type
  } else {
    "combined_scenario"
  }
  current_result$applied_options <- applied_options
  current_result$financing_scenarios <- financing_step$financing_scenarios

  current_result
}

