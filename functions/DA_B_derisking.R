# ------------------------------------------------------------
# Derisking mechanisms: guarantee fund and insurance
# ------------------------------------------------------------
# Guarantee fund:
#   A third party covers a predefined share of lender losses if the farmer
#   defaults. This protects the lender and fills the collateral gap. It only
#   applies when a financing scenario with a loan exists.
#
# Insurance:
#   The farmer pays annual premiums. If a predefined risk occurs, such as
#   drought, extreme weather, yield loss, or price/revenue shock, the farmer
#   receives a payout. Insurance can be selected with or without financing.

calc_discount_outputs <- function(
    bottom_line_benefit,
    treeless_bottom_line_benefit,
    discount_rate_p
) {
  AF_NPV <- discount(
    bottom_line_benefit,
    discount_rate = discount_rate_p,
    calculate_NPV = TRUE
  )

  AF_cash_flow <- discount(
    bottom_line_benefit,
    discount_rate = discount_rate_p,
    calculate_NPV = FALSE
  )

  AF_cum_cash_flow <- cumsum(AF_cash_flow)

  Decision_benefit <- bottom_line_benefit - treeless_bottom_line_benefit

  NPV_decision <- discount(
    Decision_benefit,
    discount_rate = discount_rate_p,
    calculate_NPV = TRUE
  )

  CF_decision <- discount(
    Decision_benefit,
    discount_rate = discount_rate_p,
    calculate_NPV = FALSE
  )

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

calculate_derisking_mechanisms <- function(
    base_result,
    Treeless_bottom_line_benefit,
    Apple_yield_reduction_due_to_weather,
    n_years_c,
    discount_rate_p,

    use_guarantee = FALSE,
    use_insurance = FALSE,

    guarantee_cover_rate_c = 0,
    guarantee_default_loss_rate_c = 100,
    guarantee_fee_rate_c = 0,
    guarantee_fee_paid_by_farmer = FALSE,

    insurance_cover_rate_c = 0,
    insurance_payout_amount_c = 0,
    insurance_annual_premium_c = 0,
    insurance_annual_premium_surcharge_c = 0
) {

  AF_total_investment_cost_adj_derisk <- base_result$AF_total_investment_cost
  AF_total_running_cost_adj_derisk <- base_result$AF_total_running_cost
  AF_total_benefit_adj_derisk <- base_result$AF_total_benefit

  loan_draw <- if (!is.null(base_result$loan_draw)) base_result$loan_draw else 0
  has_financing <- if (!is.null(base_result$has_financing)) {
    isTRUE(base_result$has_financing)
  } else {
    !is.na(loan_draw) && loan_draw > 0
  }

  repayment_vector <- if (!is.null(base_result$repayment_vector)) {
    base_result$repayment_vector
  } else {
    rep(0, n_years_c)
  }

  guarantee_applied <- isTRUE(use_guarantee) && isTRUE(has_financing)
  guarantee_requested_but_no_loan <- isTRUE(use_guarantee) && !isTRUE(has_financing)

  guarantee_message <- if (isTRUE(guarantee_requested_but_no_loan)) {
    "Guarantee fund was selected but not applied because no loan/financing scenario exists. Insurance can still be applied without financing."
  } else if (isTRUE(guarantee_applied)) {
    "Guarantee fund applied as lender-side default-risk protection. It does not reduce farmer investment cost."
  } else {
    "Guarantee fund not selected."
  }

  # Guarantee fund ---------------------------------------------------------

  guarantee_cover_rate <- pmin(pmax(guarantee_cover_rate_c / 100, 0), 1)
  guarantee_default_loss_rate <- pmin(pmax(guarantee_default_loss_rate_c / 100, 0), 1)
  guarantee_fee_rate <- pmin(pmax(guarantee_fee_rate_c / 100, 0), 1)

  default_loss_exposure <- rep(0, n_years_c)
  guarantee_payout_to_lender <- rep(0, n_years_c)
  lender_uncovered_loss <- rep(0, n_years_c)
  guarantee_fee_vector <- rep(0, n_years_c)
  lender_loss_protection <- 0
  collateral_equivalent <- 0

  if (isTRUE(guarantee_applied)) {

    collateral_equivalent <- loan_draw * guarantee_cover_rate

    remaining_repayment_obligation <- rev(cumsum(rev(repayment_vector)))
    default_loss_exposure <- remaining_repayment_obligation * guarantee_default_loss_rate

    guarantee_payout_to_lender <- default_loss_exposure * guarantee_cover_rate
    lender_uncovered_loss <- default_loss_exposure - guarantee_payout_to_lender
    lender_loss_protection <- sum(guarantee_payout_to_lender)

    guarantee_fee_vector[1:n_years_c] <- loan_draw * guarantee_fee_rate

    if (isTRUE(guarantee_fee_paid_by_farmer)) {
      AF_total_running_cost_adj_derisk <-
        AF_total_running_cost_adj_derisk + guarantee_fee_vector
    }
  }

  # Insurance --------------------------------------------------------------

  insurance_cover_rate <- pmin(pmax(insurance_cover_rate_c / 100, 0), 1)
  insurance_payout <- rep(0, n_years_c)
  insurance_premium_vector <- rep(0, n_years_c)

  if (isTRUE(use_insurance)) {

    has_insured_loss <- Apple_yield_reduction_due_to_weather > 0

    insurance_payout <- ifelse(
      has_insured_loss,
      insurance_cover_rate * insurance_payout_amount_c,
      0
    )

    AF_total_benefit_adj_derisk <-
      AF_total_benefit_adj_derisk + insurance_payout

    insurance_premium_vector[1:n_years_c] <- insurance_annual_premium_c

    payout_years <- which(insurance_payout > 0)
    first_payout_year <- if (length(payout_years) > 0) payout_years[1] else NA_integer_

    if (!is.na(first_payout_year)) {
      insurance_premium_vector[first_payout_year:n_years_c] <-
        insurance_annual_premium_surcharge_c
    }

    AF_total_running_cost_adj_derisk <-
      AF_total_running_cost_adj_derisk + insurance_premium_vector
  }

  # Recalculate farmer-side AF results ------------------------------------

  AF_total_cost_adj_derisk <-
    AF_total_investment_cost_adj_derisk + AF_total_running_cost_adj_derisk

  AF_bottom_line_benefit_adj_derisk <-
    AF_total_benefit_adj_derisk - AF_total_cost_adj_derisk

  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj_derisk,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )

  return(list(
    has_financing = has_financing,
    loan_draw = loan_draw,
    repayment_vector = repayment_vector,

    AF_total_investment_cost = AF_total_investment_cost_adj_derisk,
    AF_total_running_cost = AF_total_running_cost_adj_derisk,
    AF_total_benefit = AF_total_benefit_adj_derisk,
    AF_total_cost = AF_total_cost_adj_derisk,
    AF_bottom_line_benefit = AF_bottom_line_benefit_adj_derisk,

    AF_NPV = discounted$AF_NPV,
    AF_cash_flow = discounted$AF_cash_flow,
    AF_cum_cash_flow = discounted$AF_cum_cash_flow,

    Decision_benefit = discounted$Decision_benefit,
    NPV_decision = discounted$NPV_decision,
    CF_decision = discounted$CF_decision,
    CumCF_decision = discounted$CumCF_decision,

    AF_NPV_adj_derisk = discounted$AF_NPV,
    AF_cash_flow_adj_derisk = discounted$AF_cash_flow,
    AF_cum_cash_flow_adj_derisk = discounted$AF_cum_cash_flow,
    NPV_decision_adj_derisk = discounted$NPV_decision,
    CF_decision_adj_derisk = discounted$CF_decision,
    CumCF_decision_adj_derisk = discounted$CumCF_decision,

    guarantee_applied = guarantee_applied,
    guarantee_requested_but_no_loan = guarantee_requested_but_no_loan,
    guarantee_message = guarantee_message,
    guarantee_collateral_equivalent = collateral_equivalent,
    guarantee_default_loss_exposure = default_loss_exposure,
    guarantee_payout_to_lender = guarantee_payout_to_lender,
    lender_uncovered_loss = lender_uncovered_loss,
    lender_loss_protection = lender_loss_protection,
    guarantee_fee_vector = guarantee_fee_vector,

    insurance_applied = isTRUE(use_insurance),
    insurance_payout = insurance_payout,
    insurance_premium_vector = insurance_premium_vector,

    use_guarantee = use_guarantee,
    use_insurance = use_insurance
  ))
}
