# ------------------------------------------------------------
# Advisory support mechanisms
# ------------------------------------------------------------
# This script calculates how AF costs and profitability change when the user
# selects advisory support. The UI/server choices stay in app.R; this function
# only receives those choices and recalculates NPV, cash flow, and cumulative
# cash flow.

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

calculate_advisory_support <- function(
    base_result,
    Treeless_bottom_line_benefit,
    n_years_c,
    discount_rate_p,

    use_advisory = FALSE,

    # Multiple choices are allowed. Use any combination of:
    # "organisation", "cooperative", "digital_tools", "project".
    advisory_support_types = character(0),

    # Cost vectors from the baseline model. These should be returned by
    # DA_baseline.R or passed from another scenario result.
    consultation_cost = rep(0, n_years_c),
    machinery_cost = rep(0, n_years_c),
    labour_cost = rep(0, n_years_c),
    digital_tool_subscription = rep(0, n_years_c),

    # Organisation participation: nominal fee plus partial consultation support.
    organisation_nominal_fee_c = 0,
    organisation_consultation_reduction_perc_c = 0,

    # Cooperative membership: machinery and labour costs reduced by user input.
    cooperative_machinery_reduction_perc_c = 0,
    cooperative_labour_reduction_perc_c = 0,
    cooperative_nominal_fee_c = 0,

    # Digital tools: subscription is free or discounted by a fixed amount.
    digital_tool_subscription_discount_amount_c = 0
) {

  AF_total_investment_cost_adj_advisory <- base_result$AF_total_investment_cost
  AF_total_running_cost_adj_advisory <- base_result$AF_total_running_cost
  AF_total_benefit_adj_advisory <- base_result$AF_total_benefit

  consultation_cost <- consultation_cost[1:n_years_c]
  machinery_cost <- machinery_cost[1:n_years_c]
  labour_cost <- labour_cost[1:n_years_c]
  digital_tool_subscription <- digital_tool_subscription[1:n_years_c]

  consultation_reduction <- rep(0, n_years_c)
  machinery_reduction <- rep(0, n_years_c)
  labour_reduction <- rep(0, n_years_c)
  digital_tool_reduction <- rep(0, n_years_c)
  advisory_fee_vector <- rep(0, n_years_c)

  if (isTRUE(use_advisory)) {

    has_organisation <- "organisation" %in% advisory_support_types
    has_cooperative <- "cooperative" %in% advisory_support_types
    has_digital_tools <- "digital_tools" %in% advisory_support_types
    has_project <- "project" %in% advisory_support_types

    # Organisation: the farmer pays a nominal annual fee and receives advice
    # that reduces consultation cost by a user-selected percentage.
    if (isTRUE(has_organisation)) {
      organisation_consultation_reduction_rate <-
        pmin(pmax(organisation_consultation_reduction_perc_c / 100, 0), 1)

      consultation_reduction <- pmax(
        consultation_reduction,
        consultation_cost * organisation_consultation_reduction_rate
      )

      advisory_fee_vector <- advisory_fee_vector + organisation_nominal_fee_c
    }

    # Cooperative: the farmer gets shared access to machinery/labour services.
    # This reduces machinery and labour costs by user-selected percentages.
    if (isTRUE(has_cooperative)) {
      cooperative_machinery_reduction_rate <-
        pmin(pmax(cooperative_machinery_reduction_perc_c / 100, 0), 1)
      cooperative_labour_reduction_rate <-
        pmin(pmax(cooperative_labour_reduction_perc_c / 100, 0), 1)

      machinery_reduction <- machinery_cost * cooperative_machinery_reduction_rate
      labour_reduction <- labour_cost * cooperative_labour_reduction_rate

      advisory_fee_vector <- advisory_fee_vector + cooperative_nominal_fee_c
    }

    # Digital tools: the farmer receives a fixed subscription discount.
    # The reduction is capped at the original subscription cost.
    if (isTRUE(has_digital_tools)) {
      digital_tool_reduction <- pmin(
        digital_tool_subscription,
        digital_tool_subscription_discount_amount_c
      )
    }

    # Project participation: consultation is fully covered by the project.
    # This overrides partial organisation support where both are selected.
    # The farmer does not pay a project participation fee.
    if (isTRUE(has_project)) {
      consultation_reduction <- consultation_cost
    }

    # Consultation is treated as an investment/planning cost.
    AF_total_investment_cost_adj_advisory <- pmax(
      AF_total_investment_cost_adj_advisory - consultation_reduction,
      0
    )

    # Machinery, labour, digital tools, and membership fees affect running cost.
    AF_total_running_cost_adj_advisory <-
      AF_total_running_cost_adj_advisory -
      machinery_reduction -
      labour_reduction -
      digital_tool_reduction +
      advisory_fee_vector

    AF_total_running_cost_adj_advisory <-
      pmax(AF_total_running_cost_adj_advisory, 0)
  }

  AF_total_cost_adj_advisory <-
    AF_total_investment_cost_adj_advisory + AF_total_running_cost_adj_advisory

  AF_bottom_line_benefit_adj_advisory <-
    AF_total_benefit_adj_advisory - AF_total_cost_adj_advisory

  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj_advisory,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )

  return(list(
    AF_NPV_adj_advisory = discounted$AF_NPV,
    AF_cash_flow_adj_advisory = discounted$AF_cash_flow,
    AF_cum_cash_flow_adj_advisory = discounted$AF_cum_cash_flow,

    NPV_decision_adj_advisory = discounted$NPV_decision,
    CF_decision_adj_advisory = discounted$CF_decision,
    CumCF_decision_adj_advisory = discounted$CumCF_decision,

    AF_total_investment_cost_adj_advisory =
      AF_total_investment_cost_adj_advisory,
    AF_total_running_cost_adj_advisory =
      AF_total_running_cost_adj_advisory,
    AF_total_benefit_adj_advisory = AF_total_benefit_adj_advisory,
    AF_total_cost_adj_advisory = AF_total_cost_adj_advisory,
    AF_bottom_line_benefit_adj_advisory =
      AF_bottom_line_benefit_adj_advisory,

    consultation_reduction = consultation_reduction,
    machinery_reduction = machinery_reduction,
    labour_reduction = labour_reduction,
    digital_tool_reduction = digital_tool_reduction,
    advisory_fee_vector = advisory_fee_vector,

    use_advisory = use_advisory,
    advisory_support_types = advisory_support_types
  ))
}
