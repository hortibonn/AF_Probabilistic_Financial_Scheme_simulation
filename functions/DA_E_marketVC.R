# ------------------------------------------------------------
# Market and value-chain access mechanisms
# ------------------------------------------------------------
# If market access is not selected, production remains sold at prevailing
# market prices, as already reflected in the incoming base_result.
#
# If market access is selected, users can choose one or both mechanisms:
#   1. price_guarantees: advance purchase agreements/procurement contracts
#      fix or floor prices for a defined share of output.
#   2. price_premia: labelling, certification, or ESG procurement raises unit
#      prices by a selected percentage for a defined share of output.
#
# This module changes revenues only. Production volumes are not changed.

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

as_year_vector <- function(value, n_years_c) {
  if (length(value) == 0 || all(is.na(value))) {
    return(rep(0, n_years_c))
  }

  if (length(value) == 1) {
    return(rep(value, n_years_c))
  }

  value[1:n_years_c]
}

bounded_share <- function(value) {
  pmin(pmax(value / 100, 0), 1)
}

apply_price_guarantee <- function(
    market_price,
    guaranteed_price,
    covered_share,
    guarantee_type = "floor"
) {
  if (guarantee_type == "fixed") {
    covered_price <- guaranteed_price
  } else {
    covered_price <- pmax(market_price, guaranteed_price)
  }

  covered_share * covered_price + (1 - covered_share) * market_price
}

apply_price_premium <- function(
    price,
    premium_rate,
    premium_share
) {
  price * (1 + premium_rate * premium_share)
}

calculate_market_value_chain_access <- function(
    base_result,
    Treeless_bottom_line_benefit,
    n_years_c,
    discount_rate_p,

    use_market_access = FALSE,

    # Multiple choices are allowed. Use either or both:
    # "price_guarantees", "price_premia".
    market_access_types = character(0),

    # Production volumes from DA_baseline.R. These should be returned by the
    # baseline model and passed here by app.R or the orchestrator.
    Table_apple_yield = rep(0, n_years_c),
    B_qual_table_apple_yield = rep(0, n_years_c),
    Juice_apple_yield = rep(0, n_years_c),
    AF_maize_yield = rep(0, n_years_c),
    AF_wheat_yield = rep(0, n_years_c),
    AF_barley_yield = rep(0, n_years_c),
    AF_rapeseed_yield = rep(0, n_years_c),

    # Prevailing market prices. These can be scalars or year vectors.
    table_apple_market_price = 0,
    bqual_apple_market_price = 0,
    juice_apple_market_price = 0,
    maize_market_price = 0,
    wheat_market_price = 0,
    barley_market_price = 0,
    rapeseed_market_price = 0,

    # Guaranteed/floor prices for the covered share of output.
    price_guarantee_type = "floor",
    price_guarantee_share_c = 0,
    table_apple_guaranteed_price_c = 0,
    bqual_apple_guaranteed_price_c = 0,
    juice_apple_guaranteed_price_c = 0,
    maize_guaranteed_price_c = 0,
    wheat_guaranteed_price_c = 0,
    barley_guaranteed_price_c = 0,
    rapeseed_guaranteed_price_c = 0,

    # Price premia for the covered share of output.
    price_premium_share_c = 0,
    table_apple_price_premium_c = 0,
    bqual_apple_price_premium_c = 0,
    juice_apple_price_premium_c = 0,
    maize_price_premium_c = 0,
    wheat_price_premium_c = 0,
    barley_price_premium_c = 0,
    rapeseed_price_premium_c = 0
) {

  AF_total_investment_cost_adj_market <- base_result$AF_total_investment_cost
  AF_total_running_cost_adj_market <- base_result$AF_total_running_cost
  AF_total_benefit_original <- base_result$AF_total_benefit

  Table_apple_yield <- as_year_vector(Table_apple_yield, n_years_c)
  B_qual_table_apple_yield <- as_year_vector(B_qual_table_apple_yield, n_years_c)
  Juice_apple_yield <- as_year_vector(Juice_apple_yield, n_years_c)
  AF_maize_yield <- as_year_vector(AF_maize_yield, n_years_c)
  AF_wheat_yield <- as_year_vector(AF_wheat_yield, n_years_c)
  AF_barley_yield <- as_year_vector(AF_barley_yield, n_years_c)
  AF_rapeseed_yield <- as_year_vector(AF_rapeseed_yield, n_years_c)

  table_apple_market_price <- as_year_vector(table_apple_market_price, n_years_c)
  bqual_apple_market_price <- as_year_vector(bqual_apple_market_price, n_years_c)
  juice_apple_market_price <- as_year_vector(juice_apple_market_price, n_years_c)
  maize_market_price <- as_year_vector(maize_market_price, n_years_c)
  wheat_market_price <- as_year_vector(wheat_market_price, n_years_c)
  barley_market_price <- as_year_vector(barley_market_price, n_years_c)
  rapeseed_market_price <- as_year_vector(rapeseed_market_price, n_years_c)

  table_apple_price_adj <- table_apple_market_price
  bqual_apple_price_adj <- bqual_apple_market_price
  juice_apple_price_adj <- juice_apple_market_price
  maize_price_adj <- maize_market_price
  wheat_price_adj <- wheat_market_price
  barley_price_adj <- barley_market_price
  rapeseed_price_adj <- rapeseed_market_price

  price_guarantee_applied <- FALSE
  price_premium_applied <- FALSE

  if (isTRUE(use_market_access)) {

    has_price_guarantees <- "price_guarantees" %in% market_access_types
    has_price_premia <- "price_premia" %in% market_access_types

    if (isTRUE(has_price_guarantees)) {
      price_guarantee_applied <- TRUE
      guarantee_share <- bounded_share(price_guarantee_share_c)

      table_apple_price_adj <- apply_price_guarantee(
        table_apple_price_adj,
        as_year_vector(table_apple_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
      bqual_apple_price_adj <- apply_price_guarantee(
        bqual_apple_price_adj,
        as_year_vector(bqual_apple_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
      juice_apple_price_adj <- apply_price_guarantee(
        juice_apple_price_adj,
        as_year_vector(juice_apple_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
      maize_price_adj <- apply_price_guarantee(
        maize_price_adj,
        as_year_vector(maize_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
      wheat_price_adj <- apply_price_guarantee(
        wheat_price_adj,
        as_year_vector(wheat_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
      barley_price_adj <- apply_price_guarantee(
        barley_price_adj,
        as_year_vector(barley_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
      rapeseed_price_adj <- apply_price_guarantee(
        rapeseed_price_adj,
        as_year_vector(rapeseed_guaranteed_price_c, n_years_c),
        guarantee_share,
        price_guarantee_type
      )
    }

    if (isTRUE(has_price_premia)) {
      price_premium_applied <- TRUE
      premium_share <- bounded_share(price_premium_share_c)

      table_apple_price_adj <- apply_price_premium(
        table_apple_price_adj,
        bounded_share(table_apple_price_premium_c),
        premium_share
      )
      bqual_apple_price_adj <- apply_price_premium(
        bqual_apple_price_adj,
        bounded_share(bqual_apple_price_premium_c),
        premium_share
      )
      juice_apple_price_adj <- apply_price_premium(
        juice_apple_price_adj,
        bounded_share(juice_apple_price_premium_c),
        premium_share
      )
      maize_price_adj <- apply_price_premium(
        maize_price_adj,
        bounded_share(maize_price_premium_c),
        premium_share
      )
      wheat_price_adj <- apply_price_premium(
        wheat_price_adj,
        bounded_share(wheat_price_premium_c),
        premium_share
      )
      barley_price_adj <- apply_price_premium(
        barley_price_adj,
        bounded_share(barley_price_premium_c),
        premium_share
      )
      rapeseed_price_adj <- apply_price_premium(
        rapeseed_price_adj,
        bounded_share(rapeseed_price_premium_c),
        premium_share
      )
    }
  }

  Table_apple_benefit_adj_market <- Table_apple_yield * table_apple_price_adj
  B_qual_apple_benefit_adj_market <- B_qual_table_apple_yield * bqual_apple_price_adj
  Juice_apple_benefit_adj_market <- Juice_apple_yield * juice_apple_price_adj

  AF_apple_benefit_adj_market <-
    Table_apple_benefit_adj_market +
    B_qual_apple_benefit_adj_market +
    Juice_apple_benefit_adj_market

  AF_maize_benefit_adj_market <- AF_maize_yield * maize_price_adj
  AF_wheat_benefit_adj_market <- AF_wheat_yield * wheat_price_adj
  AF_barley_benefit_adj_market <- AF_barley_yield * barley_price_adj
  AF_rapeseed_benefit_adj_market <- AF_rapeseed_yield * rapeseed_price_adj

  AF_crop_benefit_adj_market <-
    AF_apple_benefit_adj_market +
    AF_maize_benefit_adj_market +
    AF_wheat_benefit_adj_market +
    AF_barley_benefit_adj_market +
    AF_rapeseed_benefit_adj_market

  original_crop_benefit <-
    Table_apple_yield * table_apple_market_price +
    B_qual_table_apple_yield * bqual_apple_market_price +
    Juice_apple_yield * juice_apple_market_price +
    AF_maize_yield * maize_market_price +
    AF_wheat_yield * wheat_market_price +
    AF_barley_yield * barley_market_price +
    AF_rapeseed_yield * rapeseed_market_price

  market_revenue_change <- AF_crop_benefit_adj_market - original_crop_benefit

  AF_total_benefit_adj_market <- AF_total_benefit_original + market_revenue_change

  AF_total_cost_adj_market <-
    AF_total_investment_cost_adj_market + AF_total_running_cost_adj_market

  AF_bottom_line_benefit_adj_market <-
    AF_total_benefit_adj_market - AF_total_cost_adj_market

  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj_market,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )

  return(list(
    AF_total_investment_cost = AF_total_investment_cost_adj_market,
    AF_total_running_cost = AF_total_running_cost_adj_market,
    AF_total_benefit = AF_total_benefit_adj_market,
    AF_total_cost = AF_total_cost_adj_market,
    AF_bottom_line_benefit = AF_bottom_line_benefit_adj_market,

    AF_NPV = discounted$AF_NPV,
    AF_cash_flow = discounted$AF_cash_flow,
    AF_cum_cash_flow = discounted$AF_cum_cash_flow,

    Decision_benefit = discounted$Decision_benefit,
    NPV_decision = discounted$NPV_decision,
    CF_decision = discounted$CF_decision,
    CumCF_decision = discounted$CumCF_decision,

    AF_NPV_adj_market = discounted$AF_NPV,
    AF_cash_flow_adj_market = discounted$AF_cash_flow,
    AF_cum_cash_flow_adj_market = discounted$AF_cum_cash_flow,
    NPV_decision_adj_market = discounted$NPV_decision,
    CF_decision_adj_market = discounted$CF_decision,
    CumCF_decision_adj_market = discounted$CumCF_decision,

    market_revenue_change = market_revenue_change,
    original_crop_benefit = original_crop_benefit,
    AF_crop_benefit_adj_market = AF_crop_benefit_adj_market,

    table_apple_price_adj = table_apple_price_adj,
    bqual_apple_price_adj = bqual_apple_price_adj,
    juice_apple_price_adj = juice_apple_price_adj,
    maize_price_adj = maize_price_adj,
    wheat_price_adj = wheat_price_adj,
    barley_price_adj = barley_price_adj,
    rapeseed_price_adj = rapeseed_price_adj,

    price_guarantee_applied = price_guarantee_applied,
    price_premium_applied = price_premium_applied,
    use_market_access = use_market_access,
    market_access_types = market_access_types
  ))
}
