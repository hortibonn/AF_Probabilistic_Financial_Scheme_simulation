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
calc_discount_outputs <- function(bottom_line_benefit, treeless_bottom_line_benefit, discount_rate_p) {
  
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
    AF_bottom_line_benefit = bottom_line_benefit,
    AF_NPV = AF_NPV,
    AF_cash_flow = AF_cash_flow,
    AF_cum_cash_flow = AF_cum_cash_flow,
    Decision_benefit = Decision_benefit,
    NPV_decision = NPV_decision,
    CF_decision = CF_decision,
    CumCF_decision = CumCF_decision
  )
}


# ------------------------------------------------------------
# Helper: risk mitigation scenario
# Supports guarantee only, insurance only, or both
# ------------------------------------------------------------
calc_risk_mitigation_scenario <- function(
    AF_total_investment_cost,
    AF_total_running_cost,
    AF_total_benefit,
    Treeless_bottom_line_benefit,
    discount_rate_p,
    n_years_c,
    Apple_yield_reduction_due_to_weather,
    guarantee_amount_c = 0,
    insurance_cover_rate_c = 0,
    insurance_payout_amount_c = 0,
    insurance_annual_premium_c = 0,
    insurance_annual_premium_surcharge_c = 0,
    use_guarantee = FALSE,
    use_insurance = FALSE
) {
  
  # Start from baseline AF scenario
  AF_total_investment_cost_adj <- AF_total_investment_cost
  AF_total_running_cost_adj    <- AF_total_running_cost
  AF_total_benefit_adj         <- AF_total_benefit
  
  # ---------------------------
  # Guarantee adjustment
  # ---------------------------
  if (isTRUE(use_guarantee)) {
    AF_total_investment_cost_adj <- AF_total_investment_cost_adj - guarantee_amount_c
    AF_total_investment_cost_adj <- pmax(AF_total_investment_cost_adj, 0)
  }
  
  # ---------------------------
  # Insurance adjustment
  # ---------------------------
  insurance_payout <- rep(0, n_years_c)
  insurance_premium_vec <- rep(0, n_years_c)
  
  if (isTRUE(use_insurance)) {
    
    insurance_cover_rate <- insurance_cover_rate_c / 100
    has_yield_loss <- Apple_yield_reduction_due_to_weather > 0
    
    insurance_payout <- ifelse(
      has_yield_loss,
      insurance_cover_rate * insurance_payout_amount_c,
      0
    )
    
    AF_total_benefit_adj <- AF_total_benefit_adj + insurance_payout
    
    insurance_premium_vec <- rep(insurance_annual_premium_c, n_years_c)
    
    payout_years <- which(insurance_payout > 0)
    first_payout_year <- if (length(payout_years) > 0) payout_years[1] else NA_integer_
    
    if (!is.na(first_payout_year)) {
      insurance_premium_vec[first_payout_year:n_years_c] <- insurance_annual_premium_surcharge_c
    }
    
    AF_total_running_cost_adj <- AF_total_running_cost_adj + insurance_premium_vec
  }
  
  # ---------------------------
  # Total cost and bottom line
  # ---------------------------
  AF_total_cost_adj <- AF_total_investment_cost_adj + AF_total_running_cost_adj
  AF_bottom_line_benefit_adj <- AF_total_benefit_adj - AF_total_cost_adj
  
  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )
  
  list(
    AF_total_investment_cost_adj = AF_total_investment_cost_adj,
    AF_total_running_cost_adj = AF_total_running_cost_adj,
    AF_total_benefit_adj = AF_total_benefit_adj,
    AF_total_cost_adj = AF_total_cost_adj,
    insurance_payout = insurance_payout,
    insurance_premium_vec = insurance_premium_vec,
    
    AF_bottom_line_benefit_adj = discounted$AF_bottom_line_benefit,
    AF_NPV_adj = discounted$AF_NPV,
    AF_cash_flow_adj = discounted$AF_cash_flow,
    AF_cum_cash_flow_adj = discounted$AF_cum_cash_flow,
    Decision_benefit_adj = discounted$Decision_benefit,
    NPV_decision_adj = discounted$NPV_decision,
    CF_decision_adj = discounted$CF_decision,
    CumCF_decision_adj = discounted$CumCF_decision
  )
}
# ------------------------------------------------------------
# Helper: generic loan scenario
# Reduces year-1 investment outflow by the loan draw and
# adds a repayment vector to running costs
# ------------------------------------------------------------
calc_loan_scenario <- function(
    AF_total_investment_cost,
    AF_total_running_cost,
    AF_total_benefit,
    Treeless_bottom_line_benefit,
    discount_rate_p,
    loan_draw,
    repayment_vector
) {
  
  AF_total_investment_cost_adj <- AF_total_investment_cost
  AF_total_investment_cost_adj[1] <- max(AF_total_investment_cost_adj[1] - loan_draw, 0)
  
  AF_total_running_cost_adj <- AF_total_running_cost + repayment_vector
  AF_total_cost_adj <- AF_total_investment_cost_adj + AF_total_running_cost_adj
  AF_bottom_line_benefit_adj <- AF_total_benefit - AF_total_cost_adj
  
  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )
  
  list(
    AF_total_investment_cost_adj = AF_total_investment_cost_adj,
    AF_total_running_cost_adj = AF_total_running_cost_adj,
    AF_total_cost_adj = AF_total_cost_adj,
    AF_bottom_line_benefit_adj = discounted$AF_bottom_line_benefit,
    AF_NPV_adj = discounted$AF_NPV,
    AF_cash_flow_adj = discounted$AF_cash_flow,
    AF_cum_cash_flow_adj = discounted$AF_cum_cash_flow,
    Decision_benefit_adj = discounted$Decision_benefit,
    NPV_decision_adj = discounted$NPV_decision,
    CF_decision_adj = discounted$CF_decision,
    CumCF_decision_adj = discounted$CumCF_decision
  )
}

# ------------------------------------------------------------
# Helper: risk mitigation scenario
# Supports guarantee only, insurance only, or both
# ------------------------------------------------------------
calc_risk_mitigation_scenario <- function(
    AF_total_investment_cost,
    AF_total_running_cost,
    AF_total_benefit,
    Treeless_bottom_line_benefit,
    discount_rate_p,
    n_years_c,
    Apple_yield_reduction_due_to_weather,
    guarantee_amount_c = 0,
    insurance_cover_rate_c = 0,
    insurance_payout_amount_c = 0,
    insurance_annual_premium_c = 0,
    insurance_annual_premium_surcharge_c = 0,
    use_guarantee = FALSE,
    use_insurance = FALSE
) {
  
  # Start from baseline AF scenario
  AF_total_investment_cost_adj <- AF_total_investment_cost
  AF_total_running_cost_adj    <- AF_total_running_cost
  AF_total_benefit_adj         <- AF_total_benefit
  
  # ---------------------------
  # Guarantee adjustment
  # ---------------------------
  if (isTRUE(use_guarantee)) {
    AF_total_investment_cost_adj[1] <- AF_total_investment_cost_adj[1] - guarantee_amount_c
    AF_total_investment_cost_adj[1] <- max(AF_total_investment_cost_adj[1], 0)
  }
  
  # ---------------------------
  # Insurance adjustment
  # ---------------------------
  insurance_payout <- rep(0, n_years_c)
  insurance_premium_vec <- rep(0, n_years_c)
  
  if (isTRUE(use_insurance)) {
    
    insurance_cover_rate <- insurance_cover_rate_c / 100
    has_yield_loss <- Apple_yield_reduction_due_to_weather > 0
    
    insurance_payout <- ifelse(
      has_yield_loss,
      insurance_cover_rate * insurance_payout_amount_c,
      0
    )
    
    AF_total_benefit_adj <- AF_total_benefit_adj + insurance_payout
    
    insurance_premium_vec <- rep(insurance_annual_premium_c, n_years_c)
    
    payout_years <- which(insurance_payout > 0)
    first_payout_year <- if (length(payout_years) > 0) payout_years[1] else NA_integer_
    
    if (!is.na(first_payout_year)) {
      insurance_premium_vec[first_payout_year:n_years_c] <- insurance_annual_premium_surcharge_c
    }
    
    AF_total_running_cost_adj <- AF_total_running_cost_adj + insurance_premium_vec
  }
  
  # ---------------------------
  # Total cost and bottom line
  # ---------------------------
  AF_total_cost_adj <- AF_total_investment_cost_adj + AF_total_running_cost_adj
  AF_bottom_line_benefit_adj <- AF_total_benefit_adj - AF_total_cost_adj
  
  discounted <- calc_discount_outputs(
    bottom_line_benefit = AF_bottom_line_benefit_adj,
    treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p
  )
  
  list(
    AF_total_investment_cost_adj = AF_total_investment_cost_adj,
    AF_total_running_cost_adj = AF_total_running_cost_adj,
    AF_total_benefit_adj = AF_total_benefit_adj,
    AF_total_cost_adj = AF_total_cost_adj,
    insurance_payout = insurance_payout,
    insurance_premium_vec = insurance_premium_vec,
    
    AF_bottom_line_benefit_adj = discounted$AF_bottom_line_benefit,
    AF_NPV_adj = discounted$AF_NPV,
    AF_cash_flow_adj = discounted$AF_cash_flow,
    AF_cum_cash_flow_adj = discounted$AF_cum_cash_flow,
    Decision_benefit_adj = discounted$Decision_benefit,
    NPV_decision_adj = discounted$NPV_decision,
    CF_decision_adj = discounted$CF_decision,
    CumCF_decision_adj = discounted$CumCF_decision
  )
}


#Defining the probabilistic model
#!!NOTE:variables that are in all lower case are from the input_table.csv and the rest (with any upper case letter) are defined and calculated within the code
AF_benefit_with_Risks <- function(x, varnames)
{
  #System modulators ####
  # ------------------------------------------------------------
  # User-selected finance scheme flags from UI
  # ------------------------------------------------------------
  use_bank_loan <- !is.na(loan_bank_selected_c) && as.numeric(loan_bank_selected_c) == 1
  use_impact_loan <- !is.na(loan_impact_selected_c) && as.numeric(loan_impact_selected_c) == 1
  use_dev_bank_loan <- !is.na(loan_dev_bank_selected_c) && as.numeric(loan_dev_bank_selected_c) == 1
  
  use_guarantee <- !is.na(risk_mitigation_guarantee_c) && as.numeric(risk_mitigation_guarantee_c) == 1
  use_insurance <- !is.na(risk_mitigation_insurance_c) && as.numeric(risk_mitigation_insurance_c) == 1
  #yield failure due to weather events
  
  Arable_yield_if_extreme_weather <- vv(value_if_extreme_weather_p, var_CV = var_cv_p, n = n_years_c, lower_limit = 0.8, upper_limit = 0.9)
  AF_arable_yield_if_extreme_weather <- vv(Arable_yield_if_extreme_weather, var_CV = var_cv_p, n = n_years_c, absolute_trend = trees_yield_buffering_effect_trend_p, lower_limit = 0.8, upper_limit = 1)
  
  Value_if_not <- rep(1, n_years_c)
  
  
  Yield_reduction_due_to_weather <-
    chance_event(chance = chance_extreme_weather_t,
                 value_if = Arable_yield_if_extreme_weather,
                 value_if_not = Value_if_not) # 5-30% chance that the event will occur and result in 10-20% yield reduction
  AF_yield_reduction_due_to_weather <-
    chance_event(chance = chance_extreme_weather_t,
                 value_if = AF_arable_yield_if_extreme_weather,
                 value_if_not = Value_if_not) # 5-30 chance that the event will occur and result in 10-20% yield reduction at first, gradually reducing to 1-15% yield reduction as system matures
  
  Apple_yield_reduction_due_to_weather <- AF_yield_reduction_due_to_weather
  
  #-----------------------------------------------------------------------------------------------------------
  #Introduction of variables, which are not system specific
  #Indices: represent the placement of each crop within the crop rotation, every fifth year the crop rotation repeats.
  #This way the time frame over which the crop rotation will be simulated can be changed by changing the value of "n_years_c" in the input table
  Maize_indices <- seq(from = 1, to = n_years_c, by = 4)
  Wheat_indices <- seq(from = 2, to = n_years_c, by = 4)
  Barley_indices <- seq(from = 3, to = n_years_c, by = 4)
  Rapeseed_indices <- seq(from = 4, to = n_years_c, by = 4)
  
  #Labour hours needed to manage crops (per ha per year)
  Maize_labour <- rep(0, n_years_c)
  Maize_labour[Maize_indices] <- vv(maize_labour_p, cv_maize_labour_c, length(Maize_indices))
  Wheat_labour <- rep(0, n_years_c)
  Wheat_labour[Wheat_indices] <- vv(wheat_labour_p, cv_wheat_labour_c, length(Wheat_indices)) 
  Barley_labour <- rep(0, n_years_c)
  Barley_labour[Barley_indices] <- vv(barley_labour_p, cv_barley_labour_c, length(Barley_indices))
  Rapeseed_labour <- rep(0, n_years_c)
  Rapeseed_labour[Rapeseed_indices] <- vv(rapeseed_labour_p, cv_rapeseed_labour_c, length(Rapeseed_indices))
  #----------------------------------------------------------------------------------------------------------- 
  
  #BASELINE SYSTEM - TREELESS ARABLE AGRICULTURE ####
  
  #Cost of managing arable system
  #No investment cost considered, since the arable system is already established and running.
  #The model depicts the implementation of an AF system into an already existing arable system. 
  
  #Running cost ####
  
  #First creating a vector, with as many zeros as there are years in the simulation, indicated by the value of "n_years_c" in the input table
  #Then, filling the vector with the cost-specific values at specific positions of the vector (determined by the "crop indices".
  #These positions correspond with the crops position within the crop rotation.
  #Adding up all crop-specific costs [€] to generate a value for the total cost associated with cultivating a specific crop
  
  #maize
  Treeless_maize_sowing_cost <- rep(0, n_years_c)#create vector with as many zeros as number of years in simulation
  Treeless_maize_sowing_cost[Maize_indices] <- vv(maize_seed_price_p, cv_maize_seed_price_c, length(Maize_indices)) * arable_area_treeless_c #fill vector with values at specific positions corresponding to the years maize is present in the crop rotation #cost of seed [€/ha]*area managed [ha]
  
  Treeless_maize_fertilizer_cost <- rep(0, n_years_c)
  Treeless_maize_fertilizer_cost[Maize_indices] <- vv(maize_fert_price_p, cv_maize_fert_price_c, length(Maize_indices)) * arable_area_treeless_c #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_maize_pesticide_cost <- rep(0, n_years_c)
  Treeless_maize_pesticide_cost[Maize_indices] <- vv(maize_cides_price_p, cv_maize_cides_price_c, length(Maize_indices)) * arable_area_treeless_c #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_maize_machinery_cost <- rep(0, n_years_c)
  Treeless_maize_machinery_cost[Maize_indices] <- vv(maize_mach_price_p, cv_maize_mach_price_c, length(Maize_indices)) * arable_area_treeless_c #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_maize_insurance_cost <- rep(0, n_years_c)
  Treeless_maize_insurance_cost[Maize_indices] <- vv(maize_insurance_p, cv_maize_insurance_c, length(Maize_indices)) * arable_area_treeless_c #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_maize_labour_cost <- rep(0, n_years_c)
  Treeless_maize_labour_cost <- Maize_labour * arable_area_treeless_c * labour_cost_p #Labour cost associated with maize cultivation in treeless system
  
  Treeless_total_maize_cost <- Treeless_maize_sowing_cost + Treeless_maize_fertilizer_cost + Treeless_maize_pesticide_cost + Treeless_maize_machinery_cost + Treeless_maize_insurance_cost + Treeless_maize_labour_cost
  
  #wheat
  Treeless_wheat_sowing_cost <- rep(0, n_years_c)
  Treeless_wheat_sowing_cost[Wheat_indices] <- vv(wheat_seed_price_p, cv_wheat_seed_price_c, length(Wheat_indices)) * arable_area_treeless_c #cost of seed [€/ha]*area managed [ha]
  
  Treeless_wheat_fertilizer_cost <- rep(0, n_years_c)
  Treeless_wheat_fertilizer_cost[Wheat_indices] <- vv(wheat_fert_price_p, cv_wheat_fert_price_c, length(Wheat_indices)) * arable_area_treeless_c #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_wheat_pesticide_cost <- rep(0, n_years_c)
  Treeless_wheat_pesticide_cost[Wheat_indices] <- vv(wheat_cides_price_p, cv_wheat_cides_price_c, length(Wheat_indices)) * arable_area_treeless_c #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_wheat_machinery_cost <- rep(0, n_years_c)
  Treeless_wheat_machinery_cost[Wheat_indices] <- vv(wheat_mach_price_p, cv_wheat_mach_price_c, length(Wheat_indices)) * arable_area_treeless_c #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_wheat_insurance_cost <- rep(0, n_years_c)
  Treeless_wheat_insurance_cost[Wheat_indices] <- vv(wheat_insurance_p, cv_wheat_insurance_c, length(Wheat_indices)) * arable_area_treeless_c #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_wheat_labour_cost <- rep(0, n_years_c)
  Treeless_wheat_labour_cost <- Wheat_labour * arable_area_treeless_c * labour_cost_p #Labour cost associated with wheat cultivation in treeless system
  
  Treeless_total_wheat_cost <- Treeless_wheat_sowing_cost + Treeless_wheat_fertilizer_cost + Treeless_wheat_pesticide_cost + Treeless_wheat_machinery_cost + Treeless_wheat_insurance_cost + Treeless_wheat_labour_cost
  
  #barley
  Treeless_barley_sowing_cost <- rep(0, n_years_c)
  Treeless_barley_sowing_cost[Barley_indices] <- vv(barley_seed_price_p, cv_barley_seed_price_c, length(Barley_indices)) * arable_area_treeless_c #cost of seed [€/ha]*area managed [ha]
  
  Treeless_barley_fertilizer_cost <- rep(0, n_years_c)
  Treeless_barley_fertilizer_cost[Barley_indices] <- vv(barley_fert_price_p, cv_barley_fert_price_c, length(Barley_indices)) * arable_area_treeless_c #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_barley_pesticide_cost <- rep(0, n_years_c)
  Treeless_barley_pesticide_cost[Barley_indices] <- vv(barley_cides_price_p, cv_barley_cides_price_c, length(Barley_indices)) * arable_area_treeless_c #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_barley_machinery_cost <- rep(0, n_years_c)
  Treeless_barley_machinery_cost[Barley_indices] <- vv(barley_mach_price_p, cv_barley_mach_price_c, length(Barley_indices)) * arable_area_treeless_c #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_barley_insurance_cost <- rep(0, n_years_c)
  Treeless_barley_insurance_cost[Barley_indices] <- vv(barley_insurance_p, cv_barley_insurance_c, length(Barley_indices)) * arable_area_treeless_c #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_barley_labour_cost <- rep(0, n_years_c)
  Treeless_barley_labour_cost <- Barley_labour * arable_area_treeless_c * labour_cost_p #Labour cost associated with barley cultivation in treeless system
  
  Treeless_total_barley_cost <- Treeless_barley_sowing_cost + Treeless_barley_fertilizer_cost + Treeless_barley_pesticide_cost + Treeless_barley_machinery_cost + Treeless_barley_insurance_cost + Treeless_barley_labour_cost
  
  #rapeseed
  Treeless_rapeseed_sowing_cost <- rep(0, n_years_c)
  Treeless_rapeseed_sowing_cost[Rapeseed_indices] <- vv(rapeseed_seed_price_p, cv_rapeseed_seed_price_c, length(Rapeseed_indices)) * arable_area_treeless_c #cost of seed [€/ha]*area managed [ha]
  
  Treeless_rapeseed_fertilizer_cost <- rep(0, n_years_c)
  Treeless_rapeseed_fertilizer_cost[Rapeseed_indices] <- vv(rapeseed_fert_price_p, cv_rapeseed_fert_price_c, length(Rapeseed_indices)) * arable_area_treeless_c #cost of fertilizer [€/ha]*area managed [ha]
  
  Treeless_rapeseed_pesticide_cost <- rep(0, n_years_c)
  Treeless_rapeseed_pesticide_cost[Rapeseed_indices] <- vv(rapeseed_cides_price_p, cv_rapeseed_cides_price_c, length(Rapeseed_indices)) * arable_area_treeless_c #cost of pesticides [€/ha]*area managed [ha]
  
  Treeless_rapeseed_machinery_cost <- rep(0, n_years_c)
  Treeless_rapeseed_machinery_cost[Rapeseed_indices] <- vv(rapeseed_mach_price_p, cv_rapeseed_mach_price_c, length(Rapeseed_indices)) * arable_area_treeless_c #cost of machinery [€/ha]*area managed [ha]
  
  Treeless_rapeseed_insurance_cost <- rep(0, n_years_c)
  Treeless_rapeseed_insurance_cost[Rapeseed_indices] <- vv(rapeseed_insurance_p, cv_rapeseed_insurance_c, length(Rapeseed_indices)) * arable_area_treeless_c #cost of insurance [€/ha]*area managed [ha]
  
  Treeless_rapeseed_labour_cost <- rep(0, n_years_c)
  Treeless_rapeseed_labour_cost <- Rapeseed_labour * arable_area_treeless_c * labour_cost_p #Labour cost associated with rapeseed cultivation in treeless system
  
  Treeless_total_rapeseed_cost <- Treeless_rapeseed_sowing_cost + Treeless_rapeseed_fertilizer_cost + Treeless_rapeseed_pesticide_cost + Treeless_rapeseed_machinery_cost + Treeless_rapeseed_insurance_cost + Treeless_rapeseed_labour_cost
  
  #total cost of arable component of the AF system
  Treeless_total_arable_management_cost <- Treeless_total_maize_cost + Treeless_total_wheat_cost + Treeless_total_barley_cost + Treeless_total_rapeseed_cost
  
  #Benefits treeless system ####
  
  # Arable system is managed with crop rotation of Maize(CCM)-Wheat-Barley-Rapeseed -> one crop is grown once every 4 years 
  #First creating a vector, with as many zeros as there are years in the simulation, indicated by the value of "n_years_c" in the input table
  #Then, fill the vector with the yield values at specific positions of the vector. These positions correspond with the crops position within the crop rotation
  #Lastly, multiply the yield of each crop with the value of the respective crop to calculate the benefit/revenue 
  
  Treeless_maize_yield <- rep(0, n_years_c) #create vector with as many zeros as number of years in simulation
  Treeless_maize_yield[Maize_indices] <- vv(maize_yield_p, cv_maize_yield_c, length(Maize_indices)) * arable_area_treeless_c #fill vector with values at specific positions corresponding to the years maize is present in the crop rotation
  Treeless_maize_benefit <- vv(maize_value_p, cv_maize_value_c, n_years_c) * Treeless_maize_yield * Yield_reduction_due_to_weather#calculate the benefit, i.e. revenue #The possibility of a yield reduction due to extreme weather is inegrated here
  
  Treeless_wheat_yield <- rep(0, n_years_c)
  Treeless_wheat_yield[Wheat_indices] <- vv(wheat_yield_p, cv_wheat_yield_c, length(Wheat_indices)) * arable_area_treeless_c
  Treeless_wheat_benefit <- vv(wheat_value_p, cv_wheat_value_c, n_years_c) * Treeless_wheat_yield * Yield_reduction_due_to_weather
  
  Treeless_barley_yield <- rep(0, n_years_c)
  Treeless_barley_yield[Barley_indices] <- vv(barley_yield_p, cv_barley_yield_c, length(Barley_indices)) * arable_area_treeless_c
  Treeless_barley_benefit <- vv(barley_value_p, cv_barley_value_c, n_years_c) * Treeless_barley_yield * Yield_reduction_due_to_weather
  
  Treeless_rapeseed_yield <- rep(0, n_years_c)
  Treeless_rapeseed_yield[Rapeseed_indices] <- vv(rapeseed_yield_p, cv_rapeseed_yield_c, length(Rapeseed_indices)) * arable_area_treeless_c
  Treeless_rapeseed_benefit <- vv(rapeseed_value_p, cv_rapeseed_value_c, n_years_c) * Treeless_rapeseed_yield * Yield_reduction_due_to_weather
  
  Treeless_total_benefit <- Treeless_maize_benefit + Treeless_wheat_benefit + Treeless_barley_benefit + Treeless_rapeseed_benefit
  
  #Treeless system bottom line ####
  Treeless_bottom_line_benefit <- Treeless_total_benefit - Treeless_total_arable_management_cost 
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the baseline system
  
  NPV_treeless_system <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate_p,
                                  calculate_NPV = TRUE) #NVP of treeless arable system #Treeless_total_benefit
  Treeless_cash_flow <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate_p,
                                 calculate_NPV = FALSE) #Cash flow of treeless system #Treeless_total_benefit
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of treeless system
  #-----------------------------------------------------------------------------------------------------------  
  
  # AGROFORESTRY SYSTEM ####
  
  #Calculating AF benefits####
  
  #Apples in AF system ####
  
  #First apple yield estimated to happen in year 4 or 5 (according to farmer)
  Time_to_first_apple <- chance_event(chance = 0.5, value_if = time_to_first_apple1_c, value_if_not = time_to_first_apple2_c, n = 1)
  
  #Second yield stage is adapted from data received from experts on intensive apple plantations. Second yield stage is estimated to set in in year 7 or 8. 
  Time_to_second_apple <- chance_event(chance = 0.5, value_if = time_to_second_apple1_c, value_if_not = time_to_second_apple2_c, n = 1)
  
  #Yield of one apple tree [kg/tree]
  AF_apple_yield <- rep(0, n_years_c)
  AF_apple_yield <- gompertz_yield(max_harvest = apple_yield_max_p,
                                   time_to_first_yield_estimate = Time_to_first_apple,
                                   time_to_second_yield_estimate = Time_to_second_apple,
                                   first_yield_estimate_percent = apple_yield_first_p,
                                   second_yield_estimate_percent = apple_yield_second_p,
                                   n_years=n_years_c,
                                   var_CV = var_cv_p,
                                   no_yield_before_first_estimate = TRUE)
  #Yield of 473 apple trees [kg]  
  AF_tot_apple_yield <- (AF_apple_yield - AF_apple_yield * apple_postharvest_loss_p) * num_trees_c *Apple_yield_reduction_due_to_weather #Post-harvest losses and possibility of yield reduction due to extreme weather integrated here.
  #Calculate how many kg have table apple quality and can therefore be marketed at a highest price in percentage
  Pc_table_apples <- vv(perc_table_apple_p, var_CV = var_cv_p, n_years_c)/100
  
  Table_apple_yield <- AF_tot_apple_yield * Pc_table_apples #amount of highest quality apples [kg]
  
  Lower_qual_apple_yield <- AF_tot_apple_yield * (1-Pc_table_apples) #rest of yield is classified as lower quality
  
  Pc_b_qual_apple <- vv(perc_bqual_apple_p, var_CV = var_cv_p, n_years_c)/100 #B-quality apples can still be sold in direct selling operation, but at significantly lower price
  
  B_qual_table_apple_yield <- Lower_qual_apple_yield * Pc_b_qual_apple #amount of  B-quality apples [kg]
  
  Juice_apple_yield <- Lower_qual_apple_yield * (1-Pc_b_qual_apple) #Rest of the apple yield can be sold as juicing apples at lowest price 
  
  #The benefits from table apples and juice apples are calculated by multiplying their yields by their respective prices  
  Table_apple_benefit <- Table_apple_yield * table_apple_price_p
  B_qual_apple_benefit <- B_qual_table_apple_yield * bqual_apple_price_p
  Juice_apple_benefit <-  Juice_apple_yield * juice_apple_price_p
  
  AF_apple_benefit <- Table_apple_benefit + B_qual_apple_benefit + Juice_apple_benefit
  
  # to calculate diff in uncertainty in price for a financial scheme
  
  table_apple_price_market <- vv(table_apple_price_p, var_CV = var_cv_p, n_years_c)
  B_qual_apple_price_market <- vv(bqual_apple_price_p, var_CV = var_cv_p, n_years_c)
  Juice_apple_price_market <- vv(juice_apple_price_p, var_CV = var_cv_p, n_years_c)
  
  #Arable crop component in AF system ####
  
  AF_maize_yield <- rep(0, n_years_c)
  AF_wheat_yield <- rep(0, n_years_c)
  AF_barley_yield <- rep(0, n_years_c)
  AF_rapeseed_yield <- rep(0, n_years_c)
  
  # account for yield reduction due to shading and competition from trees 
  perc_yield_reduction <- gompertz_yield(
    max_harvest = yield_reduc_max_p,
    time_to_first_yield_estimate = time_to_first_reduction_c,
    time_to_second_yield_estimate = time_to_second_reduction_c,
    first_yield_estimate_percent = perc_max_first_reduction_p,
    second_yield_estimate_percent = perc_max_second_reduction_p,
    n_years = n_years_c)
  
  #Crop rotation in AF system
  
  #Calculating what percentage of arable field remains in AF system.
  #This way, the calculated values from the treeless system (Treeless_maize_yield, Treeless_maize_fertilizer_cost etc.) can be used to calculate the values for the AF system.
  #This ensures, that exact same values for the variables from the input table are used in the calculations, which ensures max. comparability between scenarios.
  AF_arable_area_perc <- (arable_area_treeless_c - tree_row_area_c)/arable_area_treeless_c 
  
  
  AF_maize_yield <- Treeless_maize_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather *(1 - perc_yield_reduction)
  AF_maize_benefit <- AF_maize_yield * maize_value_p
  
  AF_wheat_yield <- Treeless_wheat_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather*(1 - perc_yield_reduction)
  AF_wheat_benefit <- AF_wheat_yield * wheat_value_p
  
  AF_barley_yield <- Treeless_barley_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather*(1 - perc_yield_reduction)
  AF_barley_benefit<- AF_barley_yield * barley_value_p
  
  AF_rapeseed_yield <- Treeless_rapeseed_yield*AF_arable_area_perc*AF_yield_reduction_due_to_weather*(1 - perc_yield_reduction)
  AF_rapeseed_benefit <- AF_rapeseed_yield * rapeseed_value_p
  
  #Financial Support ####
  # list of available funding scalars
  # annual_external_support_c
  
  # Annual Subsidy for AF system
  ES3_subsidy <- rep(0, n_years_c)
  # ES3_subsidy[1:n_years_c] <- es3_subsidy * tree_row_area_c
  # annual_funding_schemes_c <- annual_funding_schemes_c %||% 0
  ES3_subsidy[1:n_years_c] <- (annual_funding_schemes_c %||% 0) * tree_row_area_c
  
  AF_hedge_treerow_funding <- rep(0, n_years_c)
  AF_hedge_treerow_funding[1:n_years_c] <- (annual_funding_per_m_schemes_c %||% 0) * (tree_row_length_c / 200)# since the unit is per 200m
  
  AF_per_tree_funding <- rep(0, n_years_c)
  AF_per_tree_funding [1:n_years_c] <- (annual_funding_per_tree_schemes_c %||% 0) * num_trees_c
  
  # Special Regional Subsidy
  LEADER_subsidy <- rep(0, n_years_c)
  # funding_onetime_schemes_c <- funding_onetime_schemes_c %||% 0
  LEADER_subsidy[1] <- funding_onetime_schemes_c
  
  # external support
  Annual_external_support <- rep((tree_row_area_c * (annual_external_support_c %||% 0)),  n_years_c)
  Onetime_external_support <- rep(0, n_years_c)
  Onetime_external_support[1] <- onetime_external_support_c
  
  
  #Calculating costs in AF system ####
  #First creating vector, with as many zeros as there are years in the simulation, indicated by the value of "n_years_c" in the input table
  #Then, filling the vector with the cost-specific values
  #Adding up all costs to generate a value for the total investment cost
  
  #AF investment costs with relevant funding
  
  #Planning and consulting
  AF_planning_cost <- rep(0, n_years_c) #Invoice of service provider (planners/consultants with fudning to support consultation), planning the AF system [€] + amount of time spent by the farmer to conceptualize the system
  gov_onetime_percentage_consult <- pmin(pmax(funding_onetime_percentage_consult_schemes_c %||% 0, 0), 1)
  extern_onetime_percentage_consult <- pmin(pmax(onetime_external_percentage_consult_schemes_c %||% 0, 0), 1)
  consult_cost_net <- planning_consulting_p * (1 - gov_onetime_percentage_consult) * (1 - extern_onetime_percentage_consult)
  AF_planning_cost[1] <- pmax(0, consult_cost_net) + farmer_planning_time_p * labour_cost_p
  
  AF_pruning_course <- rep(0, n_years_c) #Costs of the pruning training of one employee [€]
  AF_pruning_course[1] <- pruning_course_p
  
  #Field prep
  AF_gps_measuring <- rep(0, n_years_c) #First step of implementation: measuring tree strips using GPS[€]
  AF_gps_measuring[1] <- gps_field_measuring_p * labour_cost_p
  
  AF_dig_plant_holes <- rep(0, n_years_c) #Second step of implementation: digging/drilling holes for the trees [€]
  AF_dig_plant_holes[1] <- dig_planting_holes_p * labour_cost_p
  
  AF_tree_cost <- rep(0, n_years_c) #Cost per tree [€]
  AF_tree_cost[1] <- pmax(0,((appletree_price_p * num_trees_c) -(funding_onetime_per_tree_schemes_c * num_trees_c)))
  
  AF_plant_tree_cost <- rep(0, n_years_c) #Labour cost for planting one tree [€]
  AF_plant_tree_cost[1] <- planting_trees_p * labour_cost_p
  
  AF_guard_cost <- rep(0, n_years_c)
  guard_cost_per_tree <- vole_protection_p + deer_protection_p
  effective_subsidy_per_tree <- pmin(funding_onetime_guard_per_tree_schemes_c, guard_cost_per_tree)
  AF_guard_cost[1] <- (guard_cost_per_tree - effective_subsidy_per_tree) * num_trees_c
  
  AF_weed_protect_cost <- rep(0, n_years_c) #Material cost of weed suppressing fleece [€]
  AF_weed_protect_cost[1] <- weed_protection_p * num_trees_c
  
  AF_compost_cost <- rep(0, n_years_c) #Cost of compost used during planting [€]
  AF_compost_cost[1] <- compost_after_planting_p * compost_price_c * num_trees_c
  
  AF_irrigation_system_cost <- rep(0, n_years_c) #Material and labour cost of installing a drip irrigation system in the tree rows [€]
  AF_irrigation_system_cost[1] <- irrigation_sys_install_p
  
  Irrigation_repair_indices <- sample(1:n_years_c, size = round(n_years_c * chance_irrigation_repair_p), replace = FALSE)
  
  AF_irrigation_system_repair_cost <- rep(0, n_years_c)
  AF_irrigation_system_repair_cost[Irrigation_repair_indices] <- AF_irrigation_system_cost[1] * irrigation_repair_cost_p
  
  AF_irrigation_after_planting_cost <- rep(0, n_years_c) #Cost for watering in newly planted trees [€]
  AF_irrigation_after_planting_cost[1] <- irrigation_after_planting_p * water_price_p * num_trees_c
  
  AF_processing_facility_cost <- rep(0, n_years_c)
  AF_processing_facility_cost[1] <- processing_sys_install_p
  
  #one time funding application cost
  LEADER_application <- rep(0, n_years_c)
  LEADER_application[1] <- leader_application_p * labour_cost_p
  
  #funding onetime for tree row as windbreak
  AF_funding_as_windbreak <- rep(0, n_years_c)
  AF_funding_as_windbreak[1] <- funding_onetime_per_m_treerow_schemes_c * tree_row_length_c
  
  #funidng onetime for hedgerows
  AF_funding_as_hedgerow <- rep(0, n_years_c)
  AF_funding_as_hedgerow[1] <- funding_onetime_per_m_hedgerow_schemes_c * tree_row_length_c
  
  AF_planting_cost <- AF_gps_measuring + AF_dig_plant_holes + AF_tree_cost + AF_plant_tree_cost + AF_guard_cost + AF_weed_protect_cost + AF_compost_cost + AF_irrigation_system_cost + AF_irrigation_after_planting_cost + AF_processing_facility_cost  #All costs associated with planting
  
  # Onetime funding per ha 
  AF_onetime_per_ha_subsidy <- rep(0, n_years_c)
  AF_onetime_per_ha_subsidy[1] <- (funding_onetime_per_ha_schemes_c %||% 0) * tree_row_area_c
  
  # Other one time % funding
  # Safe % helpers (assumes % is defined between 0 to 1; clamps just in case)
  gov_onetime_percentage_initial_cost <- pmin(pmax(funding_onetime_percentage_initial_cost_schemes_c %||% 0, 0), 1)
  extern_onetime_percentage_initial_cost <- pmin(pmax(onetime_external_percentage_incost_schemes_c %||% 0, 0), 1)
  # % supports sequential (no double-paying same invoice)
  AF_planting_cost_after_pct <- AF_planting_cost * (1 - gov_onetime_percentage_initial_cost) * (1 - extern_onetime_percentage_initial_cost)
  # subtract any fixed annual € supports (windbreak, hedgerow etc.)
  AF_planting_cost_after_fixed <- AF_planting_cost_after_pct -
    (AF_funding_as_windbreak + AF_funding_as_hedgerow + AF_onetime_per_ha_subsidy)
  # don't allow negative costs
  AF_total_planting_cost <- pmax(0, AF_planting_cost_after_fixed)
  
  AF_investment_cost <- AF_planning_cost + AF_pruning_course + AF_total_planting_cost + LEADER_application  #Investment cost of AF system implementation
  
  AF_total_investment_cost <- pmax(0, AF_investment_cost - (LEADER_subsidy[1] + Onetime_external_support[1]))
  
  
  #Running costs
  ES3_application <- rep(0, n_years_c) #Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  ES3_application[1] <- es3_application_p * labour_cost_p #application for Eco Scheme subsidy has to be repeated annually, but the first time takes considerably longer, since utilisation concept has to be established
  ES3_application[2:n_years_c] <- es3_application_p*0.1 * labour_cost_p
  
  Digital_tool_subscription <- rep(0, n_years_c)
  Digital_tool_subscription [1:n_years_c] <- digital_tool_subscripion_p
  
  AF_pruning <- rep(0, n_years_c)#Labour cost of pruning fruit trees [€]
  AF_pruning[1:5] <- pruning_juv1_p * labour_cost_p * num_trees_c
  AF_pruning[6:10] <- pruning_juv2_p * labour_cost_p * num_trees_c
  AF_pruning[11:15] <- pruning_adult1_p * labour_cost_p * num_trees_c
  AF_pruning[16:n_years_c] <- pruning_adult2_p * labour_cost_p * num_trees_c
  AF_pruning <- AF_pruning[1:n_years_c]
  
  AF_root_pruning <- rep(0, n_years_c) #Labour cost of pruning roots of trees next to tree rows [€]
  AF_root_pruning[1:n_years_c] <- root_pruning_p * labour_cost_p
  
  AF_annual_irrigation <- rep(0, n_years_c) #Cost of annual irrigation of tree rows [€]
  AF_annual_irrigation[1:3] <- irrigation_123_p
  AF_annual_irrigation[4:n_years_c] <- irrigation_annual_p
  AF_annual_irrigation_cost <- AF_annual_irrigation * water_price_p
  
  AF_codling_moth_protect <- rep(0, n_years_c) #Cost of hanging up pheromone dispensers for codling moth control (includes material and labour cost) [€]
  AF_codling_moth_protect[1:4] <- 0
  AF_codling_moth_protect[4:n_years_c] <- codling_moth_protect_p * tree_row_area_c
  
  AF_mowing_treerow <- rep(0, n_years_c) #Labour cost of mowing the tree rows manually [€]
  AF_mowing_treerow[1:n_years_c] <- mowing_treerow_p * tree_row_area_c * labour_cost_p
  
  AF_processing_facility_running_cost <- rep(0, n_years_c)
  AF_processing_facility_running_cost <- vv(processing_annual_p, var_cv_p, n_years_c, relative_trend=15)
  
  AF_apple_harvest <- rep(0, n_years_c) #Labour cost of harvesting apples manually [€/kg]
  
  AF_apple_harvest[1:n_years_c] <- apple_harvest_p * AF_tot_apple_yield #cost calculated per kg of apple * total amount of apples in kg
  
  AF_total_treerow_management_cost <- ES3_application + AF_pruning + AF_root_pruning + AF_annual_irrigation_cost + AF_codling_moth_protect + AF_mowing_treerow + AF_apple_harvest + AF_processing_facility_running_cost + Digital_tool_subscription
  
  #Management cost of arable component in AF system
  
  #Maize
  AF_maize_sowing_cost <- Treeless_maize_sowing_cost * AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_maize_fertilizer_cost <- Treeless_maize_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_maize_pesticide_cost <- Treeless_maize_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_maize_machinery_cost <- Treeless_maize_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_maize_insurance_cost <- Treeless_maize_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  
  #Labour cost associated with maize cultivation in AF.
  #Total labour time is estimated to increase by 5-30% communicated by the farmer (extra_arable_time_p/100) due to complicated navigation of machinery in AF system.
  AF_maize_labour_cost <- rep(0, n_years_c)
  AF_maize_labour_cost <- (Maize_labour + Maize_labour * (extra_arable_time_p/100)) * labour_cost_p * (arable_area_treeless_c*AF_arable_area_perc)
  
  AF_total_maize_cost <- AF_maize_sowing_cost + AF_maize_fertilizer_cost + AF_maize_pesticide_cost + AF_maize_machinery_cost + AF_maize_insurance_cost + AF_maize_labour_cost
  
  #Wheat
  AF_wheat_sowing_cost <- Treeless_wheat_sowing_cost*AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_wheat_fertilizer_cost <- Treeless_wheat_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_wheat_pesticide_cost <- Treeless_wheat_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_wheat_machinery_cost <- Treeless_wheat_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_wheat_insurance_cost <- Treeless_wheat_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  AF_wheat_labour_cost <- rep(0, n_years_c)
  AF_wheat_labour_cost <- (Wheat_labour + Wheat_labour * (vv(extra_arable_time_p, var_cv_p, n_years_c)/100)) * labour_cost_p * (arable_area_treeless_c*AF_arable_area_perc) #Labour cost associated with wheat cultivation
  
  AF_total_wheat_cost <- AF_wheat_sowing_cost + AF_wheat_fertilizer_cost + AF_wheat_pesticide_cost + AF_wheat_machinery_cost + AF_wheat_insurance_cost + AF_wheat_labour_cost
  
  #Barley
  AF_barley_sowing_cost <- Treeless_barley_sowing_cost*AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_barley_fertilizer_cost <- Treeless_barley_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_barley_pesticide_cost <- Treeless_barley_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_barley_machinery_cost <- Treeless_barley_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_barley_insurance_cost <- Treeless_barley_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  AF_barley_labour_cost <- rep(0, n_years_c)
  AF_barley_labour_cost <- (Barley_labour + Barley_labour * (vv(extra_arable_time_p, var_cv_p, n_years_c)/100)) * labour_cost_p * (arable_area_treeless_c*AF_arable_area_perc) #Labour cost associated with barley cultivation
  
  AF_total_barley_cost <- AF_barley_sowing_cost + AF_barley_fertilizer_cost + AF_barley_pesticide_cost + AF_barley_machinery_cost + AF_barley_insurance_cost + AF_barley_labour_cost
  
  #Rapeseed
  AF_rapeseed_sowing_cost <- Treeless_rapeseed_sowing_cost*AF_arable_area_perc #cost of seed [€/ha]*area managed [ha]
  AF_rapeseed_fertilizer_cost <- Treeless_rapeseed_fertilizer_cost*AF_arable_area_perc #cost of fertilizer [€/ha]*area managed [ha]
  AF_rapeseed_pesticide_cost <- Treeless_rapeseed_pesticide_cost*AF_arable_area_perc #cost of pesticides [€/ha]*area managed [ha]
  AF_rapeseed_machinery_cost <- Treeless_rapeseed_machinery_cost*AF_arable_area_perc #cost of machinery [€/ha]*area managed [ha]
  AF_rapeseed_insurance_cost <- Treeless_rapeseed_insurance_cost*AF_arable_area_perc #cost of insurance [€/ha]*area managed [ha]
  AF_rapeseed_labour_cost <- rep(0, n_years_c)
  AF_rapeseed_labour_cost <- (Rapeseed_labour + Rapeseed_labour * (vv(extra_arable_time_p, var_cv_p, n_years_c)/100)) * labour_cost_p * (arable_area_treeless_c*AF_arable_area_perc) #Labour cost associated with rapeseed cultivation
  
  AF_total_rapeseed_cost <- AF_rapeseed_sowing_cost + AF_rapeseed_fertilizer_cost + AF_rapeseed_pesticide_cost + AF_rapeseed_machinery_cost + AF_rapeseed_insurance_cost + AF_rapeseed_labour_cost
  
  
  AF_total_arable_management_cost <- AF_total_maize_cost + AF_total_wheat_cost + AF_total_barley_cost + AF_total_rapeseed_cost #Total cost of arable component in AF system
  
  AF_total_running_cost <- AF_total_treerow_management_cost + AF_total_arable_management_cost #Total running cost of AF system
  
  AF_total_cost <- AF_total_investment_cost + AF_total_running_cost #Total cost of AF system
  
  #Scenario 1: Agroforestry bottom line with ES3 + regional one-time funding (LEADER Region Steinfurter Land) ####
  #Framer out-of-pocket investment
  #AF_farmer_capital <- AF_total_investment_cost
  
  AF_total_benefit <- AF_apple_benefit + AF_maize_benefit + AF_wheat_benefit + AF_barley_benefit + AF_rapeseed_benefit + ES3_subsidy + AF_hedge_treerow_funding + AF_per_tree_funding + Annual_external_support 
  
  AF_bottom_line_benefit <- AF_total_benefit - AF_total_cost
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  #AF System
  AF_NPV <- discount(AF_bottom_line_benefit, discount_rate=discount_rate_p,
                     calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow <- discount(AF_bottom_line_benefit,discount_rate=discount_rate_p,
                           calculate_NPV = FALSE)# Cash flow of AF system
  AF_cum_cash_flow <- cumsum(AF_cash_flow) #Cumulative cash flow of AF system
  
  Treeless_cash_flow <- discount(Treeless_bottom_line_benefit,discount_rate=discount_rate_p,
                                 calculate_NPV = FALSE)#Cash flow of AF system
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of AF system
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  Decision_benefit <- AF_bottom_line_benefit - Treeless_bottom_line_benefit
  NPV_decision <- discount(Decision_benefit, discount_rate = discount_rate_p,
                           calculate_NPV = TRUE ) #NPV of the decision
  CF_decision <- discount(Decision_benefit, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision <- cumsum(CF_decision) #Cumulative cash flow of the decision
  #-----------------------------------------------------------------------------------------------------------  
  
  #CREATING THE FUNDING SCENARIOS#######################################################################
  #The default scenario includes ES 3
  
  #Scenario 2: Where the farmer funds the establishment of AF by taking a bank loan (partially or fully) ####
  #Loan cannot exceed the year-1 investment outflow we are financing
  # bank_draw <- min(bank_loan_amount_c, AF_total_investment_cost[1])
  # # Adjust investment cost vector (reduce year-1 outflow by the borrowed amount)
  # AF_total_investment_cost_adj_bank <- AF_total_investment_cost
  # AF_total_investment_cost_adj_bank[1] <- max(AF_total_investment_cost[1] - bank_draw, 0)
  # #repayment vector (scaled so outstanding balance hits ~0 by maturity)
  # bank_loan_repayment_vector <- rep(0, n_years_c)
  # bank_repayment_end_year <-
  #   if (!is.null(bank_maturity_year_c) && !is.na(bank_maturity_year_c)) bank_maturity_year_c else n_years_c
  # bank_repayment_end_year <- min(bank_repayment_end_year, n_years_c)
  # # allow a slightly increasing repayment profile using vv + cummax
  # bank_annual_repayment_amount_p_adj <-
  #   cummax(vv(bank_annual_repayment_amount_p, 10, bank_repayment_end_year, relative_trend = 10))
  # bank_payoff_function <- function(scale_factor) {
  #   bal <- bank_draw
  #   for (yr in seq_len(bank_repayment_end_year)) {
  #     repay <- if (yr >= bank_repayment_start_year_c) scale_factor * bank_annual_repayment_amount_p_adj[yr] else 0
  #     bal <- bal * (1 + bank_interest_rate_c) - repay
  #   }
  #   bal
  # }
  # 
  # lower_bound <- 0
  # upper_bound <- max(bank_draw, 1e-6)
  # while (bank_payoff_function(upper_bound) > 0) upper_bound <- upper_bound * 2
  # 
  # bank_scaling_factor <- uniroot(bank_payoff_function,
  #                                lower = lower_bound,
  #                                upper = upper_bound * (1 + bank_interest_rate_c)^(bank_repayment_end_year))$root
  # 
  # # fill repayment vector from start year to end year
  # start_yr <- max(1, bank_repayment_start_year_c)
  # end_yr   <- bank_repayment_end_year
  # if (bank_draw > 0 && start_yr <= end_yr) {
  #   bank_loan_repayment_vector[start_yr:end_yr] <-
  #     bank_scaling_factor * bank_annual_repayment_amount_p_adj[start_yr:end_yr]
  # }
  # 
  # # Adjusted running + total cost (baseline structure preserved)
  # AF_total_running_cost_adj_bank <- AF_total_running_cost + bank_loan_repayment_vector
  # AF_total_cost_adj_bank <- AF_total_investment_cost_adj_bank + AF_total_running_cost_adj_bank
  # 
  # # New bottom line (benefits unchanged; only costs change)
  # AF_bottom_line_benefit_adj_bank <- AF_total_benefit - AF_total_cost_adj_bank
  # 
  # #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  # AF_NPV_adj_bank <- discount(AF_bottom_line_benefit_adj_bank, discount_rate=discount_rate_p, calculate_NPV = TRUE)
  # AF_CF_adj_bank  <- discount(AF_bottom_line_benefit_adj_bank, discount_rate=discount_rate_p, calculate_NPV = FALSE)
  # AF_CCF_adj_bank <- cumsum(AF_CF_adj_bank)
  # 
  # #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  # Decision_benefit_adj_bank <- AF_bottom_line_benefit_adj_bank - Treeless_bottom_line_benefit
  # NPV_decision_adj_bank <- discount(Decision_benefit_adj_bank, discount_rate = discount_rate_p,calculate_NPV = TRUE ) #NPV of the decision
  # CF_decision_adj_bank <- discount(Decision_benefit_adj_bank, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  # CumCF_decision_adj__bank <- cumsum(CF_decision_adj_bank) #Cumulative cash flow of the decision
  
  # Scenario 2: Traditional bank loan ####
  if (isTRUE(use_bank_loan)) {
    
    bank_draw <- min(bank_loan_amount_c, AF_total_investment_cost[1])
    
    bank_loan_repayment_vector <- rep(0, n_years_c)
    
    bank_repayment_end_year <-
      if (!is.null(bank_maturity_year_c) && !is.na(bank_maturity_year_c)) bank_maturity_year_c else n_years_c
    bank_repayment_end_year <- min(bank_repayment_end_year, n_years_c)
    
    bank_annual_repayment_amount_p_adj <-
      cummax(vv(bank_annual_repayment_amount_p, 10, bank_repayment_end_year, relative_trend = 10))
    
    bank_payoff_function <- function(scale_factor) {
      bal <- bank_draw
      for (yr in seq_len(bank_repayment_end_year)) {
        repay <- if (yr >= bank_repayment_start_year_c) scale_factor * bank_annual_repayment_amount_p_adj[yr] else 0
        bal <- bal * (1 + bank_interest_rate_c) - repay
      }
      bal
    }
    
    lower_bound <- 0
    upper_bound <- max(bank_draw, 1e-6)
    while (bank_payoff_function(upper_bound) > 0) upper_bound <- upper_bound * 2
    
    bank_scaling_factor <- uniroot(
      bank_payoff_function,
      lower = lower_bound,
      upper = upper_bound * (1 + bank_interest_rate_c)^(bank_repayment_end_year)
    )$root
    
    start_yr <- max(1, bank_repayment_start_year_c)
    end_yr <- bank_repayment_end_year
    
    if (bank_draw > 0 && start_yr <= end_yr) {
      bank_loan_repayment_vector[start_yr:end_yr] <-
        bank_scaling_factor * bank_annual_repayment_amount_p_adj[start_yr:end_yr]
    }
    
    bank_results <- calc_loan_scenario(
      AF_total_investment_cost = AF_total_investment_cost,
      AF_total_running_cost = AF_total_running_cost,
      AF_total_benefit = AF_total_benefit,
      Treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
      discount_rate_p = discount_rate_p,
      loan_draw = bank_draw,
      repayment_vector = bank_loan_repayment_vector
    )
    
    AF_total_investment_cost_adj_bank <- bank_results$AF_total_investment_cost_adj
    AF_total_running_cost_adj_bank    <- bank_results$AF_total_running_cost_adj
    AF_total_cost_adj_bank            <- bank_results$AF_total_cost_adj
    AF_bottom_line_benefit_adj_bank   <- bank_results$AF_bottom_line_benefit_adj
    AF_NPV_adj_bank                   <- bank_results$AF_NPV_adj
    AF_CF_adj_bank                    <- bank_results$AF_cash_flow_adj
    AF_CCF_adj_bank                   <- bank_results$AF_cum_cash_flow_adj
    Decision_benefit_adj_bank         <- bank_results$Decision_benefit_adj
    NPV_decision_adj_bank             <- bank_results$NPV_decision_adj
    CF_decision_adj_bank              <- bank_results$CF_decision_adj
    CumCF_decision_adj_bank           <- bank_results$CumCF_decision_adj
    
  } else {
    
    AF_total_investment_cost_adj_bank <- AF_total_investment_cost
    AF_total_running_cost_adj_bank    <- AF_total_running_cost
    AF_total_cost_adj_bank            <- AF_total_cost
    AF_bottom_line_benefit_adj_bank   <- AF_bottom_line_benefit
    AF_NPV_adj_bank                   <- AF_NPV
    AF_CF_adj_bank                    <- AF_cash_flow
    AF_CCF_adj_bank                   <- AF_cum_cash_flow
    Decision_benefit_adj_bank         <- Decision_benefit
    NPV_decision_adj_bank             <- NPV_decision
    CF_decision_adj_bank              <- CF_decision
    CumCF_decision_adj_bank           <- CumCF_decision
  }
  
  #Scenario 2: No funding at all. ####
  #All other scenarios contain at least the annual support through ES 3. 
  AF_total_benefit_no_fund <- AF_apple_benefit + AF_maize_benefit + AF_wheat_benefit + AF_barley_benefit + AF_rapeseed_benefit
  
  AF_total_cost_no_fund <- AF_total_cost - ES3_application #No time has to be invested into applying for ES 3 funding 
  
  AF_bottom_line_benefit_no_fund <- AF_total_benefit_no_fund - AF_total_cost_no_fund
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system without ES 3 funding
  #AF System
  AF_NPV_no_fund <- discount(AF_bottom_line_benefit_no_fund, discount_rate=discount_rate_p,
                             calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow_no_fund <- discount(AF_bottom_line_benefit_no_fund,discount_rate=discount_rate_p,
                                   calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow_no_fund <- cumsum(AF_cash_flow_no_fund) #Cumulative cash flow of AF system
  
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision
  Decision_benefit_no_fund <- AF_bottom_line_benefit_no_fund - Treeless_bottom_line_benefit
  NPV_decision_no_fund <- discount(Decision_benefit_no_fund, discount_rate = discount_rate_p,
                                   calculate_NPV = TRUE ) #NPV of the decision
  CF_decision_no_fund <- discount(Decision_benefit_no_fund, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision_no_fund <- cumsum(CF_decision_no_fund) #Cumulative cash flow of the decision
  
  # Scenario 3 "DeFAF-Subsidy" ####
  #Investment funding of 100 % for first 10 ha of wooded area, 80 % of additional 10 ha of wooded area and 50 % of every additional ha after 20 ha of total wooded area. 
  #Additionally: annual subsidy of 600 €/ha of wooded area.
  
  #DeFAF annual subsidy 
  
  DeFAF_ES3 <- annual_funding_schemes_c*3 - ES3_application #suggested level of ES3 subsidy
  
  DeFAF_annual_sub <- DeFAF_ES3
  
  #DeFAF Investment support funding scheme (first 10 ha 100 % funded, next 10 ha 80 % funded, every additional ha 50 % funded)
  
  #Calculate the investment cost per hectare
  Invest_cost_per_ha <- AF_total_investment_cost[1]/tree_row_area_c
  
  #Create a modified vector based on the conditions
  AF_total_invest_cost_DeFAF <- AF_total_investment_cost
  
  #If tree_row_area_c is equal or smaller than 10 ha, then 100 % of the investment cost is subsidised
  if (tree_row_area_c > 0 && tree_row_area_c <= 10) {
    AF_total_invest_cost_DeFAF[1] <- 0
    #If tree row area is over 10 ha, the first 10 ha are subsidised 100 %, but additional ha are subsidised differently
  } else if (tree_row_area_c > 10) {
    #Check if there's a remainder after deducting 10 hectares, since every additional ha up  to 20 ha are subsidised 80%
    Remainder <- tree_row_area_c - 10
    if (Remainder > 0 && Remainder <= 10) {
      #If remainder is smaller or equal to 10, deduct 80% of the cost for the remaining hectares
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - (10 * Invest_cost_per_ha) - (Remainder * Invest_cost_per_ha * 0.8)
    } else {
      #If remainder is greater than 10, deduct 10 hectares and the next 10 get 80% off and the rest 50% off
      AF_total_invest_cost_DeFAF[1] <- AF_total_invest_cost_DeFAF[1] - (10 * Invest_cost_per_ha) - (10 * Invest_cost_per_ha * 0.8) - ((Remainder - 10)*Invest_cost_per_ha * 0.5)
    }
  }
  
  #Ensure the modified investment cost is not negative (code above should not be able to create negative values but next line is added as insurance that negative investment cost is never included in the calculations)
  AF_total_invest_cost_DeFAF[1] <- max(0, AF_total_invest_cost_DeFAF[1])
  
  #Calculate total benefit of DeFAF-subsidy
  
  AF_total_cost_DeFAF <- AF_total_invest_cost_DeFAF + AF_total_running_cost - DeFAF_annual_sub - AF_mowing_treerow
  
  AF_bottom_line_benefit_DeFAF <- AF_total_benefit - AF_total_cost_DeFAF #Bottom line, in Scenario "DeFAF-Subsidy"
  AF_NPV_DeFAF <- discount(AF_bottom_line_benefit_DeFAF, discount_rate=discount_rate_p,
                           calculate_NPV = TRUE) #NVP of AF system, in Scenario "DeFAF-Subsidy"
  AF_cash_flow_DeFAF <- discount(AF_bottom_line_benefit_DeFAF,discount_rate=discount_rate_p,
                                 calculate_NPV = FALSE) #Cash flow of AF system, in Scenario "DeFAF-Subsidy"
  AF_cum_cash_flow_DeFAF <- cumsum(AF_cash_flow_DeFAF) #Cumulative cash flow of AF system, in Scenario "DeFAF-Subsidy"
  
  #Decision DeFAF (difference between AF system with DeFAF-subsidy and treeless baseline system)
  Decision_benefit_DeFAF <- AF_bottom_line_benefit_DeFAF - Treeless_bottom_line_benefit
  NPV_decision_DeFAF <- discount(Decision_benefit_DeFAF, discount_rate = discount_rate_p,
                                 calculate_NPV = TRUE ) #NPV of the decision, in Scenario "DeFAF-Subsidy"
  CF_decision_DeFAF <- discount(Decision_benefit_DeFAF, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision, in Scenario "DeFAF-Subsidy"
  CumCF_decision_DeFAF <- cumsum(CF_decision_DeFAF) #Cumulative cash flow of the decision, in Scenario "DeFAF-Subsidy"
  
  #Scenario 4: Agroforestry ES3 + LEADER Region Steinfurter Land + EMEA'S Impact investment####
  AF_total_investment_cost_adj_impact <- AF_total_investment_cost - impact_invst_fund_loan_c
  # But investment cost cannot go negative
  AF_total_investment_cost_adj_impact <- max(AF_total_investment_cost_adj_impact, 0)
  impact_loan_repayment_vector <- rep(0, n_years_c)
  # clear the loan within the user selected simulation time
  impact_repayment_end_year <-
    if (!is.null(impact_invst_bank_maturity_year_c) &&
        !is.na(impact_invst_bank_maturity_year_c)) {
      impact_invst_bank_maturity_year_c
    } else {
      n_years_c
    }
  impact_loan_repayment_years <- impact_repayment_end_year - impact_invst_fund_repayment_start_year_c + 1
  impact_invst_annual_repayment_amount_p_adj <- cummax(vv(impact_invst_annual_repayment_amount_p,10, impact_repayment_end_year, relative_trend = 10))
  impact_invst_payoff_function <- function(scale_factor) {
    impact_invst_balance <- impact_invst_fund_loan_c
    for (repayment in seq_len(impact_repayment_end_year)){
      impact_invst_balance <- impact_invst_balance * (1 + impact_invst_fund_interest_rate_c) - scale_factor * impact_invst_annual_repayment_amount_p_adj[repayment]
    }
    impact_invst_balance
  }
  lower_bound <-0
  upper_bound <- impact_invst_fund_loan_c
  while (impact_invst_payoff_function(upper_bound)>0) { upper_bound <- upper_bound*2}
  impact_scaling_factor <-  uniroot(impact_invst_payoff_function,lower = lower_bound,upper = upper_bound *
                                      (1 + impact_invst_fund_interest_rate_c)^impact_loan_repayment_years)$root

  impact_loan_repayment_vector[impact_invst_fund_repayment_start_year_c:impact_repayment_end_year] <- impact_scaling_factor * impact_invst_annual_repayment_amount_p_adj

  #Updated Total Running Cost
  AF_total_running_cost_adj_impact <- AF_total_running_cost + impact_loan_repayment_vector
  #Updated Total Cost
  AF_total_cost_adj_impact <- AF_total_investment_cost_adj_impact + AF_total_running_cost_adj_impact
  AF_total_benefit <- AF_apple_benefit + AF_maize_benefit + AF_wheat_benefit + AF_barley_benefit + AF_rapeseed_benefit + ES3_subsidy + LEADER_subsidy + AF_hedge_treerow_funding + AF_per_tree_funding
  #Updated Bottom Line Benefit
  AF_bottom_line_benefit_adj_impact <- AF_total_benefit - AF_total_cost_adj_impact

  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  #AF System
  AF_NPV_adj_impact <- discount(AF_bottom_line_benefit_adj_impact, discount_rate=discount_rate_p,
                                calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow_adj_impact <- discount(AF_bottom_line_benefit_adj_impact,discount_rate=discount_rate_p,
                                      calculate_NPV = FALSE)#Cash flow of AF system
  AF_cum_cash_flow_adj_impact <- cumsum(AF_cash_flow_adj_impact) #Cumulative cash flow of AF system


  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  Decision_benefit_adj_impact <- AF_bottom_line_benefit_adj_impact - Treeless_bottom_line_benefit
  NPV_decision_adj_impact <- discount(Decision_benefit_adj_impact, discount_rate = discount_rate_p,
                                      calculate_NPV = TRUE ) #NPV of the decision
  CF_decision_adj_impact <- discount(Decision_benefit_adj_impact, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision_adj_impact <- cumsum(CF_decision_adj_impact) #Cumulative cash flow of the decision
  
  # Scenario 4: Impact investment ####
  if (isTRUE(use_impact_loan)) {
    
    impact_draw <- min(impact_invst_fund_loan_c, AF_total_investment_cost[1])
    impact_loan_repayment_vector <- rep(0, n_years_c)
    
    impact_repayment_end_year <-
      if (!is.null(impact_invst_bank_maturity_year_c) && !is.na(impact_invst_bank_maturity_year_c)) {
        impact_invst_bank_maturity_year_c
      } else {
        n_years_c
      }
    
    impact_loan_repayment_years <- impact_repayment_end_year - impact_invst_fund_repayment_start_year_c + 1
    
    impact_invst_annual_repayment_amount_p_adj <-
      cummax(vv(impact_invst_annual_repayment_amount_p, 10, impact_repayment_end_year, relative_trend = 10))
    
    impact_invst_payoff_function <- function(scale_factor) {
      impact_invst_balance <- impact_draw
      for (repayment in seq_len(impact_repayment_end_year)) {
        impact_invst_balance <-
          impact_invst_balance * (1 + impact_invst_fund_interest_rate_c) -
          scale_factor * impact_invst_annual_repayment_amount_p_adj[repayment]
      }
      impact_invst_balance
    }
    
    lower_bound <- 0
    upper_bound <- max(impact_draw, 1e-6)
    while (impact_invst_payoff_function(upper_bound) > 0) {
      upper_bound <- upper_bound * 2
    }
    
    impact_scaling_factor <- uniroot(
      impact_invst_payoff_function,
      lower = lower_bound,
      upper = upper_bound * (1 + impact_invst_fund_interest_rate_c)^impact_loan_repayment_years
    )$root
    
    impact_loan_repayment_vector[
      impact_invst_fund_repayment_start_year_c:impact_repayment_end_year
    ] <- impact_scaling_factor *
      impact_invst_annual_repayment_amount_p_adj[
        impact_invst_fund_repayment_start_year_c:impact_repayment_end_year
      ]
    
    impact_results <- calc_loan_scenario(
      AF_total_investment_cost = AF_total_investment_cost,
      AF_total_running_cost = AF_total_running_cost,
      AF_total_benefit = AF_total_benefit,
      Treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
      discount_rate_p = discount_rate_p,
      loan_draw = impact_draw,
      repayment_vector = impact_loan_repayment_vector
    )
    
    AF_total_investment_cost_adj_impact <- impact_results$AF_total_investment_cost_adj
    AF_total_running_cost_adj_impact    <- impact_results$AF_total_running_cost_adj
    AF_total_cost_adj_impact            <- impact_results$AF_total_cost_adj
    AF_bottom_line_benefit_adj_impact   <- impact_results$AF_bottom_line_benefit_adj
    AF_NPV_adj_impact                   <- impact_results$AF_NPV_adj
    AF_cash_flow_adj_impact             <- impact_results$AF_cash_flow_adj
    AF_cum_cash_flow_adj_impact         <- impact_results$AF_cum_cash_flow_adj
    Decision_benefit_adj_impact         <- impact_results$Decision_benefit_adj
    NPV_decision_adj_impact             <- impact_results$NPV_decision_adj
    CF_decision_adj_impact              <- impact_results$CF_decision_adj
    CumCF_decision_adj_impact           <- impact_results$CumCF_decision_adj
    
  } else {
    
    AF_total_investment_cost_adj_impact <- AF_total_investment_cost
    AF_total_running_cost_adj_impact    <- AF_total_running_cost
    AF_total_cost_adj_impact            <- AF_total_cost
    AF_bottom_line_benefit_adj_impact   <- AF_bottom_line_benefit
    AF_NPV_adj_impact                   <- AF_NPV
    AF_cash_flow_adj_impact             <- AF_cash_flow
    AF_cum_cash_flow_adj_impact         <- AF_cum_cash_flow
    Decision_benefit_adj_impact         <- Decision_benefit
    NPV_decision_adj_impact             <- NPV_decision
    CF_decision_adj_impact              <- CF_decision
    CumCF_decision_adj_impact           <- CumCF_decision
  }
  
  ### Scenario 5.1: Agroforestry ES3 + LEADER Region Steinfurter Land + Risk Mitigation Instruments -> Guaranteed Fund ----
  
  ### Scenario 5: Agroforestry ES3 + LEADER + Risk Mitigation Instruments ----
  ### Supports guarantee fund, insurance, or both together
  
  # Replace these with the actual input variables coming from your UI / Excel
  use_guarantee <- isTRUE(risk_mitigation_guarantee_c == 1) || isTRUE(risk_mitigation_guarantee_c)
  use_insurance <- isTRUE(risk_mitigation_insurance_c == 1) || isTRUE(risk_mitigation_insurance_c)
  
  risk_mit_results <- calc_risk_mitigation_scenario(
    AF_total_investment_cost = AF_total_investment_cost,
    AF_total_running_cost = AF_total_running_cost,
    AF_total_benefit = AF_total_benefit,
    Treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
    discount_rate_p = discount_rate_p,
    n_years_c = n_years_c,
    Apple_yield_reduction_due_to_weather = Apple_yield_reduction_due_to_weather,
    guarantee_amount_c = guarantee_amount_c,
    insurance_cover_rate_c = insurance_cover_rate_c,
    insurance_payout_amount_c = insurance_payout_amount_c,
    insurance_annual_premium_c = insurance_annual_premium_c,
    insurance_annual_premium_surcharge_c = insurance_annual_premium_surcharge_c,
    use_guarantee = use_guarantee,
    use_insurance = use_insurance
  )
  
  AF_total_investment_cost_adj_risk_mit <- risk_mit_results$AF_total_investment_cost_adj
  AF_total_running_cost_adj_risk_mit    <- risk_mit_results$AF_total_running_cost_adj
  AF_total_benefit_adj_risk_mit         <- risk_mit_results$AF_total_benefit_adj
  AF_total_cost_adj_risk_mit            <- risk_mit_results$AF_total_cost_adj
  
  insurance_payout                      <- risk_mit_results$insurance_payout
  insurance_premium_vec                 <- risk_mit_results$insurance_premium_vec
  
  AF_bottom_line_benefit_adj_risk_mit   <- risk_mit_results$AF_bottom_line_benefit_adj
  AF_NPV_adj_risk_mit                   <- risk_mit_results$AF_NPV_adj
  AF_cash_flow_adj_risk_mit             <- risk_mit_results$AF_cash_flow_adj
  AF_cum_cash_flow_adj_risk_mit         <- risk_mit_results$AF_cum_cash_flow_adj
  
  Decision_benefit_adj_risk_mit         <- risk_mit_results$Decision_benefit_adj
  NPV_decision_adj_risk_mit             <- risk_mit_results$NPV_decision_adj
  CF_decision_adj_risk_mit              <- risk_mit_results$CF_decision_adj
  CumCF_decision_adj_risk_mit           <- risk_mit_results$CumCF_decision_adj
  
  ### Scenario 6: Agroforestry ES3 + LEADER + Partnerships - Startups & Digital Tools ----
  
  # --- 1) Adjust planning/report cost in YEAR 1 only (component-level) ---
  AF_planning_cost_adj_partners <- AF_planning_cost
  AF_planning_cost_adj_partners[1] <- pmax(AF_planning_cost[1] - reduction_planning_cost_c, 0)
  
  # Rebuild investment cost vector using adjusted planning cost
  AF_investment_cost_adj_partners <- AF_investment_cost
  AF_investment_cost_adj_partners[1] <-AF_planning_cost_adj_partners[1] + AF_pruning_course[1] + AF_total_planting_cost[1] + LEADER_application[1]
  
  # one-time subsidies / external support like baseline (keep your existing logic)
  AF_total_investment_cost_adj_partners <- pmax(0,AF_investment_cost_adj_partners - (LEADER_subsidy[1] + Onetime_external_support[1]))
  
  # digital tool subscription EACH YEAR (component-level) ---
  Digital_tool_subscription_adj_partners <- rep(0, n_years_c)
  Digital_tool_subscription_adj_partners[1:n_years_c] <-
    pmax(digital_tool_subscripion_p - reduction_digital_tool_subscription_c, 0)
  
  # Replace only the subscription inside treerow management cost
  AF_total_treerow_management_cost_adj_partners <-
    AF_total_treerow_management_cost - Digital_tool_subscription + Digital_tool_subscription_adj_partners
  
  # Rebuild total running cost (treerow + arable)
  AF_total_running_cost_adj_partners <-
    pmax(0, AF_total_treerow_management_cost_adj_partners + AF_total_arable_management_cost)
  
  # 
  AF_total_cost_adj_partners <- AF_total_investment_cost_adj_partners + AF_total_running_cost_adj_partners
  AF_bottom_line_benefit_adj_partners <- AF_total_benefit - AF_total_cost_adj_partners
  
  AF_NPV_adj_partners <- discount(AF_bottom_line_benefit_adj_partners, discount_rate=discount_rate_p, calculate_NPV = TRUE)
  AF_cash_flow_adj_partners <- discount(AF_bottom_line_benefit_adj_partners, discount_rate=discount_rate_p, calculate_NPV = FALSE)
  AF_cum_cash_flow_adj_partners <- cumsum(AF_cash_flow_adj_partners)
  
  Decision_benefit_adj_partners <- AF_bottom_line_benefit_adj_partners - Treeless_bottom_line_benefit
  NPV_decision_adj_partners <- discount(Decision_benefit_adj_partners, discount_rate = discount_rate_p, calculate_NPV = TRUE)
  CF_decision_adj_partners <- discount(Decision_benefit_adj_partners, discount_rate = discount_rate_p, calculate_NPV = FALSE)
  CumCF_decision_adj_partners <- cumsum(CF_decision_adj_partners)
  
  
  ### Scenario 7: Agroforestry ES3 + LEADER Region Steinfurter Land + Advisory Services Support----
  #Adjust the affected cost components
  AF_planning_cost_adj_advisory <- AF_planning_cost * 0.20              # 80% reduction
  LEADER_application_adj_advisory <- LEADER_application * 0.10          # 90% reduction
  ES3_application_adj_advisory <- ES3_application * 0.10                # 90% reduction
  # Advisory fees
  Advisory_fee_onetime <- rep(0, n_years_c)
  Advisory_fee_onetime[1] <- advisory_service_cost_onetime_c
  Advisory_fee_annual <- rep(advisory_service_cost_annual_c, n_years_c)
  
  # Adjusted investment cost (adjusted planning + adjusted LEADER app + add one-time fee)
  AF_total_investment_cost_adj_advisory <-AF_pruning_course +AF_total_planting_cost + AF_planning_cost_adj_advisory +LEADER_application_adj_advisory +Advisory_fee_onetime
  
  # treerow management running costs
  AF_total_treerow_management_cost_adj_advisory <- (AF_total_treerow_management_cost - ES3_application) +ES3_application_adj_advisory +Advisory_fee_annual
  
  AF_total_running_cost_adj_advisory <- AF_total_treerow_management_cost_adj_advisory +AF_total_arable_management_cost
  AF_total_cost_adj_advisory <- AF_total_investment_cost_adj_advisory + AF_total_running_cost_adj_advisory
  # NPV Calculation
  AF_bottom_line_benefit_adj_advisory <- AF_total_benefit - AF_total_cost_adj_advisory
  
  AF_NPV_adj_advisory <- discount(AF_bottom_line_benefit_adj_advisory,discount_rate = discount_rate_p,calculate_NPV = TRUE)
  AF_CF_adj_advisory <- discount(AF_bottom_line_benefit_adj_advisory,discount_rate = discount_rate_p,calculate_NPV = FALSE)
  AF_CCF_adj_advisory <- cumsum(AF_CF_adj_advisory)
  #decision NPV
  Decision_benefit_adj_advisory <- AF_bottom_line_benefit_adj_advisory - Treeless_bottom_line_benefit
  NPV_decision_adj_advisory <- discount(Decision_benefit_adj_advisory, discount_rate = discount_rate_p,calculate_NPV = TRUE ) #NPV of the decision
  CF_decision_adj_advisory <- discount(Decision_benefit_adj_advisory, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision_adj_advisory <- cumsum(CF_decision_adj_advisory) #Cumulative cash flow of the decision
  
  ### Scenario 8: Agroforestry ES3 + LEADER Region Steinfurter Land + Development Banks Loan & Funding ----
  # Development bank loan for irrigation and processing facility only!
  # Build the financed CAPEX (vector, but typically only year 1 is non-zero)
  # Dev_bank_financed_capex <- AF_irrigation_system_cost + AF_processing_facility_cost
  # 
  # # Optional safety: don't finance more than exists (esp. if any of those are 0)
  # Dev_bank_financed_capex_year1 <- Dev_bank_financed_capex[1]
  # 
  # # Effective loan draw cannot exceed eligible capex in year 1
  # Dev_bank_draw <- min(Dev_bank_loan_amount_c, Dev_bank_financed_capex_year1)
  # 
  # # Adjust ONLY those CAPEX items in year 1 proportionally
  # # (so you don't need to guess which is bigger; it scales both down consistently)
  # AF_irrigation_system_cost_adj_Dev_bank <- AF_irrigation_system_cost
  # AF_processing_facility_cost_adj_Dev_bank <- AF_processing_facility_cost
  # 
  # if (Dev_bank_financed_capex_year1 > 0 && Dev_bank_draw > 0) {
  #   scale_down <- (Dev_bank_financed_capex_year1 - Dev_bank_draw) / Dev_bank_financed_capex_year1
  # 
  #   AF_irrigation_system_cost_adj_Dev_bank[1] <- AF_irrigation_system_cost[1] * scale_down
  #   AF_processing_facility_cost_adj_Dev_bank[1] <- AF_processing_facility_cost[1] * scale_down
  # }
  # 
  # # Rebuild planting cost + investment costs using the adjusted CAPEX pieces
  # AF_total_planting_cost_adj_Dev_bank <- AF_gps_measuring + AF_dig_plant_holes + AF_tree_cost + AF_plant_tree_cost +
  #   AF_guard_cost + AF_weed_protect_cost +  AF_compost_cost + AF_irrigation_system_cost_adj_Dev_bank +   # adjusted
  #   AF_irrigation_after_planting_cost + AF_processing_facility_cost_adj_Dev_bank   # adjusted
  # 
  # AF_total_investment_cost_adj_Dev_bank <-
  #   AF_planning_cost + AF_pruning_course + AF_total_planting_cost_adj_Dev_bank + LEADER_application
  # 
  # AF_total_investment_cost_adj_Dev_bank <- max(AF_total_investment_cost_adj_Dev_bank, 0)
  # # Dev Bank repayment schedule (same logic you already use)
  # 
  # Dev_bank_loan_repayment_vector <- rep(0, n_years_c)
  # Dev_bank_repayment_end_year <-
  #   if (!is.null(Dev_bank_maturity_year_c) && !is.na(Dev_bank_maturity_year_c)) Dev_bank_maturity_year_c else n_years_c
  # 
  # Dev_bank_loan_repayment_years <- Dev_bank_repayment_end_year - Dev_bank_repayment_start_year_c + 1
  # 
  # Dev_bank_annual_repayment_amount_p_adj <-
  #   cummax(vv(Dev_bank_annual_repayment_amount_p, 10, Dev_bank_repayment_end_year, relative_trend = 10))
  # 
  # Dev_bank_invst_payoff_function <- function(scale_factor) {
  #   Dev_bank_invst_balance <- Dev_bank_draw  # IMPORTANT: repay only what you actually drew
  #   for (repayment in seq_len(Dev_bank_repayment_end_year)) {
  #     Dev_bank_invst_balance <-
  #       Dev_bank_invst_balance * (1 + Dev_bank_interest_rate_c) -
  #       scale_factor * Dev_bank_annual_repayment_amount_p_adj[repayment]
  #   }
  #   Dev_bank_invst_balance
  # }
  # 
  # lower_bound <- 0
  # upper_bound <- max(Dev_bank_draw, 1e-6)
  # 
  # while (Dev_bank_invst_payoff_function(upper_bound) > 0) {
  #   upper_bound <- upper_bound * 2
  # }
  # 
  # Dev_bank_scaling_factor <-
  #   uniroot(
  #     Dev_bank_invst_payoff_function,
  #     lower = lower_bound,
  #     upper = upper_bound * (1 + Dev_bank_interest_rate_c)^Dev_bank_loan_repayment_years
  #   )$root
  # 
  # Dev_bank_loan_repayment_vector[Dev_bank_repayment_start_year_c:Dev_bank_repayment_end_year] <-
  #   Dev_bank_scaling_factor * Dev_bank_annual_repayment_amount_p_adj[Dev_bank_repayment_start_year_c:Dev_bank_repayment_end_year]
  # 
  # # Recompute running cost + total cost
  # AF_total_running_cost_adj_Dev_bank <- AF_total_running_cost + Dev_bank_loan_repayment_vector
  # 
  # AF_total_cost_adj_Dev_bank <- AF_total_investment_cost_adj_Dev_bank + AF_total_running_cost_adj_Dev_bank
  # 
  # AF_bottom_line_benefit_adj_Dev_bank <- AF_total_benefit - AF_total_cost_adj_Dev_bank
  # 
  # #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  # #AF System
  # AF_NPV_adj_Dev_bank <- discount(AF_bottom_line_benefit_adj_Dev_bank, discount_rate=discount_rate_p,
  #                                 calculate_NPV = TRUE)#NVP of AF system
  # AF_cash_flow_adj_Dev_bank <- discount(AF_bottom_line_benefit_adj_Dev_bank,discount_rate=discount_rate_p,
  #                                       calculate_NPV = FALSE)#Cash flow of AF system
  # AF_cum_cash_flow_adj_Dev_bank <- cumsum(AF_cash_flow_adj_Dev_bank) #Cumulative cash flow of AF system
  # 
  # 
  # #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  # Decision_benefit_adj_Dev_bank <- AF_bottom_line_benefit_adj_Dev_bank - Treeless_bottom_line_benefit
  # NPV_decision_adj_Dev_bank <- discount(Decision_benefit_adj_Dev_bank, discount_rate = discount_rate_p,
  #                                       calculate_NPV = TRUE ) #NPV of the decision
  # CF_decision_adj_Dev_bank <- discount(Decision_benefit_adj_Dev_bank, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  # CumCF_decision_adj_Dev_bank <- cumsum(CF_decision_adj_Dev_bank) #Cumulative cash flow of the decision

  # Scenario 8: Development bank loan ####
  if (isTRUE(use_dev_bank_loan)) {
    
    Dev_bank_financed_capex <- AF_irrigation_system_cost + AF_processing_facility_cost
    Dev_bank_financed_capex_year1 <- Dev_bank_financed_capex[1]
    Dev_bank_draw <- min(Dev_bank_loan_amount_c, Dev_bank_financed_capex_year1)
    
    AF_irrigation_system_cost_adj_Dev_bank <- AF_irrigation_system_cost
    AF_processing_facility_cost_adj_Dev_bank <- AF_processing_facility_cost
    
    if (Dev_bank_financed_capex_year1 > 0 && Dev_bank_draw > 0) {
      scale_down <- (Dev_bank_financed_capex_year1 - Dev_bank_draw) / Dev_bank_financed_capex_year1
      
      AF_irrigation_system_cost_adj_Dev_bank[1] <- AF_irrigation_system_cost[1] * scale_down
      AF_processing_facility_cost_adj_Dev_bank[1] <- AF_processing_facility_cost[1] * scale_down
    }
    
    AF_total_planting_cost_adj_Dev_bank <- AF_gps_measuring + AF_dig_plant_holes + AF_tree_cost +
      AF_plant_tree_cost + AF_guard_cost + AF_weed_protect_cost + AF_compost_cost +
      AF_irrigation_system_cost_adj_Dev_bank + AF_irrigation_after_planting_cost +
      AF_processing_facility_cost_adj_Dev_bank
    
    AF_total_investment_cost_adj_Dev_bank <- AF_planning_cost + AF_pruning_course +
      AF_total_planting_cost_adj_Dev_bank + LEADER_application
    
    AF_total_investment_cost_adj_Dev_bank <- pmax(AF_total_investment_cost_adj_Dev_bank, 0)
    
    Dev_bank_loan_repayment_vector <- rep(0, n_years_c)
    Dev_bank_repayment_end_year <-
      if (!is.null(Dev_bank_maturity_year_c) && !is.na(Dev_bank_maturity_year_c)) Dev_bank_maturity_year_c else n_years_c
    
    Dev_bank_loan_repayment_years <- Dev_bank_repayment_end_year - Dev_bank_repayment_start_year_c + 1
    
    Dev_bank_annual_repayment_amount_p_adj <-
      cummax(vv(Dev_bank_annual_repayment_amount_p, 10, Dev_bank_repayment_end_year, relative_trend = 10))
    
    Dev_bank_invst_payoff_function <- function(scale_factor) {
      Dev_bank_invst_balance <- Dev_bank_draw
      for (repayment in seq_len(Dev_bank_repayment_end_year)) {
        Dev_bank_invst_balance <-
          Dev_bank_invst_balance * (1 + Dev_bank_interest_rate_c) -
          scale_factor * Dev_bank_annual_repayment_amount_p_adj[repayment]
      }
      Dev_bank_invst_balance
    }
    
    lower_bound <- 0
    upper_bound <- max(Dev_bank_draw, 1e-6)
    while (Dev_bank_invst_payoff_function(upper_bound) > 0) {
      upper_bound <- upper_bound * 2
    }
    
    Dev_bank_scaling_factor <- uniroot(
      Dev_bank_invst_payoff_function,
      lower = lower_bound,
      upper = upper_bound * (1 + Dev_bank_interest_rate_c)^Dev_bank_loan_repayment_years
    )$root
    
    Dev_bank_loan_repayment_vector[
      Dev_bank_repayment_start_year_c:Dev_bank_repayment_end_year
    ] <- Dev_bank_scaling_factor *
      Dev_bank_annual_repayment_amount_p_adj[
        Dev_bank_repayment_start_year_c:Dev_bank_repayment_end_year
      ]
    
    AF_total_running_cost_adj_Dev_bank <- AF_total_running_cost + Dev_bank_loan_repayment_vector
    AF_total_cost_adj_Dev_bank <- AF_total_investment_cost_adj_Dev_bank + AF_total_running_cost_adj_Dev_bank
    AF_bottom_line_benefit_adj_Dev_bank <- AF_total_benefit - AF_total_cost_adj_Dev_bank
    
    dev_bank_discounted <- calc_discount_outputs(
      bottom_line_benefit = AF_bottom_line_benefit_adj_Dev_bank,
      treeless_bottom_line_benefit = Treeless_bottom_line_benefit,
      discount_rate_p = discount_rate_p
    )
    
    AF_NPV_adj_Dev_bank           <- dev_bank_discounted$AF_NPV
    AF_cash_flow_adj_Dev_bank     <- dev_bank_discounted$AF_cash_flow
    AF_cum_cash_flow_adj_Dev_bank <- dev_bank_discounted$AF_cum_cash_flow
    Decision_benefit_adj_Dev_bank <- dev_bank_discounted$Decision_benefit
    NPV_decision_adj_Dev_bank     <- dev_bank_discounted$NPV_decision
    CF_decision_adj_Dev_bank      <- dev_bank_discounted$CF_decision
    CumCF_decision_adj_Dev_bank   <- dev_bank_discounted$CumCF_decision
    
  } else {
    
    AF_total_running_cost_adj_Dev_bank <- AF_total_running_cost
    AF_total_cost_adj_Dev_bank         <- AF_total_cost
    AF_bottom_line_benefit_adj_Dev_bank <- AF_bottom_line_benefit
    AF_NPV_adj_Dev_bank                <- AF_NPV
    AF_cash_flow_adj_Dev_bank          <- AF_cash_flow
    AF_cum_cash_flow_adj_Dev_bank      <- AF_cum_cash_flow
    Decision_benefit_adj_Dev_bank      <- Decision_benefit
    NPV_decision_adj_Dev_bank          <- NPV_decision
    CF_decision_adj_Dev_bank           <- CF_decision
    CumCF_decision_adj_Dev_bank        <- CumCF_decision
  }
  
  
  #Scenario 9: Agroforestry ES3 + LEADER Region Steinfurter Land + APAs & ESG Procurement Contracts ####
  Table_apple_benefit_adj_APA <- Table_apple_yield * yield_apple_price_guarantee_c
  # B_qual_apple_benefit_adj_APA <- B_qual_table_apple_yield * yield_apple_price_guarantee_c
  # Juice_apple_benefit_adj_APA <-  Juice_apple_yield * yield_apple_price_guarantee_c
  
  AF_apple_benefit_adj_APA <- Table_apple_benefit_adj_APA + B_qual_apple_benefit + Juice_apple_benefit
  
  AF_maize_benefit_adj_APA <- AF_maize_yield * yield_maize_price_guarantee_c
  AF_wheat_benefit_adj_APA <- AF_wheat_yield * yield_wheat_price_guarantee_c
  AF_rapeseed_benefit_adj_APA <- AF_rapeseed_yield * yield_rapeseed_price_guarantee_c
  AF_barley_benefit_adj_APA <- AF_barley_yield * yield_barley_price_guarantee_c
  
  AF_total_benefit_adj_APA <- AF_apple_benefit_adj_APA + AF_maize_benefit_adj_APA + AF_wheat_benefit_adj_APA + AF_barley_benefit_adj_APA + AF_rapeseed_benefit_adj_APA + ES3_subsidy + AF_per_tree_funding + AF_hedge_treerow_funding + LEADER_subsidy + Annual_external_support + Onetime_external_support
  
  AF_bottom_line_benefit_adj_APA <- AF_total_benefit_adj_APA - AF_total_cost
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  #AF System
  AF_NPV_adj_APA <- discount(AF_bottom_line_benefit_adj_APA, discount_rate=discount_rate_p,
                             calculate_NPV = TRUE)#NVP of AF system
  AF_cash_flow_adj_APA <- discount(AF_bottom_line_benefit_adj_APA,discount_rate=discount_rate_p,
                                   calculate_NPV = FALSE)# Cash flow of AF system
  AF_cum_cash_flow_adj_APA <- cumsum(AF_cash_flow_adj_APA) #Cumulative cash flow of AF system
  
  #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  Decision_benefit_adj_APA <- AF_bottom_line_benefit_adj_APA - Treeless_bottom_line_benefit
  NPV_decision_adj_APA <- discount(Decision_benefit_adj_APA, discount_rate = discount_rate_p,
                                   calculate_NPV = TRUE ) #NPV of the decision
  CF_decision_adj_APA <- discount(Decision_benefit_adj_APA, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  CumCF_decision_adj_APA <- cumsum(CF_decision_adj_APA) #Cumulative cash flow of the decision
  
  # #Scenario 8.1: Agroforestry ES3 + LEADER Region Steinfurter Land + APAs & ESG Procurement Contracts with price uncertainity ####
  # 
  # table_apple_price_apa <- vv(table_apple_price_p,var_CV = (var_cv_p * (1 - (apa_uncert_reduction/100))), n_years_c)
  # B_qual_apple_price_apa <- vv(bqual_apple_price_p,var_CV = (var_cv_p * (1 - (apa_uncert_reduction/100))), n_years_c)
  # Juice_apple_price_apa <- vv(juice_apple_price_p,var_CV = (var_cv_p * (1 - (apa_uncert_reduction/100))), n_years_c)
  #                             
  # table_apple_price_apa <- (yield_apa_contracted_share_c/100) * yield_apple_price_guarantee_c +
  #   (1 - (yield_apa_contracted_share_c/100)) * vv(table_apple_price_p, var_CV = var_cv_p, n_years_c)
  # 
  #                          
  # Table_apple_benefit_adj_APA <- Table_apple_yield * yield_apple_price_guarantee_c
  # B_qual_apple_benefit_adj_APA <- B_qual_table_apple_yield * yield_apple_price_guarantee_c
  # Juice_apple_benefit_adj_APA <-  Juice_apple_yield * yield_apple_price_guarantee_c
  # 
  # AF_apple_benefit_adj_APA <- Table_apple_benefit_adj_APA + B_qual_apple_benefit_adj_APA + Juice_apple_benefit_adj_APA
  # 
  # AF_maize_benefit_adj_APA <- AF_maize_yield * yield_maize_price_guarantee_c
  # AF_wheat_benefit_adj_APA <- AF_wheat_yield * yield_wheat_price_guarantee_c
  # AF_rapeseed_benefit_adj_APA <- AF_rapeseed_yield * yield_rapeseed_price_guarantee_c
  # AF_barley_benefit_adj_APA <- AF_barley_yield * yield_barley_price_guarantee_c
  # AF_total_benefit_adj_APA <- AF_apple_benefit_adj_APA + AF_maize_benefit_adj_APA + AF_wheat_benefit_adj_APA + AF_barley_benefit_adj_APA + AF_rapeseed_benefit_adj_APA + ES3_subsidy + LEADER_subsidy + Annual_external_support + Onetime_external_support
  # 
  # AF_bottom_line_benefit_adj_APA <- AF_total_benefit_adj_APA - AF_total_cost
  # 
  # #Calculating NPV, Cash Flow and Cumulative Cash Flow of the agroforestry system
  # #AF System
  # AF_NPV_adj_APA <- discount(AF_bottom_line_benefit_adj_APA, discount_rate=discount_rate_p,
  #                            calculate_NPV = TRUE)#NVP of AF system
  # AF_cash_flow_adj_APA <- discount(AF_bottom_line_benefit_adj_APA,discount_rate=discount_rate_p,
  #                                  calculate_NPV = FALSE)# Cash flow of AF system
  # AF_cum_cash_flow_adj_APA <- cumsum(AF_cash_flow_adj_APA) #Cumulative cash flow of AF system
  # 
  # #Calculating NPV, Cash Flow and Cumulative Cash Flow of the decision, i.e. the difference between the NPV of the baseline system and the NPV of the AF system
  # Decision_benefit_adj_APA <- AF_bottom_line_benefit_adj_APA - Treeless_bottom_line_benefit
  # NPV_decision_adj_APA <- discount(Decision_benefit_adj_APA, discount_rate = discount_rate_p,
  #                                  calculate_NPV = TRUE ) #NPV of the decision
  # CF_decision_adj_APA <- discount(Decision_benefit_adj_APA, discount_rate = discount_rate_p, calculate_NPV = FALSE) #Cashflow of the decision
  # CumCF_decision_adj_APA <- cumsum(CF_decision_adj_APA) #Cumulative cash flow of the decision
  
  
  #-----------------------------------------------------------------------------------------------------------  
  
  #Defining Monte Carlo output variables #####
  
  return(list(
    # Variables for display in the mainPanel
    
    Agroforestry_Investment = AF_investment_cost,
    Farmer_Out_of_Pocket_Investment = AF_total_investment_cost,
    
    #NPV of the decision in different scenarios
    NPV_decis_AF_ES3 = NPV_decision,
    NPV_decis_no_fund = NPV_decision_no_fund,
    NPV_decis_DeFAF = NPV_decision_DeFAF,
    #NPV of AF system in different funding scenarios
    NPV_Agroforestry_System = AF_NPV,
    NPV_Agroforestry_no_fund = AF_NPV_no_fund,
    NPV_Treeless_System = NPV_treeless_system,
    NPV_DeFAF_Suggestion = AF_NPV_DeFAF,
    # #cumulative cash flow of the AF system
    AF_CCF_ES3=AF_cum_cash_flow,
    AF_CCF_no_fund=AF_cum_cash_flow_no_fund,
    AF_CCF_DeFAF = AF_cum_cash_flow_DeFAF,
    AF_CF = AF_cash_flow,
    Treeless_CF = Treeless_cash_flow,
    Treeless_CCF = Treeless_cum_cash_flow,
    # With normal bank loan
    NPV_decis_AF_adj_bank = NPV_decision_adj_bank,
    NPV_Agroforestry_adj_bank = AF_NPV_adj_bank,
    AF_CF_adj_bank = AF_CF_adj_bank,
    AF_CCF_adj_bank = AF_CCF_adj_bank, 
    # Impact Investment funding
    NPV_decis_AF_adj_impact = NPV_decision_adj_impact,
    NPV_Agroforestry_adj_impact = AF_NPV_adj_impact,
    AF_CF_adj_impact = AF_cash_flow_adj_impact,
    AF_CCF_adj_impact = AF_cum_cash_flow_adj_impact,
    # Risk-Mitigation Instruments (Guarantees, First-Loss, Insurance)
    NPV_decis_AF_adj_risk_mit = NPV_decision_adj_risk_mit,
    NPV_Agroforestry_adj_risk_mit = AF_NPV_adj_risk_mit,
    AF_CF_adj_risk_mit = AF_cash_flow_adj_risk_mit,
    AF_CCF_adj_risk_mit = AF_cum_cash_flow_adj_risk_mit,
    # Partnerships - Startups & Digital Tools
    NPV_decis_AF_adj_partners = NPV_decision_adj_partners,
    NPV_Agroforestry_adj_partners = AF_NPV_adj_partners,
    AF_CF_adj_partners = AF_cash_flow_adj_partners,
    AF_CCF_adj_partners = AF_cum_cash_flow_adj_partners,
    # # Development Banks Loan
    NPV_decis_AF_adj_dev_bank = NPV_decision_adj_Dev_bank,
    NPV_Agroforestry_adj_dev_bank = AF_NPV_adj_Dev_bank,
    AF_CF_adj_dev_bank = AF_cash_flow_adj_Dev_bank,
    AF_CCF_adj_dev_bank = AF_cum_cash_flow_adj_Dev_bank, 
    # Advance Purchase Agreements (APAs) & ESG Procurement Contracts
    NPV_decis_AF_adj_APA = NPV_decision_adj_APA,
    NPV_Agroforestry_adj_APA = AF_NPV_adj_APA,
    AF_CF_adj_APA = AF_cash_flow_adj_APA,
    AF_CCF_adj_APA = AF_cum_cash_flow_adj_APA 
    
  ))
}
# END of the Decision Model ####
#Run the Monte Carlo analysis of the model
# mcSimulation_results <- mcSimulation(
#   estimate = estimate_read_csv(fileName = "Apple_AF_Steinfurt_wRisk_30.csv"),
#   model_function = AF_benefit_with_Risks,
#   numberOfModelRuns = num_simulations_c,
#   functionSyntax = "plainNames")
