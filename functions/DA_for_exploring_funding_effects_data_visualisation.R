# # Functions to visualise the decision model from:
# #"Agroforestry adoption in Germany: using decision analysis to explore the impact of funding mechanisms on system profitability"
# 
# # Load required libraries
# library(decisionSupport)
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(patchwork)
# library(ggh4x)
# library(reshape2)
# 
# # VISUALISING THE NPV OF THE DECISION FOR TEN FUNDING SCENARIOS AND FOUR SIMULATION LENGTHS ####
# 
# # Creating a function, to prepare the outputs from the Monte Carlo simulation for visualisation
# #The function
# # 1. extracts the y data frame from the monte carlo output from decisionSupport::mcSimulation()
# # 2. filters the NPV distributions based on a prefix
# # 3. prepares the NPV data in a way, that ggplot2 fucntions can read and plot it
# 
# Prepare_mc_output <- function(mc_output, prefixes){
#   # Extract the "y" element from the Monte Carlo output list
#   y_data <- mc_output$y
#   
#   # Create an empty list to hold the selected columns by prefix
#   selected_cols <- list()
#   
#   # Loop over each prefix
#   for (prefix in prefixes) {
#     # Find columns with the prefix in their names
#     cols_with_prefix <- y_data[, grep(paste0("^", prefix), colnames(y_data))]
#     
#     # Only add to the list if columns with the prefix are found
#     if (ncol(cols_with_prefix) > 0) {
#       selected_cols[[prefix]] <- cols_with_prefix
#     }
#   }
#   
#   # Combine all selected columns into one data frame
#   filtered_data <- bind_cols(selected_cols)
#   
#   # Stack data so it can be used in ggplot2 functions
#   stacked_data <- stack(filtered_data)
#   # Find the value associated with "total_area" in the input_table
#   total_area_value <- 10.14 #hard coded!! Make sure to change value, if function is used on other decision models with different area value
#   
#   # Convert values in the stacked data
#   # Divide values by 1000 and by the total area (K€/ha)
#   stacked_data <- stacked_data %>%
#     mutate(values = values / 1000) %>%
#     mutate(values = values / total_area_value)
#   
#   # Calculate median values for each output group
#   scenario_medians <- stacked_data %>%
#     group_by(ind) %>%
#     summarize(median_value = median(values, na.rm = TRUE))
#   
#   # Reorder 'ind' levels based on median values (from low to high)
#   stacked_data$ind <- factor(stacked_data$ind, 
#                              levels = scenario_medians$ind[order(scenario_medians$median_value, decreasing = FALSE)])
#   
#   # Generate unique colors for each 'ind' using a color palette
#   unique_ind <- unique(stacked_data$ind)
#   num_colors <- length(unique_ind)
#   color_palette <- hcl.colors(num_colors, palette =  "Earth") # Adjust for up to 12 unique colors
#   return(stacked_data)
# }
# 
# # Creating a function, to process and visualise outputs from the Monte Carlo simulation
# #The function
# #1. runs mcSimulations for multiple files of one folder (e.g. to compare multiple simulation lengths)
# #2. prepares the output of the mcSimulation by filtering for the relevant prefix and then sorting the data to make usable in ggplot2
# #3. plots the data with ggplot2 to create multiple boxplots
# #NOTE: Function 'Prepare_mc_output()' is used within the function 'run_process_plot_mcSim'!!
# run_process_plot_mcSim <- function(folder_path, model_function, number_of_model_runs, prefixes) {
#   # List all CSV files in the folder
#   csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
#   
#   # Create empty lists to store results
#   mc_results   <- list()
#   prepared_data <- list()
#   boxplots     <- list()
#   
#   # Loop through each CSV file
#   for (file in csv_files) {
#     # Extract file name without extension for naming objects
#     file_name <- tools::file_path_sans_ext(basename(file))
#     
#     # Step 1: Run the Monte Carlo Simulation
#     mc_result <- mcSimulation(
#       estimate = estimate_read_csv(fileName = file),
#       model_function = model_function,
#       numberOfModelRuns = number_of_model_runs,
#       functionSyntax = "plainNames"
#     )
#     mc_results[[file_name]] <- mc_result
#     
#     # Step 2: Prepare the output of the Monte Carlo Simulation
#     prepared_output <- Prepare_mc_output(mc_result, prefixes)
#     prepared_data[[file_name]] <- prepared_output
#     
#     # Step 3: Create the initial boxplot for this simulation run
#     p <- ggplot(prepared_output, aes(x = ind, y = values, fill = ind)) +
#       geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.2) +
#       theme_minimal() +
#       theme(
#         axis.text.y    = element_blank(),
#         axis.ticks.y   = element_blank(),
#         legend.position = "none"
#       ) +
#       scale_fill_manual(values = c(
#         "NPV_decis_BB"     = "#703c14",
#         "NPV_decis_no_fund" = "darkgrey",
#         "NPV_decis_AF"     = "orange",
#         "NPV_decis_SN"     = "#5f613d",
#         "NPV_decis_NI"     = "#5c1eae",
#         "NPV_decis_BY"     = "#61d88a",
#         "NPV_decis_MV"     = "#2782cd",
#         "NPV_decis_BW"     = "#ffda23",
#         "NPV_decis_TH"     = "#e5545b",
#         "NPV_decis_DeFAF"  = "darkgreen"
#       )) +
#       labs(x = NULL, y = NULL, fill = "Funding scenarios") +
#       coord_flip() +
#       stat_boxplot(geom = "errorbar", width = 0.2) +
#       geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5)
#     
#     # Save the raw boxplot (axis limits and breaks will be added next)
#     boxplots[[file_name]] <- p
#   }
#   
#   # Set fixed horizontal axis limits and breaks
#   common_limits <- c(-25, 125)
#   common_breaks <- seq(-25, 125, by = 25)
#   
#   # Update each boxplot to use the same horizontal axis settings.
#   # (Note: after coord_flip(), the y-axis becomes horizontal.)
#   boxplots <- lapply(boxplots, function(p) {
#     p + scale_y_continuous(limits = common_limits, breaks = common_breaks)
#   })
#   
#   # Return the results as a list of lists
#   return(list(
#     mc_results   = mc_results,
#     prepared_data = prepared_data,
#     boxplots     = boxplots
#   ))
# }
# 
# # Example ####
# 
# # Specify the folder path containing your CSV files
# folder_path <- "Input_files_w_dif_sim_lengths"
# 
# # Call the function
# mcSim_NPV_data_and_boxplots_list <- run_process_plot_mcSim(
#   folder_path = folder_path,
#   model_function = AF_benefit_with_Risks,
#   number_of_model_runs = 100,
#   prefixes = "NPV_decis")
# 
# # Extract the results
# mc_results <- mcSim_NPV_data_and_boxplots_list$mc_results
# prepared_data <- mcSim_NPV_data_and_boxplots_list$prepared_data
# boxplots <- mcSim_NPV_data_and_boxplots_list$boxplots
# 
# #Order the boxplots from shortest to longest simulation length
# ordered_boxplots <- list(boxplots[[4]], boxplots[[1]], boxplots[[2]], boxplots[[3]])
# 
# # Combine the boxplots into a single figure
# compound_figure <- wrap_plots(ordered_boxplots, ncol = 1)
# 
# 
# 
# 
# #VISUALISING THE CUMULATIVE CASH FLOW AND CALCULATING THE PAYBACK PERIOD FOR TEN FUNDING SCENARIOS AND A SIMULATION LENGTH OF 20 YEARS ####
# 
# #Example cumulative cash flow
# MC_20years <- mc_results$Apple_AF_Steinfurt_wRisk_20 #Variable 'Apple_AF_Steinfurt_wRisk_20' comes from file name in folder "Input_files_w_dif_sim_lengths"
# #CCF of the AF system with no funding
# plot_cashflow(mcSimulation_object = MC_20years, 
#               cashflow_var_name = "AF_CCF_no_fund",
#               x_axis_name = "Timeline of intervention [a]",
#               y_axis_name = "Cumulative Cashflow [€]",
#               color_25_75 = "navajowhite4",
#               color_5_95 = "navajowhite2",
#               color_median = "darkgreen",
#               facet_labels = "No funding")
# 
# # Function to calculate the year when a percentile OF the cumulative cash flow distribution first becomes >= 0
# find_payback_year <- function(values_matrix, percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
#   # Calculate percentiles for each year
#   year_percentiles <- apply(values_matrix, 2, function(year_values) {
#     quantile(year_values, probs = percentiles, na.rm = TRUE)
#   })
#   
#   # Find the first year each percentile becomes >= 0
#   payback_years <- apply(year_percentiles, 1, function(percentile_row) {
#     first_year <- which(percentile_row >= 0)[1]
#     if (is.na(first_year)) {
#       return(">20") # If no year satisfies the condition
#     } else {
#       return(first_year)
#     }
#   })
#   
#   return(payback_years)
# }
# 
# # Extract unique scenario names
# MC_y_20 <- MC_20years$y
# CCF_20 <- MC_y_20[, grep(paste0("^AF_CCF"), colnames(MC_y_20))]
# scenario_names <- unique(sub("[0-9]+$", "", colnames(CCF_20)))
# 
# # Initialize a data frame to store the results
# percentile_results <- data.frame(row.names = c("5th", "25th", "50th", "75th", "95th"))
# 
# # Loop through each scenario
# for (scenario in scenario_names) {
#   # Select columns for the current scenario
#   scenario_data <- CCF_20[, grepl(paste0("^", scenario), colnames(CCF_20))]
#   
#   # Find payback years for the scenario
#   payback_years <- find_payback_year(scenario_data)
#   
#   # Add results to the output data frame
#   percentile_results[[scenario]] <- payback_years
# }
# 
# #Transpose for easier reading and extraction of confidence intervals
# trans_percentile_results <- as.data.frame(t(percentile_results))
# 
# 
# 
# #VISUALISING THE EXPECTED VALUE OF PERFECT PARAMETER INFORMATION ####
# 
# #Creating a fundtion that:
# #Iterates EVPPI function over multiple mcSimulation objects and multiple decision scenarios, 
# #creating one output data frame per mcSimulation output,
# #containing the decision scenarios and the input variables with the corresponding EVPPI scores 
# 
# #function uses binary_multi_evppi function from Johannes Kopton
# devtools::install_github("johanneskopton/evpi", subdir="r/evpi")
# library(evpi)
# # Define the function
# mc_results <- mcSim_NPV_data_and_boxplots_list$mc_results
# 
# iterate_EVPPI <- function(mc_results) {
#   
#   # Initialize an empty list to store results
#   
#   evppi_results <- list()
#   
#   # Iterate over each object in mc_results
#   
#   for (i in seq_along(mc_results)) {
#     
#     # Extract the current object
#     
#     current_object <- mc_results[[i]]
#     
#     # Extract the y and x data frames
#     
#     y <- current_object[["y"]]
#     
#     x <- current_object[["x"]]
#     
#     # Select the first 10 output variables that start with "NPV_decis"
#     
#     output_vars <- grep("^NPV_decis", names(y), value = TRUE)[1:10]
#     
#     # Prepare the input matrix for the evppi function
#     
#     x_matrix <- data.matrix(x)  # Convert x to a matrix
#     
#     # Initialize a data frame to store the EVPPI results for the current object
#     
#     evppi_df <- data.frame(matrix(nrow = ncol(x_matrix), ncol = length(output_vars)))
#     
#     rownames(evppi_df) <- names(x)  # Set row names to input variable names
#     
#     colnames(evppi_df) <- output_vars  # Set column names to output variable names
#     
#     # Iterate over each selected output variable
#     
#     for (j in seq_along(output_vars)) {
#       
#       # Create a single-column matrix for the current output variable
#       
#       y_single <- data.matrix(y[output_vars[j]])
#       
#       # Calculate the EVPPI using binary_multi_evppi
#       
#       evppi_values <- evpi::binary_multi_evppi(x_matrix, y_single)
#       
#       # Store the results in the data frame
#       
#       evppi_df[, j] <- evppi_values
#       
#     }
#     
#     # Name the data frame according to the current object
#     
#     df_name <- paste0("df_", names(mc_results)[i])
#     
#     evppi_results[[df_name]] <- evppi_df
#     
#   }
#   
#   
#   
#   return(evppi_results)
#   
# }
# 
# evppi_results <- iterate_EVPPI(mc_results)
# 
# evppi_5years <- evppi_results$df_Apple_AF_Steinfurt_wRisk_5
# evppi_10years <- evppi_results$df_Apple_AF_Steinfurt_wRisk_10
# evppi_15years <- evppi_results$df_Apple_AF_Steinfurt_wRisk_15
# evppi_20years <- evppi_results$df_Apple_AF_Steinfurt_wRisk_20
# 
# # Creating colour strips in x-direction to match the colours of the NPV plot
# strip <- strip_themed(background_x = elem_list_rect(fill = c("lightgrey", "darkgrey", "#ffda23",
#                                                              "#703c14","#61d88a","#2782cd",
#                                                              "#e5545b","#5f613d", "#5c1eae",
#                                                              "darkgreen"),
#                                                     alpha = 0.2))
# 
# evppi_5years_plot <- 
#   evppi_5years %>% 
#   mutate(var_name = rownames(.)) %>% 
#   filter(NPV_decis_AF_ES3 != 0 |
#            NPV_decis_no_fund != 0 |
#            NPV_decis_NI != 0 |
#            NPV_decis_BY  != 0 |
#            NPV_decis_MV != 0 |
#            NPV_decis_SN != 0 |
#            NPV_decis_BW != 0 |
#            NPV_decis_TH != 0 |
#            NPV_decis_BB != 0 |
#            NPV_decis_DeFAF != 0) %>% 
#   select(c(11,2,1,3:10)) %>%
#   melt(1) %>% 
#   mutate(years = "5")
# 
# evppi_10years_plot <- 
#   evppi_10years %>% 
#   mutate(var_name = rownames(.)) %>% 
#   filter(NPV_decis_AF_ES3 != 0 |
#            NPV_decis_no_fund != 0 |
#            NPV_decis_NI != 0 |
#            NPV_decis_BY  != 0 |
#            NPV_decis_MV != 0 |
#            NPV_decis_SN != 0 |
#            NPV_decis_BW != 0 |
#            NPV_decis_TH != 0 |
#            NPV_decis_BB != 0 |
#            NPV_decis_DeFAF != 0) %>% 
#   select(c(11,2,1,3:10)) %>%
#   melt(1) %>% 
#   mutate(years = "10")
# 
# evppi_15years_plot <- 
#   evppi_15years %>% 
#   mutate(var_name = rownames(.)) %>% 
#   filter(NPV_decis_AF_ES3 != 0 |
#            NPV_decis_no_fund != 0 |
#            NPV_decis_NI != 0 |
#            NPV_decis_BY  != 0 |
#            NPV_decis_MV != 0 |
#            NPV_decis_SN != 0 |
#            NPV_decis_BW != 0 |
#            NPV_decis_TH != 0 |
#            NPV_decis_BB != 0 |
#            NPV_decis_DeFAF != 0) %>% 
#   select(c(11,2,1,3:10)) %>%
#   melt(1) %>% 
#   mutate(years = "15")
# 
# 
# evppi_20years_plot <- 
#   evppi_20years %>% 
#   mutate(var_name = rownames(.)) %>% 
#   filter(NPV_decis_AF_ES3 != 0 |
#            NPV_decis_no_fund != 0 |
#            NPV_decis_NI != 0 |
#            NPV_decis_BY  != 0 |
#            NPV_decis_MV != 0 |
#            NPV_decis_SN != 0 |
#            NPV_decis_BW != 0 |
#            NPV_decis_TH != 0 |
#            NPV_decis_BB != 0 |
#            NPV_decis_DeFAF != 0) %>% 
#   select(c(11,2,1,3:10)) %>%
#   melt(1) %>% 
#   mutate(years = "20")
# 
# plot_data <-
#   rbind(evppi_5years_plot,
#         evppi_10years_plot,
#         evppi_15years_plot,
#         evppi_20years_plot) %>% 
#   select(c(4,1:3))
# 
# # evppi_plot_5_10_15_20
# evppi_plot <- plot_data %>% 
#   mutate(var_name = factor(var_name,
#                            levels = c("apple_yield_max","table_apple_price",
#                                       "apple_harvest", "perc_table_apple", "labour_cost"),
#                            label = c("Max. apple yield","Table apple price","Apple harvest cost","Percentage of table apples", "Labour cost")
#   )) %>%
#   mutate(variable = factor(variable,
#                            levels = c("NPV_decis_no_fund", "NPV_decis_AF_ES3", "NPV_decis_BW", "NPV_decis_BB",
#                                       "NPV_decis_BY", "NPV_decis_MV", "NPV_decis_TH", "NPV_decis_SN", "NPV_decis_NI", "NPV_decis_DeFAF"),
#                            label = c("No funding","Eco Scheme","BW","BB","BY","MV","TH","SN","NI","DeFAF")
#   )) %>%
#   mutate(years = factor(years,
#                         levels = c("5", "10","15","20"),
#                         label = c("5 a", "10 a", "15 a", "20 a")
#   )) %>%
#   ggplot(aes(var_name, value)) +
#   geom_bar(stat = "identity") +
#   xlab("") +
#   ylab("EVPI [€]") +
#   facet_wrap2(~years+variable,4,
#               strip = strip) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
#         strip.text = element_text(color = "black", face = "bold", size = 8))
# 
# evppi_plot
