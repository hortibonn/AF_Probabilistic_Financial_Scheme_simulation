# RShiny app 
# Changes to be made to adapt in this script have been commented with 'Provide'

# # Install + load libraries ----
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("waiter", quietly = TRUE)) {
  install.packages("waiter")
}
library(waiter)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)

if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}
library(shinythemes)

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
library(shinyWidgets)

if (!requireNamespace("decisionSupport", quietly = TRUE)) {
  install.packages("decisionSupport")
}
library(decisionSupport)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)  # For reading and writing CSV files

if (!requireNamespace("ggridges", quietly = TRUE)) {
  install.packages("ggridges")
}
library(ggridges)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")
}
library(ggtext)
if (!requireNamespace("ggh4x", quietly = TRUE)) {
  install.packages("ggh4x")
}
library(ggh4x)

# Provide Location of DA model script, dynamic-helper and funding-server scripts
# source("functions/saveLoad-module.R")
#source("functions/DA_for_exploring_funding_effects_data_visualisation.R")
source("functions/DA_baseline.R")
source("functions/dynamic-helper.R")
source("functions/DA_statutory_funding.R") #funding_server.R")
source("functions/DA_financing_orchestrator.R")
source_decision_modules("functions")
# Provide Location of excel workbook containing the input parameters (prepared for the dynamic-helper)
file_path_vars <- "data/AF_parameters.xlsx"
file_path_vars1 <- "data/AF_Financial_Scheme_TZ.xlsx"

sheet_meta <- readxl::read_excel(file_path_vars, sheet = "sheet_names",
                                 col_types = c("text", "text"))
sheet_names <- sheet_meta$sheet_names
sheet_icons <- setNames(sheet_meta$icon, sheet_meta$sheet_names)

sheet_meta_finance <- readxl::read_excel(file_path_vars1, sheet = "sheet_names",
                                         col_types = c("text", "text"))
sheet_names_finance <- sheet_meta_finance$sheet_names
sheet_icons_finance <- setNames(sheet_meta_finance$icon, sheet_meta_finance$sheet_names)

# Finance visibility configuration ----

bank_loan_vars <- c(
  "bank_loan_amount_c",
  "bank_interest_rate_c",
  "bank_maturity_year_c",
  "bank_repayment_start_year_c",
  "bank_annual_repayment_amount_p"
)

impact_loan_vars <- c(
  "impact_invst_fund_loan_c",
  "impact_invst_fund_interest_rate_c",
  "impact_invst_bank_maturity_year_c",
  "impact_invst_fund_repayment_start_year_c",
  "impact_invst_annual_repayment_amount_p"
)

dev_bank_loan_vars <- c(
  "Dev_bank_loan_amount_c",
  "Dev_bank_interest_rate_c",
  "Dev_bank_maturity_year_c",
  "Dev_bank_repayment_start_year_c",
  "Dev_bank_annual_repayment_amount_p"
)

guarantee_vars <- c(
  "guarantee_cover_rate_c",
  "guarantee_default_loss_rate_c",
  "guarantee_fee_rate_c"
)

insurance_vars <- c(
  "insurance_cover_rate_c",
  "insurance_payout_amount_c",
  "insurance_annual_premium_c",
  "insurance_annual_premium_surcharge_c"
)

advisory_organisation_vars <- c(
  "organisation_nominal_fee_c",
  "organisation_consultation_reduction_perc_c",
  "perc_adv_exp_p",
  "perc_adv_design_p"
)

advisory_cooperative_vars <- c(
  "cooperative_nominal_fee_c",
  "cooperative_machinery_reduction_perc_c",
  "cooperative_labour_reduction_perc_c"
)

advisory_digital_vars <- c(
  "digital_tool_subscription_discount_amount_c"
)

market_price_guarantee_vars <- c(
  "price_guarantee_share_c",
  "table_apple_guaranteed_price_p",
  "bqual_apple_guaranteed_price_p",
  "juice_apple_guaranteed_price_p",
  "maize_guaranteed_price_p",
  "wheat_guaranteed_price_p",
  "barley_guaranteed_price_p",
  "rapeseed_guaranteed_price_p"
)

market_price_premium_vars <- c(
  "price_premium_share_p",
  "table_apple_price_premium_p",
  "bqual_apple_price_premium_p",
  "juice_apple_price_premium_p",
  "maize_price_premium_p",
  "wheat_price_premium_p",
  "barley_price_premium_p",
  "rapeseed_price_premium_p"
)

finance_condition_for_var <- function(var_name) {
  if (
    is.null(var_name) ||
    length(var_name) == 0L ||
    is.na(var_name[[1]]) ||
    !nzchar(as.character(var_name[[1]]))
  ) {
    return("true")
  }
  
  var_name <- as.character(var_name[[1]])
  
  if (var_name %in% bank_loan_vars) {
    return("input['selected_loan_scheme_c'] === 'bank'")
  }
  
  if (var_name %in% impact_loan_vars) {
    return("input['selected_loan_scheme_c'] === 'impact'")
  }
  
  if (var_name %in% dev_bank_loan_vars) {
    return("input['selected_loan_scheme_c'] === 'dev_bank'")
  }
  
  if (var_name %in% guarantee_vars) {
    return("input['risk_mitigation_guarantee_c'] === true")
  }
  
  if (var_name %in% insurance_vars) {
    return("input['risk_mitigation_insurance_c'] === true")
  }
  
  if (var_name %in% advisory_organisation_vars) {
    return(
      "input['use_advisory'] === true &&
       input['advisory_support_types'] &&
       input['advisory_support_types'].indexOf('organisation') >= 0"
    )
  }
  
  if (var_name %in% advisory_cooperative_vars) {
    return(
      "input['use_advisory'] === true &&
       input['advisory_support_types'] &&
       input['advisory_support_types'].indexOf('cooperative') >= 0"
    )
  }
  
  if (var_name %in% advisory_digital_vars) {
    return(
      "input['use_advisory'] === true &&
       input['advisory_support_types'] &&
       input['advisory_support_types'].indexOf('digital_tools') >= 0"
    )
  }
  
  if (var_name %in% market_price_guarantee_vars) {
    return(
      "input['use_market_access'] === true &&
       input['market_access_types'] &&
       input['market_access_types'].indexOf('price_guarantees') >= 0"
    )
  }
  
  if (var_name %in% market_price_premium_vars) {
    return(
      "input['use_market_access'] === true &&
       input['market_access_types'] &&
       input['market_access_types'].indexOf('price_premia') >= 0"
    )
  }
  
  "true"
}

# UI ----
ui <- fluidPage(
  
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")),
  
  use_waiter(),
  
  # Set actual browser tab title and favicon
  tags$head(
    tags$title("Agroforestry Financial Decision Support Tool"),
    tags$link(rel = "shortcut icon", href = "INRES.png"),
    
    tags$style(HTML("
    /* --- Resizable sidebar layout --- */
#resizable-layout {
  display: flex;
  width: 100%;
}

/* Sidebar width is controlled here */
#sidebar {
  width: 380px;     /* initial width */
  min-width: 260px;
  max-width: 650px;
}

/* Main takes remaining space */
#main {
  flex: 1;
  min-width: 0;     /* important so plotly/plots can shrink properly */
}

/* Center the finance summary table (header + cells) */
#finance_summary_table table th,
#finance_summary_table table td {
  text-align: center !important;
  vertical-align: middle;
}

/* Drag handle */
#dragbar {
  width: 6px;
  cursor: col-resize;
  background: #e0e0e0;
}

#dragbar:hover {
  background: #bdbdbd;
}

    
    /* Scroll wrapper: scrolls horizontally *and* vertically only when needed */
    .scroll-xy {
      overflow-x: auto;                 /* leftÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œright scroll  */
      overflow-y: auto;                 /* topÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œbottom scroll  */
      -webkit-overflow-scrolling: touch;/* smooth on iOS      */
      max-height: 80vh;                 /* optional: stop it taking more than
                                         80 % of the viewport height       */
  }
  
  /* Keep any Shiny plot inside that wrapper from shrinking */
  .scroll-xy .shiny-plot-output {
    min-width: 900px;                 /* choose your desktop width */
  }
                    ")
    )
  ),
  
  tags$div(
    style = "display:flex; align-items:center;justify-content:space-between;
      width: 100% !important; margin: 20px; padding: 0 15px;
      box-sizing: border-box; background-color: #f2f2f2;",
    tags$script(HTML("
(function() {
  var dragging = false;
  var startX = 0;
  var startWidth = 0;

  document.addEventListener('mousedown', function(e) {
    if (e.target && e.target.id === 'dragbar') {
      dragging = true;
      startX = e.clientX;
      startWidth = document.getElementById('sidebar').offsetWidth;
      document.body.style.cursor = 'col-resize';
      e.preventDefault();
    }
  });

  document.addEventListener('mousemove', function(e) {
    if (!dragging) return;

    var newWidth = startWidth + (e.clientX - startX);
    newWidth = Math.max(260, Math.min(650, newWidth));
    document.getElementById('sidebar').style.width = newWidth + 'px';
  });

  document.addEventListener('mouseup', function() {
    if (!dragging) return;
    dragging = false;
    document.body.style.cursor = 'default';
  });
})();
")
    ),
    
    # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
    tags$img(src = "UniBonnHortiBonn_logo_transparent.png", height = "100px",
             style = "margin-left: auto; max-width: 20%; height: auto; cursor: pointer;"),
    tags$img(src = "emea_logo_large.jpg", height = "20px",
             style = "display: inline-block; vertical-align: middle; max-width: 6%; height: auto; cursor: pointer;"),
    
    
    # ),
    # Provide Title of the DA model
    tags$h2(tags$div("Decision:"),
            tags$div("Transition to Fruit Tree Alley-Cropping"),
            style = "text-align: center; flex-grow: 1;"),
    # Provide Project Logo
    # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
    tags$img(src = "ReFOREST_logo_horizontal_transparent.png", height = "100px",
             style = "margin-right: auto; max-width: 30%; height: auto; cursor: pointer;")
    # ),
  ),
  
  
  ## Sidebar ----
  #sidebarLayout(
  #sidebarPanel(
  tags$div(
    id = "resizable-layout",
    tags$div(
      id = "sidebar",
      sidebarPanel(
        width = NULL,   # IMPORTANT: let CSS control width
        #width = 4,
        style = "height: 100%; overflow-y: auto",
        
        accordion(
          id = "collapseSidebar",
          open = FALSE,
          
          div(
            class = "text-center",
            actionButton("run_simulation", "Run Model",
                         icon = icon("play"), class = "btn-primary")
          ),
          br(),
          
          ### Save/Load functionality ----
          # saveLoadUI("savemod"),
          accordion_panel(
            title = "Save / Load project", icon = icon("folder-open"),
            tagList(
              textInput("state_name", "Project name"),
              actionButton("save_btn",  label = tagList(icon("floppy-disk"),  "Save"  ), class = "btn btn-dark"),
              
              br(), br(),
              selectInput("state_picker", "Saved versions", choices = NULL),
              
              fluidRow(
                column(6, actionButton("load_btn",   tagList(icon("rotate"),  "Load"  ), class = "btn btn-secondary")),
                column(6, actionButton("delete_btn", tagList(icon("trash"),   "Delete"), class = "btn btn-secondary"))
              ),
              hr(),
              downloadButton("download_csv", label = tagList(icon("download"), "Download current inputs (.csv)"))
            )
          ),
          
          
          
          ### Expertise filter ----
          accordion_panel(
            title = "Expertise",
            icon = icon("clipboard-question"),
            tagList(
              tags$h5(
                "I am a/an ..",
                tags$span(
                  icon("circle-question"),
                  title = "Select your main expertise to view and edit only relevant variables.\nNot selecting any box shows all variables.",
                  style = "cursor: help; margin-left: 8px;"
                )
              ),
              uiOutput("category_filter_ui")
            )
          ),
          
          # ### Crop selector and rotation ----
          # accordion_panel(
          #   title = "Crops",
          #   icon = icon("clipboard-question"),
          #   accordion_panel(
          #     title = "Crop selector",
          #     icon = icon("seedling"),
          #     uiOutput("crop_rot_filter_ui")
          #   ),
          #   uiOutput("rotation_builder_ui"),   # rendered only when crops picked
          #   verbatimTextOutput("rotation_vec") # convenient preview
          # ),
          

          uiOutput("dynamic_element_ui"),
          br(),
          ### funding scheme ----
          ### Staturory Funding
          accordion_panel(
            title = "Existing Financial Support", icon = icon("euro-sign"),
            create_funding_ui("funding")
          ),
          #br(), 
          ### EMEA finance scheme selectors ----
          accordion_panel(
            title = "EMEA's AF Finance Scheme",
            icon = icon("sitemap"),
            tagList(
              tags$p(
                "Select relevant finance, risk-mitigation, advisory, and market/value-chain modules to include.", # Detailed numeric assumptions are shown in the EMEA finance panels below when relevant.",
                style = "margin-bottom: 10px;"
              ),

              tags$h5("Loan scheme"),
              radioButtons(
                inputId = "selected_loan_scheme_c",
                label = NULL,
                choices = c(
                  "No loan" = "none",
                  "Traditional bank loan" = "bank",
                  "Impact investment" = "impact",
                  "Development bank loan" = "dev_bank"
                ),
                selected = "none"
              ),

              tags$hr(),

              tags$h5("Risk-mitigation instruments"),
              checkboxInput(
                inputId = "risk_mitigation_guarantee_c",
                label   = "Include Guarantee Fund",
                value   = FALSE
              ),
              checkboxInput(
                inputId = "risk_mitigation_insurance_c",
                label   = "Include Insurance",
                value   = FALSE
              ),

              tags$hr(),

              tags$h5("Advisory support"),
              checkboxInput(
                inputId = "use_advisory",
                label = "Include advisory support",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.use_advisory === true",
                checkboxGroupInput(
                  inputId = "advisory_support_types",
                  label = "Type of advisory support",
                  choices = c(
                    "Agroforestry organisation" = "organisation",
                    "Cooperative" = "cooperative",
                    "Digital tools" = "digital_tools",
                    "Project participation" = "project"
                  )
                )
              ),

              tags$hr(),

              tags$h5("Market and value-chain access"),
              checkboxInput(
                inputId = "use_market_access",
                label = "Include market/value-chain access",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.use_market_access === true",
                checkboxGroupInput(
                  inputId = "market_access_types",
                  label = "Market arrangement",
                  choices = c(
                    "Price guarantees" = "price_guarantees",
                    "Price premia" = "price_premia"
                  )
                ),
                conditionalPanel(
                  condition = "input.market_access_types && input.market_access_types.indexOf('price_guarantees') >= 0",
                  selectInput(
                    inputId = "price_guarantee_type",
                    label = tagList(
                      "Price guarantee type",
                      tags$span(
                        icon("circle-question"),
                        title = "Floor price: buyer guarantees a minimum price.\nFixed price: buyer and farmer agree on one price in advance.",
                        style = "cursor: help; margin-left: 8px;"
                      )
                    ),
                    choices = c("Floor price" = "floor", "Fixed price" = "fixed"),
                    selected = "floor"
                  )
                )
              )
            )
          ),
          #br(),
          uiOutput("dynamic_finance_element_ui"),
          br(),
          

        )
      )
    ),
    # drag handle
    tags$div(id = "dragbar"),
    ## Main Panel ----
    tags$div(
      id = "main",
      mainPanel(
        width = NULL,
        #width = 8,
        # Provide brief explanation of the DA model
        tags$h6(
          "App estimates long-term profitability of transition from a treeless arable field to an alley-cropping system with fruit trees under temperate European conditions.", # It works for any tree species, so you can easily explore the impact on your farmÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â‚¬Å¾Ã‚Â¢s Net Present Value (NPV).",
          tags$br(),
          tags$br(),
          "Use the tabs on the left to adjust variable ranges based on your local conditions or design goals.",
          "App displays costs and prices in EUROS (€), please enter your data in your local currency. Funding information has been collected in each country in it's own currency, so using yours will keep the results accurate.",
          tags$br(),
          tags$br(),
          "Click ‘Run model’ to perform a Monte Carlo simulation using random combinations from your defined ranges.You can save/load inputs, and once the model runs, results will appear below. You can also save these figures for your reports.", #tags$br(),
          tags$br(),
          #"In the ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¹Ã…â€œFunding schemesÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â‚¬Å¾Ã‚Â¢ tab, select any relevant funding options for your region.",
          tags$br(),
          # "DeFAF-suggested funding for German agroforestry: Annual support of 600 ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ per ha of wooded area and investment costs are to be funded at 100 % for first 10 ha of wooded area, 80 % for the next 10 ha, 50 % for additional area.",
          tags$br(),
          "AF Financial Scheme developed by EMEA is modeled here, and you can select the ones that are most relevant to you. If you think of any innovative support scheme we have not considered here please reach out to",
          tags$a(href = "mailto:tiago.zibecchi@euromed-economists.org", "Tiago Zibecchi."),
          tags$br(),
          tags$br(),
          "We are happy to receive your feedback to improve our app, write to",
          tags$a(href = "mailto:pkasargo@uni-bonn.de", "Prajna Kasargodu Anebagilu"), "or", tags$a(href = "mailto:afuelle1@uni-bonn.de", "Adrain Fuelle."),
        ),
        br(), br(),
        
        br(), br(),
        uiOutput("finance_summary_header"),
        tableOutput("finance_summary_table"),
        uiOutput("finance_summary_note"),
        
        br(), br(),
        div(class = "scroll-xy",
            #plotOutput("plot1_ui", height = "700px"),
            plotlyOutput("plot1_ui", height = "700px")
        ),
        uiOutput("plot1_caption"),   # <--- caption rendered as HTML
        #br(),
        br(),
        uiOutput("plot1_dl_ui"),
        br(), #br(),br(), br(),
        
        # div(class = "scroll-xy",
        #     plotOutput("plot2_ui", height = "700px"),
        # ),
        # br(),
        # uiOutput("plot2_dl_ui"),
        # br(), #br(),br(), br(),
        div(class = "scroll-xy",
            plotlyOutput("plot3_ui", height = "700px")
        ),
        uiOutput("plot3_caption"),
        br(),
        uiOutput("plot3_dl_ui"),
        
        
        div(class = "scroll-xy",
            plotlyOutput("plot4_ui", height = "700px"),
        ),
        uiOutput("plot4_caption"),
        br(),
        uiOutput("plot4_dl_ui"),
        br(),
        
        div(class = "scroll-xy",
            plotlyOutput("plot5_ui", height = "700px")
        ),
        uiOutput("plot5_caption"),
        br(),
        uiOutput("plot5_dl_ui"),
        br(),
        
        div(class = "scroll-xy",
            plotlyOutput("plot6_ui", height = "700px")
        ),
        uiOutput("plot6_caption"),
        br(),
        uiOutput("plot6_dl_ui"),
        # br(), br(),
        # # 
        # # div(class = "scroll-xy", plotOutput("plot7_ui", height = "550px"),),
        # # br(),
        # # uiOutput("plot7_dl_ui"),
        # # br(), br(),br(), br(),
        # 
        # div(class = "scroll-xy", 
        #     plotOutput("plot8_ui", height = "550px"),
        # ),
        br(),
        # uiOutput("plot8_dl_ui"),
        tags$img(src = "Funding_declaration.png", height = "100px",
                 style = "margin-right: auto; max-width: 100%; height: auto; cursor: pointer;"),
        tags$p(
          tags$a("Disclaimer", href = "https://agroreforest.eu/reforest-tools-disclaimer/",
                 target = "_blank"),
          " | ",  
          tags$a("View Source", href = "https://github.com/hortibonn/AF_Probabilistic_Financial_Scheme_simulation",
                 target = "_blank")
        ),
        br(), #br(),br(), br(),
      )
      
    )
  )
)


# Server ----
server <- function(input, output, session) {
  
  ## Dynamic funding module ----
  funding <- funding_server("funding")   # returns a list of reactives
  
  output$`funding-financial-support` <- renderUI({
    funding$financial_support_links
  })
  # output$summary <- renderPrint({
  #   result$category_totals()          
  output$summary <- renderTable({
    
    # Get the full funding totals (gov + private) as a named list
    total_funding <- funding$total_funding_with_private()
    
    # Debug: data frame for table output ### remove for the final or can be displayed in the mainPanel too - upto @Adrain
    data.frame(
      `Funding Category` = str_to_title(str_replace_all(str_remove(names(total_funding), "_c$"), "_", " ")),
      `Total Financial Support` = round(unname(total_funding), 2),
      check.names = FALSE,
      row.names = NULL
    )
  })
  ## Helper for safe extraction from named vector
  safe_get <- function(vec, name) {
    if (is.null(vec) || length(vec) == 0 || is.na(vec[name])) return(0)
    if (! name %in% names(vec)) return(0)
    as.numeric(vec[name])
  }
  
  
  rv_fin <- reactiveValues(
    finance_summary_table = NULL
  )
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
  
  sanitize_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

  ## Dynamic expertise-filter module ----
  # helper that sanitises category names into safe IDs
  sanitize <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  
  # all categories across every sheet
  categories <- reactive({
    cats <- unique(unlist(lapply(excelData(), function(df) df$Expertise)))
    cats <- cats[!is.na(cats) & cats != ""]
    trimws(unique(unlist(strsplit(cats, ";"))))
  })
  
  # filter bar UI
  output$category_filter_ui <- renderUI({
    if (length(categories()) == 0) return(NULL)
    tagList(
      lapply(categories(), function(cat){
        checkboxInput(
          paste0("cat_", sanitize_id(cat)), cat, value = FALSE)
      })
    )
  })
  
  ## Dynamic UI inputs ----
  
  # read in input xlsx file
  excelData <- reactive({
    sheet_number <- seq_along(sheet_names)+1
    all_sheets <- lapply(sheet_number, function(sht) {
      readxl::read_excel(file_path_vars, sheet = sht,
                         col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "guess", "guess", "text", "text")
      )
    })
    names(all_sheets) <- sheet_names
    all_sheets
  })
  
  
  # util: turns a category vector into a JS condition 
  ### render but hide unchecked expertise categories - default show-all ----
  panel_condition <- function(cat_vec) {
    cat_vec <- trimws(cat_vec)
    cat_vec <- cat_vec[cat_vec != "" & !is.na(cat_vec)]
    if (length(cat_vec) == 0) return("true")
    
    cat_ids <- sprintf("input['cat_%s']", sanitize_id(cat_vec))
    
    cat_show_all <- paste0(
      "Object.keys(input).filter(k => k.startsWith('cat_')).",
      "every(k => input[k] === false)"
    )
    
    sprintf("(%s) || (%s)",            # show when *no* cat box ticked
            cat_show_all,              #ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€šÃ‚Â¦or any matching cat ticked
            paste(cat_ids, collapse = ' || '))
  }
  
  output$dynamic_element_ui <- renderUI({
    
    data_list   <- excelData()
    sheet_names <- names(data_list)
    
    panels <- lapply(seq_along(data_list), function(j) {
      
      sheet <- data_list[[j]]
      
      cats  <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
      cats  <- cats[cats != ""]
      
      # Skip empty / broken rows safely
      sheet_clean <- sheet %>%
        dplyr::filter(!is.na(variable), trimws(variable) != "")
      
      ui_elems <- lapply(seq_len(nrow(sheet_clean)), function(i) {
        row <- sheet_clean[i, ]
        var_name <- as.character(row$variable %||% "")
        
        elem <- tryCatch(
          create_ui_element(row),
          error = function(e) {
            message("Skipping UI row ", i, " in sheet '", sheet_names[j], "': ", e$message)
            NULL
          }
        )
        
        if (is.null(elem)) return(NULL)
        
        conditionalPanel(
          condition = finance_condition_for_var(var_name),
          elem
        )
      })
      
      ui_elems <- Filter(Negate(is.null), ui_elems)
      
      conditionalPanel(
        condition = panel_condition(cats),
        accordion_panel(
          title = sheet_names[j],
          icon  = icon(sheet_icons[[ sheet_names[j] ]] %||% "circle-dot"),
          tagList(ui_elems)
        )
      )
    })
    
    tagList(panels)
  })
  
  excelFinanceData <- reactive({
    sheet_number <- seq_along(sheet_names_finance) + 1
    all_sheets <- lapply(sheet_number, function(sht) {
      readxl::read_excel(file_path_vars1, sheet = sht,
                         col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "guess", "guess", "text", "text")
      )
    })
    names(all_sheets) <- sheet_names_finance
    all_sheets
  })
  
  output$dynamic_finance_element_ui <- renderUI({
    data_list <- excelFinanceData()
    sheet_names <- names(data_list)
    
    panels <- lapply(seq_along(data_list), function(j) {
      sheet <- data_list[[j]]
      
      cats <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
      cats <- cats[cats != ""]
      
      sheet_clean <- sheet %>%
        dplyr::filter(!is.na(variable), trimws(variable) != "")
      
      ui_elems <- lapply(seq_len(nrow(sheet_clean)), function(i) {
        row <- sheet_clean[i, ]
        var_name <- as.character(row$variable %||% "")
        
        elem <- tryCatch(
          create_ui_element(row),
          error = function(e) {
            message("Skipping finance UI row ", i, " in sheet '", sheet_names[j], "': ", e$message)
            NULL
          }
        )
        
        if (is.null(elem)) return(NULL)
        
        conditionalPanel(
          condition = finance_condition_for_var(var_name),
          elem
        )
      })
      
      ui_elems <- Filter(Negate(is.null), ui_elems)
      if (length(ui_elems) == 0) return(NULL)
      
      conditionalPanel(
        condition = panel_condition(cats),
        accordion_panel(
          title = sheet_names[j],
          icon  = icon(sheet_icons_finance[[ sheet_names[j] ]] %||% "circle-dot"),
          tagList(ui_elems)
        )
      )
    })
    
    panels <- Filter(Negate(is.null), panels)
    tagList(panels)
  })
  
  # output$dynamic_element_ui <- renderUI({
  #   
  #   data_list   <- excelData()
  #   sheet_names <- names(data_list)
  #   
  #   # build one accordion panel per sheet
  #   # the elements are generated via the external function create_ui_element()
  #   panels <- lapply(seq_along(data_list), function(j) {
  #     
  #     sheet <- data_list[[j]]
  #     
  #     cats  <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
  #     cats  <- cats[cats != ""]
  #     
  #     ui_elems <- lapply(seq_len(nrow(sheet)), function(i) {
  #       create_ui_element(sheet[i, ])
  #     })
  #     
  #     conditionalPanel(
  #       condition = panel_condition(cats),   # hide panel is empty
  #       accordion_panel(
  #         title = sheet_names[j],
  #         icon  = icon(sheet_icons[[ sheet_names[j] ]] %||% "circle-dot"),
  #         tagList(ui_elems)
  #       )
  #     )
  #   })
  #   
  #   tagList(panels)   # render the list
  # })
  
  
  ## Save, Load and Delete module
  all_inputs <- reactive({
    names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))]
  })
  
  current_input_table <- reactive({
    variables <- all_inputs()
    
    # lower_values <- sapply(variables, function(v) {
    #   val <- input[[v]]
    #   if (length(val) == 1) as.numeric(val) else as.numeric(val[1])
    # })
    # upper_values <- sapply(variables, function(v) {
    #   val <- input[[v]]
    #   if (length(val) == 1) as.numeric(val) else as.numeric(val[2])
    # })
    lower_values <- sapply(variables, function(v) {
      val <- input[[v]]
      
      if (is.logical(val)) {
        return(as.numeric(val))
      }
      
      if (is.character(val)) {
        return(NA_real_)
      }
      
      if (length(val) == 1) as.numeric(val) else as.numeric(val[1])
    })
    
    upper_values <- sapply(variables, function(v) {
      val <- input[[v]]
      
      if (is.logical(val)) {
        return(as.numeric(val))
      }
      
      if (is.character(val)) {
        return(NA_real_)
      }
      
      if (length(val) == 1) as.numeric(val) else as.numeric(val[2])
    })
    # 2. Re-read Excel (keeps original bounds & distributions)
    all_sheets <- c(excelData(), excelFinanceData()) # list of data-frames from both workbooks
    input_file <- bind_rows(all_sheets)  # one big table
    
    # Overwrite lower/upper with current UI inputs
    input_file <- input_file %>%
      left_join(
        tibble(variable = variables,
               lower    = lower_values,
               upper    = upper_values),
        by = "variable",
        suffix = c("", ".new")
      ) %>%
      mutate(
        lower = coalesce(lower.new, lower),
        upper = coalesce(upper.new, upper)
      ) %>%
      select(-ends_with(".new"))
    
    # 3. Append funding scalars
    # View(input_file)
    #print(1)
    
    funding_names <- 
      c("funding_onetime_percentage_initial_cost_schemes_c", "annual_funding_schemes_c",
        "funding_onetime_percentage_consult_schemes_c","funding_onetime_per_tree_schemes_c", "funding_onetime_guard_per_tree_schemes_c",
        "funding_onetime_per_m_treerow_schemes_c", "funding_onetime_per_m_hedgerow_schemes_c","annual_funding_per_m_schemes_c",
        "annual_funding_per_tree_schemes_c", "funding_onetime_schemes_c",
        "onetime_external_percentage_incost_schemes_c","onetime_external_percentage_consult_schemes_c",
        "funding_onetime_per_ha_schemes_c", "onetime_external_support_c", "annual_external_support_c",
        "risk_mitigation_guarantee_c", "risk_mitigation_insurance_c", "selected_loan_scheme_c",
        "loan_bank_selected_c", "loan_impact_selected_c", "loan_dev_bank_selected_c")
    funding_df <- data.frame(variable = funding_names,
                             lower = 0,
                             upper = 0,
                             distribution = "const",
                             stringsAsFactors = FALSE)
    
    try(total_funding <- funding$total_funding_with_private())
    
    if ("total_funding" %in% ls()) {
      #print(2)
      
      input_file <- 
        data.frame(variable = names(total_funding),
                   lower = unname(total_funding),
                   upper = unname(total_funding),
                   distribution = "const") %>% 
        bind_rows(input_file, .)
      
      remain <- funding_names[!(funding_names %in% input_file$variable)]
      input_file <- funding_df %>% 
        filter(variable %in% remain) %>% 
        bind_rows(input_file, .)
      
      # View(input_file)
    }else {
      input_file <- bind_rows(input_file, funding_df)
    }
    # 3b. Append finance scheme selectors as model-ready constants
    selected_loan <- input$selected_loan_scheme_c %||% "none"
    
    loan_selector_df <- data.frame(
      variable = c(
        "loan_bank_selected_c",
        "loan_impact_selected_c",
        "loan_dev_bank_selected_c"
      ),
      lower = c(
        as.numeric(selected_loan == "bank"),
        as.numeric(selected_loan == "impact"),
        as.numeric(selected_loan == "dev_bank")
      ),
      upper = c(
        as.numeric(selected_loan == "bank"),
        as.numeric(selected_loan == "impact"),
        as.numeric(selected_loan == "dev_bank")
      ),
      distribution = "const",
      stringsAsFactors = FALSE
    )
    
    risk_selector_df <- data.frame(
      variable = c(
        "risk_mitigation_guarantee_c",
        "risk_mitigation_insurance_c"
      ),
      lower = c(
        as.numeric(isTRUE(input$risk_mitigation_guarantee_c)),
        as.numeric(isTRUE(input$risk_mitigation_insurance_c))
      ),
      upper = c(
        as.numeric(isTRUE(input$risk_mitigation_guarantee_c)),
        as.numeric(isTRUE(input$risk_mitigation_insurance_c))
      ),
      distribution = "const",
      stringsAsFactors = FALSE
    )
    
    input_file <- input_file %>%
      dplyr::filter(
        !variable %in% c(
          "loan_bank_selected_c",
          "loan_impact_selected_c",
          "loan_dev_bank_selected_c",
          "risk_mitigation_guarantee_c",
          "risk_mitigation_insurance_c",
          "selected_loan_scheme_c"
        )
      ) %>%
      bind_rows(loan_selector_df, risk_selector_df)
    
    
    #View(input_file)
    
    # # 4. Save UI snapshot (optional)
    # saveRDS(list(sheet_names, input_file), "data/Walnut_grain_veg_tub_ui_updated.RDS")
    
    # 5. clean-up: keep only numeric rows
    input_file <- input_file %>%
      filter(
        !is.na(lower), !is.na(upper),
        is.finite(lower), is.finite(upper)
      )
    
    # write.csv(input_file,"data/input_table.csv",row.names = F)
    
    input_file
  })
  
  ## Save/Load functionality ----
  # saveLoadServer("savemod", current_input_table)
  # Provide Folder name instead of the current 'Germany' to store user saves
  get_base_dir <- function() {
    if (Sys.info()[["sysname"]] == "Windows")
      "user-states/AFEuropeanFinancialSupport"
    else
      "/srv/shiny-app-data/user-states/AFEuropeanFinancialSupport"
  }
  
  get_user_dir <- function() {
    uid <- session$user
    safe_uid <- if (is.null(uid) || uid == "") "anon"
    else gsub("[^A-Za-z0-9_.-]", "_", uid)
    dir <- file.path(get_base_dir(), safe_uid)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
  }
  
  timestamp_name <- function(raw) {
    paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),"_",
           gsub("[^A-Za-z0-9_.-]", "_", raw), ".rds")
  }
  
  observeEvent(input$save_btn, {
    dir  <- get_user_dir()
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(files) >= 5) {
      showModal(modalDialog("You already have five versions. Delete one first.",
                            easyClose = TRUE))
      return()
    }
    
    req(nzchar(input$state_name))
    saveRDS(
      list(input_table = current_input_table(),
           raw_inputs  = reactiveValuesToList(input)),
      file.path(dir, timestamp_name(input$state_name))
    )
  })
  
  saved_files <- reactiveFileReader(
    2000, session, get_user_dir(),
    function(dir) sort(list.files(dir, pattern = "\\.rds$", full.names = TRUE),decreasing = T)
  )
  
  observe({
    updateSelectInput(session, "state_picker",
                      choices = basename(saved_files()))
  })
  
  observeEvent(input$load_btn, {
    req(input$state_picker)
    obj <- readRDS(file.path(get_user_dir(), input$state_picker))
    bslib::accordion_panel_open("collapseSidebar",TRUE,session)
    vals <- obj$raw_inputs
    
    restore_one <- function(id, val) {
      if (is.null(val)) return()
      switch(class(val)[1],
             numeric   = updateNumericInput(session, id, value = val),
             integer   = updateNumericInput(session, id, value = val),
             character = updateTextInput   (session, id, value = val),
             logical   = updateCheckboxInput(session, id, value = val),
             factor    = updateSelectInput (session, id, selected = as.character(val)),
             # length-2 numeric == slider
             { if (is.numeric(val) && length(val) == 2)
               updateSliderInput(session, id, value = val) }
      )
    }
    
    # ordinary widgets
    lapply(names(vals), \(id) try(restore_one(id, vals[[id]]), silent = TRUE))
    
    # funding module widgets  (country + state first, the rest after rebuild)
    ns <- NS("funding")   # helper to prepend "funding-"
    
    # (a) push country and state immediately 
    try(updateSelectInput(session, ns("country"),
                          selected = vals[[ns("country")]]), silent = TRUE)
    try(updateSelectInput(session, ns("state"),
                          selected = vals[[ns("state")]]),   silent = TRUE)
    
    # (b) *once* the state really is set, restore the rest
    observeEvent(input[[ns("state")]], {
      if (!identical(input[[ns("state")]], vals[[ns("state")]])) return()
      
      try(updateSelectInput(session, ns("one_schemes"),
                            selected = vals[[ns("one_schemes")]]), silent = TRUE)
      try(updateSelectInput(session, ns("annual_schemes"),
                            selected = vals[[ns("annual_schemes")]]), silent = TRUE)
      try(updateNumericInput(session, ns("onetime_private"),
                             value = vals[[ns("onetime_private")]]),  silent = TRUE)
      try(updateNumericInput(session, ns("annual_private"),
                             value = vals[[ns("annual_private")]]),   silent = TRUE)
    }, once = TRUE, ignoreInit = FALSE)
  })
  
  observeEvent(input$delete_btn, {
    req(input$state_picker)
    unlink(file.path(get_user_dir(), input$state_picker))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("current_input_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(current_input_table(), file)
  )
  
  output$finance_summary_header <- renderUI({
    req(rv_fin$finance_summary_table)
    tags$h4("Finance Summary")
  })
  
  output$finance_summary_note <- renderUI({
    req(rv_fin$finance_summary_table)
    tags$p(
      "Values shown as median (5-95% quantiles) across simulations.",
      style = "text-align:left; font-size: 12px;"
    )
  })
  
  output$finance_summary_table <- renderTable({
    req(rv_fin$finance_summary_table)
    rv_fin$finance_summary_table
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  ## Monte Carlo Simulation ----
  mcSimulation_results <- eventReactive(input$run_simulation, {
    
    waiter_show(
      html = tagList(
        spin_fading_circles(),
        "Simulating and generating your plots ..."
      ),
      color = "rgba(0, 0, 0, 0.8)"
    )
    
    # If something throws, hide the waiter so users aren't stuck
    ok <- FALSE
    on.exit({
      if (!ok) waiter_hide()
    }, add = TRUE)
    
    input_file <- current_input_table()
    
    # 6. Run Monte-Carlo
    # Provide model_function
    data <- decisionSupport::mcSimulation(
      estimate          = decisionSupport::as.estimate(input_file),
      model_function    = AF_benefit_with_Risks,
      numberOfModelRuns = input$num_simulations_c,
      functionSyntax    = "plainNames"
    )
    # 6b. Apply the modular financing/derisking/advisory/market pipeline.
    # The baseline model is still run through mcSimulation; optional modules are
    # applied row-by-row afterwards so scripts only affect selected scenarios.
    get_row_value <- function(row, name, default = NA_real_) {
      if (name %in% names(row)) as.numeric(row[[name]]) else default
    }

    get_row_vector <- function(row, prefix, n_years, default = 0) {
      cols <- paste0(prefix, seq_len(n_years))
      if (all(cols %in% names(row))) {
        return(as.numeric(row[cols]))
      }
      if (prefix %in% names(row)) {
        value <- as.numeric(row[[prefix]])
        return(rep(value, n_years))
      }
      rep(default, n_years)
    }

    make_baseline_result_from_row <- function(row, n_years) {
      AF_total_investment_cost <- get_row_vector(row, "AF_total_investment_cost", n_years)
      AF_total_running_cost <- get_row_vector(row, "AF_total_running_cost", n_years)
      AF_total_benefit <- get_row_vector(row, "AF_total_benefit", n_years)
      AF_total_cost <- AF_total_investment_cost + AF_total_running_cost
      AF_bottom_line_benefit <- AF_total_benefit - AF_total_cost

      AF_total_investment_cost_subs <- get_row_vector(row, "AF_total_investment_cost_subs", n_years)
      AF_total_running_cost_subs <- get_row_vector(row, "AF_total_running_cost_subs", n_years)
      AF_total_benefit_subs <- get_row_vector(row, "AF_total_benefit_subs", n_years)
      AF_total_cost_subs <- AF_total_investment_cost_subs + AF_total_running_cost_subs
      AF_bottom_line_benefit_subs <- AF_total_benefit_subs - AF_total_cost_subs

      list(
        Treeless_NPV = get_row_value(row, "Treeless_NPV"),
        Treeless_cash_flow = get_row_vector(row, "Treeless_cash_flow", n_years),
        Treeless_cum_cash_flow = get_row_vector(row, "Treeless_cum_cash_flow", n_years),
        Treeless_bottom_line_benefit = get_row_vector(row, "Treeless_bottom_line_benefit", n_years),

        AF_NPV = get_row_value(row, "AF_NPV"),
        AF_cash_flow = get_row_vector(row, "AF_cash_flow", n_years),
        AF_cum_cash_flow = get_row_vector(row, "AF_cum_cash_flow", n_years),
        AF_total_investment_cost = AF_total_investment_cost,
        AF_total_running_cost = AF_total_running_cost,
        AF_total_benefit = AF_total_benefit,
        AF_total_cost = AF_total_cost,
        AF_bottom_line_benefit = AF_bottom_line_benefit,

        AF_NPV_subs = get_row_value(row, "AF_NPV_subs"),
        AF_cash_flow_subs = get_row_vector(row, "AF_cash_flow_subs", n_years),
        AF_cum_cash_flow_subs = get_row_vector(row, "AF_cum_cash_flow_subs", n_years),
        AF_total_investment_cost_subs = AF_total_investment_cost_subs,
        AF_total_running_cost_subs = AF_total_running_cost_subs,
        AF_total_benefit_subs = AF_total_benefit_subs,
        AF_total_cost_subs = AF_total_cost_subs,
        AF_bottom_line_benefit_subs = AF_bottom_line_benefit_subs,

        NPV_decision = get_row_value(row, "NPV_decision"),
        CF_decision = get_row_vector(row, "CF_decision", n_years),
        CumCF_decision = get_row_vector(row, "CumCF_decision", n_years),

        Apple_yield_reduction_due_to_weather =
          get_row_vector(row, "Apple_yield_reduction_due_to_weather", n_years),
        consultation_cost = get_row_vector(row, "consultation_cost", n_years),
        machinery_cost = get_row_vector(row, "machinery_cost", n_years),
        labour_cost = get_row_vector(row, "labour_cost", n_years),
        digital_tool_subscription = get_row_vector(row, "digital_tool_subscription", n_years),

        Table_apple_yield = get_row_vector(row, "Table_apple_yield", n_years),
        B_qual_table_apple_yield = get_row_vector(row, "B_qual_table_apple_yield", n_years),
        Juice_apple_yield = get_row_vector(row, "Juice_apple_yield", n_years),
        AF_maize_yield = get_row_vector(row, "AF_maize_yield", n_years),
        AF_wheat_yield = get_row_vector(row, "AF_wheat_yield", n_years),
        AF_barley_yield = get_row_vector(row, "AF_barley_yield", n_years),
        AF_rapeseed_yield = get_row_vector(row, "AF_rapeseed_yield", n_years),
        table_apple_price_market = get_row_vector(row, "table_apple_price_market", n_years),
        B_qual_apple_price_market = get_row_vector(row, "B_qual_apple_price_market", n_years),
        Juice_apple_price_market = get_row_vector(row, "Juice_apple_price_market", n_years),
        maize_value_p = get_row_vector(row, "maize_value_p", n_years),
        wheat_value_p = get_row_vector(row, "wheat_value_p", n_years),
        barley_value_p = get_row_vector(row, "barley_value_p", n_years),
        rapeseed_value_p = get_row_vector(row, "rapeseed_value_p", n_years)
      )
    }

    add_vector_columns <- function(df, prefix, values) {
      for (yr in seq_along(values)) {
        df[[paste0(prefix, yr)]] <- values[[yr]]
      }
      df
    }

    funding_value_sum <- input_file %>%
      dplyr::filter(grepl("funding|external_support", variable)) %>%
      dplyr::summarise(total = sum(abs(as.numeric(lower)), na.rm = TRUE)) %>%
      dplyr::pull(total)
    use_subsidies <- isTRUE(funding_value_sum > 0)
    selected_loan <- input$selected_loan_scheme_c %||% "none"

    financing_type <- dplyr::case_when(
      selected_loan == "bank" ~ "commercial_loan",
      selected_loan == "impact" ~ "impact_investment_loan",
      selected_loan == "dev_bank" ~ "development_bank_loan",
      TRUE ~ "none"
    )

    financing_inputs <- list(
      financing_type = financing_type,
      farmer_own_capital_c = input$farmer_own_capital_c %||% 0,
      commercial_loan_amount_c = input$bank_loan_amount_c %||% 0,
      commercial_annual_repayment_amount_p = input$bank_annual_repayment_amount_p %||% 0,
      commercial_interest_rate_c = input$bank_interest_rate_c %||% 0,
      commercial_repayment_start_year_c = input$bank_repayment_start_year_c %||% 1,
      commercial_maturity_year_c = input$bank_maturity_year_c %||% input$n_years_c,
      Dev_bank_loan_amount_c = input$Dev_bank_loan_amount_c %||% 0,
      Dev_bank_annual_repayment_amount_p = input$Dev_bank_annual_repayment_amount_p %||% 0,
      Dev_bank_interest_rate_c = input$Dev_bank_interest_rate_c %||% 0,
      Dev_bank_repayment_start_year_c = input$Dev_bank_repayment_start_year_c %||% 1,
      Dev_bank_maturity_year_c = input$Dev_bank_maturity_year_c %||% input$n_years_c,
      impact_invst_fund_loan_c = input$impact_invst_fund_loan_c %||% 0,
      impact_invst_annual_repayment_amount_p = input$impact_invst_annual_repayment_amount_p %||% 0,
      impact_invst_fund_interest_rate_c = input$impact_invst_fund_interest_rate_c %||% 0,
      impact_invst_fund_repayment_start_year_c = input$impact_invst_fund_repayment_start_year_c %||% 1,
      impact_invst_bank_maturity_year_c = input$impact_invst_bank_maturity_year_c %||% input$n_years_c
    )

    derisking_inputs <- list(
      use_guarantee = isTRUE(input$risk_mitigation_guarantee_c),
      use_insurance = isTRUE(input$risk_mitigation_insurance_c),
      guarantee_cover_rate_c = input$guarantee_cover_rate_c %||% input$guarantee_amount_c %||% 0,
      guarantee_default_loss_rate_c = input$guarantee_default_loss_rate_c %||% 100,
      guarantee_fee_rate_c = input$guarantee_fee_rate_c %||% 0,
      guarantee_fee_paid_by_farmer =
        identical(input$guarantee_fee_paid_by_farmer, TRUE) ||
        identical(input$guarantee_fee_paid_by_farmer, "TRUE") ||
        identical(input$guarantee_fee_paid_by_farmer, "true") ||
        identical(input$guarantee_fee_paid_by_farmer, 1),
      insurance_cover_rate_c = input$insurance_cover_rate_c %||% 0,
      insurance_payout_amount_c = input$insurance_payout_amount_c %||% 0,
      insurance_annual_premium_c = input$insurance_annual_premium_c %||% 0,
      insurance_annual_premium_surcharge_c = input$insurance_annual_premium_surcharge_c %||% 0
    )
    # derisking_inputs <- list(
    #   use_guarantee = isTRUE(input$risk_mitigation_guarantee_c),
    #   use_insurance = isTRUE(input$risk_mitigation_insurance_c),
    #   guarantee_cover_rate_c = input$guarantee_cover_rate_c %||% 0, #input$guarantee_amount_c %||%
    #   guarantee_default_loss_rate_c = input$guarantee_default_loss_rate_c %||% 100,
    #   guarantee_fee_rate_c = input$guarantee_fee_rate_c %||% 0,
    #   guarantee_fee_paid_by_farmer = isTRUE(input$guarantee_fee_paid_by_farmer),
    #   insurance_cover_rate_c = input$insurance_cover_rate_c %||% 0,
    #   insurance_payout_amount_c = input$insurance_payout_amount_c %||% 0,
    #   insurance_annual_premium_c = input$insurance_annual_premium_c %||% 0,
    #   insurance_annual_premium_surcharge_c = input$insurance_annual_premium_surcharge_c %||% 0
    # )

    advisory_inputs <- list(
      use_advisory = isTRUE(input$use_advisory),
      advisory_support_types = input$advisory_support_types %||% character(0),
      organisation_nominal_fee_c = input$organisation_nominal_fee_c %||% 0,
      organisation_consultation_reduction_perc_c = input$organisation_consultation_reduction_perc_c %||% 0,
      cooperative_machinery_reduction_perc_c = input$cooperative_machinery_reduction_perc_c %||% 0,
      cooperative_labour_reduction_perc_c = input$cooperative_labour_reduction_perc_c %||% 0,
      cooperative_nominal_fee_c = input$cooperative_nominal_fee_c %||% 0,
      digital_tool_subscription_discount_amount_c = input$digital_tool_subscription_discount_amount_c %||% 0
    )

    market_inputs <- list(
      use_market_access = isTRUE(input$use_market_access),
      market_access_types = input$market_access_types %||% character(0),
      price_guarantee_type = input$price_guarantee_type %||% "floor",
      price_guarantee_share_c = input$price_guarantee_share_c %||% 0,
      table_apple_guaranteed_price_c = input$table_apple_guaranteed_price_c %||% 0,
      bqual_apple_guaranteed_price_c = input$bqual_apple_guaranteed_price_c %||% 0,
      juice_apple_guaranteed_price_c = input$juice_apple_guaranteed_price_c %||% 0,
      maize_guaranteed_price_c = input$maize_guaranteed_price_c %||% 0,
      wheat_guaranteed_price_c = input$wheat_guaranteed_price_c %||% 0,
      barley_guaranteed_price_c = input$barley_guaranteed_price_c %||% 0,
      rapeseed_guaranteed_price_c = input$rapeseed_guaranteed_price_c %||% 0,
      price_premium_share_c = input$price_premium_share_c %||% 0,
      table_apple_price_premium_c = input$table_apple_price_premium_c %||% 0,
      bqual_apple_price_premium_c = input$bqual_apple_price_premium_c %||% 0,
      juice_apple_price_premium_c = input$juice_apple_price_premium_c %||% 0,
      maize_price_premium_c = input$maize_price_premium_c %||% 0,
      wheat_price_premium_c = input$wheat_price_premium_c %||% 0,
      barley_price_premium_c = input$barley_price_premium_c %||% 0,
      rapeseed_price_premium_c = input$rapeseed_price_premium_c %||% 0
    )

    scenario_rows <- lapply(seq_len(nrow(data$y)), function(i) {
      row <- data$y[i, , drop = FALSE]
      baseline_result <- make_baseline_result_from_row(row, input$n_years_c)
      discount_rate_row <- get_row_value(row, "discount_rate_p", input$discount_rate_p %||% 0)
      scenario_result <- run_selected_scenario(
        baseline_result = baseline_result,
        n_years_c = input$n_years_c,
        discount_rate_p = discount_rate_row,
        use_subsidies = use_subsidies,
        financing_inputs = financing_inputs,
        derisking_inputs = derisking_inputs,
        advisory_inputs = advisory_inputs,
        market_inputs = market_inputs
      )
      
      plot3_financing_no_subs <- if (selected_loan != "none") {
        run_selected_scenario(
          baseline_result = baseline_result,
          n_years_c = input$n_years_c,
          discount_rate_p = discount_rate_row,
          use_subsidies = FALSE,
          financing_inputs = financing_inputs,
          derisking_inputs = list(use_guarantee = FALSE, use_insurance = FALSE),
          advisory_inputs = list(use_advisory = FALSE),
          market_inputs = list(use_market_access = FALSE)
        )
      } else {
        NULL
      }
      
      plot3_financing_with_subs <- if (selected_loan != "none" && isTRUE(use_subsidies)) {
        run_selected_scenario(
          baseline_result = baseline_result,
          n_years_c = input$n_years_c,
          discount_rate_p = discount_rate_row,
          use_subsidies = TRUE,
          financing_inputs = financing_inputs,
          derisking_inputs = list(use_guarantee = FALSE, use_insurance = FALSE),
          advisory_inputs = list(use_advisory = FALSE),
          market_inputs = list(use_market_access = FALSE)
        )
      } else {
        NULL
      }
      
      has_full_financial_scheme <- selected_loan != "none" ||
        isTRUE(input$risk_mitigation_guarantee_c) ||
        isTRUE(input$risk_mitigation_insurance_c) ||
        isTRUE(input$use_advisory) ||
        isTRUE(input$use_market_access)
      
      full_scheme_no_subs <- if (isTRUE(has_full_financial_scheme)) {
        run_selected_scenario(
          baseline_result = baseline_result,
          n_years_c = input$n_years_c,
          discount_rate_p = discount_rate_row,
          use_subsidies = FALSE,
          financing_inputs = financing_inputs,
          derisking_inputs = derisking_inputs,
          advisory_inputs = advisory_inputs,
          market_inputs = market_inputs
        )
      } else {
        NULL
      }
      
      full_scheme_with_subs <- if (isTRUE(has_full_financial_scheme) && isTRUE(use_subsidies)) {
        run_selected_scenario(
          baseline_result = baseline_result,
          n_years_c = input$n_years_c,
          discount_rate_p = discount_rate_row,
          use_subsidies = TRUE,
          financing_inputs = financing_inputs,
          derisking_inputs = derisking_inputs,
          advisory_inputs = advisory_inputs,
          market_inputs = market_inputs
        )
      } else {
        NULL
      }

      out <- data.frame(
        NPV_Treeless_System = baseline_result$Treeless_NPV,
        NPV_Agroforestry_no_fund = baseline_result$AF_NPV,
        NPV_AF_no_financing_no_subsidy = baseline_result$AF_NPV,
        NPV_AF_financing_no_subsidy = if (!is.null(plot3_financing_no_subs)) plot3_financing_no_subs$AF_NPV else NA_real_,
        NPV_AF_financing_with_subsidies = if (!is.null(plot3_financing_with_subs)) plot3_financing_with_subs$AF_NPV else NA_real_,
        NPV_AF_no_financing_with_subsidies = if (selected_loan == "none" && isTRUE(use_subsidies)) baseline_result$AF_NPV_subs else NA_real_,
        NPV_AF_no_scheme_no_subsidies = baseline_result$AF_NPV,
        NPV_AF_no_scheme_with_subsidies = if (isTRUE(use_subsidies)) baseline_result$AF_NPV_subs else NA_real_,
        NPV_AF_full_scheme_no_subsidies = if (!is.null(full_scheme_no_subs)) full_scheme_no_subs$AF_NPV else NA_real_,
        NPV_AF_full_scheme_with_subsidies = if (!is.null(full_scheme_with_subs)) full_scheme_with_subs$AF_NPV else NA_real_,
        NPV_Agroforestry_System = scenario_result$AF_NPV,
        NPV_decis_AF_ES3 = scenario_result$NPV_decision,
        NPV_DeFAF_Suggestion = baseline_result$AF_NPV_subs,
        NPV_Agroforestry_adj_bank = if (selected_loan == "bank") scenario_result$AF_NPV else NA_real_,
        NPV_Agroforestry_adj_impact = if (selected_loan == "impact") scenario_result$AF_NPV else NA_real_,
        NPV_Agroforestry_adj_dev_bank = if (selected_loan == "dev_bank") scenario_result$AF_NPV else NA_real_,
        NPV_Agroforestry_adj_risk_mit = if (isTRUE(input$risk_mitigation_guarantee_c) || isTRUE(input$risk_mitigation_insurance_c)) scenario_result$AF_NPV else NA_real_,
        NPV_Agroforestry_adj_partners = if (isTRUE(input$use_advisory)) scenario_result$AF_NPV else NA_real_,
        NPV_Agroforestry_adj_APA = if (isTRUE(input$use_market_access)) scenario_result$AF_NPV else NA_real_,
        Agroforestry_Investment1 = baseline_result$AF_total_investment_cost[1],
        Total_Loan_Amount1 = scenario_result$loan_draw %||% 0,
        Total_Investment_Selected1 = if (isTRUE(use_subsidies)) baseline_result$AF_total_investment_cost_subs[1] else baseline_result$AF_total_investment_cost[1],
        Farmer_Capital_Required1 = pmax((if (isTRUE(use_subsidies)) baseline_result$AF_total_investment_cost_subs[1] else baseline_result$AF_total_investment_cost[1]) - (scenario_result$loan_draw %||% 0), 0),
        Farmer_Out_of_Pocket_Investment1 = pmax((if (isTRUE(use_subsidies)) baseline_result$AF_total_investment_cost_subs[1] else baseline_result$AF_total_investment_cost[1]) - (scenario_result$loan_draw %||% 0), 0),
        scenario_type = paste(scenario_result$scenario_type %||% "baseline", collapse = "+"),
        applied_options = paste(scenario_result$applied_options %||% character(0), collapse = "+"),
        stringsAsFactors = FALSE
      )

      out <- add_vector_columns(out, "AF_CF", scenario_result$AF_cash_flow)
      out <- add_vector_columns(out, "AF_CF_adj_bank", if (selected_loan == "bank") scenario_result$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_adj_impact", if (selected_loan == "impact") scenario_result$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_adj_dev_bank", if (selected_loan == "dev_bank") scenario_result$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_adj_risk_mit", if (isTRUE(input$risk_mitigation_guarantee_c) || isTRUE(input$risk_mitigation_insurance_c)) scenario_result$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_adj_partners", if (isTRUE(input$use_advisory)) scenario_result$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_adj_APA", if (isTRUE(input$use_market_access)) scenario_result$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_no_scheme_no_subsidies", baseline_result$AF_cash_flow)
      out <- add_vector_columns(out, "AF_CF_no_scheme_with_subsidies", if (isTRUE(use_subsidies)) baseline_result$AF_cash_flow_subs else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_full_scheme_no_subsidies", if (!is.null(full_scheme_no_subs)) full_scheme_no_subs$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CF_full_scheme_with_subsidies", if (!is.null(full_scheme_with_subs)) full_scheme_with_subs$AF_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CumCF_no_scheme_no_subsidies", baseline_result$AF_cum_cash_flow)
      out <- add_vector_columns(out, "AF_CumCF_no_scheme_with_subsidies", if (isTRUE(use_subsidies)) baseline_result$AF_cum_cash_flow_subs else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CumCF_full_scheme_no_subsidies", if (!is.null(full_scheme_no_subs)) full_scheme_no_subs$AF_cum_cash_flow else rep(NA_real_, input$n_years_c))
      out <- add_vector_columns(out, "AF_CumCF_full_scheme_with_subsidies", if (!is.null(full_scheme_with_subs)) full_scheme_with_subs$AF_cum_cash_flow else rep(NA_real_, input$n_years_c))
      out
    })

    scenario_y <- dplyr::bind_rows(scenario_rows)
    data$y <- dplyr::bind_cols(
      data$y,
      scenario_y[, setdiff(names(scenario_y), names(data$y)), drop = FALSE]
    )
    # Ensure data folder exists
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)
    
    # Save full mcSimulation object (for debugging)
    # saveRDS(
    #   data,
    #   file = file.path("data", paste0("mcSimulation_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
    # )
    
    # Debug!!! Optional: also save quick-inspection CSVs
    # if (!is.null(data$y)) {
    #   write.csv(data$y, file.path("data", "mcSimulation_y_outputs.csv"), row.names = FALSE)
    # }
    # if (!is.null(data$x)) {
    #   write.csv(data$x, file.path("data", "mcSimulation_x_inputs.csv"), row.names = FALSE)
    # }
    
    # Hide AFTER dependent outputs have re-rendered
    session$onFlushed(function() waiter_hide(), once = TRUE)
    ok <- TRUE
    
    data
    
  })
  
  ## Generating plots ----
  # helper to add title subtile caption etc
  add_meta <- function(p, title, subtitle = NULL, caption = NULL,
                       legend = "bottom") {
    
    p +
      labs(title = title, subtitle = subtitle, caption = caption) +
      theme(
        plot.title = element_textbox_simple(
          size   = 24,
          face   = "bold",
          width  = unit(1, "npc"),  # full plot width
          halign = 0.5,              # centered
          margin = margin(t = 6,b = 20)
        ),
        plot.subtitle = element_textbox_simple(
          size   = 18,
          width  = unit(1, "npc"),
          halign = 0.5,
          margin = margin(t = 6,b = 20)
        ),
        plot.caption  = element_textbox_simple(
          size   = 16,
          width  = unit(0.98, "npc"),
          halign = 0,              # left-aligned
          margin = margin(t = 6,b = 20),
          hjust = 0,
          vjust = 1
        ),
        axis.title = element_text(size = 18, margin = margin (t=50)),
        axis.text = element_text(size = 10, margin = margin (r=10)),
        legend.text     = element_text(size = 14, hjust = 0.5),
        legend.position = legend,
        plot.margin = margin(t = 50, r = 10, b = 50, l = 10, unit = "pt")
        
      )  }
  
  # download helper
  make_download <- function(id, plot_obj, filename, width = 13, height = 5, dpi = 300, scale = 2) {
    output[[id]] <- downloadHandler(
      filename = function() filename,
      content  = function(file) {
        # device is inferred from file extension; here "png"
        ggsave(file, plot_obj, width = width, height = height, dpi = dpi, scale = scale)
      }
    )
  }
  
  ggplotly_with_meta <- function(
    p,
    tooltip = c("x", "y", "colour"),
    legend = NULL,
    subtitle_size = 14,
    caption_size  = 14,
    axis_title_size = 10,
    legend_text_size = 10,
    subtitle_y = 1.02,
    axis_title_y = -0.015,
    caption_y  = -0.35,
    caption_width   = 120
  ) {
    # Apply theme modifications BEFORE converting to plotly
    p <- p +
      theme(
        axis.title = element_text(size = axis_title_size),
        legend.text = element_text(size = legend_text_size)
      )
    # Convert ggplot -> plotly
    pl <- ggplotly(p, tooltip = tooltip)
    
    # Extract labels
    labs <- ggplot_build(p)$plot$labels
    subtitle_txt <- labs$subtitle %||% ""
    caption_txt  <- labs$caption %||% ""
    
    # ---- Annotations ----
    ann <- list()
    
    # Subtitle (above plot)
    if (!is.null(subtitle_txt) && nzchar(subtitle_txt)) {
      ann <- c(ann, list(
        list(
          text = subtitle_txt,
          x = 0.5, xref = "paper",
          y = 1.08, yref = "paper",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "bottom",
          font = list(size = subtitle_size)
        )
      ))
    }
    
    if (!is.null(caption_txt) && nzchar(caption_txt)) {
      wrapped_caption <- paste(
        strwrap(caption_txt, width = caption_width),
        collapse = "<br>"
      )
      ann <- c(ann, list(
        list(
          text = wrapped_caption,
          x = 0, xref = "paper",
          y = caption_y, yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = caption_size)
        )
      ))
    }
    
    pl <- pl %>% layout(annotations = ann)
    
    if (!is.null(legend)) {
      pl <- pl %>% layout(legend = legend)
    }
    
    pl
  }
  
  
  observeEvent(mcSimulation_results(), {
    tryCatch({
      mc_data <- mcSimulation_results()
      
      `%||%` <- function(a, b) if (!is.null(a)) a else b # used in the DA function
      
      # ---- helpers (put once, anywhere in server scope; safe to keep here too) ----
      fmt_ci <- function(x, digits = 2) {
        # Returns "median (p05 to p95)" or NA.
        if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
        med <- stats::median(x, na.rm = TRUE)
        p05 <- stats::quantile(x, 0.05, na.rm = TRUE, names = FALSE, type = 7)
        p95 <- stats::quantile(x, 0.95, na.rm = TRUE, names = FALSE, type = 7)
        sprintf(
          paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"),
          med, p05, p95
        )
      }
      
      nz_num <- function(x) {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) return(0)
        as.numeric(x[1])
      }
      
      # Finance summary table (Main panel) ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â with uncertainty
      mc_data <- mcSimulation_results()
      req(!is.null(mc_data$y))
      
      validate(
        need("Total_Loan_Amount1" %in% names(mc_data$y), "Missing model output: Total_Loan_Amount1"),
        need("Total_Investment_Selected1" %in% names(mc_data$y), "Missing model output: Total_Investment_Selected1"),
        need("Farmer_Capital_Required1" %in% names(mc_data$y), "Missing model output: Farmer_Capital_Required1")
      )
      
      loan_dist <- mc_data$y$Total_Loan_Amount1
      inv_dist <- mc_data$y$Total_Investment_Selected1
      cap_dist <- mc_data$y$Farmer_Capital_Required1
      
      # Build single-row table (strings for CI columns)
      rv_fin$finance_summary_table <- data.frame(
        `Total loan amount` = fmt_ci(loan_dist, digits = 2),
        `Total investment required` = fmt_ci(inv_dist, digits = 2),
        `Farmer capital required` = fmt_ci(cap_dist, digits = 2),
        check.names = FALSE
      )
      
      # Plots section
      # build a long data frame from mc_data
      plot1_data <- mc_data$y %>%
        dplyr::select(NPV_Treeless_System, NPV_Agroforestry_System) %>%
        tidyr::pivot_longer(
          cols      = everything(),
          names_to  = "System",
          values_to = "NPV"
        ) %>%
        dplyr::mutate(
          System = dplyr::recode(
            System,
            "NPV_Treeless_System"      = "Monoculture",
            "NPV_Agroforestry_System"  = "Agroforestry with selected support"
          )
        )
      
      # static ggplot (used both for ggplotly *and* for download)
      plot1_b <- ggplot(plot1_data, aes(x = NPV, fill = System, colour = System)) +
        geom_density(alpha = 0.5) +
        theme_minimal(base_size = 10) +
        labs(
          x = paste0("Net Present Value (€) over ",input$n_years_c, " years for ",
                     input$arable_area_treeless_c, " ha."
          ),
          y    = "Probability",
          fill = "System",
          colour = "System"
        ) +
        theme(
          legend.position = "bottom"
        )
      plot1 <- plot1_b |>
        add_meta(
          title    = "Figure 1. Probabilistic distributions of Net Present Value",
          subtitle = "Agroforestry with selected support vs. conventional farming",
          caption  = "The figure above shows the comparison of Net Present Value (NPV) outcomes for agroforestry (alley cropping with fruit trees) vs a monoculture system. The x-axis displays NPV values (i.e., the sum of discounted annual cash flows). The higher and wider the distribution, the greater the potential return and variability in outcomes under that system."
        ) +
        theme(
          plot.title = element_textbox_simple(
            size   = 16,         
            face   = "bold",
            width  = unit(1, "npc"),
            halign = 0.5,
            margin = margin(t = 6, b = 20)
          )
        )
      # Extract caption text from the ggplot (used for HTML below plot)
      plot1_caption_txt <- ggplot_build(plot1)$plot$labels$caption
      
      output$plot1_caption <- renderUI({
        if (is.null(plot1_caption_txt) || !nzchar(plot1_caption_txt)) return(NULL)
        
        tags$p(
          plot1_caption_txt,
          style = "
      text-align: center;
      margin-top: 5px;
      margin-bottom: 5px;
      max-width: 100%;
    "
        )
      })
      
      # Plot 2
      # plot2 <- decisionSupport::plot_distributions(
      #   mc_data, "NPV_decis_AF_ES3",
      #   method     = "smooth_simple_overlay",
      #   old_names  = "NPV_decis_AF_ES3",
      #   new_names  = "Agroforestry ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œ Treeless",
      #   x_axis_name= "NPV (ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬)",
      #   y_axis_name= "Probability") |>
      #   add_meta(
      #     title    = "Figure 2. Distribution of the *incremental* NPV",
      #     subtitle = "Difference between agroforestry and treeless farming under identical conditions",
      #     caption  = "Figure 2 shows the NPV distributions of the decision to establish the fruit-tree alley cropping system
      #           as compared to the decision to continue with monoculture for the specified time (i.e., NPV agroforestry - NPV monoculture under identical conditions).
      #           The x-axis displays NPV values (i.e., the sum of discounted annual cash flows) and y-axis displays the probability of each NPV amount to occur (i.e., higer y-values indicate higher probability)"
      #     , legend = "none")
      
      # Figure 3: AF NPV across financing and subsidy combinations (interactive)
      plot3_data <- mc_data$y %>%
        dplyr::select(
          NPV_AF_no_financing_no_subsidy,
          NPV_AF_financing_no_subsidy,
          NPV_AF_financing_with_subsidies,
          NPV_AF_no_financing_with_subsidies
        ) %>%
        tidyr::pivot_longer(
          cols      = everything(),
          names_to  = "Scheme",
          values_to = "NPV"
        ) %>%
        dplyr::filter(!is.na(NPV)) %>%
        dplyr::mutate(
          Scheme = dplyr::recode(
            Scheme,
            "NPV_AF_no_financing_no_subsidy" = "No financing + no subsidies",
            "NPV_AF_financing_no_subsidy" = "Selected financing + no subsidies",
            "NPV_AF_financing_with_subsidies" = "Selected financing + subsidies",
            "NPV_AF_no_financing_with_subsidies" = "No financing + subsidies"
          ),
          Scheme = factor(
            Scheme,
            levels = c(
              "No financing + no subsidies",
              "No financing + subsidies",
              "Selected financing + no subsidies",
              "Selected financing + subsidies"
            )
          )
        )
      
      plot3_b <- ggplot(
        plot3_data, aes(x     = NPV, fill  = Scheme, colour= Scheme,text  = Scheme)) +
        geom_density(alpha = 0.5) +
        theme_minimal(base_size = 10) +
        labs(
          x = paste0("Net Present Value of agroforestry system over ",input$n_years_c, " years for ",
                     input$arable_area_treeless_c, " ha."
          ),
          y = "Probability",
          fill   = "Scenario",
          colour = "Scenario"
        ) +
        theme(
          legend.position = "bottom"
          # legend.margin   = margin(t = 5, b = 20),   # space between legend and caption
          # axis.title.x    = element_text(margin = margin(t = 10, b = 5)),  # small space above legend
          # plot.margin     = margin(t = 20, r = 20, b = 40, l = 20)  # extra bottom room overall
        )
      
      plot3 <- plot3_b |>
        add_meta(
          title    = "Figure 2. Net Present Value (NPV) Outcomes for Agroforestry under Financing and Subsidy Scenarios",
          subtitle = "Agroforestry NPV compared across selected financing and subsidy combinations",
          caption  = "This figure compares agroforestry net present value (NPV) under no financing and no subsidies, selected financing without subsidies, and selected financing with subsidies. If no financing scheme is selected, only the no-financing scenarios are shown."
        ) +
        theme(
          plot.title = element_textbox_simple(
            size   = 16,
            face   = "bold",
            width  = unit(1, "npc"),
            halign = 0.5,
            margin = margin(t = 6, b = 20)
          )
        )
      
      # Extract caption for HTML under the plot
      plot3_caption_txt <- ggplot_build(plot3)$plot$labels$caption
      
      output$plot3_caption <- renderUI({
        if (is.null(plot3_caption_txt) || !nzchar(plot3_caption_txt)) return(NULL)
        
        tags$p(
          plot3_caption_txt,
          style = "
      text-align: center;
      margin-top: 10px;
      margin-bottom: 5px;
      max-width: 100%;
    "
        )
      })
      
      # Figure 4: AF NPV across full scheme and no-scheme subsidy combinations (interactive)
      plot4_data <- mc_data$y %>%
        dplyr::select(
          NPV_AF_no_scheme_no_subsidies,
          NPV_AF_no_scheme_with_subsidies,
          NPV_AF_full_scheme_no_subsidies,
          NPV_AF_full_scheme_with_subsidies
        ) %>%
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "Scheme",
          values_to = "NPV"
        ) %>%
        dplyr::filter(!is.na(NPV)) %>%
        dplyr::mutate(
          Scheme = dplyr::recode(
            Scheme,
            "NPV_AF_no_scheme_no_subsidies" = "No scheme + no subsidies",
            "NPV_AF_no_scheme_with_subsidies" = "No scheme + subsidies",
            "NPV_AF_full_scheme_no_subsidies" = "Full financial scheme + no subsidies",
            "NPV_AF_full_scheme_with_subsidies" = "Full financial scheme + subsidies"
          ),
          Scheme = factor(
            Scheme,
            levels = c(
              "No scheme + no subsidies",
              "No scheme + subsidies",
              "Full financial scheme + no subsidies",
              "Full financial scheme + subsidies"
            )
          )
        )
      
      validate(need(nrow(plot4_data) > 0, "Plot4: No NPV scenario data found."))
      
      plot4 <- ggplot(
        plot4_data,
        aes(x = NPV, fill = Scheme, colour = Scheme, text = Scheme)
      ) +
        geom_density(alpha = 0.5) +
        theme_minimal(base_size = 10) +
        labs(
          x = paste0(
            "Net Present Value of agroforestry system over ",
            input$n_years_c, " years for ", input$arable_area_treeless_c, " ha."
          ),
          y = "Probability",
          fill = "Scenario",
          colour = "Scenario",
          title = "Figure 3. AF NPV under Full Financial Scheme and Subsidy Scenarios",
          subtitle = "Full selected financial scheme compared with no scheme, with and without subsidies",
          caption = "This figure compares agroforestry net present value (NPV) under the full selected financial scheme, including selected financing, derisking, advisory, and market/value-chain mechanisms, against no scheme. Each is shown with and without subsidies where applicable."
        ) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 0)
        )
      
      plot4_caption_txt <- ggplot_build(plot4)$plot$labels$caption
      
      output$plot4_caption <- renderUI({
        if (is.null(plot4_caption_txt) || !nzchar(plot4_caption_txt)) return(NULL)
        tags$p(
          plot4_caption_txt,
          style = "text-align:center; margin-top:10px; margin-bottom:5px; max-width:100%;"
        )
      })
            scenario_labels <- c(
        "no_scheme_no_subsidies" = "No scheme + no subsidies",
        "no_scheme_with_subsidies" = "No scheme + subsidies",
        "full_scheme_no_subsidies" = "Full financial scheme + no subsidies",
        "full_scheme_with_subsidies" = "Full financial scheme + subsidies"
      )
      
      scenario_levels <- unname(scenario_labels)
      scenario_palette <- c(
        "No scheme + no subsidies" = "#1B6CA8",
        "No scheme + subsidies" = "#45A778",
        "Full financial scheme + no subsidies" = "#C55A11",
        "Full financial scheme + subsidies" = "#7B3294"
      )
      
      build_cashflow_summary <- function(mc_y, prefix, value_name) {
        mc_y %>%
          dplyr::select(dplyr::matches(paste0("^", prefix, "_(no_scheme_no_subsidies|no_scheme_with_subsidies|full_scheme_no_subsidies|full_scheme_with_subsidies)\\d+$"))) %>%
          tidyr::pivot_longer(
            cols = everything(),
            names_to = "name",
            values_to = value_name
          ) %>%
          dplyr::mutate(
            Year = as.integer(stringr::str_extract(name, "\\d+$")),
            Scenario_key = stringr::str_remove(name, paste0("^", prefix, "_")),
            Scenario_key = stringr::str_remove(Scenario_key, "\\d+$"),
            Scenario = dplyr::recode(Scenario_key, !!!scenario_labels),
            Scenario = factor(Scenario, levels = scenario_levels)
          ) %>%
          dplyr::filter(!is.na(Year), Year <= input$n_years_c, !is.na(Scenario)) %>%
          dplyr::group_by(Scenario, Year) %>%
          dplyr::summarise(
            q05 = stats::quantile(.data[[value_name]], 0.05, na.rm = TRUE),
            median_value = stats::quantile(.data[[value_name]], 0.50, na.rm = TRUE),
            q95 = stats::quantile(.data[[value_name]], 0.95, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::filter(!is.na(q05), !is.na(median_value), !is.na(q95))
      }
      
      plot5_summary <- build_cashflow_summary(
        mc_y = mc_data$y,
        prefix = "AF_CF",
        value_name = "Cashflow"
      )
      
      validate(need(nrow(plot5_summary) > 0, "Plot5: No annual cash-flow scenario data found."))
      
      plot5 <- ggplot(plot5_summary, aes(x = Year, group = Scenario)) +
        geom_ribbon(aes(ymin = q05, ymax = q95, fill = Scenario), alpha = 0.14, show.legend = FALSE) +
        geom_line(aes(y = median_value, colour = Scenario, text = Scenario), linewidth = 1.1) +
        scale_x_continuous(
          limits = c(1, input$n_years_c),
          breaks = seq(1, input$n_years_c, by = 4),
          expand = c(0, 0)
        ) +
        labs(
          x = "Number of years",
          y = "Annual cash-flow from agroforestry",
          colour = "Scenario",
          fill = "Scenario",
          title = "Figure 4. Annual Cash-Flow under Full Financial Scheme and Subsidy Scenarios",
          subtitle = "Median annual cash-flow with 5-95% uncertainty bands",
          caption = "This figure compares annual agroforestry cash-flow under no scheme and the full selected financial scheme, each with and without subsidies. Lines show median cash-flow per year; shaded ribbons show 5-95% uncertainty."
        ) +
        scale_colour_manual(values = scenario_palette, drop = TRUE) +
        scale_fill_manual(values = scenario_palette, drop = TRUE) +
        guides(fill = "none") +
        theme_minimal(base_size = 10) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 0)
        )
      
      plot5_caption_txt <- ggplot_build(plot5)$plot$labels$caption
      output$plot5_caption <- renderUI({
        if (is.null(plot5_caption_txt) || !nzchar(plot5_caption_txt)) return(NULL)
        tags$p(
          plot5_caption_txt,
          style = "text-align:center; margin-top:10px; margin-bottom:5px; max-width:100%;"
        )
      })
      
      plot6_summary <- build_cashflow_summary(
        mc_y = mc_data$y,
        prefix = "AF_CumCF",
        value_name = "CumCashflow"
      )
      
      validate(need(nrow(plot6_summary) > 0, "Plot6: No cumulative cash-flow scenario data found."))
      
      plot6 <- ggplot(plot6_summary, aes(x = Year, group = Scenario)) +
        geom_ribbon(aes(ymin = q05, ymax = q95, fill = Scenario), alpha = 0.14, show.legend = FALSE) +
        geom_line(aes(y = median_value, colour = Scenario, text = Scenario), linewidth = 1.1) +
        scale_x_continuous(
          limits = c(1, input$n_years_c),
          breaks = seq(1, input$n_years_c, by = 4),
          expand = c(0, 0)
        ) +
        labs(
          x = "Number of years",
          y = "Cumulative cash-flow from agroforestry",
          colour = "Scenario",
          fill = "Scenario",
          title = "Figure 5. Cumulative Cash-Flow under Full Financial Scheme and Subsidy Scenarios",
          subtitle = "Median cumulative cash-flow with 5-95% uncertainty bands",
          caption = "This figure compares cumulative agroforestry cash-flow under no scheme and the full selected financial scheme, each with and without subsidies. Lines show median cumulative cash-flow per year; shaded ribbons show 5-95% uncertainty."
        ) +
        scale_colour_manual(values = scenario_palette, drop = TRUE) +
        scale_fill_manual(values = scenario_palette, drop = TRUE) +
        guides(fill = "none") +
        theme_minimal(base_size = 10) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 0)
        )
      
      plot6_caption_txt <- ggplot_build(plot6)$plot$labels$caption
      output$plot6_caption <- renderUI({
        if (is.null(plot6_caption_txt) || !nzchar(plot6_caption_txt)) return(NULL)
        tags$p(
          plot6_caption_txt,
          style = "text-align:center; margin-top:10px; margin-bottom:5px; max-width:100%;"
        )
      })
      
      # plot5 <- decisionSupport::plot_cashflow(
      #   mc_data, "AF_CCF_ES3",
      #   x_axis_name = "",
      #   y_axis_name = "Cumulative cash-flow from Agroforestry (ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬)",
      #   color_25_75 = "navajowhite",
      #   color_5_95 = "green4",
      #   color_median = "lightblue",
      #   facet_labels = "") |>
      #   add_meta(
      #     title   = "Figure 5. Cumulative cash-flow of the agroforestry intervention", 
      #     subtitle = "Long-term cumulative cash-flow projection for an agroforestry system",
      #     caption = "Figure 5  illustrates how total cash-flow (expressed in ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬) accumulates over time from an agroforestry intervention, based on a range of simulated outcomes. The shaded areas represent uncertainty (spread of possible results), and the blue line indicates the median trajectory. Cumulative returns grow steadily over time, showing the long-term profitability potential of agroforestry. Despite initial variability, the system trends positively, reinforcing the case for agroforestry as a viable financial investment over the long run."
      #   )
      
      # plot6 <- decisionSupport::plot_cashflow(
      #   mc_data, "Cashflow_AF1_decision",
      #   x_axis_name = "",
      #   y_axis_name = "Annual cash-flow (ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬)",
      #   facet_labels = "") |>
      #   add_meta(
      #     title   = "Figure 6. Incremental annual cash-flow",
      #     subtitle= "Agroforestry minus baseline farming",
      #     caption = "Figure 6 shows the difference (expressed in ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.")
      # 
      # plot7 <- decisionSupport::plot_cashflow(
      #   mc_data, "Cum_Cashflow_AF1_decision",
      #   x_axis_name = "",
      #   y_axis_name = "Cumulative cash-flow (ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬)",
      #   facet_labels = "") |>
      #   add_meta(
      #     title   = "Figure 7. Incremental cumulative cash-flow",
      #     subtitle= "Agroforestry minus baseline farming",
      #     caption = 'Figure 7 shows the cumulative difference (expressed in ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.')
      # 
      
      # Send plots to UI
      
      # INTERACTIVE version for the app
      output$plot1_ui <- renderPlotly({
        ggplotly_with_meta(
          plot1,
          tooltip = c("x", "y", "colour"),   # no duplicate System
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            title = list(text = "System")
          )
          #,
          # axis_title_size  = 12,   # customize axis title size
          # legend_text_size = 12,   # customize legend text size
          # subtitle_size    = 12,
          # caption_width    = 140,
          # caption_size     = 16
        )
      })
      make_download("download_plot1", plot1, "Figure1_NPV.png")
      output$plot1_dl_ui <- renderUI({
        downloadButton("download_plot1", "Download Figure 1")
      })
      
      # output$plot1_ui <- renderPlotly({
      #   ggplotly(plot1, tooltip = c("x", "y", "colour"))
      # })
      #   
      #   #renderPlot({ plot1 })
      # make_download("download_plot1", plot1, "Figure1_NPV.png")
      # output$plot1_dl_ui <- renderUI({
      #   downloadButton("download_plot1", "Download Figure 1")
      # })
      
      # output$plot2_ui <- renderPlot({ plot2 })
      # make_download("download_plot2", plot2, "Figure2_Decision_NPV.png")
      # output$plot2_dl_ui <- renderUI({
      #   downloadButton("download_plot2", "Download Figure 2")
      # })
      
      output$plot3_ui <- renderPlotly({
        ggplotly_with_meta(
          plot3,
          tooltip = c("x", "y", "text"),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            title = list(text = "Scenario")
          )
        )
      })
      
      make_download("download_plot3", plot3, "Figure2_Funding_NPVs.png")
      
      output$plot3_dl_ui <- renderUI({
        downloadButton("download_plot3", "Download Figure 2")
      })
      output$plot4_ui <- renderPlotly({
        ggplotly_with_meta(
          plot4,
          tooltip = c("x", "y", "text"),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            title = list(text = "Scenario")
          )
        )
      })
            # ---- Download (must use plot4 object built above) ----
      make_download("download_plot4", plot4, "Figure3_Full_Scheme_NPVs.png")
      
      output$plot4_dl_ui <- renderUI({
        downloadButton("download_plot4", "Download Figure 3")
      })
      
      output$plot5_ui <- renderPlotly({
        ggplotly_with_meta(
          plot5,
          tooltip = c("x", "y", "text"),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            title = list(text = "Scenario")
          )
        )
      })
      make_download("download_plot5", plot5, "Figure4_Annual_Cashflow.png")
      output$plot5_dl_ui <- renderUI({
        downloadButton("download_plot5", "Download Figure 4")
      })
      
      output$plot6_ui <- renderPlotly({
        ggplotly_with_meta(
          plot6,
          tooltip = c("x", "y", "text"),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            title = list(text = "Scenario")
          )
        )
      })
      make_download("download_plot6", plot6, "Figure5_Cumulative_Cashflow.png")
      output$plot6_dl_ui <- renderUI({
        downloadButton("download_plot6", "Download Figure 5")
      })
      # 
      # output$plot7_ui <- renderPlot({ plot7 })
      # make_download("download_plot7", plot7, "Figure7_Incremental_Cumulative_CF.png")
      # output$plot7_dl_ui <- renderUI({
      #   downloadButton("download_plot7", "Download Figure 7")
      # })
      
      
      # # Ask user whether to run EVPI (takes time!)
      # showModal(modalDialog(
      #   title = "Run EVPI analysis?",
      #   "Do you want to assess the Expected Value of Perfect Information (EVPI)?",
      #   br(),
      #   "This step may take a while, but you can explore the other graphs while the EVPI is processed.
      #   The EVPI graph will appear at the bottom of the page, below the last graph.",
      #   footer = tagList(
      #     modalButton("No"),
      #     actionButton("confirm_evpi", "Yes, run EVPI")
      #   )
      # ))
      # 
      # # Handle user confirmation to run EVPI
      # observeEvent(input$confirm_evpi, {
      #   
      #   removeModal()  # remove popup
      #   
      #   # Try running EVPI only if it can return meaningful values
      #   tryCatch({
      #     evpi_input <- as.data.frame(cbind(
      #       mc_data$x,
      #       NPV_decision_AF1 = mc_data$y$NPV_decis_AF_ES3
      #     ))
      #     # Provide the NPV_decision variable to calculate EVPI
      #     evpi_result <- decisionSupport::multi_EVPI(evpi_input, "NPV_decis_AF_ES3")
      #     
      #     # saveRDS(evpi_input, "evpi_input_test.rds")
      #     # evpi_input <- readRDS("evpi_input_test.rds")
      #     
      #     # saveRDS(evpi_result, "evpi_result_test.rds")
      #     # evpi_result <- readRDS("evpi_result_test.rds")
      #     
      #     var_lookup <- bind_rows(excelData()) %>%
      #       filter(!is.na(variable), !is.na(name)) %>%
      #       distinct(variable, name) %>%
      #       deframe()
      #     
      #     plot8 <- plot_evpi(evpi_result, decision_vars = "NPV_decis_AF_ES3",
      #                        new_names = "") +
      #       scale_y_discrete(labels = var_lookup)
      #     
      #     plot8 <- plot8 |>
      #       add_meta(title = "Figure 8. EVPI for Each Variable",
      #                subtitle = "Maximum amount worth paying for perfect information on each variable."
      #       )
      #     
      #     output$plot8_ui <- renderPlot({ plot8 })
      #     
      #     make_download("download_plot8", plot8, "Figure8_EVPI.png")
      #     
      #     output$plot8_dl_ui <- renderUI({
      #       downloadButton("download_plot8", "Download Figure 8")
      #     })
      #     
      #   }, error = function(e) {
      #     warning("EVPI plot skipped due to error: ", e$message)
      #     output$plot8_ui <- renderPlot({
      #       plot.new()
      #       text(0.5, 0.5, "There are no variables with a positive EVPI.\nGetting better information will \nnot reduce the level of uncertainty of the decision.", cex = 1.2)
      #     })
      #   })
      # })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error while building outputs",
        pre(e$message),
        easyClose = TRUE
      ))
    })
  })
  
}
shinyApp(ui = ui, server = server)


