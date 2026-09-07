# ============================================================
# COWPEA Zn-Fe DECISION SUPPORT FRAMEWORK
# Prototype V1.5
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(ranger)
library(nnet)

# ------------------------------------------------------------
# 1. LOAD FROZEN ANALYTICAL ENGINE V1.0
# ------------------------------------------------------------

load("ML_framework_V1.0.RData")

# ------------------------------------------------------------
# 1A. EXPERIMENTAL DOMAIN CHECK FOR CLOUD
# ------------------------------------------------------------

check_domain <- function(Zn, Fe, AppMode) {

  valid_zn <- c(0, 7.5, 15, 22.5)
  valid_fe <- c(0, 5, 15, 25)
  valid_mode <- c("S", "F", "SF")

  within_domain <-
    Zn %in% valid_zn &&
    Fe %in% valid_fe &&
    AppMode %in% valid_mode

  warning_message <- if (within_domain) {
    paste(
      "Recommendation is within the experimental domain.",
      "It corresponds to one of the 48 Zn × Fe × application-mode",
      "combinations evaluated in the field experiment."
    )
  } else {
    paste(
      "Warning: recommendation is outside the experimental domain.",
      "This prototype should only be used for the 48 experimentally",
      "tested Zn × Fe × application-mode combinations."
    )
  }

  data.frame(
    Within_Experimental_Domain = within_domain,
    Warning = warning_message,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 1A. RESTORE SCENARIO RECOMMENDATION FUNCTION FOR CLOUD
# ------------------------------------------------------------

recommend_treatment_scenario <- function(
    objective = c(
      "max_yield",
      "max_zn",
      "max_fe",
      "max_profit",
      "balanced"
    ),
    scenario = c(
      "Baseline",
      "Favorable",
      "Unfavorable"
    )
) {

  objective <- match.arg(objective)
  scenario  <- match.arg(scenario)

  scenario_data <- resultado_cenarios %>%
    filter(Scenario == scenario)

  recommendation <- switch(

    objective,

    max_yield =
      scenario_data %>%
      arrange(desc(Pred_Yield)) %>%
      slice_head(n = 1),

    max_zn =
      scenario_data %>%
      arrange(desc(Pred_Zn_mgkg)) %>%
      slice_head(n = 1),

    max_fe =
      scenario_data %>%
      arrange(desc(Pred_Fe_mgkg)) %>%
      slice_head(n = 1),

    max_profit =
      scenario_data %>%
      arrange(desc(Net_Profit_Scenario)) %>%
      slice_head(n = 1),

    balanced =
      scenario_data %>%
      filter(Pareto_4obj_scenario) %>%
      arrange(Dist_Ideal_4_scenario) %>%
      slice_head(n = 1)
  )

  domain_result <- check_domain(
    Zn = recommendation$Zn[1],
    Fe = recommendation$Fe[1],
    AppMode = as.character(recommendation$AppMode[1])
  )

  objective_label <- switch(
    objective,
    max_yield  = "Maximum Yield",
    max_zn     = "Maximum Grain Zn",
    max_fe     = "Maximum Grain Fe",
    max_profit = "Maximum Net Profit",
    balanced   = "Best Balanced Bioeconomic Compromise"
  )

  output <- data.frame(
    Scenario = scenario,
    Objective = objective_label,
    Zn_kg_ha = recommendation$Zn[1],
    Fe_kg_ha = recommendation$Fe[1],
    Application_Mode = as.character(recommendation$AppMode[1]),
    Predicted_Yield_kg_ha = recommendation$Pred_Yield[1],
    Predicted_Grain_Zn_mg_kg = recommendation$Pred_Zn_mgkg[1],
    Predicted_Grain_Fe_mg_kg = recommendation$Pred_Fe_mgkg[1],
    Net_Profit_USD_ha = recommendation$Net_Profit_Scenario[1],
    Within_Experimental_Domain =
      domain_result$Within_Experimental_Domain[1],
    Reliability_Message =
      domain_result$Warning[1]
  )

  return(output)
}

# ------------------------------------------------------------
# 1A.2 REBUILD TREATMENT-LEVEL EXPERIMENTAL UNCERTAINTY
# ------------------------------------------------------------

if (!exists("dados_decisao")) {

  if (exists("dados_real")) {

    dados_decisao <- dados_real %>%
      transmute(
        Block   = factor(Block),
        AppMode = factor(AppMode),
        Zn      = as.numeric(as.character(Zn)),
        Fe      = as.numeric(as.character(Fe)),
        Yield   = as.numeric(Yield),
        Zn_mgkg = as.numeric(Zn_mgkg),
        Fe_mgkg = as.numeric(Fe_mgkg)
      )

  } else {

    stop(
      "Neither 'dados_decisao' nor 'dados_real' was found in ML_framework_V1.0.RData."
    )
  }
}

treatment_uncertainty <- dados_decisao %>%
  group_by(
    Zn,
    Fe,
    AppMode
  ) %>%
  summarise(

    Experimental_n = dplyr::n(),

    Observed_Yield_Mean = mean(
      Yield,
      na.rm = TRUE
    ),

    Observed_Yield_SD = sd(
      Yield,
      na.rm = TRUE
    ),

    Observed_Zn_Mean = mean(
      Zn_mgkg,
      na.rm = TRUE
    ),

    Observed_Zn_SD = sd(
      Zn_mgkg,
      na.rm = TRUE
    ),

    Observed_Fe_Mean = mean(
      Fe_mgkg,
      na.rm = TRUE
    ),

    Observed_Fe_SD = sd(
      Fe_mgkg,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  mutate(

    t_critical =
      qt(
        0.975,
        df = Experimental_n - 1
      ),

    Observed_Yield_SE =
      Observed_Yield_SD /
      sqrt(Experimental_n),

    Observed_Zn_SE =
      Observed_Zn_SD /
      sqrt(Experimental_n),

    Observed_Fe_SE =
      Observed_Fe_SD /
      sqrt(Experimental_n),

    Observed_Yield_LCL95 =
      Observed_Yield_Mean -
      t_critical *
      Observed_Yield_SE,

    Observed_Yield_UCL95 =
      Observed_Yield_Mean +
      t_critical *
      Observed_Yield_SE,

    Observed_Zn_LCL95 =
      Observed_Zn_Mean -
      t_critical *
      Observed_Zn_SE,

    Observed_Zn_UCL95 =
      Observed_Zn_Mean +
      t_critical *
      Observed_Zn_SE,

    Observed_Fe_LCL95 =
      Observed_Fe_Mean -
      t_critical *
      Observed_Fe_SE,

    Observed_Fe_UCL95 =
      Observed_Fe_Mean +
      t_critical *
      Observed_Fe_SE
  )

stopifnot(
  nrow(treatment_uncertainty) == 48
)

# ------------------------------------------------------------
# 1A.3 SELF-CONTAINED RECOMMENDATION + UNCERTAINTY WRAPPER
# ------------------------------------------------------------

recommend_treatment_with_uncertainty <- function(
    objective = c(
      "max_yield",
      "max_zn",
      "max_fe",
      "max_profit",
      "balanced"
    ),
    scenario = c(
      "Baseline",
      "Favorable",
      "Unfavorable"
    )
) {

  objective <- match.arg(objective)
  scenario  <- match.arg(scenario)

  recommendation <-
    recommend_treatment_scenario(
      objective = objective,
      scenario = scenario
    )

  uncertainty_row <-
    treatment_uncertainty %>%
    filter(
      Zn == recommendation$Zn_kg_ha[1],
      Fe == recommendation$Fe_kg_ha[1],
      as.character(AppMode) ==
        recommendation$Application_Mode[1]
    ) %>%
    slice_head(
      n = 1
    )

  if (nrow(uncertainty_row) != 1) {

    stop(
      "Experimental uncertainty information was not found for the selected treatment."
    )
  }

  output <- bind_cols(
    recommendation,
    uncertainty_row %>%
      select(
        Experimental_n,
        Observed_Yield_Mean,
        Observed_Yield_LCL95,
        Observed_Yield_UCL95,
        Observed_Zn_Mean,
        Observed_Zn_LCL95,
        Observed_Zn_UCL95,
        Observed_Fe_Mean,
        Observed_Fe_LCL95,
        Observed_Fe_UCL95
      )
  )

  return(output)
}

# ------------------------------------------------------------
# 1B. RECONSTRUCT VISUALIZATION OBJECTS
# ------------------------------------------------------------

# Pareto dataset used by the Shiny visualization.
pareto_plot_data <- grid_economico %>%
  mutate(
    Pareto = Pareto_4obj
  )

# Economic robustness dataset.
economic_robustness <- sensibilidade_economica %>%
  mutate(
    Treatment = paste0(
      format(Zn, trim = TRUE, scientific = FALSE),
      " Zn + ",
      format(Fe, trim = TRUE, scientific = FALSE),
      " Fe + ",
      AppMode
    )
  )

# Strategic treatments used in the economic robustness plot.
economic_key_treatments <- data.frame(
  Zn = c(
    15,
    15,
    15,
    22.5,
    0
  ),
  Fe = c(
    15,
    5,
    25,
    5,
    15
  ),
  AppMode = c(
    "SF",
    "SF",
    "SF",
    "SF",
    "F"
  ),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Treatment = paste0(
      format(Zn, trim = TRUE, scientific = FALSE),
      " Zn + ",
      format(Fe, trim = TRUE, scientific = FALSE),
      " Fe + ",
      AppMode
    )
  )

# Internal consistency checks
stopifnot(
  nrow(pareto_plot_data) == 48,
  nrow(economic_robustness) == 144,
  nrow(economic_key_treatments) == 5
)

# ------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ------------------------------------------------------------

application_label <- function(x) {
  
  dplyr::case_when(
    x == "S"  ~ "Soil",
    x == "F"  ~ "Foliar",
    x == "SF" ~ "Soil + Foliar",
    TRUE      ~ x
  )
}

objective_labels <- c(
  "Maximum grain yield" = "max_yield",
  "Maximum grain Zn" = "max_zn",
  "Maximum grain Fe" = "max_fe",
  "Maximum net profit" = "max_profit",
  "Balanced bioeconomic compromise" = "balanced"
)

scenario_labels <- c(
  "Baseline" = "Baseline",
  "Favorable" = "Favorable",
  "Unfavorable" = "Unfavorable"
)

# ------------------------------------------------------------
# 3. MODEL RELIABILITY DATA
# ------------------------------------------------------------

plot_level_performance <- data.frame(
  Outcome = c(
    "Grain yield",
    "Grain Zn",
    "Grain Fe"
  ),
  Model = c(
    "ANN ensemble",
    "Random Forest",
    "Random Forest"
  ),
  RMSE = c(
    14.13,
    25.09,
    27.22
  ),
  MAE = c(
    11.64,
    19.44,
    20.88
  ),
  R2 = c(
    0.9900,
    0.108,
    -0.023
  ),
  Pearson_r = c(
    0.9950,
    0.407,
    0.268
  ),
  Spearman_rho = c(
    0.9928,
    0.422,
    0.284
  ),
  check.names = FALSE
)

treatment_level_performance <- data.frame(
  Outcome = c(
    "Grain yield",
    "Grain Zn",
    "Grain Fe"
  ),
  Model = c(
    "ANN ensemble",
    "Random Forest",
    "Random Forest"
  ),
  RMSE = c(
    4.60,
    3.55,
    3.18
  ),
  MAE = c(
    3.28,
    3.00,
    2.48
  ),
  R2 = c(
    0.99894,
    0.961,
    0.961
  ),
  Pearson_r = c(
    0.99948,
    0.983,
    0.984
  ),
  Spearman_rho = c(
    0.99946,
    0.980,
    0.982
  ),
  check.names = FALSE
)

# ------------------------------------------------------------
# 4. USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(
    "Cowpea Zn-Fe Decision Support Framework"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Decision settings"),
      
      selectInput(
        inputId = "objective",
        label = "Decision objective",
        choices = objective_labels,
        selected = "balanced"
      ),
      
      selectInput(
        inputId = "scenario",
        label = "Economic scenario",
        choices = scenario_labels,
        selected = "Baseline"
      ),
      
      actionButton(
        inputId = "generate",
        label = "Generate recommendation",
        class = "btn-primary",
        width = "100%"
      ),
      
      br(),
      br(),
      
      helpText(
        paste(
          "Recommendations are restricted to the 48 experimentally tested",
          "Zn × Fe × application-mode combinations."
        )
      )
    ),
    
    mainPanel(
      
      h3(
        "Multi-objective agronomic biofortification decision support"
      ),
      
      p(
        paste(
          "Select a decision objective and economic scenario, then generate",
          "a recommendation within the experimentally represented management domain."
        )
      ),
      
      tabsetPanel(
        
        id = "main_tabs",
        
        # ======================================================
        # TAB 1 — RECOMMENDATION
        # ======================================================
        
        tabPanel(
          title = "Recommendation",
          
          br(),
          
          uiOutput(
            "recommendation_output"
          )
        ),
        
        # ======================================================
        # TAB 2 — PARETO ANALYSIS
        # ======================================================
        
        tabPanel(
          title = "Pareto analysis",
          
          br(),
          
          h4("Four-objective Pareto decision map"),
          
          p(
            paste(
              "The plot is a two-dimensional projection of the",
              "four-objective optimization involving grain yield,",
              "grain Zn, grain Fe and net profit. Pareto efficiency",
              "is determined in the complete four-objective space."
            )
          ),
          
          wellPanel(
            fluidRow(
              
              column(
                width = 4,
                strong("Experimental alternatives"),
                tags$br(),
                "48 tested combinations"
              ),
              
              column(
                width = 4,
                strong("Pareto-efficient alternatives"),
                tags$br(),
                "18 non-dominated combinations"
              ),
              
              column(
                width = 4,
                strong("Current recommendation"),
                tags$br(),
                textOutput(
                  "pareto_selected_summary",
                  inline = TRUE
                )
              )
            )
          ),
          
          plotOutput(
            "pareto_plot",
            height = "620px"
          )
        ),
        
        # ======================================================
        # TAB 3 — ECONOMIC ROBUSTNESS
        # ======================================================
        
        tabPanel(
          title = "Economic robustness",
          
          br(),
          
          h4("Economic robustness across scenarios"),
          
          p(
            paste(
              "Net profit is compared across unfavorable, baseline",
              "and favorable economic scenarios for selected strategic treatments."
            )
          ),
          
          plotOutput(
            "economic_plot",
            height = "560px"
          )
        ),
        
        # ======================================================
        # TAB 4 — MODEL RELIABILITY
        # ======================================================
        
        tabPanel(
          title = "Model reliability",
          
          br(),
          
          h3("Predictive performance and decision reliability"),
          
          p(
            paste(
              "Model reliability is summarized at two complementary levels.",
              "Plot-level grouped out-of-fold performance evaluates prediction",
              "for individual experimental observations, whereas treatment-level",
              "recovery evaluates how well the models recover mean responses",
              "across the 48 experimentally represented management combinations."
            )
          ),
          
          hr(),
          
          h4("Plot-level grouped out-of-fold performance"),
          
          tableOutput(
            "plot_level_table"
          ),
          
          br(),
          
          wellPanel(
            
            strong("Interpretation"),
            
            p(
              paste(
                "The ANN showed excellent plot-level predictive performance for grain yield.",
                "Random Forest models for grain Zn and Fe showed substantially weaker",
                "plot-level predictive performance, indicating greater unexplained",
                "variation among individual experimental observations."
              )
            )
          ),
          
          hr(),
          
          h4("Treatment-level recovery"),
          
          tableOutput(
            "treatment_level_table"
          ),
          
          br(),
          
          wellPanel(
            
            strong("Interpretation"),
            
            p(
              paste(
                "After grouped out-of-fold predictions were aggregated at treatment level,",
                "recovery of expected treatment responses was strong for grain yield,",
                "grain Zn and grain Fe. These results support comparison and ranking",
                "of the experimentally represented management alternatives."
              )
            )
          ),
          
          hr(),
          
          h4("Reliability statement"),
          
          wellPanel(
            
            p(
              strong(
                "IMPORTANT — treatment-level recovery is not independent or external validation."
              )
            ),
            
            p(
              paste(
                "The framework was developed from one location, one growing season",
                "and one cowpea genotype. Model performance therefore supports internal",
                "decision making within the experimental domain but does not establish",
                "generalizability to other environments, soils, seasons or genotypes."
              )
            ),
            
            p(
              paste(
                "External multi-environment and multi-genotype validation is required",
                "before broader agronomic deployment."
              )
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 5. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 5.1 GENERATE RECOMMENDATION
  # ----------------------------------------------------------
  
  recommendation <- eventReactive(
    input$generate,
    {
      recommend_treatment_with_uncertainty(
        objective = input$objective,
        scenario = input$scenario
      )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # 5.2 RECOMMENDATION PANEL
  # ----------------------------------------------------------
  
  output$recommendation_output <- renderUI({
    
    req(recommendation())
    
    rec <- recommendation()
    
    mode_name <- application_label(
      rec$Application_Mode
    )
    
    tagList(
      
      h3("Recommended management"),
      
      p(
        strong("Decision objective: "),
        rec$Objective,
        tags$br(),
        strong("Economic scenario: "),
        rec$Scenario
      ),
      
      # ------------------------------------------------------
      # 1. MANAGEMENT
      # ------------------------------------------------------
      
      wellPanel(
        
        h4("1. Management"),
        
        fluidRow(
          
          column(
            width = 4,
            
            p(
              strong("Zn rate"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                paste0(
                  round(rec$Zn_kg_ha, 1),
                  " kg ha⁻¹"
                )
              )
            )
          ),
          
          column(
            width = 4,
            
            p(
              strong("Fe rate"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                paste0(
                  round(rec$Fe_kg_ha, 1),
                  " kg ha⁻¹"
                )
              )
            )
          ),
          
          column(
            width = 4,
            
            p(
              strong("Application mode"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                mode_name
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # 2. PREDICTED OUTCOMES
      # ------------------------------------------------------
      
      wellPanel(
        
        h4("2. Predicted outcomes"),
        
        fluidRow(
          
          column(
            width = 4,
            
            p(
              strong("Grain yield"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                paste0(
                  round(rec$Predicted_Yield_kg_ha, 1),
                  " kg ha⁻¹"
                )
              )
            )
          ),
          
          column(
            width = 4,
            
            p(
              strong("Grain Zn"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                paste0(
                  round(rec$Predicted_Grain_Zn_mg_kg, 2),
                  " mg kg⁻¹"
                )
              )
            )
          ),
          
          column(
            width = 4,
            
            p(
              strong("Grain Fe"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                paste0(
                  round(rec$Predicted_Grain_Fe_mg_kg, 2),
                  " mg kg⁻¹"
                )
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # 3. ECONOMIC OUTCOME
      # ------------------------------------------------------
      
      wellPanel(
        
        h4("3. Economic outcome"),
        
        fluidRow(
          
          column(
            width = 6,
            
            p(
              strong("Net profit"),
              tags$br(),
              
              tags$span(
                style = "font-size: 24px;",
                paste0(
                  "US$ ",
                  round(rec$Net_Profit_USD_ha, 2),
                  " ha⁻¹"
                )
              )
            )
          ),
          
          column(
            width = 6,
            
            p(
              strong("Economic scenario"),
              tags$br(),
              
              tags$span(
                style = "font-size: 22px;",
                rec$Scenario
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # 4. EXPERIMENTAL EVIDENCE
      # ------------------------------------------------------
      
      wellPanel(
        
        h4("4. Experimental evidence"),
        
        p(
          paste(
            "Observed treatment means and empirical 95% confidence intervals",
            "from the four experimental replicates are shown below."
          )
        ),
        
        fluidRow(
          
          column(
            width = 4,
            
            p(
              strong("Observed mean yield"),
              tags$br(),
              
              paste0(
                round(rec$Observed_Yield_Mean, 1),
                " kg ha⁻¹"
              ),
              
              tags$br(),
              
              tags$small(
                paste0(
                  "95% CI: ",
                  round(rec$Observed_Yield_LCL95, 1),
                  "–",
                  round(rec$Observed_Yield_UCL95, 1)
                )
              )
            )
          ),
          
          column(
            width = 4,
            
            p(
              strong("Observed mean grain Zn"),
              tags$br(),
              
              paste0(
                round(rec$Observed_Zn_Mean, 2),
                " mg kg⁻¹"
              ),
              
              tags$br(),
              
              tags$small(
                paste0(
                  "95% CI: ",
                  round(rec$Observed_Zn_LCL95, 2),
                  "–",
                  round(rec$Observed_Zn_UCL95, 2)
                )
              )
            )
          ),
          
          column(
            width = 4,
            
            p(
              strong("Observed mean grain Fe"),
              tags$br(),
              
              paste0(
                round(rec$Observed_Fe_Mean, 2),
                " mg kg⁻¹"
              ),
              
              tags$br(),
              
              tags$small(
                paste0(
                  "95% CI: ",
                  round(rec$Observed_Fe_LCL95, 2),
                  "–",
                  round(rec$Observed_Fe_UCL95, 2)
                )
              )
            )
          )
        ),
        
        p(
          strong("Experimental replicates: "),
          rec$Experimental_n
        )
      ),
      
      # ------------------------------------------------------
      # 5. DECISION RELIABILITY
      # ------------------------------------------------------
      
      wellPanel(
        
        h4("5. Decision reliability"),
        
        p(
          strong(
            if (isTRUE(rec$Within_Experimental_Domain)) {
              
              "SUPPORTED — experimentally represented treatment"
              
            } else {
              
              "OUTSIDE EXPERIMENTAL DOMAIN"
            }
          )
        ),
        
        p(
          rec$Reliability_Message
        ),
        
        hr(),
        
        p(
          strong("Important limitation"),
          tags$br(),
          
          paste(
            "This prototype is an internal decision-support tool based on one",
            "location, one growing season and one cowpea genotype.",
            "Recommendations are restricted to the 48 experimentally tested",
            "Zn × Fe × application-mode combinations and should not be interpreted",
            "as externally validated farmer recommendations."
          )
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # 5.3 PARETO DECISION MAP
  # ----------------------------------------------------------
  
  output$pareto_selected_summary <- renderText({
    
    req(recommendation())
    
    rec <- recommendation()
    
    paste0(
      format(
        rec$Zn_kg_ha,
        trim = TRUE,
        scientific = FALSE
      ),
      " Zn + ",
      format(
        rec$Fe_kg_ha,
        trim = TRUE,
        scientific = FALSE
      ),
      " Fe + ",
      rec$Application_Mode
    )
  })
  
  output$pareto_plot <- renderPlot({
    
    req(recommendation())
    
    rec <- recommendation()
    
    plot_data <- pareto_plot_data %>%
      mutate(
        
        Pareto_Status = factor(
          
          ifelse(
            Pareto_4obj,
            "Pareto-efficient",
            "Dominated"
          ),
          
          levels = c(
            "Dominated",
            "Pareto-efficient"
          )
        )
      )
    
    current_rec <- plot_data %>%
      filter(
        Zn == rec$Zn_kg_ha,
        Fe == rec$Fe_kg_ha,
        AppMode == rec$Application_Mode
      ) %>%
      mutate(
        Selected_Label = "Selected"
      )
    
    ggplot(
      plot_data,
      aes(
        x = Pred_Yield,
        y = Net_Profit
      )
    ) +
      
      # Dominated alternatives
      geom_point(
        data = filter(
          plot_data,
          Pareto_Status == "Dominated"
        ),
        aes(
          size = Pred_Zn_mgkg,
          fill = Pred_Fe_mgkg
        ),
        shape = 21,
        alpha = 0.35,
        color = "grey45",
        stroke = 0.4
      ) +
      
      # Pareto-efficient alternatives
      geom_point(
        data = filter(
          plot_data,
          Pareto_Status == "Pareto-efficient"
        ),
        aes(
          size = Pred_Zn_mgkg,
          fill = Pred_Fe_mgkg
        ),
        shape = 24,
        alpha = 0.90,
        color = "black",
        stroke = 0.7
      ) +
      
      # Selected recommendation
      geom_point(
        data = current_rec,
        aes(
          x = Pred_Yield,
          y = Net_Profit
        ),
        inherit.aes = FALSE,
        shape = 23,
        size = 6.5,
        stroke = 1.6,
        fill = "white",
        color = "black"
      ) +
      
      geom_label(
        data = current_rec,
        aes(
          x = Pred_Yield,
          y = Net_Profit,
          label = Selected_Label
        ),
        inherit.aes = FALSE,
        nudge_y = 22,
        size = 3.8,
        label.size = 0.25,
        fill = "white"
      ) +
      
      scale_size_continuous(
        name = "Predicted grain Zn\n(mg kg⁻¹)",
        range = c(
          2.5,
          7
        )
      ) +
      
      scale_fill_gradient(
        name = "Predicted grain Fe\n(mg kg⁻¹)",
        low = "grey85",
        high = "grey20"
      ) +
      
      labs(
        x = "Predicted grain yield (kg ha⁻¹)",
        y = "Net profit (US$ ha⁻¹)",
        
        subtitle = paste(
          "Circles = dominated alternatives;",
          "triangles = Pareto-efficient alternatives"
        ),
        
        caption = paste(
          "Pareto efficiency is determined simultaneously from predicted grain yield,",
          "predicted grain Zn, predicted grain Fe and net profit.",
          "The figure is a two-dimensional projection of that four-objective decision space."
        )
      ) +
      
      guides(
        size = guide_legend(
          order = 1
        ),
        fill = guide_colorbar(
          order = 2
        )
      ) +
      
      theme_minimal(
        base_size = 12
      ) +
      
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank(),
        
        plot.subtitle = element_text(
          size = 11
        ),
        
        plot.caption = element_text(
          hjust = 0,
          size = 9
        )
      )
  })
  
  # ----------------------------------------------------------
  # 5.4 ECONOMIC ROBUSTNESS
  # ----------------------------------------------------------
  
  output$economic_plot <- renderPlot({
    
    req(recommendation())
    
    plot_econ <- economic_robustness %>%
      filter(
        Treatment %in%
          economic_key_treatments$Treatment
      ) %>%
      mutate(
        
        Scenario = factor(
          Scenario,
          levels = c(
            "Unfavorable",
            "Baseline",
            "Favorable"
          )
        )
      )
    
    ggplot(
      plot_econ,
      aes(
        x = Scenario,
        y = Net_Profit_Scenario,
        group = Treatment
      )
    ) +
      
      geom_line(
        aes(
          linetype = Treatment
        ),
        linewidth = 1
      ) +
      
      geom_point(
        aes(
          shape = Treatment
        ),
        size = 2.5
      ) +
      
      labs(
        x = "Economic scenario",
        y = "Net profit (US$ ha⁻¹)",
        linetype = "Treatment",
        shape = "Treatment",
        
        caption = paste(
          "Economic robustness across unfavorable, baseline and favorable",
          "scenarios for selected strategic treatments."
        )
      ) +
      
      theme_minimal(
        base_size = 12
      )
  })
  
  # ----------------------------------------------------------
  # 5.5 MODEL RELIABILITY TABLES
  # ----------------------------------------------------------
  
  output$plot_level_table <- renderTable({
    
    plot_level_performance %>%
      transmute(
        Outcome,
        Model,
        RMSE = sprintf(
          "%.2f",
          RMSE
        ),
        MAE = sprintf(
          "%.2f",
          MAE
        ),
        `R²` = sprintf(
          "%.3f",
          R2
        ),
        `Pearson r` = sprintf(
          "%.3f",
          Pearson_r
        ),
        `Spearman ρ` = sprintf(
          "%.3f",
          Spearman_rho
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "m")
  
  output$treatment_level_table <- renderTable({
    
    treatment_level_performance %>%
      transmute(
        Outcome,
        Model,
        RMSE = sprintf(
          "%.2f",
          RMSE
        ),
        MAE = sprintf(
          "%.2f",
          MAE
        ),
        `R²` = sprintf(
          "%.3f",
          R2
        ),
        `Pearson r` = sprintf(
          "%.3f",
          Pearson_r
        ),
        `Spearman ρ` = sprintf(
          "%.3f",
          Spearman_rho
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "m")
}

# ------------------------------------------------------------
# 6. RUN APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)