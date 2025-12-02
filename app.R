# Six Sigma Running Shoe Mileage Analysis Dashboard (app.R)
# CALCULATIONS BASED ON PROVIDED EXPERIMENTAL AND RUNREPEAT DATA

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tools)
library(tibble) # Required for some dplyr functions
library(MASS)   # Required for fitdistr (Weibull calculation)

# --- 1. Hardcoded Input Data and Analysis Pipeline (Global Scope) ---

# Experimental Data (Retired Shoes)
df_shoes_raw <- data.frame(
  Specimen = c("S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", "S09", "S10", "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19", "S20", "S21", "S22", "S23", "S24", "S25", "S26", "S27"),
  Shoe_Brand = c("New Balance", "Hoka", "Hoka", "Nike", "Nike", "Nike", "Nike", "Nike", "Asics", "Nike", "Nike", "Nike", "Nike", "Nike", "Nike", "Nike", "Nike", "Brooks", "Brooks", "New Balance", "New Balance", "New Balance", "Hoka", "Brooks", "Brooks", "Hoka", "Brooks"),
  Avg_Retired_SA = c(10.9, 17.9, 17.8, 10.2, 14.0, 9.7, 14.5, 17.3, 10.8, 10.1, 12.5, 13.3, 16.5, 18.3, 11.2, 14.6, 16.1, 13.1, 13.0, 12.0, 11.9, 12.9, 18.3, 15.2, 17.7, 15.2, 20.2),
  stringsAsFactors = FALSE
)

# RunRepeat Data (New Shoes & Cost)
df_ref_raw <- data.frame(
  Specimen = c("S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", "S09", "S10", "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19", "S20", "S21", "S22", "S23", "S24", "S25", "S26", "S27"),
  New_SA = c(18.9, 20.4, 20.4, 16.4, 15.5, 16.4, 16.8, 15.0, 13.0, 14.5, 17.6, 17.6, 17.6, 17.3, 14.5, 19.1, 19.0, 21.6, 21.6, 15.6, 15.6, 15.6, 20.4, 21.6, 15.9, 20.4, 19.6),
  Cost = c(150, 145, 145, 190, 130, 190, 250, 160, 140, 180, 130, 130, 130, 120, 180, 275, 225, 140, 140, 140, 140, 140, 135, 140, 140, 145, 120),
  stringsAsFactors = FALSE
)

# Constants for Conversion (from Appendix A logic)
E_eva <- 18 # MPa (Reference for Young's Modulus)
stride <- 1.25 # m (Average stride length for cycles to miles)
y_int <- 0.304 # Constant from Empirical Curve
customer_target <- 400 # Customer Requirement
all_brands <- c("Brooks", "Hoka", "New Balance", "Nike")

# Combine and Process Data
mileage_data_calculated <- inner_join(df_shoes_raw, df_ref_raw, by = "Specimen", suffix = c(".retired", ".new")) %>%
  # Normalize Brand Names
  mutate(Shoe_Brand = tools::toTitleCase(as.character(Shoe_Brand))) %>%
  # Filter Step: Only keep shoes where stiffness decreased (Avg_Retired_SA < New_SA)
  filter(Avg_Retired_SA < New_SA) %>%
  mutate(
    # 1. Shore A to Young's Modulus (Enew / Eold)
    Enew = 10^(0.0235 * New_SA - 0.6403),
    Eold = 10^(0.0235 * Avg_Retired_SA - 0.6403),
    R_E = Eold / Enew, # Ratio of Change of E
    
    # 2. Approximate Density (rho)
    rho_shoes = sqrt(Enew / E_eva),
    
    # 3. Calculate Slope (Linear interpolation of degradation rate)
    slope = 0.000000075 * (rho_shoes - 0.17) / 0.06 - 0.000000245,
    
    # 4. Cycles Calculation (Empirical Curve: log_N = y_int * (R_E - 1) / slope)
    Cycles = y_int * (R_E - 1) / slope,
    
    # 5. Cycles to Miles Conversion (* 2 * stride / 1609)
    Mileage = round(Cycles * 2 * stride / 1609, 0)
  ) %>%
  # Filter down to the 4 brands used in the ANOVA/CI sections of the report
  filter(Shoe_Brand %in% c("Brooks", "Hoka", "New Balance", "Nike"))

# Final dataset for plotting and metrics
mileage_data <- mileage_data_calculated

# --- 2. Calculate Key Metrics based on ACTUAL Calculated Mileage (Global Scope) ---

# Overall Metrics (N=23)
qoi_mean <- round(mean(mileage_data$Mileage, na.rm = TRUE)) # Should be ~424
n_obs <- nrow(mileage_data)
sd_obs <- sd(mileage_data$Mileage, na.rm = TRUE)
se_obs <- sd_obs / sqrt(n_obs)
z_val_95 <- qnorm(0.975)
error_margin <- z_val_95 * se_obs
ci_lower <- round(qoi_mean - error_margin)
ci_upper <- round(qoi_mean + error_margin)

# Calculate brand means for UI display
brand_means_data <- mileage_data %>%
  group_by(Shoe_Brand) %>%
  summarise(
    Mean = round(mean(Mileage, na.rm = TRUE), 0),
    sd_mileage = sd(Mileage, na.rm = TRUE),
    n = n(),
    se_mileage = sd_mileage / sqrt(n),
    error_margin = z_val_95 * se_mileage,
    ci_lower = Mean - error_margin,
    ci_upper = Mean + error_margin
  ) %>%
  ungroup()

# Summary Data (Means, SEM)
summary_df <- mileage_data %>%
  group_by(Shoe_Brand) %>%
  summarise(
    mean_mileage = mean(Mileage, na.rm = TRUE),
    sd_mileage = sd(Mileage, na.rm = TRUE),
    n = n(),
    se_mileage = sd_mileage / sqrt(n)
  ) %>%
  ungroup()

# Tukey HSD Significance Calculation
aov_model <- aov(Mileage ~ Shoe_Brand, data = mileage_data)
tukey_hsd_df <- TukeyHSD(aov_model)$Shoe_Brand %>%
  as.data.frame() %>%
  rownames_to_column(var = "comparison") %>%
  dplyr::select(comparison, `p adj`) %>%
  mutate(
    p_label = case_when(
      `p adj` <= 0.001 ~ "***",
      `p adj` <= 0.01 ~ "**",
      `p adj` <= 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  filter(grepl("Brooks", comparison))


# --- 3. Data Sample for Display (Illustrating Input Variables) ---
data_sample <- mileage_data %>%
  dplyr::select(Shoe_Brand, Specimen, Avg_Retired_SA, New_SA, Cost, Mileage) %>%
  head(5) %>%
  rename(`Retired SA` = Avg_Retired_SA, `New SA` = New_SA, `Predicted Mileage` = Mileage) %>%
  as.data.frame() # Convert to data frame for robust rendering

# --- 4. Shiny UI (Dashboard Layout) ---
ui <- fluidPage(
  
  # Set up a clean, modern aesthetic using robust CSS loading
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      /* Added container styling for max-width on large screens */
      .main-container {
        max-width: 1600px; /* Optimal width for 1920 screen */
        margin: 0 auto; /* Center the content */
        padding: 0 15px; /* Padding for the sides */
      }
      body { font-family: 'Inter', sans-serif; background-color: #f0f4f7; }
      .panel-title { font-weight: 700; color: #0d47a1; margin-bottom: 10px; border-bottom: 3px solid #0d47a1; padding-bottom: 5px; font-size: 1.25rem; }
      .metric-box { background-color: #ffffff; padding: 20px; margin: 10px 0; border-radius: 10px; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15); text-align: center; border: 1px solid #ddd; }
      .metric-value { font-size: 3em; font-weight: 700; color: #00897b; line-height: 1.2; }
      .ci-text { font-size: 1.2em; font-weight: 600; color: #455a64; }
      .customer-req { color: #e53935; font-weight: 700; }
      .poster-section { background-color: #ffffff; padding: 20px; margin-bottom: 20px; border-radius: 12px; box-shadow: 0 6px 15px rgba(0, 0, 0, 0.08); }
      .brand-list { text-align: left; padding: 0 10px; margin-top: 10px; font-size: 0.9em; }
      .brand-item { display: flex; justify-content: space-between; padding: 3px 0; border-bottom: 1px dashed #eee; }
      .ci-status-good { color: #2e7d32; font-weight: 700; } /* Green */
      .ci-status-bad { color: #c62828; font-weight: 700; }  /* Red */
      .ci-status-warning { color: #f9a825; font-weight: 700; } /* Orange/Yellow */
    "))
  ),
  
  tags$div(class = "main-container", # Wraps content for max-width and centering
     # --- Header ---
     titlePanel(div(
       h1("Miles to Metrics: Determining Shoe Retirement Mileage", style = "color: #0d47a1; font-weight: 700;"),
       p("Six Sigma Project", style = "color: #607d8b;")
     )),
     
     # --- Poster Grid Layout (4 columns for the top narrative) ---
     fluidRow(
       # (1) Research Question
       column(3, class = "poster-section",
              tags$div(class = "panel-title", "1. Define & Measure"),
              tags$ul(style = "list-style-type: disc;",
                      tags$li("Research Question: What is the average useful life of a running shoe?"),
                      tags$li("Measures: Shoe brand, mileage at retirement, and cost per mile "),
                      tags$li(paste0("Sample Size: N=", n_obs, " retired shoes measured for Shore Hardness A.")),
                      tags$li("Sampling: Varisty teams, club teams, friends, and family."),
                      tags$li("Customer Target: 400 miles.")
              )
       ),
       # (3) Method
       column(3, class = "poster-section",
              tags$div(class = "panel-title", "2. Analyze: Method"),
              tags$ul(style = "list-style-type: disc;",
                      tags$li("Mileage Estimation: Shore Hardness A => Young's Modulus => Mileage."),
                      tags$li("Distribution: Weibull fit (p=0.108) used to estimate expected life."),
                      tags$li("Statistical Test: One-way ANOVA and Tukey HSD for brand comparison."),
                      tags$li("Confidence Intervals: Retirement mileage intervals estimated using the Weibull (overall) or Normal distributions (brand specific).")
              )
       ),
       # (4) Quantity of Interest & CI Display
       column(3,
              tags$div(class = "metric-box",
                       p("Overall Mean Useful Life", style = "font-size: 1.2em; color: #555;"),
                       tags$span(class = "metric-value", paste0(qoi_mean, " mi")),
                       p("Customer Requirement: ", tags$span(class = "customer-req", paste0(customer_target, " mi"))),
                       tags$hr(),
                       p("95% CI (Overall)", style = "font-size: 1.2em; color: #555;"),
                       tags$p(class = "ci-text",
                              paste0("[", ci_lower, " mi, ", ci_upper, " mi]")
                       ),
                       tags$hr(),
                       p("Brand-Specific Mean Mileages", style = "font-size: 1.2em; color: #555;"),
                       # Output the detailed brand means table
                       uiOutput("brand_means_table")
              )
       ),
       # (5) Discussion/Implications
       column(3, class = "poster-section",
              tags$div(class = "panel-title", "3. Improve / Control: Implications"),
              tags$ul(style = "list-style-type: disc;",
                      tags$li(paste0("Brooks (", brand_means_data[brand_means_data$Shoe_Brand == "Brooks", ]$Mean, " mi) is a positive outlier, skewing the overall mean.")),
                      tags$li("Hoka, Nike, and New Balance fail the 400-mile customer requirement (CI passes below 400 mi)."),
                      tags$li("Recommendation: These brands must increase average life by 124-197 miles."),
                      tags$li("Financial: High cost/mile for underperforming brands leads to customer dissatisfaction.")
              )
       )
     ),
     
     # --- Results Section (Plots) ---
     fluidRow(
       tags$div(class = "panel-title", style = "margin-left: 15px; font-size: 1.5em; border-bottom: none; color: #00897b;", "4. Results"),
       
       # Plot 1 (Distribution Analysis) - Weibull Fit
       column(6, class = "poster-section",
              tags$div(class = "panel-title", style="border-bottom: 2px solid #ddd;", "Overall Mileage Distribution (Weibull Fit)"),
              plotOutput("plot_distribution", height = 550)
       ),
       
       # Plot 2 (Factor Impact) - Mean Bar Plot with SEM and Significance
       column(6, class = "poster-section",
              tags$div(class = "panel-title", style="border-bottom: 2px solid #ddd;", "Brand-Specific Mileage Comparison"),
              plotOutput("plot_brand_impact", height = 550)
       )
     ),
     
     # --- Interactive ---
     # --- Top Row: Simulator, Analysis, Metrics, Implications ---
     fluidRow(
       # (1) Simulator Controls (Width 3)
       column(3, class = "poster-section",
              tags$div(class = "panel-title", "5. Improvement Simulator"),
              tags$p("Select a brand and simulate improvement in midsole durability (mean mileage)."),
              
              selectInput("sim_brand", "Target Brand:", choices = all_brands, selected = "Hoka"),
              
              sliderInput("sim_mileage_increase", 
                          "Simulated Mean Mileage Increase (mi):",
                          min = 0, max = 500, value = 0, step = 10),
              
              tags$hr(),
              tags$p(paste0("Current Customer Target: ", customer_target, " miles"))
       ),
       
       # (2) Analysis & Simulation Results (Width 3)
       column(3, class = "poster-section",
              tags$div(class = "panel-title", "6. Simulated Results"),
              tags$ul(style = "list-style-type: disc;",
                      tags$li(tags$b("Base Mileage (Mean):"), textOutput("base_mean")),
                      tags$li(tags$b("Simulated Mean Mileage:"), textOutput("sim_mean")),
                      tags$li(tags$b("Initial Cost/Mile:"), textOutput("base_cost_per_mile")),
                      tags$li(tags$b("Simulated Cost/Mile:"), textOutput("sim_cost_per_mile"))
              )
       ),
       
       # (3) CI Status & QoI (Increased Width to 4)
       column(4,
              tags$div(class = "metric-box",
                       p("Simulated Mean Useful Life (QoI)", style = "font-size: 1.2em; color: #555;"),
                       tags$span(class = "metric-value", textOutput("qoi_output_sim")),
                       p("Target: ", tags$span(class = "customer-req", paste0(customer_target, " mi"))),
                       tags$hr(),
                       p("95% CI Status (Six Sigma Control)", style = "font-size: 1.2em; color: #555;"),
                       # Conditional CI Output
                       uiOutput("ci_status_output")
              )
       ),
       
       # (4) Discussion/Implications (Decreased Width to 2)
       column(2, class = "poster-section",
              tags$div(class = "panel-title", "6. Improvement Goal"),
              tags$ul(style = "list-style-type: disc;",
                      tags$li("Goal is achieved when the entire CI is above 400 mi."),
                      tags$li("Control limits must be set for midsole degradation.")
              )
       ),
     ),
           
     # --- Data Sample Section ---
     fluidRow(
       column(12, class = "poster-section",
              tags$div(class = "panel-title", style="border-bottom: 2px solid #ddd;", "Data Sample (Inputs & Calculated Mileage)"),
              tableOutput("data_sample_table")
       )
     )
  )
)

# --- 5. Shiny Server (Logic and Reactivity) ---
server <- function(input, output) {
  # Reactive Filtered Dataset (Only selected brand)
  sim_data_base <- reactive({
    mileage_data_calculated %>%
      filter(Shoe_Brand == input$sim_brand)
  })
  
  # Base metrics for the selected brand
  base_metrics <- reactive({
    data <- sim_data_base()
    n <- nrow(data)
    if (n < 2) return(NULL)
    
    mean_m <- mean(data$Mileage, na.rm = TRUE)
    mean_cost <- mean(data$Cost, na.rm = TRUE)
    sd_m <- sd(data$Mileage, na.rm = TRUE)
    
    list(mean=mean_m, sd=sd_m, cost=mean_cost)
  })
  
  # Reactive Simulated Metrics
  sim_metrics <- reactive({
    base <- base_metrics()
    if (is.null(base)) return(NULL)
    
    # 1. New Mean (Simulated QoI)
    sim_qoi_mean <- base$mean + input$sim_mileage_increase
    
    # 2. CI Calculation (Assuming SD remains constant, which is conservative for Six Sigma)
    n <- nrow(sim_data_base())
    sd_obs <- base$sd 
    se_obs <- sd_obs / sqrt(n)
    z_val_95 <- qnorm(0.975)
    error_margin <- z_val_95 * se_obs
    
    sim_ci_lower <- round(sim_qoi_mean - error_margin)
    sim_ci_upper <- round(sim_qoi_mean + error_margin)
    
    # 3. Cost Per Mile
    sim_cost_per_mile <- base$cost / sim_qoi_mean
    
    list(qoi=round(sim_qoi_mean), ci_l=sim_ci_lower, ci_u=sim_ci_upper, cpm=sim_cost_per_mile)
  })
  
  # --- Output: Analysis & Simulation Results ---
  output$base_mean <- renderText({
    metrics <- base_metrics()
    if (is.null(metrics)) return("N/A")
    paste0(round(metrics$mean), " mi")
  })
  
  output$sim_mean <- renderText({
    metrics <- sim_metrics()
    if (is.null(metrics)) return("N/A")
    paste0(metrics$qoi, " mi")
  })
  
  output$base_cost_per_mile <- renderText({
    metrics <- base_metrics()
    if (is.null(metrics)) return("N/A")
    paste0("$", format(metrics$cost / metrics$mean, digits = 3), "/mi")
  })
  
  output$sim_cost_per_mile <- renderText({
    metrics <- sim_metrics()
    if (is.null(metrics)) return("N/A")
    paste0("$", format(metrics$cpm, digits = 3), "/mi")
  })
  
  output$qoi_output_sim <- renderText({
    metrics <- sim_metrics()
    if (is.null(metrics)) return("N/A")
    paste0(metrics$qoi, " mi")
  })
  
  # --- Output: Conditional CI Status ---
  output$ci_status_output <- renderUI({
    metrics <- sim_metrics()
    if (is.null(metrics)) {
      return(tags$p(class = "ci-text", "Insufficient Data"))
    }
    
    ci_text <- paste0("[", metrics$ci_l, " mi, ", metrics$ci_u, " mi]")
    
    # Logic for Conditional Coloring
    if (metrics$ci_l >= customer_target) {
      # GOOD: Entire CI is above 400 mi
      status_class <- "ci-status-good"
    } else if (metrics$ci_u < customer_target) {
      # BAD: Entire CI is below 400 mi
      status_class <- "ci-status-bad"
    } else {
      # WARNING: CI spans the 400 mi line
      status_class <- "ci-status-warning"
    }
    
    tagList(
      tags$p(class = "ci-text", style="color:#455a64;", "95% Confidence Interval:"),
      tags$p(class = status_class, ci_text)
    )
  })
  
  # --- Output: Brand Means Table (Metrics Box) ---
  output$brand_means_table <- renderUI({
    tagList(
      lapply(1:nrow(brand_means_data), function(i) {
        brand <- brand_means_data$Shoe_Brand[i]
        mean_val <- round(brand_means_data$Mean[i])
        ci_lower_val <- round(brand_means_data$ci_lower[i])
        ci_upper_val <- round(brand_means_data$ci_upper[i])
        
        ci_text <- paste0("[", ci_lower_val, " - ", ci_upper_val, " mi]")
        
        tags$p(class = "brand-item",
               tags$span(style = paste0("color:", RColorBrewer::brewer.pal(4, "Set2")[i], "; width: 35%; display: inline-block;"), tags$b(brand)),
               # Right side: Mean and CI (in two lines)
               tags$span(style = "width: 60%; display: inline-block; text-align: right; line-height: 1.1;", 
                         tags$b(paste0(mean_val, " mi")), tags$br(), tags$small(ci_text))
        )
      })
    )
  })
  
  # --- Output: Data Sample Table (Bottom) ---
  output$data_sample_table <- renderTable({
    data_sample
  }, align = 'c', striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- Output: Plot 1 (Distribution) ---
  output$plot_distribution <- renderPlot({
    # Calculate Weibull parameters (used to draw the theoretical curve)
    fit_weib <- MASS::fitdistr(mileage_data$Mileage, "weibull")
    shape <- fit_weib$estimate["shape"]
    scale <- fit_weib$estimate["scale"]
    
    # Calculate theoretical Weibull density
    weibull_df <- data.frame(
      x = seq(min(mileage_data$Mileage) * 0.9, max(mileage_data$Mileage) * 1.1, length.out = 500)
    ) %>%
      mutate(y = dweibull(x, shape = shape, scale = scale))
    
    ggplot(mileage_data, aes(x = Mileage)) +
      # Histogram of actual data
      geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#a1d99b", color = "#00897b", alpha = 0.7) +
      # Theoretical Weibull curve
      geom_line(data = weibull_df, aes(x = x, y = y), color = "#0d47a1", linewidth = 1.5) +
      # Mean line
      geom_vline(xintercept = qoi_mean, linetype = "dashed", color = "#0d47a1", linewidth = 1) +
      annotate("text", x = qoi_mean + 100, y = 0.002, label = paste("Overall Mean:", qoi_mean, "mi"), color = "#0d47a1", size = 5) +
      # Customer target line
      geom_vline(xintercept = customer_target, linetype = "dotted", color = "#e53935", linewidth = 1) +
      annotate("text", x = customer_target - 100, y = 0.001, label = "Customer Target (400 mi)", color = "#e53935", size = 5) +
      # Styling and Labels
      labs(
        title = "Weibull Distribution Fit of Accumulated Mileage",
        x = "Mileage at Retirement (Miles)",
        y = "Density"
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      )
  })
  
  # --- Output: Plot 2 (Brand Impact Bar Plot) ---
  output$plot_brand_impact <- renderPlot({
    summary <- summary_df # Use non-reactive variable
    tukey_data <- tukey_hsd_df # Use non-reactive variable
    y_max <- max(mileage_data$Mileage) # For setting annotation height
    
    # 1. Base Plot (Bar, Errorbar, Jitter)
    p <- ggplot(summary, aes(x = Shoe_Brand, y = mean_mileage, fill = Shoe_Brand)) +
      # Bar Plot (Mean)
      geom_col(width = 0.5, alpha = 0.6) +
      # Error Bars (SEM)
      geom_errorbar(aes(ymin = mean_mileage - se_mileage, ymax = mean_mileage + se_mileage, color = Shoe_Brand),
                    width = 0.2, linewidth = 1.2, alpha = 1) +
      # Jittered data points for underlying distribution visibility
      geom_jitter(data = mileage_data, aes(x = Shoe_Brand, y = Mileage, color = Shoe_Brand), width = 0.05, alpha = 0.8, size = 3) +
      # Customer requirement line
      geom_hline(yintercept = customer_target, linetype = "dashed", color = "#e53935", linewidth = 1) +
      annotate("text", x = 1.2, y = customer_target + 50, label = "Customer 400 mi Requirement", color = "#e53935", size = 5)
    
    # 2. Add Significance Annotations (Tukey HSD)
    # The plot shows Brooks is significantly different from Hoka, New Balance, and Nike.
    sig_comparisons <- list(
      list(from = "Brooks", to = "Hoka", y_pos = 900, p_val = tukey_data$`p adj`[tukey_data$comparison == "Hoka-Brooks"]),
      list(from = "Brooks", to = "Nike", y_pos = 1000, p_val = tukey_data$`p adj`[tukey_data$comparison == "Nike-Brooks"]),
      list(from = "Brooks", to = "New Balance", y_pos = 950, p_val = tukey_data$`p adj`[tukey_data$comparison == "New Balance-Brooks"])
    )
    
    for (comp in sig_comparisons) {
      if (comp$p_val < 0.05) {
        p_label <- if (comp$p_val < 0.001) "***" else if (comp$p_val < 0.01) "**" else "*"
        p <- p +
          geom_segment(x = comp$from, xend = comp$from, y = comp$y_pos - 30, yend = comp$y_pos, color = "black") + # Vertical left
          geom_segment(x = comp$from, xend = comp$to, y = comp$y_pos, yend = comp$y_pos, color = "black") + # Horizontal bar
          geom_segment(x = comp$to, xend = comp$to, y = comp$y_pos - 30, yend = comp$y_pos, color = "black") + # Vertical right
          annotate("text", x = mean(c(as.numeric(factor(comp$from, levels = unique(summary$Shoe_Brand))), as.numeric(factor(comp$to, levels = unique(summary$Shoe_Brand))))),
                   y = comp$y_pos + 10, label = p_label, size = 6, fontface = "bold")
      }
    }
    
    
    # 3. Final Styling
    p +
      labs(
        title = "Impact of Brand on Shoe Useful Life",
        x = "",
        y = "Mileage at Retirement (Miles)"
      ) +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette = "Set2") +
      coord_cartesian(ylim = c(0, 1050)) + # Adjust Y-axis limit for readability
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 16),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
      )
  } )
}

# Run the application
shinyApp(ui = ui, server = server)