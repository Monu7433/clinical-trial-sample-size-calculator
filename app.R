# ─────────────────────────────────────────────────────────────────────────────
# Clinical Trial Sample Size Calculator  — Full 8-Design Version
# Designs:
#   1. Single Mean Estimation
#   2. Single Proportion Estimation
#   3. Two Mean Comparison
#   4. Two Proportion Comparison
#   5. Paired Mean Comparison
#   6. Survival Analysis
#   7. Non-Inferiority Trial
#   8. Equivalence Trial
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(shinydashboard)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1 ── Statistical Functions
# ══════════════════════════════════════════════════════════════════════════════

# 1. Single Mean Estimation (confidence interval approach)
ss_single_mean <- function(sd, margin, conf) {
  z <- qnorm(1 - (1 - conf) / 2)
  ceiling((z * sd / margin)^2)
}

# 2. Single Proportion Estimation
ss_single_prop <- function(p, margin, conf) {
  z <- qnorm(1 - (1 - conf) / 2)
  ceiling((z^2 * p * (1 - p)) / margin^2)
}

# 3. Two Mean Comparison (independent samples)
ss_two_mean <- function(sd, delta, alpha, power) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta  <- qnorm(power)
  ceiling((2 * sd^2 * (z_alpha + z_beta)^2) / delta^2)
}

# 4. Two Proportion Comparison
ss_two_prop <- function(p1, p2, alpha, power) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta  <- qnorm(power)
  num <- (z_alpha + z_beta)^2 * (p1*(1-p1) + p2*(1-p2))
  den <- (p1 - p2)^2
  ceiling(num / den)
}

# 5. Paired Mean Comparison
ss_paired <- function(sd_diff, delta, alpha, power) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta  <- qnorm(power)
  ceiling((sd_diff^2 * (z_alpha + z_beta)^2) / delta^2)
}

# 6. Survival Analysis — Schoenfeld (1981)
ss_survival <- function(hr, event_rate, alpha, power) {
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta  <- qnorm(power)
  d       <- ceiling((z_alpha + z_beta)^2 / (log(hr))^2 * 4)
  n_total <- ceiling(d / event_rate)
  n_per   <- ceiling(n_total / 2)
  list(n_per = n_per, n_total = n_total, events = d)
}

# 7. Non-Inferiority Trial (continuous outcome, one-sided alpha)
ss_noninferiority <- function(sd, delta, margin, alpha, power) {
  z_alpha <- qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  effect  <- margin - delta
  ceiling((2 * sd^2 * (z_alpha + z_beta)^2) / effect^2)
}

# 8. Equivalence Trial (TOST)
ss_equivalence <- function(sd, margin, alpha, power) {
  z_alpha <- qnorm(1 - alpha)
  z_beta  <- qnorm(power / 2 + 0.5)
  ceiling((2 * sd^2 * (z_alpha + z_beta)^2) / margin^2)
}

# Dropout adjustment (universal)
dropout_adjust <- function(n, dropout_pct) {
  ceiling(n / (1 - dropout_pct / 100))
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2 ── AI Suggestion Logic
# ══════════════════════════════════════════════════════════════════════════════

ai_suggest <- function(description) {
  desc <- tolower(description)
  
  if (grepl("non.inferior|not worse|non inferior|noninferior", desc)) {
    list(method = "7. Non-Inferiority Trial",
         reason = "Your description mentions non-inferiority testing.",
         inputs = "SD, expected effect, non-inferiority margin, alpha (one-sided), power.")
    
  } else if (grepl("equivalen|bioequivalen|same as|tost", desc)) {
    list(method = "8. Equivalence Trial",
         reason = "Your description mentions showing two treatments are equivalent.",
         inputs = "Equivalence margin, SD, alpha, power.")
    
  } else if (grepl("surviv|time.to.event|hazard|kaplan|oncolog|mortalit|death|relapse", desc)) {
    list(method = "6. Survival Analysis",
         reason = "Your description mentions time-to-event or survival outcomes.",
         inputs = "Hazard ratio, expected event rate, alpha, power.")
    
  } else if (grepl("before.after|paired|within.subject|repeated|crossover|pre.post", desc)) {
    list(method = "5. Paired Mean Comparison",
         reason = "Your description mentions a before-after or within-subject design.",
         inputs = "SD of paired differences, expected mean difference, alpha, power.")
    
  } else if (grepl("prevalence|single proportion|vaccination|infection rate|coverage", desc)) {
    list(method = "2. Single Proportion Estimation",
         reason = "Your description mentions estimating a single prevalence or proportion.",
         inputs = "Expected proportion, margin of error, confidence level.")
    
  } else if (grepl("estimate.*mean|mean.*estimate|average blood|mean tumor|mean cholesterol", desc)) {
    list(method = "1. Single Mean Estimation",
         reason = "Your description mentions estimating a single population mean.",
         inputs = "Expected SD, margin of error, confidence level.")
    
  } else if (grepl("proportion|rate|response|binary|responder|percent|success", desc)) {
    list(method = "4. Two Proportion Comparison",
         reason = "Your description mentions comparing binary outcomes across two groups.",
         inputs = "Expected proportions for each group, alpha, power.")
    
  } else if (grepl("mean|average|continuous|score|hba1c|cholesterol|weight|bp|blood pressure|difference", desc)) {
    list(method = "3. Two Mean Comparison",
         reason = "Your description mentions comparing continuous outcomes across two groups.",
         inputs = "Standard deviation, expected mean difference, alpha, power.")
    
  } else {
    list(method = "Unclear — more information needed",
         reason = "Try including keywords like 'survival', 'proportion', 'mean', 'paired', 'non-inferiority', or 'equivalence'.",
         inputs = "Specify the primary endpoint type: continuous, binary, time-to-event, or trial type.")
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3 ── UI
# ══════════════════════════════════════════════════════════════════════════════

DESIGNS <- c(
  "1. Single Mean Estimation",
  "2. Single Proportion Estimation",
  "3. Two Mean Comparison",
  "4. Two Proportion Comparison",
  "5. Paired Mean Comparison",
  "6. Survival Analysis",
  "7. Non-Inferiority Trial",
  "8. Equivalence Trial"
)

ESTIMATION_DESIGNS <- c("1. Single Mean Estimation", "2. Single Proportion Estimation")

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = span("Clinical Trial Calculator", style = "font-size:15px;")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator",    tabName = "calc",   icon = icon("calculator")),
      menuItem("Power Plot",    tabName = "plot",   icon = icon("chart-line")),
      menuItem("AI Suggestion", tabName = "ai",     icon = icon("robot")),
      menuItem("Export Report", tabName = "export", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius:6px; }
      pre { background:#f8f9fa; border:1px solid #dee2e6;
            padding:12px; border-radius:4px; font-size:13px; }
    "))),
    
    tabItems(
      
      # ── Tab 1 : Calculator ───────────────────────────────────────────────
      tabItem(tabName = "calc",
              fluidRow(
                
                # LEFT — parameters
                box(title = "Study Parameters", status = "primary",
                    solidHeader = TRUE, width = 4,
                    
                    selectInput("design", "Study Design", choices = DESIGNS),
                    
                    # Confidence level (estimation designs only)
                    conditionalPanel(
                      condition = "input.design == '1. Single Mean Estimation' ||
                             input.design == '2. Single Proportion Estimation'",
                      numericInput("conf", "Confidence Level", 0.95,
                                   min = 0.80, max = 0.99, step = 0.01)
                    ),
                    
                    # Alpha + Power (all hypothesis-testing designs)
                    conditionalPanel(
                      condition = "input.design != '1. Single Mean Estimation' &&
                             input.design != '2. Single Proportion Estimation'",
                      numericInput("alpha", "Alpha", 0.05,
                                   min = 0.001, max = 0.20, step = 0.005),
                      numericInput("power", "Power (1 - Beta)", 0.80,
                                   min = 0.50, max = 0.99, step = 0.05)
                    ),
                    
                    numericInput("dropout", "Dropout Rate (%)", 10,
                                 min = 0, max = 60, step = 1),
                    
                    hr(),
                    
                    # 1. Single Mean
                    conditionalPanel(
                      condition = "input.design == '1. Single Mean Estimation'",
                      numericInput("sm_sd",     "Standard Deviation",  15),
                      numericInput("sm_margin", "Margin of Error",      5)
                    ),
                    # 2. Single Proportion
                    conditionalPanel(
                      condition = "input.design == '2. Single Proportion Estimation'",
                      numericInput("sp_p",      "Expected Proportion", 0.30,
                                   min = 0.01, max = 0.99, step = 0.01),
                      numericInput("sp_margin", "Margin of Error",     0.05,
                                   min = 0.001, step = 0.005)
                    ),
                    # 3. Two Mean
                    conditionalPanel(
                      condition = "input.design == '3. Two Mean Comparison'",
                      numericInput("tm_sd",    "Standard Deviation", 10),
                      numericInput("tm_delta", "Mean Difference",     5)
                    ),
                    # 4. Two Proportion
                    conditionalPanel(
                      condition = "input.design == '4. Two Proportion Comparison'",
                      numericInput("tp_p1", "Group 1 Proportion", 0.50,
                                   min = 0.01, max = 0.99, step = 0.01),
                      numericInput("tp_p2", "Group 2 Proportion", 0.65,
                                   min = 0.01, max = 0.99, step = 0.01)
                    ),
                    # 5. Paired Mean
                    conditionalPanel(
                      condition = "input.design == '5. Paired Mean Comparison'",
                      numericInput("pm_sd",    "SD of Paired Differences", 10),
                      numericInput("pm_delta", "Expected Mean Difference",  5)
                    ),
                    # 6. Survival
                    conditionalPanel(
                      condition = "input.design == '6. Survival Analysis'",
                      numericInput("sv_hr", "Hazard Ratio",        0.70,
                                   min = 0.01, step = 0.05),
                      numericInput("sv_er", "Expected Event Rate", 0.60,
                                   min = 0.01, max = 1.00, step = 0.05),
                      numericInput("sv_fu", "Follow-up (months)",  24)
                    ),
                    # 7. Non-Inferiority
                    conditionalPanel(
                      condition = "input.design == '7. Non-Inferiority Trial'",
                      numericInput("ni_sd",     "Standard Deviation",        10),
                      numericInput("ni_delta",  "Expected Treatment Effect",   0),
                      numericInput("ni_margin", "Non-Inferiority Margin",      5,
                                   min = 0.001, step = 0.5)
                    ),
                    # 8. Equivalence
                    conditionalPanel(
                      condition = "input.design == '8. Equivalence Trial'",
                      numericInput("eq_sd",     "Standard Deviation",  10),
                      numericInput("eq_margin", "Equivalence Margin",   5,
                                   min = 0.001, step = 0.5)
                    ),
                    
                    actionButton("calc", "Calculate",
                                 icon = icon("play"), class = "btn-primary btn-block",
                                 style = "margin-top:10px;")
                ),
                
                # RIGHT — results
                column(width = 8,
                       fluidRow(
                         valueBoxOutput("box_n",     width = 4),
                         valueBoxOutput("box_adj",   width = 4),
                         valueBoxOutput("box_total", width = 4)
                       ),
                       fluidRow(
                         box(title = "Interpretation", status = "success",
                             solidHeader = TRUE, width = 12,
                             verbatimTextOutput("interpretation"))
                       ),
                       fluidRow(
                         conditionalPanel(
                           condition = "input.design == '6. Survival Analysis'",
                           box(title = "Events Required", status = "warning",
                               solidHeader = TRUE, width = 6,
                               verbatimTextOutput("events_out"))
                         )
                       )
                )
              )
      ),
      
      # ── Tab 2 : Power Plot ───────────────────────────────────────────────
      tabItem(tabName = "plot",
              fluidRow(
                box(title = "Power vs Sample Size", status = "primary",
                    solidHeader = TRUE, width = 12,
                    plotOutput("power_plot", height = "440px"),
                    helpText("Reflects the design and parameters from the Calculator tab.",
                             " Designs 1 & 2 show Margin of Error vs Sample Size instead."))
              )
      ),
      
      # ── Tab 3 : AI Suggestion ────────────────────────────────────────────
      tabItem(tabName = "ai",
              fluidRow(
                box(title = "AI Study Design Suggestion", status = "info",
                    solidHeader = TRUE, width = 12,
                    textAreaInput("study_desc", "Describe your study:",
                                  placeholder = paste("e.g. Randomized trial comparing",
                                                      "response rate of drug vs placebo",
                                                      "in Type 2 diabetes patients"),
                                  rows = 4, width = "100%"),
                    actionButton("suggest_btn", "Get Suggestion",
                                 icon = icon("magic"), class = "btn-warning"),
                    br(), br(),
                    uiOutput("ai_output"))
              )
      ),
      
      # ── Tab 4 : Export ───────────────────────────────────────────────────
      tabItem(tabName = "export",
              fluidRow(
                box(title = "Download Report", status = "info",
                    solidHeader = TRUE, width = 6,
                    p("Generates a plain-text report with all assumptions, sample size,",
                      " and interpretation — ready to paste into a Statistical Analysis Plan."),
                    p(tags$em("Click 'Calculate' on the Calculator tab first.")),
                    downloadButton("download_report", "Download Report (.txt)",
                                   class = "btn-info btn-lg"))
              )
      )
      
    ) # end tabItems
  )   # end dashboardBody
)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4 ── Server
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    n = NULL, n_adj = NULL, n_total = NULL,
    events = NULL, design = NULL, interp = NULL,
    is_est = FALSE
  )
  
  # ── Calculate ─────────────────────────────────────────────────────────────
  observeEvent(input$calc, {
    
    d        <- input$design
    rv$design <- d
    rv$events <- NULL
    rv$is_est <- d %in% ESTIMATION_DESIGNS
    
    # helper to set rv values
    set_rv <- function(n, two_group = TRUE) {
      rv$n      <- n
      rv$n_adj  <- dropout_adjust(n, input$dropout)
      rv$n_total <- if (two_group) rv$n_adj * 2 else rv$n_adj
    }
    
    if (d == "1. Single Mean Estimation") {
      set_rv(ss_single_mean(input$sm_sd, input$sm_margin, input$conf), FALSE)
      rv$interp <- paste0(
        "Interpretation:\n",
        "To estimate a population mean within \u00b1", input$sm_margin, " units\n",
        "with SD = ", input$sm_sd, " and ", input$conf*100, "% confidence,\n",
        rv$n, " subjects are required.\n",
        "After adjusting for ", input$dropout, "% dropout: ", rv$n_adj, " subjects."
      )
      
    } else if (d == "2. Single Proportion Estimation") {
      set_rv(ss_single_prop(input$sp_p, input$sp_margin, input$conf), FALSE)
      rv$interp <- paste0(
        "Interpretation:\n",
        "To estimate a proportion of ", input$sp_p*100, "%\n",
        "within \u00b1", input$sp_margin*100, "% at ", input$conf*100, "% confidence,\n",
        rv$n, " subjects are required.\n",
        "After adjusting for ", input$dropout, "% dropout: ", rv$n_adj, " subjects."
      )
      
    } else if (d == "3. Two Mean Comparison") {
      set_rv(ss_two_mean(input$tm_sd, input$tm_delta, input$alpha, input$power))
      rv$interp <- paste0(
        "Interpretation:\n",
        "To detect a mean difference of ", input$tm_delta,
        " units (SD = ", input$tm_sd, ")\n",
        "with power = ", input$power*100, "% and alpha = ", input$alpha, " (two-sided),\n",
        rv$n, " subjects per group are required (unadjusted).\n",
        "After adjusting for ", input$dropout, "% dropout: ",
        rv$n_adj, " per group | Total = ", rv$n_total, "."
      )
      
    } else if (d == "4. Two Proportion Comparison") {
      set_rv(ss_two_prop(input$tp_p1, input$tp_p2, input$alpha, input$power))
      rv$interp <- paste0(
        "Interpretation:\n",
        "To detect a difference between proportions ",
        input$tp_p1*100, "% vs ", input$tp_p2*100, "%\n",
        "with power = ", input$power*100, "% and alpha = ", input$alpha, " (two-sided),\n",
        rv$n, " subjects per group are required (unadjusted).\n",
        "After adjusting for ", input$dropout, "% dropout: ",
        rv$n_adj, " per group | Total = ", rv$n_total, "."
      )
      
    } else if (d == "5. Paired Mean Comparison") {
      set_rv(ss_paired(input$pm_sd, input$pm_delta, input$alpha, input$power), FALSE)
      rv$interp <- paste0(
        "Interpretation:\n",
        "For a paired (before-after) design, to detect a mean difference of ",
        input$pm_delta, "\nwith SD of differences = ", input$pm_sd,
        ", power = ", input$power*100, "% and alpha = ", input$alpha, ",\n",
        rv$n, " subjects are required (unadjusted).\n",
        "After adjusting for ", input$dropout, "% dropout: ", rv$n_adj, " subjects."
      )
      
    } else if (d == "6. Survival Analysis") {
      res <- ss_survival(input$sv_hr, input$sv_er, input$alpha, input$power)
      set_rv(res$n_per)
      rv$events <- res$events
      rv$interp <- paste0(
        "Interpretation:\n",
        "To detect a hazard ratio of ", input$sv_hr,
        " (event rate = ", input$sv_er*100, "%,",
        " follow-up = ", input$sv_fu, " months)\n",
        "with power = ", input$power*100, "% and alpha = ", input$alpha, " (two-sided),\n",
        res$n_per, " subjects per group are required (unadjusted).\n",
        "Total events required: ", res$events, ".\n",
        "After adjusting for ", input$dropout, "% dropout: ",
        rv$n_adj, " per group | Total = ", rv$n_total, "."
      )
      
    } else if (d == "7. Non-Inferiority Trial") {
      set_rv(ss_noninferiority(input$ni_sd, input$ni_delta,
                               input$ni_margin, input$alpha, input$power))
      rv$interp <- paste0(
        "Interpretation:\n",
        "Non-inferiority trial: margin = ", input$ni_margin,
        ", expected effect = ", input$ni_delta, ", SD = ", input$ni_sd, ".\n",
        "Alpha (one-sided) = ", input$alpha, ", power = ", input$power*100, "%.\n",
        rv$n, " subjects per group are required (unadjusted).\n",
        "After adjusting for ", input$dropout, "% dropout: ",
        rv$n_adj, " per group | Total = ", rv$n_total, "."
      )
      
    } else if (d == "8. Equivalence Trial") {
      set_rv(ss_equivalence(input$eq_sd, input$eq_margin, input$alpha, input$power))
      rv$interp <- paste0(
        "Interpretation:\n",
        "Equivalence trial (TOST): margin = \u00b1", input$eq_margin,
        ", SD = ", input$eq_sd, ".\n",
        "Alpha = ", input$alpha, ", power = ", input$power*100, "%.\n",
        rv$n, " subjects per group are required (unadjusted).\n",
        "After adjusting for ", input$dropout, "% dropout: ",
        rv$n_adj, " per group | Total = ", rv$n_total, "."
      )
    }
  })
  
  # ── Value Boxes ────────────────────────────────────────────────────────────
  output$box_n <- renderValueBox({
    lbl <- if (isTRUE(rv$is_est)) "Required (unadjusted)" else "Per Group (unadjusted)"
    valueBox(if (!is.null(rv$n)) rv$n else "\u2014", lbl,
             icon = icon("users"), color = "blue")
  })
  output$box_adj <- renderValueBox({
    lbl <- if (isTRUE(rv$is_est)) paste0("Adjusted (", input$dropout, "% dropout)")
    else                   paste0("Per Group (", input$dropout, "% dropout)")
    valueBox(if (!is.null(rv$n_adj)) rv$n_adj else "\u2014", lbl,
             icon = icon("user-minus"), color = "orange")
  })
  output$box_total <- renderValueBox({
    lbl <- if (isTRUE(rv$is_est)) "Total (= adjusted)" else "Total Sample Size"
    valueBox(if (!is.null(rv$n_total)) rv$n_total else "\u2014", lbl,
             icon = icon("user-plus"), color = "green")
  })
  
  output$interpretation <- renderText({
    if (is.null(rv$interp)) "Select a design and press 'Calculate' to see results."
    else rv$interp
  })
  
  output$events_out <- renderText({
    if (is.null(rv$events)) "" else paste("Total events required:", rv$events)
  })
  
  # ── Power / Margin Plot ────────────────────────────────────────────────────
  output$power_plot <- renderPlot({
    d <- input$design
    
    # Estimation designs — Margin vs n
    if (d == "1. Single Mean Estimation") {
      margins <- seq(2, 20, by = 1)
      sizes   <- sapply(margins, function(m) ss_single_mean(input$sm_sd, m, input$conf))
      cur_n   <- ss_single_mean(input$sm_sd, input$sm_margin, input$conf)
      plot(margins, sizes, type="b", pch=19, col="#2c7bb6", lwd=2,
           xlab="Margin of Error", ylab="Sample Size",
           main="Margin of Error vs Sample Size — Single Mean Estimation", las=1)
      grid(col="grey85", lty="dotted")
      abline(v=input$sm_margin, h=cur_n, lty=2, col="firebrick")
      points(input$sm_margin, cur_n, pch=21, cex=2.5, bg="firebrick", col="white")
      text(input$sm_margin, cur_n, labels=paste0("n=",cur_n), pos=4,
           col="firebrick", font=2)
      return()
    }
    
    if (d == "2. Single Proportion Estimation") {
      margins <- seq(0.01, 0.15, by=0.01)
      sizes   <- sapply(margins, function(m) ss_single_prop(input$sp_p, m, input$conf))
      cur_n   <- ss_single_prop(input$sp_p, input$sp_margin, input$conf)
      plot(margins*100, sizes, type="b", pch=19, col="#2c7bb6", lwd=2,
           xlab="Margin of Error (%)", ylab="Sample Size",
           main="Margin of Error vs Sample Size — Single Proportion Estimation", las=1)
      grid(col="grey85", lty="dotted")
      abline(v=input$sp_margin*100, h=cur_n, lty=2, col="firebrick")
      points(input$sp_margin*100, cur_n, pch=21, cex=2.5, bg="firebrick", col="white")
      text(input$sp_margin*100, cur_n, labels=paste0("n=",cur_n), pos=4,
           col="firebrick", font=2)
      return()
    }
    
    # Hypothesis-testing designs — Power vs n
    powers <- seq(0.60, 0.95, by=0.05)
    
    sizes <- switch(d,
                    "3. Two Mean Comparison" =
                      sapply(powers, function(p) ss_two_mean(input$tm_sd, input$tm_delta, input$alpha, p)),
                    "4. Two Proportion Comparison" =
                      sapply(powers, function(p) ss_two_prop(input$tp_p1, input$tp_p2, input$alpha, p)),
                    "5. Paired Mean Comparison" =
                      sapply(powers, function(p) ss_paired(input$pm_sd, input$pm_delta, input$alpha, p)),
                    "6. Survival Analysis" =
                      sapply(powers, function(p) ss_survival(input$sv_hr, input$sv_er, input$alpha, p)$n_per),
                    "7. Non-Inferiority Trial" =
                      sapply(powers, function(p) ss_noninferiority(input$ni_sd, input$ni_delta,
                                                                   input$ni_margin, input$alpha, p)),
                    "8. Equivalence Trial" =
                      sapply(powers, function(p) ss_equivalence(input$eq_sd, input$eq_margin, input$alpha, p)),
                    rep(NA, length(powers))
    )
    
    if (all(is.na(sizes))) {
      plot.new(); text(0.5, 0.5, "No plot available for this design.", cex=1.4)
      return()
    }
    
    cur_n <- sizes[which.min(abs(powers - input$power))]
    
    plot(powers*100, sizes, type="b", pch=19, col="#2c7bb6", lwd=2,
         xlab="Power (%)", ylab="Sample Size per Group",
         main=paste("Power vs Sample Size \u2014", d), las=1)
    grid(col="grey85", lty="dotted")
    abline(v=input$power*100, h=cur_n, lty=2, col="firebrick")
    points(input$power*100, cur_n, pch=21, cex=2.5, bg="firebrick", col="white")
    text(input$power*100, cur_n,
         labels=paste0("n=",cur_n,"\n(",input$power*100,"%)"),
         pos=4, col="firebrick", font=2)
  })
  
  # ── AI Suggestion ──────────────────────────────────────────────────────────
  observeEvent(input$suggest_btn, {
    req(nchar(trimws(input$study_desc)) > 0)
    s <- ai_suggest(input$study_desc)
    output$ai_output <- renderUI({
      tagList(
        tags$hr(),
        tags$h4(icon("check-circle"), " Recommended Method"),
        tags$p(tags$strong(s$method)),
        tags$h4(icon("info-circle"), " Reasoning"),
        tags$p(s$reason),
        tags$h4(icon("list-alt"), " Required Inputs"),
        tags$p(s$inputs),
        tags$hr(),
        tags$small(tags$em(
          "Tip: Select the matching design from the dropdown in the Calculator tab."
        ))
      )
    })
  })
  
  # ── Export Report ──────────────────────────────────────────────────────────
  output$download_report <- downloadHandler(
    filename = function()
      paste0("SampleSize_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"),
    
    content = function(file) {
      req(!is.null(rv$n))
      sep <- paste(rep("=", 50), collapse="")
      
      assump <- switch(rv$design,
                       "1. Single Mean Estimation" = c(
                         paste("SD              :", input$sm_sd),
                         paste("Margin of Error :", input$sm_margin),
                         paste("Confidence      :", input$conf*100, "%")),
                       "2. Single Proportion Estimation" = c(
                         paste("Expected Prop   :", input$sp_p),
                         paste("Margin of Error :", input$sp_margin*100, "%"),
                         paste("Confidence      :", input$conf*100, "%")),
                       "3. Two Mean Comparison" = c(
                         paste("SD              :", input$tm_sd),
                         paste("Mean Difference :", input$tm_delta),
                         paste("Alpha           :", input$alpha),
                         paste("Power           :", input$power*100, "%")),
                       "4. Two Proportion Comparison" = c(
                         paste("Group 1 Prop    :", input$tp_p1),
                         paste("Group 2 Prop    :", input$tp_p2),
                         paste("Alpha           :", input$alpha),
                         paste("Power           :", input$power*100, "%")),
                       "5. Paired Mean Comparison" = c(
                         paste("SD Differences  :", input$pm_sd),
                         paste("Mean Difference :", input$pm_delta),
                         paste("Alpha           :", input$alpha),
                         paste("Power           :", input$power*100, "%")),
                       "6. Survival Analysis" = c(
                         paste("Hazard Ratio    :", input$sv_hr),
                         paste("Event Rate      :", input$sv_er*100, "%"),
                         paste("Follow-up       :", input$sv_fu, "months"),
                         paste("Alpha           :", input$alpha),
                         paste("Power           :", input$power*100, "%")),
                       "7. Non-Inferiority Trial" = c(
                         paste("SD              :", input$ni_sd),
                         paste("Expected Effect :", input$ni_delta),
                         paste("NI Margin       :", input$ni_margin),
                         paste("Alpha (1-sided) :", input$alpha),
                         paste("Power           :", input$power*100, "%")),
                       "8. Equivalence Trial" = c(
                         paste("SD              :", input$eq_sd),
                         paste("Equiv. Margin   :", input$eq_margin),
                         paste("Alpha           :", input$alpha),
                         paste("Power           :", input$power*100, "%")),
                       character(0)
      )
      
      lines <- c(
        sep,
        "   CLINICAL TRIAL SAMPLE SIZE REPORT",
        paste("   Generated :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        sep, "",
        paste("Study Design    :", rv$design),
        paste("Dropout Rate    :", input$dropout, "%"), "",
        "-- Assumptions ---------------------------------",
        assump, "",
        "-- Sample Size Results -------------------------",
        paste("Unadjusted n    :", rv$n),
        paste("Adjusted n      :", rv$n_adj),
        paste("Total n         :", rv$n_total)
      )
      
      if (!is.null(rv$events))
        lines <- c(lines, paste("Events needed   :", rv$events))
      
      lines <- c(lines, "",
                 "-- Interpretation ------------------------------",
                 rv$interp, "",
                 sep,
                 "Verify with a qualified biostatistician before",
                 "use in study protocols or SAPs.",
                 sep
      )
      
      writeLines(lines, file)
    }
  )
  
} # end server

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)

