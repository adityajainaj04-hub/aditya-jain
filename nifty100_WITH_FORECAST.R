# =============================================================================
#  NIFTY 100 STOCK ANALYSIS + PRICE FORECASTING PLATFORM
#  New Features:
#    - AI-style Price Forecast Tab (Linear Trend, EMA Projection, Monte Carlo)
#    - Support & Resistance Zone Detection
#    - Forecast Summary Table with Bull / Base / Bear scenarios
#    - Signal Dashboard (BUY / SELL / HOLD based on technicals)
#  HOW TO RUN:
#    Option 1: Open in RStudio, click the "Run App" button
#    Option 2: In RStudio Console type:
#              shiny::runApp("nifty100_WITH_FORECAST.R")
# =============================================================================

# --------------------------------------------------------------------------- #
#  STEP 1 - Auto-install missing packages                                     #
# --------------------------------------------------------------------------- #
required_pkgs <- c(
  "shiny", "shinydashboard", "shinycssloaders",
  "plotly", "DT",
  "quantmod", "TTR", "PerformanceAnalytics",
  "dplyr", "tidyr", "lubridate", "scales", "xts", "zoo", "MASS"
)
new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(plotly)
  library(DT)
  library(quantmod)
  library(TTR)
  library(PerformanceAnalytics)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(scales)
  library(xts)
  library(zoo)
  library(MASS)
})

# --------------------------------------------------------------------------- #
#  STEP 2 - Nifty 100 universe                                                #
# --------------------------------------------------------------------------- #
nifty100_stocks <- c(
  "RELIANCE.NS"   = "Reliance Industries",
  "TCS.NS"        = "Tata Consultancy Services",
  "HDFCBANK.NS"   = "HDFC Bank",
  "INFY.NS"       = "Infosys",
  "HINDUNILVR.NS" = "Hindustan Unilever",
  "ICICIBANK.NS"  = "ICICI Bank",
  "KOTAKBANK.NS"  = "Kotak Mahindra Bank",
  "BHARTIARTL.NS" = "Bharti Airtel",
  "ITC.NS"        = "ITC",
  "LT.NS"         = "Larsen and Toubro",
  "SBIN.NS"       = "State Bank of India",
  "AXISBANK.NS"   = "Axis Bank",
  "ASIANPAINT.NS" = "Asian Paints",
  "DMART.NS"      = "Avenue Supermarts DMart",
  "BAJFINANCE.NS" = "Bajaj Finance",
  "MARUTI.NS"     = "Maruti Suzuki",
  "SUNPHARMA.NS"  = "Sun Pharmaceutical",
  "TITAN.NS"      = "Titan Company",
  "WIPRO.NS"      = "Wipro",
  "ULTRACEMCO.NS" = "UltraTech Cement",
  "HCLTECH.NS"    = "HCL Technologies",
  "NTPC.NS"       = "NTPC",
  "POWERGRID.NS"  = "Power Grid Corporation",
  "BAJAJFINSV.NS" = "Bajaj Finserv",
  "TECHM.NS"      = "Tech Mahindra",
  "ADANIENT.NS"   = "Adani Enterprises",
  "ADANIPORTS.NS" = "Adani Ports SEZ",
  "JSWSTEEL.NS"   = "JSW Steel",
  "TATAMOTORS.NS" = "Tata Motors",
  "TATASTEEL.NS"  = "Tata Steel",
  "GRASIM.NS"     = "Grasim Industries",
  "ONGC.NS"       = "ONGC",
  "COALINDIA.NS"  = "Coal India",
  "HINDALCO.NS"   = "Hindalco Industries",
  "DIVISLAB.NS"   = "Divis Laboratories",
  "DRREDDY.NS"    = "Dr Reddys Laboratories",
  "CIPLA.NS"      = "Cipla",
  "BPCL.NS"       = "BPCL",
  "EICHERMOT.NS"  = "Eicher Motors",
  "HEROMOTOCO.NS" = "Hero MotoCorp",
  "INDUSINDBK.NS" = "IndusInd Bank",
  "M&M.NS"        = "Mahindra and Mahindra",
  "NESTLEIND.NS"  = "Nestle India",
  "PIDILITIND.NS" = "Pidilite Industries",
  "SIEMENS.NS"    = "Siemens India",
  "TATACONSUM.NS" = "Tata Consumer Products",
  "VEDL.NS"       = "Vedanta",
  "ZOMATO.NS"     = "Zomato Eternal",
  "PAYTM.NS"      = "Paytm",
  "NYKAA.NS"      = "Nykaa FSN E-Commerce"
)

stock_choices <- setNames(
  names(nifty100_stocks),
  paste0(nifty100_stocks, " (", names(nifty100_stocks), ")")
)

# --------------------------------------------------------------------------- #
#  STEP 3 - Helpers                                                           #
# --------------------------------------------------------------------------- #
fetch_stock_data <- function(ticker, from_date, to_date) {
  tryCatch({
    suppressWarnings(
      getSymbols(ticker,
                 src         = "yahoo",
                 from        = from_date,
                 to          = to_date,
                 auto.assign = FALSE)
    )
  }, error = function(e) {
    showNotification(paste("Could not fetch", ticker, ":", e$message),
                     type = "error", duration = 8)
    NULL
  })
}

compute_indicators <- function(ohlcv) {
  cl  <- Cl(ohlcv)
  hlc <- HLC(ohlcv)
  list(
    sma20 = SMA(cl, n = 20),
    sma50 = SMA(cl, n = 50),
    ema20 = EMA(cl, n = 20),
    bb    = BBands(cl, n = 20, sd = 2),
    rsi14 = RSI(cl, n = 14),
    macd  = MACD(cl, nFast = 12, nSlow = 26, nSig = 9),
    atr14 = ATR(hlc, n = 14)
  )
}

# ---- Forecasting Engine --------------------------------------------------- #
run_forecast <- function(ohlcv, horizon_days, n_simulations = 500) {
  cl        <- as.numeric(Cl(ohlcv))
  dates_hist <- index(ohlcv)
  last_price <- tail(cl, 1)
  last_date  <- tail(dates_hist, 1)

  # Daily log returns
  ret <- diff(log(cl))
  ret <- ret[!is.na(ret) & is.finite(ret)]

  mu    <- mean(ret)
  sigma <- sd(ret)

  # Future business dates (approximate: skip weekends)
  future_dates <- seq(last_date + 1, by = "day", length.out = horizon_days * 2)
  future_dates <- future_dates[!weekdays(future_dates) %in% c("Saturday", "Sunday")]
  future_dates <- head(future_dates, horizon_days)

  # --- Model 1: Linear Trend Regression ---
  x_idx  <- seq_along(cl)
  lm_fit <- lm(cl ~ x_idx)
  slope  <- coef(lm_fit)[2]
  future_idx <- (length(cl) + 1):(length(cl) + horizon_days)
  linear_fc  <- coef(lm_fit)[1] + slope * future_idx
  # Residual std for confidence band
  lm_resid_sd <- sd(residuals(lm_fit))
  linear_upper <- linear_fc + 1.96 * lm_resid_sd
  linear_lower <- linear_fc - 1.96 * lm_resid_sd

  # --- Model 2: EMA Projection (last EMA + trend extrapolation) ---
  ema20     <- as.numeric(EMA(Cl(ohlcv), n = 20))
  ema_last  <- tail(na.omit(ema20), 1)
  ema_slope <- mean(diff(tail(na.omit(ema20), 10)))  # recent 10-day EMA momentum
  ema_fc    <- ema_last + ema_slope * seq_len(horizon_days)

  # --- Model 3: Monte Carlo GBM (Geometric Brownian Motion) ---
  set.seed(42)
  mc_paths <- matrix(NA, nrow = horizon_days, ncol = n_simulations)
  for (i in seq_len(n_simulations)) {
    shocks <- rnorm(horizon_days, mean = mu, sd = sigma)
    path   <- last_price * cumprod(exp(shocks))
    mc_paths[, i] <- path
  }

  mc_median <- apply(mc_paths, 1, median)
  mc_p5     <- apply(mc_paths, 1, quantile, 0.05)
  mc_p25    <- apply(mc_paths, 1, quantile, 0.25)
  mc_p75    <- apply(mc_paths, 1, quantile, 0.75)
  mc_p95    <- apply(mc_paths, 1, quantile, 0.95)

  # --- Support / Resistance Zones (rolling local min/max) ---
  window_sr <- min(20, floor(length(cl) / 5))
  highs     <- as.numeric(Hi(ohlcv))
  lows      <- as.numeric(Lo(ohlcv))
  # Recent pivot highs and lows (last 60 days)
  recent_n  <- min(60, length(highs))
  recent_h  <- tail(highs, recent_n)
  recent_l  <- tail(lows,  recent_n)

  # Cluster support/resistance into 3 zones using simple quantile bucketing
  resistance_levels <- quantile(recent_h, c(0.70, 0.85, 0.97), na.rm = TRUE)
  support_levels    <- quantile(recent_l, c(0.03, 0.15, 0.30), na.rm = TRUE)

  # --- Signal Generator ---
  ind    <- compute_indicators(ohlcv)
  rsi_v  <- tail(as.numeric(ind$rsi14), 1)
  macd_v <- tail(as.numeric(ind$macd[, "macd"]),   1)
  sig_v  <- tail(as.numeric(ind$macd[, "signal"]), 1)
  sma20  <- tail(as.numeric(ind$sma20), 1)
  sma50  <- tail(as.numeric(ind$sma50), 1)
  bb_up  <- tail(as.numeric(ind$bb[, "up"]), 1)
  bb_dn  <- tail(as.numeric(ind$bb[, "dn"]), 1)

  signals <- data.frame(
    Indicator = c("RSI (14)", "MACD Cross", "SMA 20 vs SMA 50", "Bollinger Band"),
    Value     = c(
      round(rsi_v, 2),
      round(macd_v - sig_v, 4),
      round(sma20 - sma50, 2),
      round((last_price - bb_dn) / (bb_up - bb_dn) * 100, 1)
    ),
    Signal = c(
      ifelse(rsi_v < 30, "BUY (Oversold)", ifelse(rsi_v > 70, "SELL (Overbought)", "HOLD (Neutral)")),
      ifelse(macd_v > sig_v, "BUY (Bullish Cross)", "SELL (Bearish Cross)"),
      ifelse(sma20 > sma50, "BUY (Golden Cross)", "SELL (Death Cross)"),
      ifelse(last_price < bb_dn, "BUY (Below Lower Band)",
             ifelse(last_price > bb_up, "SELL (Above Upper Band)", "HOLD (Inside Band)"))
    ),
    stringsAsFactors = FALSE
  )

  # --- Scenario Summary ---
  horizon_idx <- horizon_days
  scenarios <- data.frame(
    Scenario    = c("Bull Case (95th %ile)", "Base Case (Median)", "Bear Case (5th %ile)"),
    Model       = c("Monte Carlo", "Monte Carlo", "Monte Carlo"),
    Target_Price = c(
      round(mc_p95[horizon_idx], 2),
      round(mc_median[horizon_idx], 2),
      round(mc_p5[horizon_idx], 2)
    ),
    Change_Pct  = c(
      round((mc_p95[horizon_idx] / last_price - 1) * 100, 2),
      round((mc_median[horizon_idx] / last_price - 1) * 100, 2),
      round((mc_p5[horizon_idx] / last_price - 1) * 100, 2)
    ),
    stringsAsFactors = FALSE
  )
  colnames(scenarios) <- c("Scenario", "Model", "Target Price (Rs.)", "Expected Change (%)")

  list(
    future_dates     = future_dates,
    last_price       = last_price,
    last_date        = last_date,
    linear_fc        = linear_fc,
    linear_upper     = linear_upper,
    linear_lower     = linear_lower,
    ema_fc           = ema_fc,
    mc_median        = mc_median,
    mc_p5            = mc_p5,
    mc_p25           = mc_p25,
    mc_p75           = mc_p75,
    mc_p95           = mc_p95,
    mc_paths         = mc_paths,
    resistance_levels = resistance_levels,
    support_levels    = support_levels,
    signals          = signals,
    scenarios        = scenarios,
    mu               = mu,
    sigma            = sigma
  )
}

# --------------------------------------------------------------------------- #
#  STEP 4 - UI                                                                #
# --------------------------------------------------------------------------- #
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title      = "Nifty 100 + Forecast",
    titleWidth = 270
  ),

  dashboardSidebar(
    width = 270,
    sidebarMenu(
      id = "sidebar",
      menuItem("Chart and Technicals", tabName = "chart",       icon = icon("chart-line")),
      menuItem("Price Forecast",       tabName = "forecast",    icon = icon("rocket")),
      menuItem("Signal Dashboard",     tabName = "signals",     icon = icon("traffic-light")),
      menuItem("Performance",          tabName = "performance", icon = icon("gauge-high")),
      menuItem("Data Table",           tabName = "datatable",   icon = icon("table")),
      menuItem("Compare Stocks",       tabName = "compare",     icon = icon("arrows-left-right"))
    ),
    hr(),
    tags$div(
      style = "padding:0 15px;",
      selectInput("ticker", "Select Stock",
                  choices  = stock_choices,
                  selected = "RELIANCE.NS"),
      dateRangeInput("date_range", "Date Range",
                     start = Sys.Date() - 365,
                     end   = Sys.Date(),
                     min   = "2015-01-01",
                     max   = Sys.Date()),
      selectInput("chart_type", "Chart Type",
                  choices  = c("Candlestick" = "candlestick",
                               "OHLC"        = "ohlc",
                               "Line"        = "line"),
                  selected = "candlestick"),
      checkboxGroupInput("indicators", "Overlay Indicators",
                         choices  = c("SMA 20"          = "sma20",
                                      "SMA 50"          = "sma50",
                                      "EMA 20"          = "ema20",
                                      "Bollinger Bands" = "bb"),
                         selected = c("sma20", "sma50")),
      checkboxGroupInput("subplots", "Sub-Plots",
                         choices  = c("Volume"   = "vol",
                                      "RSI (14)" = "rsi",
                                      "MACD"     = "macd"),
                         selected = c("vol", "rsi")),
      hr(),
      tags$strong(style = "color:#ffd700;", "Forecast Settings"),
      br(),
      sliderInput("forecast_horizon", "Forecast Horizon (Days)",
                  min = 5, max = 90, value = 30, step = 5),
      sliderInput("mc_sims", "Monte Carlo Simulations",
                  min = 100, max = 2000, value = 500, step = 100),
      actionButton("load_btn", "Load / Refresh + Forecast",
                   class = "btn-primary btn-block",
                   style = "margin-top:10px;")
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-sidebar { background-color: #1a1a2e !important; }
      .skin-black .main-header .logo,
      .skin-black .main-header .navbar      { background-color: #16213e !important; }
      .skin-black .main-sidebar             { background-color: #16213e !important; }
      .box {
        background: #0f3460 !important;
        border-top-color: #e94560 !important;
        color: #eee !important;
        border-radius: 8px;
      }
      .box-header {
        color: #fff !important;
        background: #0f3460 !important;
        border-bottom: 1px solid #e94560 !important;
      }
      .small-box { border-radius: 8px !important; }
      .small-box h3 { font-size: 28px; }
      .sidebar-menu>li.active>a { border-left-color: #e94560 !important; }
      label { color: #ccc !important; }
      .btn-primary { background-color: #e94560 !important; border-color: #e94560 !important; }
      .dataTables_wrapper { color: #eee !important; }
      table.dataTable thead { background: #16213e; color: #e94560; }
      .selectize-input, .selectize-dropdown { background: #16213e !important; color: #eee !important; }
      hr { border-color: #e94560; }
      .signal-buy  { color: #00e676 !important; font-weight: bold; }
      .signal-sell { color: #ff1744 !important; font-weight: bold; }
      .signal-hold { color: #ffd700 !important; font-weight: bold; }
      .scenario-box {
        background: #16213e;
        border-radius: 10px;
        padding: 18px;
        margin: 8px;
        text-align: center;
        border: 1px solid #e94560;
      }
      .scenario-bull { border-color: #00e676 !important; }
      .scenario-base { border-color: #ffd700 !important; }
      .scenario-bear { border-color: #ff1744 !important; }
      .forecast-note {
        background: #16213e;
        border-left: 4px solid #ffd700;
        padding: 10px 15px;
        border-radius: 4px;
        margin-top: 10px;
        font-size: 12px;
        color: #aaa;
      }
      .irs-bar, .irs-bar-edge { background: #e94560 !important; border-color: #e94560 !important; }
      .irs-single { background: #e94560 !important; }
    "))),

    tabItems(

      # ---- Chart Tab ---- #
      tabItem(tabName = "chart",
        fluidRow(
          valueBoxOutput("vbox_last",   width = 3),
          valueBoxOutput("vbox_chg",    width = 3),
          valueBoxOutput("vbox_high52", width = 3),
          valueBoxOutput("vbox_low52",  width = 3)
        ),
        fluidRow(
          box(width = 12, title = "Price Chart", solidHeader = TRUE,
              withSpinner(plotlyOutput("main_chart", height = "440px"), color = "#e94560"))
        ),
        fluidRow(
          conditionalPanel(
            "input.subplots !== null && input.subplots.indexOf('vol') >= 0",
            box(width = 4, title = "Volume", solidHeader = TRUE,
                withSpinner(plotlyOutput("vol_chart", height = "200px"), color = "#e94560"))
          ),
          conditionalPanel(
            "input.subplots !== null && input.subplots.indexOf('rsi') >= 0",
            box(width = 4, title = "RSI (14)", solidHeader = TRUE,
                withSpinner(plotlyOutput("rsi_chart", height = "200px"), color = "#e94560"))
          ),
          conditionalPanel(
            "input.subplots !== null && input.subplots.indexOf('macd') >= 0",
            box(width = 4, title = "MACD (12/26/9)", solidHeader = TRUE,
                withSpinner(plotlyOutput("macd_chart", height = "200px"), color = "#e94560"))
          )
        )
      ),

      # ---- Forecast Tab ---- #
      tabItem(tabName = "forecast",
        fluidRow(
          valueBoxOutput("fc_current",  width = 3),
          valueBoxOutput("fc_bull",     width = 3),
          valueBoxOutput("fc_base",     width = 3),
          valueBoxOutput("fc_bear",     width = 3)
        ),
        fluidRow(
          box(width = 12, title = "Price Forecast Chart (Historical + Projections)",
              solidHeader = TRUE,
              withSpinner(plotlyOutput("forecast_chart", height = "480px"), color = "#e94560"))
        ),
        fluidRow(
          box(width = 6, title = "Monte Carlo Path Distribution",
              solidHeader = TRUE,
              withSpinner(plotlyOutput("mc_dist_chart", height = "300px"), color = "#e94560")),
          box(width = 6, title = "Support & Resistance Zones",
              solidHeader = TRUE,
              withSpinner(plotlyOutput("sr_chart", height = "300px"), color = "#e94560"))
        ),
        fluidRow(
          box(width = 12, title = "Forecast Scenario Summary",
              solidHeader = TRUE,
              tableOutput("scenario_table"),
              tags$div(class = "forecast-note",
                tags$b("⚠ Disclaimer:"),
                " Price forecasts are based on statistical models (Linear Regression, EMA Projection, Monte Carlo GBM) 
                and historical data only. They do NOT guarantee future performance. 
                Markets are influenced by many unpredictable factors. 
                Use this tool for research and learning purposes only. 
                Always consult a SEBI-registered financial advisor before making investment decisions."
              )
          )
        )
      ),

      # ---- Signal Dashboard ---- #
      tabItem(tabName = "signals",
        fluidRow(
          box(width = 12, title = "Technical Signal Dashboard", solidHeader = TRUE,
              withSpinner(uiOutput("signal_cards"), color = "#e94560"))
        ),
        fluidRow(
          box(width = 6, title = "RSI Gauge", solidHeader = TRUE,
              withSpinner(plotlyOutput("rsi_gauge", height = "280px"), color = "#e94560")),
          box(width = 6, title = "Overall Recommendation", solidHeader = TRUE,
              withSpinner(uiOutput("overall_signal"), color = "#e94560"))
        ),
        fluidRow(
          box(width = 12, title = "Signal Detail Table", solidHeader = TRUE,
              withSpinner(tableOutput("signal_table"), color = "#e94560"))
        )
      ),

      # ---- Performance Tab ---- #
      tabItem(tabName = "performance",
        fluidRow(
          box(width = 6, title = "Cumulative Return", solidHeader = TRUE,
              withSpinner(plotlyOutput("cum_ret_plot", height = "340px"), color = "#e94560")),
          box(width = 6, title = "Daily Return Distribution", solidHeader = TRUE,
              withSpinner(plotlyOutput("ret_dist_plot", height = "340px"), color = "#e94560"))
        ),
        fluidRow(
          box(width = 12, title = "Performance Statistics", solidHeader = TRUE,
              tableOutput("perf_table"))
        )
      ),

      # ---- Data Table Tab ---- #
      tabItem(tabName = "datatable",
        fluidRow(
          box(width = 12, title = "Historical OHLCV Data", solidHeader = TRUE,
              withSpinner(DTOutput("data_tbl"), color = "#e94560"),
              br(),
              downloadButton("dl_csv", "Download CSV", class = "btn-primary"))
        )
      ),

      # ---- Compare Tab ---- #
      tabItem(tabName = "compare",
        fluidRow(
          box(width = 12, title = "Compare Multiple Stocks (Indexed to 100)",
              solidHeader = TRUE,
              selectInput("cmp_tickers", "Select up to 5 Stocks",
                          choices  = stock_choices,
                          selected = c("RELIANCE.NS", "TCS.NS", "HDFCBANK.NS"),
                          multiple = TRUE),
              dateRangeInput("cmp_dates", "Date Range",
                             start = Sys.Date() - 365,
                             end   = Sys.Date()),
              actionButton("cmp_btn", "Compare", class = "btn-primary"),
              br(), br(),
              withSpinner(plotlyOutput("cmp_plot", height = "440px"), color = "#e94560"))
        )
      )
    )
  )
)

# --------------------------------------------------------------------------- #
#  STEP 5 - SERVER                                                            #
# --------------------------------------------------------------------------- #
server <- function(input, output, session) {

  stock_data <- eventReactive(input$load_btn, {
    req(input$ticker, input$date_range)
    withProgress(message = paste("Fetching", input$ticker, "..."), {
      fetch_stock_data(input$ticker, input$date_range[1], input$date_range[2])
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  indicators <- reactive({
    d <- stock_data(); req(!is.null(d))
    compute_indicators(d)
  })

  ohlcv_df <- reactive({
    d <- stock_data(); req(!is.null(d))
    data.frame(
      Date   = index(d),
      Open   = as.numeric(Op(d)),
      High   = as.numeric(Hi(d)),
      Low    = as.numeric(Lo(d)),
      Close  = as.numeric(Cl(d)),
      Volume = as.numeric(Vo(d))
    )
  })

  forecast_result <- reactive({
    d <- stock_data(); req(!is.null(d))
    withProgress(message = "Running forecast models...", {
      run_forecast(d, input$forecast_horizon, input$mc_sims)
    })
  })

  # ---- Value Boxes (Chart Tab) ---- #
  output$vbox_last <- renderValueBox({
    d <- stock_data(); req(d)
    cl <- as.numeric(Cl(d))
    valueBox(
      paste0("Rs. ", formatC(tail(cl, 1), format = "f", digits = 2)),
      paste("Last Close -", nifty100_stocks[input$ticker]),
      icon  = icon("indian-rupee-sign"),
      color = "blue"
    )
  })

  output$vbox_chg <- renderValueBox({
    d <- stock_data(); req(d)
    cl  <- as.numeric(Cl(d)); n <- length(cl)
    chg <- if (n >= 2) round((cl[n] - cl[n - 1]) / cl[n - 1] * 100, 2) else 0
    col <- if (chg >= 0) "green" else "red"
    valueBox(paste0(ifelse(chg >= 0, "+", ""), chg, "%"), "1-Day Change",
             icon = icon(ifelse(chg >= 0, "arrow-up", "arrow-down")), color = col)
  })

  output$vbox_high52 <- renderValueBox({
    d <- stock_data(); req(d)
    recent <- xts::last(d, "52 weeks")
    valueBox(paste0("Rs. ", formatC(max(as.numeric(Hi(recent))), format = "f", digits = 2)),
             "52-Week High", icon = icon("arrow-up"), color = "yellow")
  })

  output$vbox_low52 <- renderValueBox({
    d <- stock_data(); req(d)
    recent <- xts::last(d, "52 weeks")
    valueBox(paste0("Rs. ", formatC(min(as.numeric(Lo(recent))), format = "f", digits = 2)),
             "52-Week Low", icon = icon("arrow-down"), color = "orange")
  })

  # ---- Forecast Value Boxes ---- #
  output$fc_current <- renderValueBox({
    fc <- forecast_result(); req(fc)
    valueBox(paste0("Rs. ", round(fc$last_price, 2)), "Current Price",
             icon = icon("indian-rupee-sign"), color = "blue")
  })
  output$fc_bull <- renderValueBox({
    fc <- forecast_result(); req(fc)
    target <- round(fc$mc_p95[input$forecast_horizon], 2)
    chg    <- round((target / fc$last_price - 1) * 100, 1)
    valueBox(paste0("Rs. ", target, " (+", chg, "%)"),
             paste("Bull Case -", input$forecast_horizon, "Days"),
             icon = icon("arrow-trend-up"), color = "green")
  })
  output$fc_base <- renderValueBox({
    fc <- forecast_result(); req(fc)
    target <- round(fc$mc_median[input$forecast_horizon], 2)
    chg    <- round((target / fc$last_price - 1) * 100, 1)
    valueBox(paste0("Rs. ", target, " (", ifelse(chg >= 0, "+", ""), chg, "%)"),
             paste("Base Case -", input$forecast_horizon, "Days"),
             icon = icon("chart-line"), color = "yellow")
  })
  output$fc_bear <- renderValueBox({
    fc <- forecast_result(); req(fc)
    target <- round(fc$mc_p5[input$forecast_horizon], 2)
    chg    <- round((target / fc$last_price - 1) * 100, 1)
    valueBox(paste0("Rs. ", target, " (", chg, "%)"),
             paste("Bear Case -", input$forecast_horizon, "Days"),
             icon = icon("arrow-trend-down"), color = "red")
  })

  # ---- Main Price Chart ---- #
  output$main_chart <- renderPlotly({
    d   <- stock_data(); req(d)
    df  <- ohlcv_df(); ind <- indicators()
    dates <- df$Date

    if (input$chart_type == "line") {
      p <- plot_ly(df, x = ~Date, y = ~Close, type = "scatter", mode = "lines",
                   line = list(color = "#e94560"), name = input$ticker)
    } else if (input$chart_type == "ohlc") {
      p <- plot_ly(df, x = ~Date, type = "ohlc",
                   open = ~Open, high = ~High, low = ~Low, close = ~Close,
                   increasing = list(line = list(color = "#00e676")),
                   decreasing = list(line = list(color = "#ff1744")),
                   name = input$ticker)
    } else {
      p <- plot_ly(df, x = ~Date, type = "candlestick",
                   open = ~Open, high = ~High, low = ~Low, close = ~Close,
                   increasing = list(line = list(color = "#00e676")),
                   decreasing = list(line = list(color = "#ff1744")),
                   name = input$ticker)
    }

    p <- p %>% layout(
      paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
      font   = list(color = "#eee"),
      xaxis  = list(rangeslider = list(visible = FALSE), gridcolor = "#1e4a8a", title = ""),
      yaxis  = list(gridcolor = "#1e4a8a", title = "Price (INR)"),
      legend = list(bgcolor = "#0f3460", font = list(color = "#eee"))
    )
    if ("sma20" %in% input$indicators)
      p <- p %>% add_lines(x = dates, y = as.numeric(ind$sma20),
                            name = "SMA 20", line = list(color = "#ffd700", width = 1.5))
    if ("sma50" %in% input$indicators)
      p <- p %>% add_lines(x = dates, y = as.numeric(ind$sma50),
                            name = "SMA 50", line = list(color = "#00bcd4", width = 1.5))
    if ("ema20" %in% input$indicators)
      p <- p %>% add_lines(x = dates, y = as.numeric(ind$ema20),
                            name = "EMA 20", line = list(color = "#ff9800", width = 1.5, dash = "dot"))
    if ("bb" %in% input$indicators) {
      bb <- ind$bb
      p <- p %>%
        add_lines(x = dates, y = as.numeric(bb[, "up"]),
                  name = "BB Upper", line = list(color = "#ab47bc", width = 1, dash = "dash")) %>%
        add_lines(x = dates, y = as.numeric(bb[, "dn"]),
                  name = "BB Lower", line = list(color = "#ab47bc", width = 1, dash = "dash")) %>%
        add_lines(x = dates, y = as.numeric(bb[, "mavg"]),
                  name = "BB Mid",   line = list(color = "#ce93d8", width = 1))
    }
    p
  })

  # ---- Forecast Chart ---- #
  output$forecast_chart <- renderPlotly({
    d  <- stock_data(); req(d)
    fc <- forecast_result(); req(fc)
    df <- ohlcv_df()

    # Show last 90 days of history for clarity
    hist_n  <- min(90, nrow(df))
    df_show <- tail(df, hist_n)

    p <- plot_ly() %>%
      # Historical candlestick
      add_trace(data = df_show, x = ~Date, type = "candlestick",
                open = ~Open, high = ~High, low = ~Low, close = ~Close,
                increasing = list(line = list(color = "#00e676")),
                decreasing = list(line = list(color = "#ff1744")),
                name = "Historical") %>%

      # MC 90% band (shaded)
      add_ribbons(x = fc$future_dates,
                  ymin = fc$mc_p5,  ymax = fc$mc_p95,
                  fillcolor = "rgba(233,69,96,0.10)",
                  line = list(width = 0), name = "MC 90% Band") %>%
      # MC 50% band (darker)
      add_ribbons(x = fc$future_dates,
                  ymin = fc$mc_p25, ymax = fc$mc_p75,
                  fillcolor = "rgba(233,69,96,0.22)",
                  line = list(width = 0), name = "MC 50% Band") %>%

      # Monte Carlo Median
      add_lines(x = fc$future_dates, y = fc$mc_median,
                name = "MC Median", line = list(color = "#e94560", width = 2.5, dash = "solid")) %>%

      # Linear Trend Forecast
      add_lines(x = fc$future_dates, y = fc$linear_fc,
                name = "Linear Trend", line = list(color = "#ffd700", width = 2, dash = "dash")) %>%

      # Linear CI band
      add_ribbons(x = fc$future_dates,
                  ymin = fc$linear_lower, ymax = fc$linear_upper,
                  fillcolor = "rgba(255,215,0,0.10)",
                  line = list(width = 0), name = "Linear 95% CI") %>%

      # EMA Projection
      add_lines(x = fc$future_dates, y = fc$ema_fc,
                name = "EMA Projection", line = list(color = "#00bcd4", width = 2, dash = "dot")) %>%

      # Support / Resistance horizontal lines
      add_lines(x = c(min(df_show$Date), tail(fc$future_dates, 1)),
                y = rep(fc$resistance_levels[3], 2),
                name = "Strong Resistance", line = list(color = "#ff5252", width = 1.5, dash = "longdash")) %>%
      add_lines(x = c(min(df_show$Date), tail(fc$future_dates, 1)),
                y = rep(fc$resistance_levels[1], 2),
                name = "Resistance Zone", line = list(color = "#ff8a65", width = 1, dash = "dash")) %>%
      add_lines(x = c(min(df_show$Date), tail(fc$future_dates, 1)),
                y = rep(fc$support_levels[1], 2),
                name = "Strong Support", line = list(color = "#69f0ae", width = 1.5, dash = "longdash")) %>%
      add_lines(x = c(min(df_show$Date), tail(fc$future_dates, 1)),
                y = rep(fc$support_levels[3], 2),
                name = "Support Zone", line = list(color = "#a5d6a7", width = 1, dash = "dash")) %>%

      layout(
        paper_bgcolor = "#0f3460", plot_bgcolor  = "#0f3460",
        font   = list(color = "#eee"),
        xaxis  = list(gridcolor = "#1e4a8a", title = "",
                      rangeslider = list(visible = FALSE),
                      shapes = list(list(type = "line",
                                         x0 = fc$last_date, x1 = fc$last_date,
                                         y0 = 0, y1 = 1, yref = "paper",
                                         line = list(color = "#ffd700", width = 2, dash = "dot")))),
        yaxis  = list(gridcolor = "#1e4a8a", title = "Price (INR)"),
        legend = list(bgcolor = "#0f3460", font = list(color = "#eee")),
        title  = list(text = paste0(nifty100_stocks[input$ticker],
                                    " — ", input$forecast_horizon, "-Day Forecast"),
                      font = list(color = "#eee", size = 15))
      )
    p
  })

  # ---- Monte Carlo Distribution ---- #
  output$mc_dist_chart <- renderPlotly({
    fc <- forecast_result(); req(fc)
    final_prices <- fc$mc_paths[input$forecast_horizon, ]
    plot_ly(x = final_prices, type = "histogram",
            marker = list(color = "#e94560", line = list(color = "#0f3460", width = 0.5)),
            nbinsx = 50, name = "Simulated Prices") %>%
      add_lines(x = rep(fc$mc_median[input$forecast_horizon], 2),
                y = c(0, input$mc_sims / 10),
                line = list(color = "#ffd700", width = 2, dash = "dash"),
                name = "Median") %>%
      add_lines(x = rep(fc$last_price, 2),
                y = c(0, input$mc_sims / 10),
                line = list(color = "#00bcd4", width = 2),
                name = "Current Price") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font  = list(color = "#eee"),
             xaxis = list(gridcolor = "#1e4a8a", title = paste0("Price after ", input$forecast_horizon, " Days (Rs.)")),
             yaxis = list(gridcolor = "#1e4a8a", title = "Simulated Paths"),
             showlegend = TRUE, legend = list(bgcolor = "#0f3460"))
  })

  # ---- Support / Resistance Chart ---- #
  output$sr_chart <- renderPlotly({
    d  <- stock_data(); req(d)
    fc <- forecast_result(); req(fc)
    df <- ohlcv_df()
    hist_n  <- min(60, nrow(df))
    df_show <- tail(df, hist_n)

    p <- plot_ly(df_show, x = ~Date, y = ~Close, type = "scatter", mode = "lines",
                 line = list(color = "#e94560", width = 2), name = "Price") %>%
      add_ribbons(x = df_show$Date,
                  ymin = rep(fc$support_levels[1], hist_n),
                  ymax = rep(fc$support_levels[3], hist_n),
                  fillcolor = "rgba(105,240,174,0.18)",
                  line = list(width = 0), name = "Support Zone") %>%
      add_ribbons(x = df_show$Date,
                  ymin = rep(fc$resistance_levels[1], hist_n),
                  ymax = rep(fc$resistance_levels[3], hist_n),
                  fillcolor = "rgba(255,82,82,0.18)",
                  line = list(width = 0), name = "Resistance Zone") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font  = list(color = "#eee"),
             xaxis = list(gridcolor = "#1e4a8a", title = ""),
             yaxis = list(gridcolor = "#1e4a8a", title = "Price (INR)"),
             legend = list(bgcolor = "#0f3460"))
    p
  })

  # ---- Scenario Table ---- #
  output$scenario_table <- renderTable({
    fc <- forecast_result(); req(fc)
    fc$scenarios
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE, align = "lccc")

  # ---- Signal Cards ---- #
  output$signal_cards <- renderUI({
    fc <- forecast_result(); req(fc)
    sig <- fc$signals
    cards <- lapply(seq_len(nrow(sig)), function(i) {
      s <- sig$Signal[i]
      cls <- if (grepl("BUY", s)) "signal-buy" else if (grepl("SELL", s)) "signal-sell" else "signal-hold"
      icon_name <- if (grepl("BUY", s)) "circle-up" else if (grepl("SELL", s)) "circle-down" else "circle-minus"
      tags$div(
        class = "col-md-3",
        tags$div(
          class = "scenario-box",
          tags$h4(style = "color:#eee;", sig$Indicator[i]),
          tags$h3(class = cls, s),
          tags$p(style = "color:#aaa; font-size:13px;",
                 paste("Value:", sig$Value[i]))
        )
      )
    })
    do.call(fluidRow, cards)
  })

  # ---- RSI Gauge ---- #
  output$rsi_gauge <- renderPlotly({
    ind <- indicators(); req(ind)
    rsi_val <- round(tail(as.numeric(ind$rsi14), 1), 1)
    plot_ly(
      type = "indicator", mode = "gauge+number",
      value = rsi_val,
      title = list(text = "RSI (14)", font = list(color = "#eee")),
      gauge = list(
        axis  = list(range = list(0, 100), tickcolor = "#eee",
                     tickfont = list(color = "#eee")),
        bar   = list(color = "#e94560"),
        steps = list(
          list(range = c(0,  30), color = "rgba(105,240,174,0.3)"),
          list(range = c(30, 70), color = "rgba(255,215,0,0.15)"),
          list(range = c(70,100), color = "rgba(255,82,82,0.3)")
        ),
        threshold = list(
          line  = list(color = "#ffd700", width = 4),
          thickness = 0.75,
          value = rsi_val
        )
      ),
      number = list(font = list(color = "#eee", size = 40))
    ) %>% layout(
      paper_bgcolor = "#0f3460",
      font = list(color = "#eee"),
      margin = list(t = 60, b = 20, l = 30, r = 30)
    )
  })

  # ---- Overall Recommendation ---- #
  output$overall_signal <- renderUI({
    fc <- forecast_result(); req(fc)
    sig <- fc$signals$Signal
    buys  <- sum(grepl("BUY",  sig))
    sells <- sum(grepl("SELL", sig))
    holds <- sum(grepl("HOLD", sig))

    if (buys >= 3) {
      rec  <- "STRONG BUY"
      col  <- "#00e676"
      icon_s <- "🚀"
    } else if (buys >= 2) {
      rec  <- "BUY"
      col  <- "#69f0ae"
      icon_s <- "📈"
    } else if (sells >= 3) {
      rec  <- "STRONG SELL"
      col  <- "#ff1744"
      icon_s <- "📉"
    } else if (sells >= 2) {
      rec  <- "SELL"
      col  <- "#ff5252"
      icon_s <- "⬇"
    } else {
      rec  <- "HOLD / NEUTRAL"
      col  <- "#ffd700"
      icon_s <- "⚖️"
    }

    tags$div(
      style = "text-align:center; padding: 20px;",
      tags$h1(icon_s, style = "font-size: 60px;"),
      tags$h2(rec, style = paste0("color:", col, "; font-weight: bold;")),
      tags$p(paste0("Based on ", buys, " BUY | ", holds, " HOLD | ", sells, " SELL signals"),
             style = "color: #aaa;"),
      tags$p("from RSI, MACD, SMA Cross, and Bollinger Bands",
             style = "color: #777; font-size:12px;")
    )
  })

  # ---- Signal Table ---- #
  output$signal_table <- renderTable({
    fc <- forecast_result(); req(fc)
    fc$signals
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)

  # ---- Volume Chart ---- #
  output$vol_chart <- renderPlotly({
    df  <- ohlcv_df(); req(df)
    col <- ifelse(df$Close >= df$Open, "#00e676", "#ff1744")
    plot_ly(df, x = ~Date, y = ~Volume, type = "bar",
            marker = list(color = col), name = "Volume") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font  = list(color = "#eee"),
             xaxis = list(gridcolor = "#1e4a8a", title = ""),
             yaxis = list(gridcolor = "#1e4a8a", title = "Volume", tickformat = ".2s"),
             showlegend = FALSE)
  })

  # ---- RSI Chart ---- #
  output$rsi_chart <- renderPlotly({
    ind   <- indicators(); req(ind)
    dates <- index(stock_data())
    rsi   <- as.numeric(ind$rsi14)
    plot_ly(x = dates, y = rsi, type = "scatter", mode = "lines",
            line = list(color = "#e94560"), name = "RSI 14") %>%
      add_lines(x = dates, y = rep(70, length(dates)),
                line = list(color = "#ff5252", dash = "dash"), name = "OB 70") %>%
      add_lines(x = dates, y = rep(30, length(dates)),
                line = list(color = "#69f0ae", dash = "dash"), name = "OS 30") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font  = list(color = "#eee"),
             xaxis = list(gridcolor = "#1e4a8a", title = ""),
             yaxis = list(gridcolor = "#1e4a8a", title = "RSI", range = c(0, 100)),
             showlegend = FALSE)
  })

  # ---- MACD Chart ---- #
  output$macd_chart <- renderPlotly({
    ind       <- indicators(); req(ind)
    dates     <- index(stock_data())
    macd_line <- as.numeric(ind$macd[, "macd"])
    sig_line  <- as.numeric(ind$macd[, "signal"])
    hist_vals <- macd_line - sig_line
    col_hist  <- ifelse(!is.na(hist_vals) & hist_vals >= 0, "#00e676", "#ff1744")
    plot_ly() %>%
      add_bars(x = dates, y = hist_vals, marker = list(color = col_hist), name = "Histogram") %>%
      add_lines(x = dates, y = macd_line, line = list(color = "#ffd700"), name = "MACD") %>%
      add_lines(x = dates, y = sig_line,  line = list(color = "#ff9800", dash = "dot"), name = "Signal") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font      = list(color = "#eee"),
             xaxis     = list(gridcolor = "#1e4a8a", title = ""),
             yaxis     = list(gridcolor = "#1e4a8a", title = "MACD"),
             barmode   = "overlay", showlegend = FALSE)
  })

  # ---- Cumulative Return ---- #
  output$cum_ret_plot <- renderPlotly({
    d <- stock_data(); req(d)
    cl    <- as.numeric(Cl(d)); dates <- index(d)
    cum_r <- (cl / cl[1] - 1) * 100
    plot_ly(x = dates, y = cum_r, type = "scatter", mode = "lines",
            fill = "tozeroy", fillcolor = "rgba(233,69,96,0.15)",
            line = list(color = "#e94560"), name = "Cumulative Return") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font  = list(color = "#eee"),
             xaxis = list(gridcolor = "#1e4a8a", title = ""),
             yaxis = list(gridcolor = "#1e4a8a", title = "Return (%)"),
             showlegend = FALSE)
  })

  # ---- Return Distribution ---- #
  output$ret_dist_plot <- renderPlotly({
    d   <- stock_data(); req(d)
    ret <- diff(log(as.numeric(Cl(d)))) * 100
    ret <- ret[!is.na(ret)]
    plot_ly(x = ret, type = "histogram",
            marker = list(color = "#0f3460", line = list(color = "#e94560", width = 1)),
            nbinsx = 50, name = "Daily Returns") %>%
      layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
             font  = list(color = "#eee"),
             xaxis = list(gridcolor = "#1e4a8a", title = "Daily Return (%)"),
             yaxis = list(gridcolor = "#1e4a8a", title = "Frequency"),
             showlegend = FALSE)
  })

  # ---- Performance Stats ---- #
  output$perf_table <- renderTable({
    d <- stock_data(); req(d)
    cl  <- as.numeric(Cl(d)); ret <- diff(log(cl)); ret <- ret[!is.na(ret)]
    ann_ret  <- mean(ret) * 252 * 100
    ann_sd   <- sd(ret) * sqrt(252) * 100
    sharpe   <- ann_ret / ann_sd
    max_dd   <- as.numeric(maxDrawdown(xts(ret, order.by = index(d)[-1]))) * 100
    skew_val <- (sum((ret - mean(ret))^3) / length(ret)) / sd(ret)^3
    kurt_val <- (sum((ret - mean(ret))^4) / length(ret)) / sd(ret)^4 - 3
    win_rate <- mean(ret > 0) * 100
    data.frame(
      Metric = c("Annualised Return (%)", "Annualised Volatility (%)",
                 "Sharpe Ratio (rf=0)", "Max Drawdown (%)",
                 "Skewness", "Excess Kurtosis", "Win Rate (%)"),
      Value  = round(c(ann_ret, ann_sd, sharpe, max_dd, skew_val, kurt_val, win_rate), 3)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE, align = "lr")

  # ---- Data Table ---- #
  output$data_tbl <- renderDT({
    df <- ohlcv_df(); req(df)
    df$Date   <- as.character(df$Date)
    df$Open   <- round(df$Open,  2)
    df$High   <- round(df$High,  2)
    df$Low    <- round(df$Low,   2)
    df$Close  <- round(df$Close, 2)
    df$Volume <- format(df$Volume, big.mark = ",", scientific = FALSE)
    datatable(df[order(nrow(df):1), ],
              options = list(pageLength = 20, scrollX = TRUE, dom = "Bfrtip",
                             initComplete = JS("function(settings,json){",
                               "$(this.api().table().header()).css({'background-color':'#16213e','color':'#e94560'});",
                               "}")),
              rownames = FALSE)
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0(input$ticker, "_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(ohlcv_df(), file, row.names = FALSE)
  )

  # ---- Comparison Plot ---- #
  cmp_data <- eventReactive(input$cmp_btn, {
    req(input$cmp_tickers, input$cmp_dates)
    tickers <- head(input$cmp_tickers, 5)
    withProgress(message = "Fetching comparison data ...", {
      lapply(tickers, function(t) {
        d <- fetch_stock_data(t, input$cmp_dates[1], input$cmp_dates[2])
        if (is.null(d)) return(NULL)
        cl  <- as.numeric(Cl(d)); idx <- (cl / cl[1]) * 100
        data.frame(Date = index(d), Indexed = idx, Ticker = t)
      }) %>% bind_rows()
    })
  })

  output$cmp_plot <- renderPlotly({
    df <- cmp_data(); req(df, nrow(df) > 0)
    palette <- c("#e94560", "#ffd700", "#00bcd4", "#69f0ae", "#ff9800")
    tickers <- unique(df$Ticker)
    p <- plot_ly()
    for (i in seq_along(tickers)) {
      sub <- df[df$Ticker == tickers[i], ]
      p   <- p %>% add_lines(data = sub, x = ~Date, y = ~Indexed,
                              name = tickers[i],
                              line = list(color = palette[i], width = 2))
    }
    p %>% layout(paper_bgcolor = "#0f3460", plot_bgcolor = "#0f3460",
                 font  = list(color = "#eee"),
                 xaxis = list(gridcolor = "#1e4a8a", title = ""),
                 yaxis = list(gridcolor = "#1e4a8a", title = "Indexed Price (Base = 100)"),
                 legend = list(bgcolor = "#0f3460"))
  })
}

# --------------------------------------------------------------------------- #
#  STEP 6 - LAUNCH                                                            #
# --------------------------------------------------------------------------- #
shinyApp(ui = ui, server = server)
