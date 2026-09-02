# ============================================================
# 04_sarima.R
# ARIMA Baseline and SARIMA Modelling
#
# PURPOSE
#   1. Load the common training and testing datasets created
#      by 02_time_series_eda.R.
#   2. Use ACF/PACF of the stationary training series to
#      support ARIMA order identification.
#   3. Compare simple ARIMA candidates using AICc.
#   4. Check whether drift improves the selected ARIMA model.
#   5. Diagnose ARIMA residuals for remaining dependence,
#      particularly at seasonal lags.
#   6. Extend the ARIMA baseline to SARIMA where annual
#      seasonal structure remains.
#   7. Compare SARIMA candidates using AICc.
#   8. Diagnose the selected SARIMA model.
#   9. Forecast the common 12-month test period.
#  10. Compare ARIMA and SARIMA out-of-sample accuracy.
#
# IMPORTANT
#   - The train-test split is NOT recreated here.
#   - Stationarity testing is NOT repeated here.
#   - Ordinary differencing order d = 1 was established
#     during preprocessing in 02_time_series_eda.R.
#   - Model identification and fitting use TRAINING DATA ONLY.
#   - The TEST set is used only for final forecast evaluation.
# ============================================================


# ============================================================
# 0. REQUIRED PACKAGES
# ============================================================

if (!requireNamespace("forecast", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'forecast' is required.\n",
      "Install it once using: install.packages('forecast')"
    )
  )
}


# ============================================================
# 1. DEFINE FILE PATHS
# ============================================================

train_path <-
  "data/processed/electricity_train_2018_2023.csv"

test_path <-
  "data/processed/electricity_test_2023_2024.csv"

stationarity_path <-
  "outputs/tables/stationarity_test_results.csv"


# ============================================================
# 2. CREATE OUTPUT DIRECTORIES
# ============================================================

dir.create(
  "outputs/sarima/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/sarima/tables",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. LOAD COMMON TRAINING AND TESTING DATA
#
# These files are created ONCE in 02_time_series_eda.R
# and are shared by all forecasting models.
# ============================================================

if (!file.exists(train_path)) {
  stop(
    paste0(
      "Training data not found: ",
      train_path,
      "\nRun scripts/02_time_series_eda.R first."
    )
  )
}

if (!file.exists(test_path)) {
  stop(
    paste0(
      "Testing data not found: ",
      test_path,
      "\nRun scripts/02_time_series_eda.R first."
    )
  )
}

train_data <- read.csv(
  train_path,
  stringsAsFactors = FALSE
)

test_data <- read.csv(
  test_path,
  stringsAsFactors = FALSE
)

train_data$date <- as.Date(
  train_data$date
)

test_data$date <- as.Date(
  test_data$date
)

train_data$consumption <- as.numeric(
  train_data$consumption
)

test_data$consumption <- as.numeric(
  test_data$consumption
)


# ============================================================
# 4. VERIFY COMMON TRAIN-TEST SPLIT
# ============================================================

if (nrow(train_data) != 66) {
  stop(
    paste0(
      "Expected 66 training observations but found ",
      nrow(train_data),
      "."
    )
  )
}

if (nrow(test_data) != 12) {
  stop(
    paste0(
      "Expected 12 testing observations but found ",
      nrow(test_data),
      "."
    )
  )
}

if (
  min(train_data$date) != as.Date("2018-01-01") ||
  max(train_data$date) != as.Date("2023-06-01")
) {
  stop(
    "Training period does not match Jan 2018-Jun 2023."
  )
}

if (
  min(test_data$date) != as.Date("2023-07-01") ||
  max(test_data$date) != as.Date("2024-06-01")
) {
  stop(
    "Testing period does not match Jul 2023-Jun 2024."
  )
}


# ============================================================
# 5. CREATE TIME-SERIES OBJECTS
# ============================================================

seasonal_period <- 12

train_ts <- ts(
  train_data$consumption,
  start = c(2018, 1),
  frequency = seasonal_period
)

test_ts <- ts(
  test_data$consumption,
  start = c(2023, 7),
  frequency = seasonal_period
)

stopifnot(length(train_ts) == 66)
stopifnot(length(test_ts) == 12)

cat("\n============================================\n")
cat("04 ARIMA + SARIMA MODELLING\n")
cat("============================================\n")

cat(
  "Training:",
  format(min(train_data$date), "%b %Y"),
  "to",
  format(max(train_data$date), "%b %Y"),
  "-",
  length(train_ts),
  "observations\n"
)

cat(
  "Testing :",
  format(min(test_data$date), "%b %Y"),
  "to",
  format(max(test_data$date), "%b %Y"),
  "-",
  length(test_ts),
  "observations\n"
)

cat(
  "Seasonal period:",
  seasonal_period,
  "months\n"
)


# ============================================================
# 6. CONFIRM PREPROCESSING EVIDENCE FROM SCRIPT 02
#
# Script 02 established that one ordinary difference is
# sufficient to achieve level stationarity.
#
# Therefore:
#   d = 1
#
# Stationarity tests are not repeated here.
# ============================================================

if (!file.exists(stationarity_path)) {
  warning(
    paste0(
      "Stationarity-results file was not found: ",
      stationarity_path,
      "\nProceeding with d = 1 based on the preprocessing ",
      "decision established in Script 02."
    )
  )
}

d_order <- 1

cat(
  "\nOrdinary differencing order inherited from Script 02: d =",
  d_order,
  "\n"
)


# ============================================================
# STAGE 1: ARIMA IDENTIFICATION
# ============================================================


# ============================================================
# 7. FIRST-DIFFERENCED TRAINING SERIES
#
# This transformation is reproduced here only for ARIMA
# order identification using ACF and PACF.
#
# The decision to use d = 1 itself was made in Script 02.
# ============================================================

train_diff1 <- diff(
  train_ts,
  differences = d_order
)

png(
  "outputs/sarima/figures/01_first_differenced_training.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  train_diff1,
  type = "l",
  main = "First-Differenced Training Series",
  xlab = "Year",
  ylab = "Change in Electricity Consumption"
)

abline(
  h = 0,
  lty = 2
)

dev.off()


# ============================================================
# 8. ACF AND PACF OF FIRST-DIFFERENCED TRAINING SERIES
#
# ACF:
#   Supports identification of MA order q.
#
# PACF:
#   Supports identification of AR order p.
#
# Seasonal lags 12 and 24 are also inspected because
# the observations are monthly.
# ============================================================

acf_diff1 <- acf(
  train_diff1,
  lag.max = 24,
  plot = FALSE
)

pacf_diff1 <- pacf(
  train_diff1,
  lag.max = 24,
  plot = FALSE
)


# ACF figure

png(
  "outputs/sarima/figures/02_acf_first_differenced_training.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  acf_diff1,
  main = "ACF of First-Differenced Training Series"
)

dev.off()


# PACF figure

png(
  "outputs/sarima/figures/03_pacf_first_differenced_training.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  pacf_diff1,
  main = "PACF of First-Differenced Training Series"
)

dev.off()


# Export ACF/PACF values.

acf_pacf_table <- data.frame(
  Lag = 1:24,

  ACF = as.numeric(
    acf_diff1$acf[2:25]
  ),

  PACF = as.numeric(
    pacf_diff1$acf[1:24]
  )
)

write.csv(
  acf_pacf_table,
  "outputs/sarima/tables/01_acf_pacf_values.csv",
  row.names = FALSE
)

cat("\nACF/PACF OF FIRST-DIFFERENCED TRAINING SERIES\n")
print(acf_pacf_table)


# ============================================================
# STAGE 2: ARIMA BASELINE MODELLING
# ============================================================


# ============================================================
# 9. DEFINE SIMPLE ARIMA CANDIDATES
#
# d = 1 is fixed from Script 02.
#
# The ACF/PACF pattern does not indicate one perfectly clear
# textbook structure, so simple low-order models are compared.
#
# Candidate models:
#   ARIMA(0,1,1)
#   ARIMA(1,1,0)
#   ARIMA(1,1,1)
# ============================================================

candidate_specs <- data.frame(
  Model = c(
    "ARIMA(0,1,1)",
    "ARIMA(1,1,0)",
    "ARIMA(1,1,1)"
  ),

  p = c(
    0,
    1,
    1
  ),

  d = c(
    d_order,
    d_order,
    d_order
  ),

  q = c(
    1,
    0,
    1
  )
)


# ============================================================
# 10. FIT NON-SEASONAL ARIMA CANDIDATES
#
# No seasonal ARIMA terms are introduced yet.
# Drift is initially excluded to compare core structures.
# ============================================================

arima_models <- vector(
  "list",
  nrow(candidate_specs)
)

names(arima_models) <-
  candidate_specs$Model


for (i in seq_len(nrow(candidate_specs))) {

  arima_models[[i]] <- forecast::Arima(
    train_ts,

    order = c(
      candidate_specs$p[i],
      candidate_specs$d[i],
      candidate_specs$q[i]
    ),

    seasonal = c(
      0,
      0,
      0
    ),

    include.drift = FALSE,

    method = "ML"
  )
}


# ============================================================
# 11. COMPARE ARIMA CANDIDATES
#
# Lower AICc indicates a better balance between model fit
# and complexity.
#
# AICc is used as the main criterion because the training
# sample contains only 66 observations.
# ============================================================

arima_comparison <- data.frame(
  Model = candidate_specs$Model,

  p = candidate_specs$p,

  d = candidate_specs$d,

  q = candidate_specs$q,

  AIC = sapply(
    arima_models,
    AIC
  ),

  AICc = sapply(
    arima_models,
    function(model) model$aicc
  ),

  BIC = sapply(
    arima_models,
    BIC
  )
)

arima_comparison <- arima_comparison[
  order(arima_comparison$AICc),
]

row.names(arima_comparison) <- NULL

arima_comparison$Delta_AICc <-
  arima_comparison$AICc -
  min(arima_comparison$AICc)

write.csv(
  arima_comparison,
  "outputs/sarima/tables/02_arima_candidate_comparison.csv",
  row.names = FALSE
)

cat("\nARIMA CANDIDATE COMPARISON\n")
print(arima_comparison)


# ============================================================
# 12. IDENTIFY BEST NON-DRIFT ARIMA STRUCTURE
# ============================================================

best_base_name <-
  arima_comparison$Model[1]

best_base_row <- candidate_specs[
  candidate_specs$Model == best_base_name,
]

best_base_model <-
  arima_models[[best_base_name]]

cat(
  "\nBest non-drift ARIMA structure:",
  best_base_name,
  "\n"
)


# ============================================================
# 13. DRIFT CHECK
#
# Compare the selected structure:
#   1. Without drift
#   2. With drift
#
# The lower AICc version is retained.
# ============================================================

best_drift_model <- forecast::Arima(
  train_ts,

  order = c(
    best_base_row$p,
    best_base_row$d,
    best_base_row$q
  ),

  seasonal = c(
    0,
    0,
    0
  ),

  include.drift = TRUE,

  method = "ML"
)


drift_comparison <- data.frame(
  Model = c(
    paste0(
      best_base_name,
      " without drift"
    ),

    paste0(
      best_base_name,
      " with drift"
    )
  ),

  Drift = c(
    FALSE,
    TRUE
  ),

  AIC = c(
    AIC(best_base_model),
    AIC(best_drift_model)
  ),

  AICc = c(
    best_base_model$aicc,
    best_drift_model$aicc
  ),

  BIC = c(
    BIC(best_base_model),
    BIC(best_drift_model)
  )
)

drift_comparison <- drift_comparison[
  order(drift_comparison$AICc),
]

row.names(drift_comparison) <- NULL

write.csv(
  drift_comparison,
  "outputs/sarima/tables/03_arima_drift_comparison.csv",
  row.names = FALSE
)

cat("\nARIMA DRIFT COMPARISON\n")
print(drift_comparison)


# ============================================================
# 14. SELECT FINAL ARIMA BASELINE
# ============================================================

if (
  best_drift_model$aicc <
    best_base_model$aicc
) {

  final_arima <- best_drift_model

  final_arima_drift <- TRUE

} else {

  final_arima <- best_base_model

  final_arima_drift <- FALSE
}

final_arima_name <- paste0(
  best_base_name,

  ifelse(
    final_arima_drift,
    " + drift",
    ""
  )
)

cat(
  "\nSelected ARIMA baseline:",
  final_arima_name,
  "\n"
)


# ============================================================
# 15. EXPORT FINAL ARIMA COEFFICIENTS
# ============================================================

arima_coefficient_table <- data.frame(
  Term = names(
    coef(final_arima)
  ),

  Estimate = as.numeric(
    coef(final_arima)
  )
)

write.csv(
  arima_coefficient_table,
  "outputs/sarima/tables/04_final_arima_coefficients.csv",
  row.names = FALSE
)

cat("\nFINAL ARIMA COEFFICIENTS\n")
print(arima_coefficient_table)


# ============================================================
# STAGE 3: ARIMA RESIDUAL DIAGNOSTICS
# ============================================================


# ============================================================
# 16. ARIMA RESIDUAL TIME PLOT
# ============================================================

png(
  "outputs/sarima/figures/04_arima_residuals.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  residuals(final_arima),
  type = "l",

  main = paste(
    "Residuals of",
    final_arima_name
  ),

  xlab = "Year",
  ylab = "Residual"
)

abline(
  h = 0,
  lty = 2
)

dev.off()


# ============================================================
# 17. ARIMA RESIDUAL ACF
#
# Remaining autocorrelation, particularly around lag 12,
# suggests that annual seasonal dependence may not have
# been fully captured by the non-seasonal ARIMA model.
# ============================================================

arima_residual_acf <- acf(
  residuals(final_arima),
  lag.max = 24,
  plot = FALSE
)

png(
  "outputs/sarima/figures/05_arima_residual_acf.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  arima_residual_acf,

  main = paste(
    "Residual ACF of",
    final_arima_name
  )
)

dev.off()

# ============================================================
# 18. ARIMA RESIDUAL DISTRIBUTION
#
# The histogram is used to check whether residuals are
# reasonably centred around zero without strong skewness.
# ============================================================

arima_residuals <- residuals(final_arima)

png(
  "outputs/sarima/figures/06_arima_residual_histogram.png",
  width = 1200,
  height = 800,
  res = 150
)

hist(
  arima_residuals,
  main = paste(
    "Residual Distribution of",
    final_arima_name
  ),
  xlab = "Residual",
  breaks = "Sturges"
)

abline(
  v = 0,
  lty = 2
)

dev.off()

# ============================================================
# 19. ARIMA LJUNG-BOX TEST
#
# H0:
#   Residual autocorrelations are jointly zero.
#
# p > 0.05:
#   No strong evidence of remaining autocorrelation.
#
# p < 0.05:
#   Significant dependence remains.
# ============================================================

final_arima_p <-
  best_base_row$p

final_arima_q <-
  best_base_row$q

arima_ljung_box <- Box.test(
  residuals(final_arima),

  lag = 12,

  type = "Ljung-Box",

  fitdf =
    final_arima_p +
    final_arima_q
)

arima_residual_diagnostics <- data.frame(
  Model =
    final_arima_name,

  Ljung_Box_Lag =
    12,

  Statistic =
    unname(
      arima_ljung_box$statistic
    ),

  DF =
    unname(
      arima_ljung_box$parameter
    ),

  P_Value =
    arima_ljung_box$p.value,

  Residual_ACF_Lag12 =
    as.numeric(
      arima_residual_acf$acf[13]
    )
)

write.csv(
  arima_residual_diagnostics,
  paste0(
    "outputs/sarima/tables/",
    "05_arima_residual_diagnostics.csv"
  ),
  row.names = FALSE
)

cat("\nARIMA RESIDUAL DIAGNOSTICS\n")
print(arima_residual_diagnostics)


# ============================================================
# STAGE 4: SARIMA IDENTIFICATION
# ============================================================


# ============================================================
# 20. SEASONAL DIFFERENCING
#
# Monthly data have seasonal period m = 12.
#
# Strong autocorrelation remained at seasonal lags 12 and 24
# after ordinary first differencing.
#
# Therefore, one seasonal difference is investigated:
#
#   current month - same month one year earlier
#
# This corresponds to D = 1.
# ============================================================

seasonal_diff <- diff(
  train_ts,
  lag = 12,
  differences = 1
)


# ============================================================
# 21. PLOT SEASONALLY DIFFERENCED SERIES
# ============================================================

png(
  "outputs/sarima/figures/07_seasonal_difference_training.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  seasonal_diff,
  type = "l",
  main = "Seasonally Differenced Training Series",
  xlab = "Year",
  ylab = "Seasonal Difference"
)

abline(
  h = 0,
  lty = 2
)

dev.off()


# ============================================================
# 22. APPLY ORDINARY + SEASONAL DIFFERENCING
#
# Ordinary differencing:
#   d = 1
#
# Seasonal differencing being investigated:
#   D = 1
#
# The combined transformation is:
#   (1 - B)(1 - B^12)Yt
# ============================================================

train_diff1_seasonal <- diff(
  train_diff1,
  lag = 12,
  differences = 1
)


# ============================================================
# 23. PLOT ORDINARY + SEASONALLY DIFFERENCED SERIES
# ============================================================

png(
  "outputs/sarima/figures/08_first_and_seasonal_difference.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  train_diff1_seasonal,
  type = "l",
  main = "First and Seasonally Differenced Training Series",
  xlab = "Year",
  ylab = "Differenced Electricity Consumption"
)

abline(
  h = 0,
  lty = 2
)

dev.off()


# ============================================================
# 24. ACF AND PACF AFTER d = 1 AND D = 1
#
# These diagnostics are used to examine whether seasonal
# differencing improves the seasonal dependence structure.
# ============================================================

sarima_acf <- acf(
  train_diff1_seasonal,
  lag.max = 24,
  plot = FALSE
)

sarima_pacf <- pacf(
  train_diff1_seasonal,
  lag.max = 24,
  plot = FALSE
)

png(
  "outputs/sarima/figures/09_acf_first_seasonal_difference.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  sarima_acf,
  main = "ACF After First and Seasonal Differencing"
)

dev.off()

png(
  "outputs/sarima/figures/10_pacf_first_seasonal_difference.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  sarima_pacf,
  main = "PACF After First and Seasonal Differencing"
)

dev.off()

sarima_acf_pacf_table <- data.frame(
  Lag = 1:24,
  ACF = as.numeric(sarima_acf$acf[2:25]),
  PACF = as.numeric(sarima_pacf$acf[1:24])
)

write.csv(
  sarima_acf_pacf_table,
  "outputs/sarima/tables/06_acf_pacf_after_seasonal_difference.csv",
  row.names = FALSE
)

print(sarima_acf_pacf_table)

# ============================================================
# 25. SUMMARISE SEASONAL DIFFERENCING EVIDENCE
#
# After d = 1:
#   seasonal autocorrelation remains at lags 12 and 24.
#
# After additionally applying D = 1:
#   lag-24 autocorrelation is greatly reduced,
#   while a strong negative lag-12 correlation appears.
#
# Therefore, D is not selected from this plot alone.
# Both D = 0 and D = 1 are carried forward as small
# candidate branches for further investigation.
# ============================================================

seasonal_difference_comparison <- data.frame(
  Lag = c(12, 24),

  ACF_d1 = c(
    as.numeric(acf_diff1$acf[13]),
    as.numeric(acf_diff1$acf[25])
  ),

  ACF_d1_D1 = c(
    as.numeric(sarima_acf$acf[13]),
    as.numeric(sarima_acf$acf[25])
  ),

  PACF_d1 = c(
    as.numeric(pacf_diff1$acf[12]),
    as.numeric(pacf_diff1$acf[24])
  ),

  PACF_d1_D1 = c(
    as.numeric(sarima_pacf$acf[12]),
    as.numeric(sarima_pacf$acf[24])
  )
)

write.csv(
  seasonal_difference_comparison,
  "outputs/sarima/tables/07_seasonal_difference_comparison.csv",
  row.names = FALSE
)

print(seasonal_difference_comparison)

# ============================================================
# STAGE 5: SARIMA CANDIDATE IDENTIFICATION
# ============================================================


# ============================================================
# 26. D = 0 CANDIDATES
#
# These candidates model annual dependence using seasonal
# AR/MA terms without seasonal differencing.
#
# ARIMA(0,1,1) remains an important baseline structure,
# but nearby low-order alternatives are also allowed.
# ============================================================

sarima_D0_specs <- data.frame(
  Model = c(
    "SARIMA(0,1,1)(1,0,0)[12]",
    "SARIMA(0,1,1)(0,0,1)[12]",
    "SARIMA(0,1,1)(1,0,1)[12]",
    "SARIMA(1,1,0)(0,0,1)[12]",
    "SARIMA(1,1,1)(0,0,1)[12]",
    "SARIMA(0,1,2)(1,0,0)[12]",
    "SARIMA(0,1,2)(0,0,1)[12]"
  ),
  p = c(
    0, 0, 0, 1, 1, 0, 0
  ),

  d = rep(
    1,
    7
  ),

  q = c(
    1, 1, 1, 0, 1, 2, 2
  ),

  P = c(
    1, 0, 1, 0, 0, 1, 0
  ),

  D = rep(
    0,
    7
  ),

  Q = c(
    0, 1, 1, 1, 1, 0, 1
  )
)

# ============================================================
# 27. D = 1 CANDIDATES
#
# Seasonal differencing D = 1 is investigated with and
# without an additional seasonal MA term.
#
# Q = 0 candidates are included because the previous
# Q = 1 finalist produced an estimate extremely close to -1,
# suggesting possible redundancy after seasonal differencing.
# ============================================================

sarima_D1_specs <- data.frame(
  Model = c(
    "SARIMA(0,1,1)(0,1,0)[12]",
    "SARIMA(0,1,2)(0,1,0)[12]",
    "SARIMA(0,1,2)(1,1,0)[12]",
    "SARIMA(0,1,1)(0,1,1)[12]",
    "SARIMA(1,1,0)(0,1,1)[12]",
    "SARIMA(1,1,1)(0,1,1)[12]",
    "SARIMA(0,1,2)(0,1,1)[12]"
  ),

  p = c(
    0, 0, 0, 0, 1, 1, 0
  ),

  d = rep(
    1,
    7
  ),

  q = c(
    1, 2, 2, 1, 0, 1, 2
  ),

  P = c(
    0, 0, 1, 0, 0, 0, 0
  ),

  D = rep(
    1,
    7
  ),

  Q = c(
    0, 0, 0, 1, 1, 1, 1
  )
)

# ============================================================
# STAGE 6: FIT SARIMA CANDIDATES
# ============================================================


fit_sarima_candidates <- function(specs, series, period = 12) {

  models <- vector(
    "list",
    nrow(specs)
  )

  names(models) <- specs$Model


  for (i in seq_len(nrow(specs))) {

    models[[i]] <- forecast::Arima(
      series,

      order = c(
        specs$p[i],
        specs$d[i],
        specs$q[i]
      ),

      seasonal = list(
        order = c(
          specs$P[i],
          specs$D[i],
          specs$Q[i]
        ),

        period = period
      ),

      include.drift = FALSE,

      method = "ML"
    )
  }

  models
}


sarima_D0_models <- fit_sarima_candidates(
  sarima_D0_specs,
  train_ts,
  seasonal_period
)

sarima_D1_models <- fit_sarima_candidates(
  sarima_D1_specs,
  train_ts,
  seasonal_period
)

# ============================================================
# 28. COMPARE CANDIDATES WITHIN EACH D BRANCH
# ============================================================

compare_candidates <- function(specs, models) {

  results <- specs

  results$AIC <- sapply(
    models,
    AIC
  )

  results$AICc <- sapply(
    models,
    function(model) model$aicc
  )

  results <- results[
    order(results$AICc),
  ]

  row.names(results) <- NULL

  results$Delta_AICc <-
    results$AICc -
    min(results$AICc)

  results
}


sarima_D0_comparison <- compare_candidates(
  sarima_D0_specs,
  sarima_D0_models
)

sarima_D1_comparison <- compare_candidates(
  sarima_D1_specs,
  sarima_D1_models
)


write.csv(
  sarima_D0_comparison,
  "outputs/sarima/tables/08_D0_candidate_comparison.csv",
  row.names = FALSE
)

write.csv(
  sarima_D1_comparison,
  "outputs/sarima/tables/09_D1_candidate_comparison.csv",
  row.names = FALSE
)


cat("\nD = 0 SARIMA CANDIDATES\n")
print(sarima_D0_comparison)

cat("\nD = 1 SARIMA CANDIDATES\n")
print(sarima_D1_comparison)

# ============================================================
# 29. SELECT BEST CANDIDATE WITHIN EACH BRANCH
# ============================================================

best_D0_name <-
  sarima_D0_comparison$Model[1]

best_D1_name <-
  sarima_D1_comparison$Model[1]


best_D0_model <-
  sarima_D0_models[[best_D0_name]]

best_D1_model <-
  sarima_D1_models[[best_D1_name]]


best_D0_row <- sarima_D0_specs[
  sarima_D0_specs$Model == best_D0_name,
]

best_D1_row <- sarima_D1_specs[
  sarima_D1_specs$Model == best_D1_name,
]


cat(
  "\nBest D = 0 candidate:",
  best_D0_name,
  "\n"
)

cat(
  "Best D = 1 candidate:",
  best_D1_name,
  "\n"
)

# ============================================================
# 30. CHECK COEFFICIENT BOUNDARY BEHAVIOUR
#
# Estimates very close to +/-1 are flagged for closer
# inspection because they may indicate unstable,
# near-boundary or redundant model structure.
#
# The 0.98 threshold is used only as a practical warning flag,
# not as a formal statistical test.
# ============================================================

coefficient_check <- function(model, model_name) {

  estimates <- coef(model)

  data.frame(
    Model = model_name,
    Term = names(estimates),
    Estimate = as.numeric(estimates),
    Near_Boundary = abs(
      as.numeric(estimates)
    ) > 0.98
  )
}


D0_coefficients <- coefficient_check(
  best_D0_model,
  best_D0_name
)

D1_coefficients <- coefficient_check(
  best_D1_model,
  best_D1_name
)

finalist_coefficients <- rbind(
  D0_coefficients,
  D1_coefficients
)

write.csv(
  finalist_coefficients,
  "outputs/sarima/tables/10_sarima_finalist_coefficients.csv",
  row.names = FALSE
)

print(finalist_coefficients)

# ============================================================
# STAGE 10: FINALIST RESIDUAL DIAGNOSTICS
# ============================================================


diagnose_sarima <- function(
    model,
    model_name,
    p,
    q,
    P,
    Q
) {

  residual_values <- residuals(model)

  residual_acf <- acf(
    residual_values,
    lag.max = 24,
    plot = FALSE
  )

  ljung <- Box.test(
    residual_values,
    lag = 24,
    type = "Ljung-Box",
    fitdf = p + q + P + Q
  )

  data.frame(
    Model = model_name,

    Ljung_Box_Lag = 24,

    Statistic =
      unname(ljung$statistic),

    DF =
      unname(ljung$parameter),

    P_Value =
      ljung$p.value,

    Residual_ACF_Lag12 =
      as.numeric(
        residual_acf$acf[13]
      )
  )
}


D0_diagnostics <- diagnose_sarima(
  best_D0_model,
  best_D0_name,
  best_D0_row$p,
  best_D0_row$q,
  best_D0_row$P,
  best_D0_row$Q
)

D1_diagnostics <- diagnose_sarima(
  best_D1_model,
  best_D1_name,
  best_D1_row$p,
  best_D1_row$q,
  best_D1_row$P,
  best_D1_row$Q
)


finalist_diagnostics <- rbind(
  D0_diagnostics,
  D1_diagnostics
)


write.csv(
  finalist_diagnostics,
  "outputs/sarima/tables/11_sarima_finalist_diagnostics.csv",
  row.names = FALSE
)

print(finalist_diagnostics)

# ============================================================
# 31. FINALIST RESIDUAL PLOTS
# ============================================================

plot_model_diagnostics <- function(
    model,
    model_name,
    file_prefix
) {

  model_residuals <- residuals(model)


  # Residual time plot

  png(
    paste0(
      "outputs/sarima/figures/",
      file_prefix,
      "_residuals.png"
    ),
    width = 1200,
    height = 800,
    res = 150
  )

  plot(
    model_residuals,
    type = "l",
    main = paste(
      "Residuals of",
      model_name
    ),
    xlab = "Year",
    ylab = "Residual"
  )

  abline(
    h = 0,
    lty = 2
  )

  dev.off()


  # Residual ACF

  png(
    paste0(
      "outputs/sarima/figures/",
      file_prefix,
      "_residual_acf.png"
    ),
    width = 1200,
    height = 800,
    res = 150
  )

  acf(
    model_residuals,
    lag.max = 24,
    main = paste(
      "Residual ACF of",
      model_name
    )
  )

  dev.off()


  # Residual histogram

  png(
    paste0(
      "outputs/sarima/figures/",
      file_prefix,
      "_residual_histogram.png"
    ),
    width = 1200,
    height = 800,
    res = 150
  )

  hist(
    model_residuals,
    main = paste(
      "Residual Distribution of",
      model_name
    ),
    xlab = "Residual",
    breaks = "Sturges"
  )

  abline(
    v = 0,
    lty = 2
  )

  dev.off()
}


plot_model_diagnostics(
  best_D0_model,
  best_D0_name,
  "11_D0_finalist"
)

plot_model_diagnostics(
  best_D1_model,
  best_D1_name,
  "12_D1_finalist"
)

# ============================================================
# STAGE 11: OUT-OF-SAMPLE FORECAST EVALUATION
#
# Model identification is now complete.
#
# Baseline:
#   ARIMA(0,1,1)
#
# Final seasonal model:
#   SARIMA(0,1,1)(1,0,1)[12]
#
# Both models are evaluated on the SAME untouched test set.
# No further model tuning is performed after viewing the
# test results.
# ============================================================


# ============================================================
# 32. LOCK FINAL MODELS
# ============================================================

final_arima_model <- final_arima

final_sarima_model <- best_D0_model

final_sarima_name <- best_D0_name


cat(
  "\nFinal ARIMA:",
  final_arima_name,
  "\n"
)

cat(
  "Final SARIMA:",
  final_sarima_name,
  "\n"
)


# ============================================================
# 33. FORECAST THE TEST PERIOD
#
# Forecast horizon is determined directly from the test set.
# ============================================================

forecast_horizon <- length(test_ts)

arima_forecast <- forecast(
  final_arima_model,
  h = forecast_horizon
)

sarima_forecast <- forecast(
  final_sarima_model,
  h = forecast_horizon
)


# ============================================================
# 34. CALCULATE FORECAST ACCURACY
#
# MAE:
#   average absolute forecast error
#
# RMSE:
#   penalises larger forecast errors more heavily
#
# MAPE:
#   average percentage forecast error
#
# Lower values indicate better forecast performance.
# ============================================================

calculate_accuracy <- function(
    actual,
    predicted,
    model_name
) {

  errors <- actual - predicted

  data.frame(
    Model = model_name,

    MAE = mean(
      abs(errors)
    ),

    RMSE = sqrt(
      mean(errors^2)
    ),

    MAPE = mean(
      abs(errors / actual)
    ) * 100
  )
}


arima_accuracy <- calculate_accuracy(
  as.numeric(test_ts),
  as.numeric(arima_forecast$mean),
  final_arima_name
)

sarima_accuracy <- calculate_accuracy(
  as.numeric(test_ts),
  as.numeric(sarima_forecast$mean),
  final_sarima_name
)


forecast_accuracy <- rbind(
  arima_accuracy,
  sarima_accuracy
)

write.csv(
  forecast_accuracy,
  "outputs/sarima/tables/12_forecast_accuracy_comparison.csv",
  row.names = FALSE
)

print(forecast_accuracy)