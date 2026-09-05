# ============================================================
# 05_stl_ets.R
# STL Decomposition + ETS Forecasting
# Malaysia Monthly Electricity Consumption
#
# PURPOSE
#   1. Load the COMMON training and testing datasets prepared by
#      the repository data-setup script.
#   2. Inspect the training series using robust periodic STL.
#   3. Select the non-seasonal ETS structure using rolling-origin
#      validation conducted ONLY inside the training set.
#   4. Fit the selected STL-ETS model on the full training series.
#   5. Forecast and evaluate the untouched 12-month test set.
#   6. Diagnose residuals using ACF and the Ljung-Box test.
#   7. Refit the selected specification to all 78 observations
#      and forecast Jul 2024-Jun 2025.
#
# IMPORTANT
#   - This script does NOT use file.choose().
#   - The train-test split is NOT recreated here.
#   - Model selection uses TRAINING DATA ONLY.
#   - The TEST set is used only for final out-of-sample evaluation.
#   - STL handles the seasonal pattern, so the ETS component is
#     non-seasonal and no stationarity differencing is required.
# ============================================================

if (!requireNamespace("forecast", quietly = TRUE)) {
  stop("Package 'forecast' is required. Install with install.packages('forecast')")
}

library(forecast)
options(stringsAsFactors = FALSE)

# ============================================================
# 1. Repository paths and output folders
# ============================================================
# Run this script from the ROOT of the GitHub repository.
train_path <- "data/processed/electricity_train_2018_2023.csv"
test_path  <- "data/processed/electricity_test_2023_2024.csv"

figure_dir <- "outputs/stl_ets/figures"
table_dir  <- "outputs/stl_ets/tables"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(train_path)) {
  stop("Training file not found: ", train_path,
       "\nRun the repository data-preparation script first.")
}
if (!file.exists(test_path)) {
  stop("Testing file not found: ", test_path,
       "\nRun the repository data-preparation script first.")
}

# ============================================================
# 2. Load the common processed datasets
# ============================================================
train_data <- read.csv(train_path, stringsAsFactors = FALSE)
test_data  <- read.csv(test_path, stringsAsFactors = FALSE)

required_columns <- c("date", "consumption")
if (!all(required_columns %in% names(train_data)) ||
    !all(required_columns %in% names(test_data))) {
  stop("Both processed files must contain 'date' and 'consumption' columns.")
}

train_data$date <- as.Date(train_data$date)
test_data$date  <- as.Date(test_data$date)
train_data$consumption <- as.numeric(train_data$consumption)
test_data$consumption  <- as.numeric(test_data$consumption)

if (anyNA(train_data$date) || anyNA(test_data$date) ||
    any(!is.finite(train_data$consumption)) ||
    any(!is.finite(test_data$consumption))) {
  stop("Processed train/test data contain invalid dates or consumption values.")
}

train_data <- train_data[order(train_data$date), ]
test_data  <- test_data[order(test_data$date), ]
row.names(train_data) <- NULL
row.names(test_data) <- NULL

stopifnot(nrow(train_data) == 66)
stopifnot(nrow(test_data) == 12)
stopifnot(min(train_data$date) == as.Date("2018-01-01"))
stopifnot(max(train_data$date) == as.Date("2023-06-01"))
stopifnot(min(test_data$date) == as.Date("2023-07-01"))
stopifnot(max(test_data$date) == as.Date("2024-06-01"))

# ============================================================
# 3. Create monthly time-series objects
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

# Combine the common train and test files ONLY for the final refit
# after the untouched test-set evaluation has been completed.
total_data <- rbind(train_data, test_data)
total_data <- total_data[order(total_data$date), ]
row.names(total_data) <- NULL

electricity_ts <- ts(
  total_data$consumption,
  start = c(2018, 1),
  frequency = seasonal_period
)

stopifnot(length(electricity_ts) == 78)

cat("\n============================================\n")
cat("STL-ETS MODELLING\n")
cat("============================================\n")
cat("Training: Jan 2018 to Jun 2023 -", length(train_ts), "observations\n")
cat("Testing : Jul 2023 to Jun 2024 -", length(test_ts), "observations\n")
cat("Seasonal period:", seasonal_period, "months\n")

# ============================================================
# 4. Original series plot (repository output)
# ============================================================
png(
  file.path(figure_dir, "01_original_series.png"),
  width = 1200, height = 800, res = 150
)
plot(
  electricity_ts,
  type = "l",
  lwd = 2,
  main = "Malaysia Monthly Electricity Consumption",
  xlab = "Year",
  ylab = "Electricity Consumption"
)
grid()
dev.off()

# ============================================================
# STEP 10 - OUTLIER INSPECTION (TRAINING DATA ONLY)
# ============================================================
# This is an inspection step, NOT an automatic deletion rule.
# Valid historical observations are retained. robust STL reduces
# their influence on decomposition if they are unusual.

q1 <- as.numeric(quantile(train_ts, 0.25, type = 7))
q3 <- as.numeric(quantile(train_ts, 0.75, type = 7))
iqr_value <- q3 - q1
lower_iqr <- q1 - 1.5 * iqr_value
upper_iqr <- q3 + 1.5 * iqr_value
outlier_index <- which(train_ts < lower_iqr | train_ts > upper_iqr)

cat("\n============================================================\n")
cat("TRAINING-SAMPLE IQR OUTLIER INSPECTION\n")
cat("============================================================\n")
cat("Lower IQR bound:", round(lower_iqr, 2), "\n")
cat("Upper IQR bound:", round(upper_iqr, 2), "\n")

if (length(outlier_index) == 0) {
  cat("No observations were flagged by the 1.5 x IQR rule.\n")
} else {
  train_dates <- seq(
    as.Date("2018-01-01"),
    by = "month",
    length.out = length(train_ts)
  )
  outlier_table <- data.frame(
    Month = format(train_dates[outlier_index], "%Y-%m"),
    Consumption = as.numeric(train_ts[outlier_index])
  )
  print(outlier_table, row.names = FALSE)
}

cat(
  "Decision: valid historical observations are retained; ",
  "robust STL is used rather than deleting them.\n",
  sep = ""
)


# ============================================================
# STEP 11 - ROBUST PERIODIC STL DECOMPOSITION OF TRAINING DATA
# ============================================================
# Rationale:
# - frequency = 12 because the series is monthly.
# - s.window = "periodic" estimates a stable recurring annual
#   month-of-year seasonal pattern.
# - robust = TRUE reduces the influence of unusual observations.
# - no Box-Cox/log transform is imposed because the group EDA
#   found no strong proportional increase in variance with level.

stl_decomposition <- stl(
  train_ts,
  s.window = "periodic",
  robust = TRUE
)

cat("\n============================================================\n")
cat("ROBUST PERIODIC STL DECOMPOSITION\n")
cat("============================================================\n")
print(stl_decomposition)

png(
  "outputs/stl_ets/figures/02_training_stl_decomposition.png",
  width = 1800,
  height = 1400,
  res = 180
)
plot(
  stl_decomposition,
  main = "Robust Periodic STL Decomposition - Training Data"
)
dev.off()

seasonal_component <- stl_decomposition$time.series[, "seasonal"]
trend_component <- stl_decomposition$time.series[, "trend"]
remainder_component <- stl_decomposition$time.series[, "remainder"]

# Quantitative strengths of STL components
seasonal_strength <- max(
  0,
  1 - var(remainder_component, na.rm = TRUE) /
    var(seasonal_component + remainder_component, na.rm = TRUE)
)

trend_strength <- max(
  0,
  1 - var(remainder_component, na.rm = TRUE) /
    var(trend_component + remainder_component, na.rm = TRUE)
)

cat("\nSeasonal strength:", round(seasonal_strength, 4), "\n")
cat("Trend strength   :", round(trend_strength, 4), "\n")


# ============================================================
# STEP 12 - EXPLAIN WHY NO STATIONARITY DIFFERENCING IS APPLIED
# ============================================================
cat("\n============================================================\n")
cat("STATIONARITY / DIFFERENCING NOTE\n")
cat("============================================================\n")
cat(
  "No differencing is applied before STL-ETS. STL explicitly ",
  "decomposes the seasonal structure, while ETS models the ",
  "seasonally adjusted level/trend. Stationarity differencing ",
  "is therefore not a prerequisite for this modelling approach.\n",
  sep = ""
)


# ============================================================
# STEP 13 - DEFINE STL-ETS CANDIDATE SPECIFICATIONS
# ============================================================
# STL already models seasonality, so the ETS component is always
# non-seasonal (third character = N).
#
# Candidate structures are intentionally limited and interpretable:
#   ANN        : additive error, no trend
#   AAN        : additive error, additive trend
#   AAN damped : additive error, damped additive trend
#   MAN        : multiplicative error, additive trend
#
# The candidate is selected ONLY using rolling-origin validation
# within the training set. This avoids test-set leakage.

candidate_specs <- data.frame(
  Candidate = c(
    "ETS(A,N,N)",
    "ETS(A,A,N)",
    "ETS(A,Ad,N)",
    "ETS(M,A,N)"
  ),
  ETSModel = c("ANN", "AAN", "AAN", "MAN"),
  Damped = c(FALSE, FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

print(candidate_specs, row.names = FALSE)


# ============================================================
# STEP 14 - HELPER FUNCTION TO FIT ONE STL-ETS CANDIDATE
# ============================================================
fit_stl_ets_candidate <- function(y, ets_model, damped_value) {
  stlm(
    y,
    s.window = "periodic",
    robust = TRUE,
    method = "ets",
    etsmodel = ets_model,
    damped = damped_value,
    lambda = NULL,
    biasadj = FALSE,
    allow.multiplicative.trend = FALSE
  )
}


# ============================================================
# STEP 15 - ROLLING-ORIGIN VALIDATION INSIDE TRAINING DATA
# ============================================================
# The validation horizon is 12 months to match the final test
# horizon. The first origin uses 36 months of training history.
# Every origin then moves forward by one month until a complete
# 12-month validation horizon can no longer be formed.

cv_horizon <- 12
minimum_training_length <- 36
last_origin <- length(train_ts) - cv_horizon
origins <- minimum_training_length:last_origin

cv_results <- data.frame()

cat("\n============================================================\n")
cat("ROLLING-ORIGIN TRAINING-ONLY MODEL SELECTION\n")
cat("============================================================\n")
cat("Forecast horizon      :", cv_horizon, "months\n")
cat("Minimum training size :", minimum_training_length, "months\n")
cat("Number of origins     :", length(origins), "\n")

for (i in seq_len(nrow(candidate_specs))) {
  
  candidate_name <- candidate_specs$Candidate[i]
  ets_model_i <- candidate_specs$ETSModel[i]
  damped_i <- candidate_specs$Damped[i]
  
  all_errors <- numeric(0)
  all_apes <- numeric(0)
  successful_origins <- 0
  
  cat("\nEvaluating", candidate_name, "...\n")
  
  for (origin in origins) {
    
    origin_series <- ts(
      as.numeric(train_ts[1:origin]),
      start = start(train_ts),
      frequency = frequency(train_ts)
    )
    
    actual_values <- as.numeric(
      train_ts[(origin + 1):(origin + cv_horizon)]
    )
    
    fit_try <- try(
      fit_stl_ets_candidate(
        origin_series,
        ets_model_i,
        damped_i
      ),
      silent = TRUE
    )
    
    if (inherits(fit_try, "try-error")) {
      next
    }
    
    forecast_try <- try(
      forecast(fit_try, h = cv_horizon),
      silent = TRUE
    )
    
    if (inherits(forecast_try, "try-error")) {
      next
    }
    
    forecast_values <- as.numeric(forecast_try$mean)
    errors <- actual_values - forecast_values
    
    all_errors <- c(all_errors, errors)
    all_apes <- c(
      all_apes,
      abs(errors / actual_values) * 100
    )
    
    successful_origins <- successful_origins + 1
  }
  
  if (successful_origins == 0) {
    stop(
      "Candidate ", candidate_name,
      " failed at every rolling-origin evaluation."
    )
  }
  
  candidate_row <- data.frame(
    Candidate = candidate_name,
    Successful_Origins = successful_origins,
    CV_MAE = mean(abs(all_errors)),
    CV_RMSE = sqrt(mean(all_errors^2)),
    CV_MAPE = mean(all_apes),
    CV_ME = mean(all_errors),
    stringsAsFactors = FALSE
  )
  
  cv_results <- rbind(cv_results, candidate_row)
}

# Rank each candidate on all three primary assignment metrics.
# Lower values are better. The mean rank prevents one metric from
# dominating the selection decision.
cv_results$Rank_MAE <- rank(cv_results$CV_MAE, ties.method = "average")
cv_results$Rank_RMSE <- rank(cv_results$CV_RMSE, ties.method = "average")
cv_results$Rank_MAPE <- rank(cv_results$CV_MAPE, ties.method = "average")
cv_results$Mean_Rank <- rowMeans(
  cv_results[, c("Rank_MAE", "Rank_RMSE", "Rank_MAPE")]
)

cv_results <- cv_results[
  order(cv_results$Mean_Rank, cv_results$CV_RMSE),
]
row.names(cv_results) <- NULL

cat("\n--- ROLLING-ORIGIN RESULTS ---\n")
print(cv_results, digits = 5, row.names = FALSE)

write.csv(
  cv_results,
  "outputs/stl_ets/tables/01_cv_model_selection.csv",
  row.names = FALSE
)

selected_candidate_name <- cv_results$Candidate[1]
selected_spec <- candidate_specs[
  candidate_specs$Candidate == selected_candidate_name,
]

selected_etsmodel <- selected_spec$ETSModel[1]
selected_damped <- selected_spec$Damped[1]

cat("\nSELECTED STL-ETS SPECIFICATION:", selected_candidate_name, "\n")
cat("Selection rule: lowest mean rank across CV MAE, RMSE and MAPE.\n")
cat("The final Jul 2023-Jun 2024 test set was NOT used for selection.\n")


# ============================================================
# STEP 16 - FIT SELECTED STL-ETS MODEL TO FULL TRAINING SAMPLE
# ============================================================
stl_ets_model <- fit_stl_ets_candidate(
  train_ts,
  selected_etsmodel,
  selected_damped
)

cat("\n============================================================\n")
cat("FINAL TRAINING STL-ETS MODEL\n")
cat("============================================================\n")
print(stl_ets_model)

cat("\n--- ETS MODEL INSIDE STL-ETS ---\n")
print(stl_ets_model$model)

selected_method <- stl_ets_model$model$method
selected_aicc <- stl_ets_model$model$aicc
ets_coefficients <- coef(stl_ets_model$model)

cat("\nSelected ETS method :", selected_method, "\n")
cat("ETS AICc            :", round(selected_aicc, 3), "\n")
cat("\nETS coefficients:\n")
print(ets_coefficients)

# Extract smoothing parameters for report use
smoothing_names <- intersect(
  c("alpha", "beta", "gamma", "phi"),
  names(ets_coefficients)
)

smoothing_parameters <- ets_coefficients[smoothing_names]

cat("\nSmoothing parameters used in report:\n")
print(smoothing_parameters)


# ============================================================
# STEP 17 - FORECAST THE UNTOUCHED TEST PERIOD
# ============================================================
test_forecast <- forecast(
  stl_ets_model,
  h = length(test_ts),
  level = c(80, 95)
)

cat("\n============================================================\n")
cat("UNTOUCHED TEST-SET FORECAST\n")
cat("============================================================\n")
print(test_forecast)


# ============================================================
# STEP 18 - TEST-SET ACCURACY
# ============================================================
accuracy_results <- accuracy(
  test_forecast,
  test_ts
)

cat("\n============================================================\n")
cat("FORECAST ACCURACY\n")
cat("============================================================\n")
print(accuracy_results)

if (!("Test set" %in% row.names(accuracy_results))) {
  stop("Could not find the Test set row returned by accuracy().")
}

test_accuracy <- accuracy_results["Test set", ]

ME <- unname(test_accuracy["ME"])
MAE <- unname(test_accuracy["MAE"])
RMSE <- unname(test_accuracy["RMSE"])
MAPE <- unname(test_accuracy["MAPE"])
MASE <- if ("MASE" %in% names(test_accuracy)) {
  unname(test_accuracy["MASE"])
} else {
  NA_real_
}

cat("\n--- PRIMARY TEST METRICS ---\n")
cat("ME   :", round(ME, 2), "\n")
cat("MAE  :", round(MAE, 2), "\n")
cat("RMSE :", round(RMSE, 2), "\n")
cat("MAPE :", round(MAPE, 2), "%\n")
if (!is.na(MASE)) {
  cat("MASE :", round(MASE, 4), "\n")
}


# ============================================================
# STEP 19 - ACTUAL VS FORECAST TABLE + INTERVAL COVERAGE
# ============================================================
test_dates <- seq(
  as.Date("2023-07-01"),
  as.Date("2024-06-01"),
  by = "month"
)

comparison_table <- data.frame(
  Month = format(test_dates, "%Y-%m"),
  Actual = as.numeric(test_ts),
  Forecast = as.numeric(test_forecast$mean),
  Lower_80 = as.numeric(test_forecast$lower[, "80%"]),
  Upper_80 = as.numeric(test_forecast$upper[, "80%"]),
  Lower_95 = as.numeric(test_forecast$lower[, "95%"]),
  Upper_95 = as.numeric(test_forecast$upper[, "95%"]),
  stringsAsFactors = FALSE
)

comparison_table$Error <-
  comparison_table$Actual - comparison_table$Forecast
comparison_table$Absolute_Error <- abs(comparison_table$Error)
comparison_table$APE_Percent <-
  comparison_table$Absolute_Error / comparison_table$Actual * 100
comparison_table$Inside_80 <-
  comparison_table$Actual >= comparison_table$Lower_80 &
  comparison_table$Actual <= comparison_table$Upper_80
comparison_table$Inside_95 <-
  comparison_table$Actual >= comparison_table$Lower_95 &
  comparison_table$Actual <= comparison_table$Upper_95

coverage_80 <- mean(comparison_table$Inside_80) * 100
coverage_95 <- mean(comparison_table$Inside_95) * 100

cat("\n============================================================\n")
cat("ACTUAL VS FORECAST TABLE\n")
cat("============================================================\n")
print(comparison_table, digits = 7, row.names = FALSE)
cat("\n80% interval empirical coverage:", round(coverage_80, 1), "%\n")
cat("95% interval empirical coverage:", round(coverage_95, 1), "%\n")

write.csv(
  comparison_table,
  "outputs/stl_ets/tables/02_test_forecast.csv",
  row.names = FALSE
)


# ============================================================
# STEP 20 - TEST FORECAST PLOT
# ============================================================
png(
  "outputs/stl_ets/figures/03_test_forecast_vs_actual.png",
  width = 1800,
  height = 1100,
  res = 180
)
plot(
  test_forecast,
  main = "STL-ETS Test Forecast vs Actual Consumption",
  xlab = "Year",
  ylab = "Electricity Consumption",
  lwd = 2,
  xlim = c(2022, 2024.6)
)
lines(test_ts, col = "red", lwd = 2)
legend(
  "topleft",
  legend = c("STL-ETS forecast", "Actual"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)
grid()
dev.off()


# ============================================================
# STEP 21 - RESIDUAL DIAGNOSTICS
# ============================================================
model_residuals <- residuals(stl_ets_model)
model_residuals <- model_residuals[is.finite(model_residuals)]

cat("\n============================================================\n")
cat("RESIDUAL DIAGNOSTICS\n")
cat("============================================================\n")
cat("Residual mean:", round(mean(model_residuals), 4), "\n")
cat("Residual SD  :", round(sd(model_residuals), 4), "\n")

# ============================================================
# STEP 21 - RESIDUAL DIAGNOSTICS
# ============================================================

model_residuals <- residuals(stl_ets_model)
model_residuals <- model_residuals[is.finite(model_residuals)]

cat("\n============================================================\n")
cat("RESIDUAL DIAGNOSTICS\n")
cat("============================================================\n")

cat("Residual mean:",
    round(mean(model_residuals), 4), "\n")

cat("Residual SD:",
    round(sd(model_residuals), 4), "\n")


# ------------------------------------------------------------
# Ljung-Box test
# ------------------------------------------------------------
# lag = 12 represents one complete annual cycle for monthly data.
#
# For consistency with forecast::checkresiduals() for an STL-ETS
# model, no fitted-parameter degrees-of-freedom adjustment is
# imposed here (fitdf = 0).

ljung_lag <- 12

ljung_box <- Box.test(
  model_residuals,
  lag = ljung_lag,
  type = "Ljung-Box",
  fitdf = 0
)

cat("\nLjung-Box test at lag 12:\n")
print(ljung_box)

cat(
  "\nLjung-Box p-value:",
  round(ljung_box$p.value, 4),
  "\n"
)

if (ljung_box$p.value > 0.05) {
  
  cat(
    "Decision: Fail to reject H0.\n",
    "Interpretation: There is insufficient evidence of ",
    "significant residual autocorrelation up to lag 12.\n",
    sep = ""
  )
  
} else {
  
  cat(
    "Decision: Reject H0.\n",
    "Interpretation: Statistically significant residual ",
    "autocorrelation remains up to lag 12.\n",
    sep = ""
  )
}


# ------------------------------------------------------------
# Standard forecast-package graphical diagnostics
# ------------------------------------------------------------

png(
  "outputs/stl_ets/figures/04_residual_diagnostics.png",
  width = 1800,
  height = 1200,
  res = 180
)

checkresiduals(
  stl_ets_model,
  lag = 12
)

dev.off()


# ------------------------------------------------------------
# Separate residual ACF
# ------------------------------------------------------------

png(
  "outputs/stl_ets/figures/05_residual_acf.png",
  width = 1500,
  height = 1000,
  res = 180
)

Acf(
  model_residuals,
  lag.max = 24,
  main = "ACF of STL-ETS Residuals"
)

dev.off()

cat("\nLjung-Box test at lag 12:\n")
print(ljung_box)
cat("Ljung-Box p-value:", round(ljung_box$p.value, 4), "\n")

if (ljung_box$p.value > 0.05) {
  cat(
    "Interpretation: p > 0.05; there is insufficient evidence ",
    "of residual autocorrelation up to lag 12.\n",
    sep = ""
  )
} else {
  cat(
    "Interpretation: p <= 0.05; statistically significant ",
    "residual autocorrelation remains. This should be reported ",
    "as a model limitation rather than hidden or tuned away.\n",
    sep = ""
  )
}

# Full graphical diagnostics
png(
  "outputs/stl_ets/figures/04_residual_diagnostics.png",
  width = 1800,
  height = 1200,
  res = 180
)
checkresiduals(stl_ets_model, lag = 12)
dev.off()

# Separate ACF for easy inclusion in reports/slides
png(
  "outputs/stl_ets/figures/05_residual_acf.png",
  width = 1500,
  height = 1000,
  res = 180
)
Acf(
  model_residuals,
  lag.max = 24,
  main = "ACF of STL-ETS Residuals"
)
dev.off()


# ============================================================
# STEP 22 - CREATE COMPACT TEST-SUMMARY TABLE
# ============================================================
parameter_text <- if (length(smoothing_parameters) == 0) {
  "See model output"
} else {
  paste(
    paste(
      names(smoothing_parameters),
      round(as.numeric(smoothing_parameters), 4),
      sep = "="
    ),
    collapse = "; "
  )
}

model_summary <- data.frame(
  Item = c(
    "Selected CV candidate",
    "ETS method fitted",
    "ETS smoothing parameters",
    "ETS AICc",
    "STL seasonal window",
    "Robust STL",
    "Training period",
    "Testing period",
    "Test ME",
    "Test MAE",
    "Test RMSE",
    "Test MAPE (%)",
    "Test MASE",
    "Ljung-Box lag",
    "Ljung-Box p-value",
    "80% interval coverage (%)",
    "95% interval coverage (%)",
    "Seasonal strength",
    "Trend strength"
  ),
  Value = c(
    selected_candidate_name,
    selected_method,
    parameter_text,
    round(selected_aicc, 4),
    "periodic",
    "TRUE",
    "Jan 2018-Jun 2023",
    "Jul 2023-Jun 2024",
    round(ME, 4),
    round(MAE, 4),
    round(RMSE, 4),
    round(MAPE, 4),
    ifelse(is.na(MASE), "NA", round(MASE, 4)),
    ljung_lag,
    round(ljung_box$p.value, 6),
    round(coverage_80, 2),
    round(coverage_95, 2),
    round(seasonal_strength, 4),
    round(trend_strength, 4)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  model_summary,
  "outputs/stl_ets/tables/03_model_summary.csv",
  row.names = FALSE
)

cat("\n============================================================\n")
cat("COMPACT MODEL SUMMARY\n")
cat("============================================================\n")
print(model_summary, row.names = FALSE)


# ============================================================
# STEP 23 - REFIT SELECTED SPECIFICATION TO ALL 78 OBSERVATIONS
# ============================================================
# The model specification has already been selected using training-
# only rolling validation and evaluated once on the untouched test
# set. It is now refitted to all available observations to produce
# operational future forecasts.

final_stl_ets <- fit_stl_ets_candidate(
  electricity_ts,
  selected_etsmodel,
  selected_damped
)

cat("\n============================================================\n")
cat("FINAL STL-ETS MODEL - ALL AVAILABLE DATA\n")
cat("============================================================\n")
print(final_stl_ets)
cat("\nETS component:\n")
print(final_stl_ets$model)


# ============================================================
# STEP 24 - FORECAST NEXT 12 MONTHS: JUL 2024-JUN 2025
# ============================================================
future_forecast <- forecast(
  final_stl_ets,
  h = 12,
  level = c(80, 95)
)

cat("\n============================================================\n")
cat("12-MONTH FUTURE FORECAST: JUL 2024-JUN 2025\n")
cat("============================================================\n")
print(future_forecast)


# ============================================================
# STEP 25 - FUTURE FORECAST TABLE
# ============================================================
future_dates <- seq(
  from = max(total_data$date),
  by = "month",
  length.out = 13
)[-1]

future_table <- data.frame(
  Month = format(future_dates, "%Y-%m"),
  Forecast = as.numeric(future_forecast$mean),
  Lower_80 = as.numeric(future_forecast$lower[, "80%"]),
  Upper_80 = as.numeric(future_forecast$upper[, "80%"]),
  Lower_95 = as.numeric(future_forecast$lower[, "95%"]),
  Upper_95 = as.numeric(future_forecast$upper[, "95%"]),
  stringsAsFactors = FALSE
)

future_table[, -1] <- round(future_table[, -1], 2)

cat("\n============================================================\n")
cat("FUTURE FORECAST TABLE\n")
cat("============================================================\n")
print(future_table, row.names = FALSE)

write.csv(
  future_table,
  "outputs/stl_ets/tables/04_future_forecast.csv",
  row.names = FALSE
)


# ============================================================
# STEP 26 - FUTURE FORECAST PLOT
# ============================================================
png(
  "outputs/stl_ets/figures/06_future_forecast.png",
  width = 1800,
  height = 1100,
  res = 180
)
plot(
  future_forecast,
  main = "STL-ETS Forecast: Malaysia Electricity Consumption",
  xlab = "Year",
  ylab = "Electricity Consumption",
  lwd = 2
)
grid()
dev.off()


# ============================================================
# STEP 27 - REPORT-READY TEXT OUTPUT
# ============================================================
# This file contains the exact values to paste into the individual
# report template generated with this script.

sink("outputs/stl_ets/tables/05_report_values.txt")

cat("STL-ETS REPORT-READY VALUES\n")
cat("============================================================\n\n")
cat("Selected rolling-CV candidate: ", selected_candidate_name, "\n", sep = "")
cat("ETS method fitted on full training set: ", selected_method, "\n", sep = "")
cat("ETS smoothing parameters: ", parameter_text, "\n", sep = "")
cat("ETS AICc: ", round(selected_aicc, 3), "\n", sep = "")
cat("Seasonal strength: ", round(seasonal_strength, 3), "\n", sep = "")
cat("Trend strength: ", round(trend_strength, 3), "\n", sep = "")
cat("\nTEST SET (JUL 2023-JUN 2024)\n")
cat("ME: ", round(ME, 2), "\n", sep = "")
cat("MAE: ", round(MAE, 2), "\n", sep = "")
cat("RMSE: ", round(RMSE, 2), "\n", sep = "")
cat("MAPE: ", round(MAPE, 2), "%\n", sep = "")
if (!is.na(MASE)) {
  cat("MASE: ", round(MASE, 4), "\n", sep = "")
}
cat("Ljung-Box lag: ", ljung_lag, "\n", sep = "")
cat("Ljung-Box p-value: ", round(ljung_box$p.value, 4), "\n", sep = "")
cat("80% empirical interval coverage: ", round(coverage_80, 1), "%\n", sep = "")
cat("95% empirical interval coverage: ", round(coverage_95, 1), "%\n", sep = "")

cat("\nRESIDUAL INTERPRETATION\n")
if (ljung_box$p.value > 0.05) {
  cat(
    "The Ljung-Box test produced p > .05, so there was insufficient ",
    "evidence of remaining residual autocorrelation up to lag 12.\n",
    sep = ""
  )
} else {
  cat(
    "The Ljung-Box test produced p <= .05, indicating statistically ",
    "significant remaining residual autocorrelation. This should be ",
    "reported as a limitation despite the out-of-sample accuracy.\n",
    sep = ""
  )
}

cat("\nFUTURE FORECAST PERIOD\n")
cat("Jul 2024-Jun 2025\n")
cat("First future point forecast: ", future_table$Forecast[1], "\n", sep = "")
cat("Last future point forecast: ", future_table$Forecast[nrow(future_table)], "\n", sep = "")
cat("Minimum future point forecast: ", min(future_table$Forecast), "\n", sep = "")
cat("Maximum future point forecast: ", max(future_table$Forecast), "\n", sep = "")

cat("\nFILES GENERATED\n")
cat("outputs/stl_ets/tables/01_cv_model_selection.csv\n")
cat("outputs/stl_ets/tables/02_test_forecast.csv\n")
cat("outputs/stl_ets/tables/04_future_forecast.csv\n")
cat("outputs/stl_ets/tables/03_model_summary.csv\n")
cat("STL_ETS_01_original_series.png\n")
cat("outputs/stl_ets/figures/02_training_stl_decomposition.png\n")
cat("outputs/stl_ets/figures/03_test_forecast_vs_actual.png\n")
cat("outputs/stl_ets/figures/04_residual_diagnostics.png\n")
cat("outputs/stl_ets/figures/05_residual_acf.png\n")
cat("outputs/stl_ets/figures/06_future_forecast.png\n")

sink()


# ============================================================
# STEP 28 - FINAL CONSOLE SUMMARY
# ============================================================
cat("\n\n============================================================\n")
cat("FINAL STL-ETS SUMMARY\n")
cat("============================================================\n")
cat("Model family            : STL decomposition + ETS\n")
cat("Seasonality             : Monthly (m = 12)\n")
cat("STL seasonal window     : periodic\n")
cat("Robust STL              : TRUE\n")
cat("Transformation          : None\n")
cat("Differencing            : None required for STL-ETS\n")
cat("Selection method        : Training-only rolling-origin CV\n")
cat("Selected candidate      :", selected_candidate_name, "\n")
cat("ETS model fitted        :", selected_method, "\n")
cat("Training                : Jan 2018-Jun 2023 (n = 66)\n")
cat("Testing                 : Jul 2023-Jun 2024 (n = 12)\n")
cat("Future forecast         : Jul 2024-Jun 2025 (12 months)\n")
cat("\nTEST PERFORMANCE\n")
cat("ME                      :", round(ME, 2), "\n")
cat("MAE                     :", round(MAE, 2), "\n")
cat("RMSE                    :", round(RMSE, 2), "\n")
cat("MAPE                    :", round(MAPE, 2), "%\n")
cat("Ljung-Box p-value       :", round(ljung_box$p.value, 4), "\n")
cat("80% interval coverage   :", round(coverage_80, 1), "%\n")
cat("95% interval coverage   :", round(coverage_95, 1), "%\n")
cat("\nExact report values were written to:\n")
cat("outputs/stl_ets/tables/05_report_values.txt\n")
cat("\n============================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================\n")
