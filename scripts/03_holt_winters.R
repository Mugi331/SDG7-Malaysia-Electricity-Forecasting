# ============================================================
# 03_holt_winters.R
# Holt-Winters Exponential Smoothing Modelling
#
# PURPOSE
#   1. Load the common training and testing datasets.
#   2. Fit an additive Holt-Winters model on the training series.
#   3. Forecast the 12-month test period.
#   4. Diagnose residuals for remaining structure.
#   5. Evaluate out-of-sample forecast accuracy (MAE, RMSE, MAPE).
#
# IMPORTANT
#   - The train-test split is NOT recreated here.
#   - Model fitting uses TRAINING DATA ONLY.
#   - The TEST set is used only for final forecast evaluation.
# ============================================================

if (!requireNamespace("forecast", quietly = TRUE)) {
  stop("Package 'forecast' is required. Install with install.packages('forecast')")
}
library(forecast)

train_path <- "data/processed/electricity_train_2018_2023.csv"
test_path  <- "data/processed/electricity_test_2023_2024.csv"

dir.create("outputs/holt_winters/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/holt_winters/tables", recursive = TRUE, showWarnings = FALSE)

train_data <- read.csv(train_path, stringsAsFactors = FALSE)
test_data  <- read.csv(test_path, stringsAsFactors = FALSE)

train_data$date <- as.Date(train_data$date)
test_data$date  <- as.Date(test_data$date)
train_data$consumption <- as.numeric(train_data$consumption)
test_data$consumption  <- as.numeric(test_data$consumption)

stopifnot(nrow(train_data) == 66)
stopifnot(nrow(test_data) == 12)

# ============================================================
# 2. Create time series object
# ============================================================
seasonal_period <- 12

train_ts <- ts(train_data$consumption, start = c(2018, 1), frequency = seasonal_period)
test_ts  <- ts(test_data$consumption, start = c(2023, 7), frequency = seasonal_period)

stopifnot(length(train_ts) == 66)
stopifnot(length(test_ts) == 12)

# ============================================================
# 3. Fit Holt-Winters model on training data
# ============================================================

hw_model <- HoltWinters(
  train_ts,
  seasonal = "additive"
)

cat("\n============================================\n")
cat("HOLT-WINTERS MODEL SUMMARY\n")
cat("============================================\n")
print(hw_model)

cat("\nSmoothing parameters:\n")
cat("Alpha (level):", hw_model$alpha, "\n")
cat("Beta (trend):", hw_model$beta, "\n")
cat("Gamma (seasonal):", hw_model$gamma, "\n")
cat("SSE:", hw_model$SSE, "\n")

hw_params <- data.frame(
  Parameter = c("Alpha (level)", "Beta (trend)", "Gamma (seasonal)", "SSE"),
  Value = c(hw_model$alpha, hw_model$beta, hw_model$gamma, hw_model$SSE)
)

write.csv(hw_params, "outputs/holt_winters/tables/01_hw_parameters.csv", row.names = FALSE)


# ============================================================
# Fitted values Picture and Forecast
# ============================================================

# Plot fitted vs. observed on training data
png("outputs/holt_winters/figures/01_hw_fitted.png", width = 1200, height = 800, res = 150)
plot(hw_model, main = "Holt-Winters Fitted Values (Training Period)")
dev.off()

# Forecast the 12-month test period
forecast_horizon <- length(test_ts)

hw_forecast <- forecast(hw_model, h = forecast_horizon)

png("outputs/holt_winters/figures/02_hw_forecast.png", width = 1200, height = 800, res = 150)
plot(hw_forecast, main = "Holt-Winters Forecast vs. Actual (Jul 2023-Jun 2024)")
lines(test_ts, col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual (Test)"), col = c("blue", "red"), lty = 1)
dev.off()

# ============================================================
# 5. Residual Diagnostics
# ============================================================

hw_residuals <- residuals(hw_model)

# Residual time plot
png("outputs/holt_winters/figures/03_hw_residuals.png", width = 1200, height = 800, res = 150)
plot(hw_residuals, main = "Holt-Winters Residuals", xlab = "Year", ylab = "Residual")
abline(h = 0, lty = 2)
dev.off()

# Residual ACF
png("outputs/holt_winters/figures/04_hw_residual_acf.png", width = 1200, height = 800, res = 150)
acf(hw_residuals, lag.max = 24, main = "ACF of Holt-Winters Residuals")
dev.off()

# Residual histogram
png("outputs/holt_winters/figures/05_hw_residual_histogram.png", width = 1200, height = 800, res = 150)
hist(hw_residuals, main = "Distribution of Holt-Winters Residuals", xlab = "Residual", breaks = "Sturges")
abline(v = 0, lty = 2)
dev.off()

# Ljung-Box test
hw_ljung <- Box.test(hw_residuals, lag = 24, type = "Ljung-Box")

hw_diagnostics <- data.frame(
  Model = "Holt-Winters (Additive)",
  Ljung_Box_Lag = 24,
  Statistic = unname(hw_ljung$statistic),
  DF = unname(hw_ljung$parameter),
  P_Value = hw_ljung$p.value
)

write.csv(hw_diagnostics, "outputs/holt_winters/tables/02_hw_residual_diagnostics.csv", row.names = FALSE)
print(hw_diagnostics)

# ============================================================
# 6. Calculate Forecast Accuracy
# ============================================================

calculate_accuracy <- function(actual, predicted, model_name) {
  errors <- actual - predicted
  data.frame(
    Model = model_name,
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    MAPE = mean(abs(errors / actual)) * 100
  )
}

hw_accuracy <- calculate_accuracy(
  as.numeric(test_ts),
  as.numeric(hw_forecast$mean),
  "Holt-Winters (Additive)"
)

write.csv(hw_accuracy, "outputs/holt_winters/tables/03_hw_forecast_accuracy.csv", row.names = FALSE)
print(hw_accuracy)

# ============================================================
# Additional: Compare Additive vs Multiplicative Holt-Winters
# ============================================================

hw_model_mult <- HoltWinters(train_ts, seasonal = "multiplicative")

hw_forecast_mult <- forecast(hw_model_mult, h = forecast_horizon)

hw_accuracy_mult <- calculate_accuracy(
  as.numeric(test_ts),
  as.numeric(hw_forecast_mult$mean),
  "Holt-Winters (Multiplicative)"
)

hw_seasonal_comparison <- rbind(hw_accuracy, hw_accuracy_mult)

write.csv(
  hw_seasonal_comparison,
  "outputs/holt_winters/tables/04_hw_seasonal_type_comparison.csv",
  row.names = FALSE
)

print(hw_seasonal_comparison)

cat("\n============================================\n")
cat("03 HOLT-WINTERS MODELLING COMPLETE\n")
cat("============================================\n")






