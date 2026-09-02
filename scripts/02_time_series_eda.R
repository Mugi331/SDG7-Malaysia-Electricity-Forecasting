# ============================================================
# 02_time_series_eda.R
# Exploratory Time-Series Analysis and Stationarity Diagnostics
# Malaysia Monthly Electricity Consumption
# Full observed period: January 2018 - June 2024
# ============================================================
#
# PURPOSE OF THIS SCRIPT
#   1. Examine the original monthly time series first.
#   2. Assess long-term movement and variance behaviour.
#   3. Investigate possible annual seasonality using several
#      descriptive diagnostics rather than one graph alone.
#   4. Assess non-stationary behaviour using the time plot,
#      ACF, and diagnostic differencing.
#   5. Produce evidence needed BEFORE deciding the final
#      train-test split and model-specific preprocessing.
#
# IMPORTANT
#   - No train-test split is created in this script.
#   - Differenced series are DIAGNOSTIC COPIES only.
#   - The original series is never overwritten.
#   - Whether differencing/transformation is actually used later
#     depends on the assumptions of the selected forecasting model.
#   - Forecast-model residual diagnostics belong after modelling,
#     not in this EDA script.
# ============================================================


# ============================================================
# 1. LOAD THE CLEAN SERIES CREATED BY 01_data_setup.R
# ============================================================

processed_path <-
  "data/processed/electricity_total_2018_2024.csv"

if (!file.exists(processed_path)) {
  stop(
    paste0(
      "Processed data file not found: ", processed_path,
      "\nRun scripts/01_data_setup.R first."
    )
  )
}

electricity_total <- read.csv(
  processed_path,
  stringsAsFactors = FALSE
)

electricity_total$date <- as.Date(electricity_total$date)
electricity_total <- electricity_total[
  order(electricity_total$date),
]
row.names(electricity_total) <- NULL


# ============================================================
# 2. CREATE THE ORIGINAL MONTHLY TIME-SERIES OBJECT
# ============================================================

start_year <- as.integer(format(min(electricity_total$date), "%Y"))
start_month <- as.integer(format(min(electricity_total$date), "%m"))

electricity_ts <- ts(
  electricity_total$consumption,
  start = c(start_year, start_month),
  frequency = 12
)

cat("\nOriginal time-series structure:\n")
cat("Start:", start(electricity_ts), "\n")
cat("End:", end(electricity_ts), "\n")
cat("Frequency:", frequency(electricity_ts), "\n")
cat("Observations:", length(electricity_ts), "\n")


# ============================================================
# 3. CREATE OUTPUT DIRECTORIES
# ============================================================

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. ORIGINAL TIME PLOT
# Syllabus role: this is the FIRST descriptive time-series step.
# Examine trend, fluctuations, possible seasonality, structural
# changes, and whether the mean/variance appear stable over time.
# ============================================================

png(
  "outputs/figures/01_original_time_series.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  electricity_ts,
  type = "l",
  ylab = "Electricity Consumption",
  xlab = "Year",
  main = paste0(
    "Monthly Electricity Consumption in Malaysia, ",
    "January 2018-June 2024"
  )
)

dev.off()

# Display in the R plotting window as well.
plot(
  electricity_ts,
  type = "l",
  ylab = "Electricity Consumption",
  xlab = "Year",
  main = paste0(
    "Monthly Electricity Consumption in Malaysia, ",
    "January 2018-June 2024"
  )
)


# ============================================================
# 5. YEARLY LEVEL AND VARIABILITY SUMMARY
# Purpose:
#   - Support the visual assessment of long-term level changes.
#   - Check whether variability clearly increases with the level.
#   - This helps decide later whether a variance-stabilising
#     transformation (e.g., log / Box-Cox) is even necessary.
# ============================================================

electricity_total$Year <- as.integer(
  format(electricity_total$date, "%Y")
)

yearly_mean <- aggregate(
  consumption ~ Year,
  data = electricity_total,
  FUN = mean
)

yearly_sd <- aggregate(
  consumption ~ Year,
  data = electricity_total,
  FUN = sd
)

yearly_min <- aggregate(
  consumption ~ Year,
  data = electricity_total,
  FUN = min
)

yearly_max <- aggregate(
  consumption ~ Year,
  data = electricity_total,
  FUN = max
)

yearly_n <- aggregate(
  consumption ~ Year,
  data = electricity_total,
  FUN = length
)

yearly_summary <- data.frame(
  Year = yearly_mean$Year,
  Observations = yearly_n$consumption,
  Mean = yearly_mean$consumption,
  SD = yearly_sd$consumption,
  Min = yearly_min$consumption,
  Max = yearly_max$consumption
)

write.csv(
  yearly_summary,
  "outputs/tables/yearly_level_variability_summary.csv",
  row.names = FALSE
)

print(yearly_summary)


# ============================================================
# 6. PREPARE COMPLETE CALENDAR YEARS FOR SEASONAL COMPARISON
# ============================================================
# The full dataset ends in June 2024. Using raw calendar-month
# averages from all 78 observations would give Jan-Jun one extra
# (and relatively high-level) 2024 observation compared with Jul-Dec.
# For a balanced descriptive comparison of calendar months, use the
# complete years 2018-2023 only.
#
# IMPORTANT: 2024 is NOT deleted from the project dataset. This
# complete-year subset is used only for seasonal EDA.
# ============================================================

complete_year_data <- subset(
  electricity_total,
  date <= as.Date("2023-12-01")
)

complete_year_ts <- ts(
  complete_year_data$consumption,
  start = c(2018, 1),
  frequency = 12
)


# ============================================================
# 7. MONTHLY SEASONAL SUBSERIES PLOT
# Purpose: visually compare recurring behaviour for each month.
# ============================================================

png(
  "outputs/figures/02_monthly_seasonal_pattern_complete_years.png",
  width = 1200,
  height = 800,
  res = 150
)

monthplot(
  complete_year_ts,
  ylab = "Electricity Consumption",
  xlab = "Month",
  main = paste0(
    "Monthly Seasonal Pattern of Electricity Consumption, ",
    "2018-2023"
  )
)

dev.off()


# ============================================================
# 8. BALANCED MONTHLY DESCRIPTIVE SUMMARY
# Purpose: numerical support for recurring month-of-year effects.
# ============================================================

complete_year_data$Month_Number <- as.integer(
  format(complete_year_data$date, "%m")
)

monthly_mean <- aggregate(
  consumption ~ Month_Number,
  data = complete_year_data,
  FUN = mean
)

monthly_sd <- aggregate(
  consumption ~ Month_Number,
  data = complete_year_data,
  FUN = sd
)

monthly_min <- aggregate(
  consumption ~ Month_Number,
  data = complete_year_data,
  FUN = min
)

monthly_max <- aggregate(
  consumption ~ Month_Number,
  data = complete_year_data,
  FUN = max
)

monthly_n <- aggregate(
  consumption ~ Month_Number,
  data = complete_year_data,
  FUN = length
)

monthly_summary <- data.frame(
  Month = month.abb[monthly_mean$Month_Number],
  Observations = monthly_n$consumption,
  Mean = monthly_mean$consumption,
  SD = monthly_sd$consumption,
  Min = monthly_min$consumption,
  Max = monthly_max$consumption
)

write.csv(
  monthly_summary,
  "outputs/tables/monthly_descriptive_summary_complete_years.csv",
  row.names = FALSE
)

print(monthly_summary)


# ============================================================
# 9. CLASSICAL ADDITIVE DECOMPOSITION
# Purpose:
#   Separate the complete-year observed series into trend,
#   seasonal, and random components.
#
# Why additive at this stage?
#   The seasonal fluctuations do not obviously expand in direct
#   proportion to the overall level. The yearly variability table
#   should be inspected together with the original time plot.
#   If variance/seasonal amplitude clearly increases with level,
#   a log or Box-Cox transformation can be considered later.
# ============================================================

electricity_decomp <- decompose(
  complete_year_ts,
  type = "additive"
)

png(
  "outputs/figures/03_additive_decomposition_2018_2023.png",
  width = 1200,
  height = 1000,
  res = 150
)

plot(electricity_decomp)

dev.off()


# ============================================================
# 10. ACF OF THE ORIGINAL FULL SERIES
# Purpose:
#   - Examine serial dependence.
#   - Inspect lag 12 for annual recurrence in monthly data.
#   - Persistent autocorrelation can also support a conclusion
#     that the original series is not stationary.
# ============================================================

acf_original <- acf(
  electricity_ts,
  lag.max = 36,
  plot = FALSE
)

png(
  "outputs/figures/04_acf_original_series.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  acf_original,
  main = "ACF of Original Monthly Electricity Consumption"
)

dev.off()

lag_1_acf <- as.numeric(acf_original$acf[2])
lag_12_acf <- as.numeric(acf_original$acf[13])

cat("\nSelected original-series autocorrelations:\n")
cat("Lag 1 ACF :", round(lag_1_acf, 4), "\n")
cat("Lag 12 ACF:", round(lag_12_acf, 4), "\n")


# ============================================================
# 11. FIRST-DIFFERENCE DIAGNOSTIC
# Syllabus role:
#   If the original series is non-stationary in level/trend,
#   first differencing is one possible transformation to examine.
#
# IMPORTANT: this does NOT replace electricity_ts.
# ============================================================

first_diff <- diff(
  electricity_ts,
  differences = 1
)

png(
  "outputs/figures/05_first_difference.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  first_diff,
  type = "l",
  ylab = "First Difference",
  xlab = "Year",
  main = "First-Differenced Monthly Electricity Consumption"
)

abline(h = 0, lty = 2)

dev.off()

png(
  "outputs/figures/06_acf_first_difference.png",
  width = 1200,
  height = 800,
  res = 150
)

acf(
  first_diff,
  lag.max = 36,
  main = "ACF of First-Differenced Electricity Consumption"
)

dev.off()


# ============================================================
# 12. SEASONAL-DIFFERENCE DIAGNOSTIC (LAG 12)
# Syllabus role:
#   For monthly data with annual seasonality, seasonal
#   differencing compares each month with the same month one
#   year earlier: Y_t - Y_(t-12).
#
# IMPORTANT: this is also a diagnostic copy only.
# ============================================================

seasonal_diff <- diff(
  electricity_ts,
  lag = 12,
  differences = 1
)

png(
  "outputs/figures/07_seasonal_difference_lag12.png",
  width = 1200,
  height = 800,
  res = 150
)

plot(
  seasonal_diff,
  type = "l",
  ylab = "Seasonal Difference (Lag 12)",
  xlab = "Year",
  main = "Seasonally Differenced Electricity Consumption (Lag 12)"
)

abline(h = 0, lty = 2)

dev.off()

png(
  "outputs/figures/08_acf_seasonal_difference_lag12.png",
  width = 1200,
  height = 800,
  res = 150
)

acf(
  seasonal_diff,
  lag.max = 36,
  main = "ACF after Seasonal Differencing (Lag 12)"
)

dev.off()


# ============================================================
# 13. EDA DIAGNOSTIC SUMMARY TABLE
# ============================================================

eda_summary <- data.frame(
  Diagnostic = c(
    "Full-series observations",
    "Full-series start",
    "Full-series end",
    "Seasonal frequency",
    "Complete-year seasonal EDA period",
    "Lag 1 ACF (original)",
    "Lag 12 ACF (original)",
    "First-differenced observations",
    "Seasonally differenced observations"
  ),
  Value = c(
    length(electricity_ts),
    "Jan 2018",
    "Jun 2024",
    frequency(electricity_ts),
    "Jan 2018-Dec 2023",
    round(lag_1_acf, 4),
    round(lag_12_acf, 4),
    length(first_diff),
    length(seasonal_diff)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  eda_summary,
  "outputs/tables/time_series_eda_summary.csv",
  row.names = FALSE
)

print(eda_summary)


# ============================================================
# 14. FINAL MESSAGE
# ============================================================

cat("\n============================================\n")
cat("02 TIME-SERIES EDA COMPLETE\n")
cat("============================================\n")
cat("No train-test split has been created yet.\n")
cat("No forecasting model has been fitted yet.\n")
cat("Use these diagnostics to decide next:\n")
cat("  1. Is seasonality sufficiently supported?\n")
cat("  2. Is the original series non-stationary in mean/variance?\n")
cat("  3. Is any transformation required for a specific model?\n")
cat("  4. What test horizon preserves the seasonal structure?\n")
cat("============================================\n")