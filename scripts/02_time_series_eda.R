# ============================================================
# 02_time_series_eda.R
# Exploratory Time-Series Analysis, Seasonality Assessment,
# Common Train-Test Split, and Stationarity Preprocessing
#
# Full observed period:
#   January 2018 - June 2024
#
# PURPOSE
#   1. Load and validate the clean monthly series created by
#      01_data_setup.R.
#   2. Explore the observed time-series characteristics.
#   3. Screen unusual observations without automatically
#      removing them.
#   4. Examine long-term level, variability, seasonality,
#      decomposition, and autocorrelation.
#   5. Use the seasonal evidence to justify a common
#      train-test split for all forecasting models.
#   6. Assess stationarity using TRAINING DATA ONLY.
#   7. Examine first differencing when formal evidence
#      indicates non-stationarity.
#   8. Export shared train/test datasets and preprocessing
#      evidence for subsequent modelling scripts.
#
# IMPORTANT
#   - No forecasting model is fitted here.
#   - Outliers are FLAGGED, not automatically removed.
#   - Seasonal behaviour is investigated BEFORE deciding
#     the train-test strategy.
#   - Formal stationarity and differencing decisions use
#     training data only to avoid test-set leakage.
# ============================================================


# ============================================================
# 0. REQUIRED PACKAGE
# ============================================================

if (!requireNamespace("tseries", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'tseries' is required for ADF/KPSS tests.\n",
      "Install it once using: install.packages('tseries')"
    )
  )
}


# ============================================================
# 1. DEFINE FILE PATHS
# ============================================================

processed_path <-
  "data/processed/electricity_total_2018_2024.csv"

train_path <-
  "data/processed/electricity_train_2018_2023.csv"

test_path <-
  "data/processed/electricity_test_2023_2024.csv"

split_path <-
  "data/processed/electricity_model_split.csv"


# ============================================================
# 2. LOAD CLEAN SERIES CREATED BY 01_data_setup.R
# ============================================================

if (!file.exists(processed_path)) {
  stop(
    paste0(
      "Processed data file not found: ",
      processed_path,
      "\nRun scripts/01_data_setup.R first."
    )
  )
}

electricity_total <- read.csv(
  processed_path,
  stringsAsFactors = FALSE
)

required_columns <- c(
  "date",
  "sector",
  "consumption"
)

missing_columns <- setdiff(
  required_columns,
  names(electricity_total)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Processed dataset is missing required column(s):",
      paste(missing_columns, collapse = ", ")
    )
  )
}

electricity_total$date <- as.Date(
  electricity_total$date
)

electricity_total$consumption <- suppressWarnings(
  as.numeric(electricity_total$consumption)
)

if (
  any(is.na(electricity_total$date)) ||
  any(is.na(electricity_total$consumption))
) {
  stop(
    paste0(
      "Processed dataset contains invalid or missing ",
      "date/consumption values."
    )
  )
}

electricity_total <- electricity_total[
  order(electricity_total$date),
]

row.names(electricity_total) <- NULL


# ============================================================
# 3. VERIFY EXPECTED FULL MONTHLY SERIES
# ============================================================

expected_start <- as.Date("2018-01-01")
expected_end   <- as.Date("2024-06-01")

expected_months <- seq(
  from = expected_start,
  to = expected_end,
  by = "month"
)

if (nrow(electricity_total) != length(expected_months)) {
  stop(
    paste0(
      "Expected ",
      length(expected_months),
      " monthly observations but found ",
      nrow(electricity_total),
      "."
    )
  )
}

if (!all(electricity_total$date == expected_months)) {
  stop(
    "Processed monthly dates are incomplete or misaligned."
  )
}

if (!all(electricity_total$sector == "total")) {
  stop(
    paste0(
      "Processed dataset contains observations outside ",
      "sector = 'total'."
    )
  )
}


# ============================================================
# 4. CREATE FULL MONTHLY TIME-SERIES OBJECT
# ============================================================

electricity_ts <- ts(
  electricity_total$consumption,
  start = c(2018, 1),
  frequency = 12
)


# ============================================================
# 5. CREATE OUTPUT DIRECTORIES
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

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\n============================================\n")
cat("02 TIME-SERIES EDA\n")
cat("============================================\n")

cat(
  "Observed period:",
  format(min(electricity_total$date), "%b %Y"),
  "to",
  format(max(electricity_total$date), "%b %Y"),
  "\n"
)

cat(
  "Observations:",
  length(electricity_ts),
  "\n"
)

cat(
  "Frequency:",
  frequency(electricity_ts),
  "months\n"
)


# ============================================================
# 6. OUTLIER SCREENING USING IQR RULE
#
# Lower bound = Q1 - 1.5 * IQR
# Upper bound = Q3 + 1.5 * IQR
#
# Observations are only flagged for investigation.
# They are NOT automatically removed.
# ============================================================

q1 <- quantile(
  electricity_total$consumption,
  0.25,
  na.rm = TRUE
)

q3 <- quantile(
  electricity_total$consumption,
  0.75,
  na.rm = TRUE
)

iqr_value <- IQR(
  electricity_total$consumption,
  na.rm = TRUE
)

lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value

outlier_flag <- (
  electricity_total$consumption < lower_bound |
    electricity_total$consumption > upper_bound
)

outlier_candidates <- electricity_total[
  outlier_flag,
  c("date", "consumption")
]

outlier_summary <- data.frame(
  Measure = c(
    "Q1",
    "Q3",
    "IQR",
    "Lower IQR bound",
    "Upper IQR bound",
    "Number of flagged observations"
  ),

  Value = c(
    as.numeric(q1),
    as.numeric(q3),
    as.numeric(iqr_value),
    as.numeric(lower_bound),
    as.numeric(upper_bound),
    nrow(outlier_candidates)
  ),

  stringsAsFactors = FALSE
)

write.csv(
  outlier_summary,
  "outputs/tables/outlier_summary.csv",
  row.names = FALSE
)

write.csv(
  outlier_candidates,
  "outputs/tables/outlier_candidates.csv",
  row.names = FALSE
)

cat("\nIQR OUTLIER SUMMARY\n")
print(outlier_summary)

if (nrow(outlier_candidates) > 0) {
  print(outlier_candidates)
}


# ============================================================
# 7. ORIGINAL TIME-SERIES PLOT
#
# Purpose:
#   Examine overall level, long-term movement, variability,
#   possible seasonal behaviour, and unusual observations.
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


# ============================================================
# 8. YEARLY LEVEL AND VARIABILITY SUMMARY
#
# Purpose:
#   Examine changes in level and whether variability appears
#   to increase substantially with the series level.
# ============================================================

electricity_total$Year <- as.integer(
  format(electricity_total$date, "%Y")
)

summarise_by_year <- function(fun) {
  aggregate(
    consumption ~ Year,
    data = electricity_total,
    FUN = fun
  )
}

yearly_mean <- summarise_by_year(mean)
yearly_sd   <- summarise_by_year(sd)
yearly_min  <- summarise_by_year(min)
yearly_max  <- summarise_by_year(max)
yearly_n    <- summarise_by_year(length)

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

cat("\nYEARLY LEVEL AND VARIABILITY\n")
print(yearly_summary)


# ============================================================
# 9. DEFINE COMPLETE CALENDAR YEARS FOR BALANCED SEASONAL EDA
#
# The full dataset ends in June 2024.
#
# Including 2024 in month-by-month seasonal summaries would
# give January-June one additional observation compared with
# July-December.
#
# Therefore, balanced seasonal exploration uses:
#   January 2018 - December 2023
#   = six complete calendar years
# ============================================================

complete_year_data <- subset(
  electricity_total,
  date >= as.Date("2018-01-01") &
    date <= as.Date("2023-12-01")
)

if (nrow(complete_year_data) != 72) {
  stop(
    paste0(
      "Balanced seasonal EDA expected 72 observations ",
      "from January 2018-December 2023."
    )
  )
}

complete_year_ts <- ts(
  complete_year_data$consumption,
  start = c(2018, 1),
  frequency = 12
)


# ============================================================
# 10. MONTHLY SEASONAL PLOT
#
# Purpose:
#   Examine recurring month-of-year behaviour using
#   equally represented complete calendar years.
# ============================================================

png(
  paste0(
    "outputs/figures/",
    "02_monthly_seasonal_pattern_complete_years.png"
  ),
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
# 11. BALANCED MONTHLY DESCRIPTIVE SUMMARY
# ============================================================

complete_year_data$Month_Number <- as.integer(
  format(
    complete_year_data$date,
    "%m"
  )
)

summarise_by_month <- function(fun) {
  aggregate(
    consumption ~ Month_Number,
    data = complete_year_data,
    FUN = fun
  )
}

monthly_mean <- summarise_by_month(mean)
monthly_sd   <- summarise_by_month(sd)
monthly_min  <- summarise_by_month(min)
monthly_max  <- summarise_by_month(max)
monthly_n    <- summarise_by_month(length)

monthly_summary <- data.frame(
  Month = month.abb[
    monthly_mean$Month_Number
  ],

  Observations =
    monthly_n$consumption,

  Mean =
    monthly_mean$consumption,

  SD =
    monthly_sd$consumption,

  Min =
    monthly_min$consumption,

  Max =
    monthly_max$consumption
)

write.csv(
  monthly_summary,
  paste0(
    "outputs/tables/",
    "monthly_descriptive_summary_complete_years.csv"
  ),
  row.names = FALSE
)

cat("\nBALANCED MONTHLY SUMMARY: 2018-2023\n")
print(monthly_summary)


# ============================================================
# 12. CLASSICAL ADDITIVE DECOMPOSITION
#
# Purpose:
#   Separate the complete-year series into:
#     - observed
#     - trend
#     - seasonal
#     - random/remainder components
#
# Additive decomposition is used because the magnitude of
# seasonal fluctuations does not appear to increase strongly
# in proportion to the overall level of the series.
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

plot(
  electricity_decomp
)

dev.off()


# ============================================================
# 13. ACF OF ORIGINAL FULL SERIES
#
# Purpose:
#   Examine serial dependence and possible seasonal
#   autocorrelation.
#
# Monthly data:
#   lag 12 = 1 year
#   lag 24 = 2 years
#   lag 36 = 3 years
#
# A maximum lag of 36 therefore allows inspection across
# approximately three annual cycles.
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
  main =
    "ACF of Original Monthly Electricity Consumption"
)

dev.off()


get_acf_value <- function(
    acf_object,
    lag_number
) {

  as.numeric(
    acf_object$acf[
      lag_number + 1
    ]
  )
}


original_acf_selected <- data.frame(
  Lag = c(
    1,
    12,
    24,
    36
  ),

  ACF = c(
    get_acf_value(acf_original, 1),
    get_acf_value(acf_original, 12),
    get_acf_value(acf_original, 24),
    get_acf_value(acf_original, 36)
  )
)

write.csv(
  original_acf_selected,
  "outputs/tables/acf_original_selected_lags.csv",
  row.names = FALSE
)

cat("\nSELECTED ORIGINAL-SERIES ACF VALUES\n")
print(original_acf_selected)


# ============================================================
# 14. SUMMARISE SEASONAL EVIDENCE
#
# Seasonal behaviour is assessed using multiple pieces of
# exploratory evidence rather than a single arbitrary rule:
#
#   1. Monthly seasonal plot
#   2. Monthly descriptive summary
#   3. Additive decomposition
#   4. ACF at seasonal lags, particularly lag 12
#
# The observed series shows recurring annual behaviour.
# Because the data are monthly, the seasonal period is:
#
#   m = 12
#
# This evidence is used to justify reserving one complete
# seasonal cycle for out-of-sample forecast evaluation.
# ============================================================

seasonal_period <- frequency(
  electricity_ts
)

seasonality_summary <- data.frame(
  Diagnostic = c(
    "Series frequency",
    "Seasonal period",
    "Seasonal interpretation",
    "ACF lag 12",
    "Balanced seasonal EDA period"
  ),

  Value = c(
    frequency(electricity_ts),
    seasonal_period,
    "Annual pattern in monthly data",
    round(
      get_acf_value(acf_original, 12),
      4
    ),
    "Jan 2018-Dec 2023"
  ),

  stringsAsFactors = FALSE
)

write.csv(
  seasonality_summary,
  "outputs/tables/seasonality_assessment_summary.csv",
  row.names = FALSE
)

cat("\nSEASONALITY ASSESSMENT SUMMARY\n")
print(seasonality_summary)


# ============================================================
# 15. SELECT COMMON TRAIN-TEST STRATEGY
#
#   Training:
#     January 2018 - June 2023
#     66 observations
#
#   Testing:
#     July 2023 - June 2024
#     12 observations
#
# This split is created ONCE and should be reused by all
# forecasting models to ensure fair model comparison.
# ============================================================

test_horizon <- seasonal_period

if (test_horizon != 12) {
  stop(
    paste0(
      "Expected monthly seasonal period of 12, but found ",
      test_horizon,
      "."
    )
  )
}

train_end  <- as.Date("2023-06-01")
test_start <- as.Date("2023-07-01")

train_data <- subset(
  electricity_total,
  date <= train_end
)

test_data <- subset(
  electricity_total,
  date >= test_start
)

if (nrow(train_data) != 66) {
  stop(
    paste0(
      "Training set should contain 66 observations, but found ",
      nrow(train_data),
      "."
    )
  )
}

if (nrow(test_data) != 12) {
  stop(
    paste0(
      "Testing set should contain 12 observations, but found ",
      nrow(test_data),
      "."
    )
  )
}

expected_test_dates <- seq(
  from = test_start,
  to = expected_end,
  by = "month"
)

if (!all(test_data$date == expected_test_dates)) {
  stop(
    "Testing period is incomplete or does not contain 12 consecutive months."
  )
}


# ============================================================
# 16. CREATE TRAINING AND TEST TIME-SERIES OBJECTS
# ============================================================

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

stopifnot(
  length(train_ts) == 66
)

stopifnot(
  length(test_ts) == 12
)

cat("\n============================================\n")
cat("COMMON TRAIN-TEST SPLIT\n")
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
  "Test horizon:",
  length(test_ts),
  "months = one complete seasonal cycle\n"
)


# ============================================================
# 17. EXPORT COMMON TRAIN-TEST DATASETS
# ============================================================

train_export <- train_data[
  , c(
    "date",
    "sector",
    "consumption"
  )
]

test_export <- test_data[
  , c(
    "date",
    "sector",
    "consumption"
  )
]

write.csv(
  train_export,
  train_path,
  row.names = FALSE
)

write.csv(
  test_export,
  test_path,
  row.names = FALSE
)


# Also export one combined dataset with an explicit split label.

model_split_data <- electricity_total[
  , c(
    "date",
    "sector",
    "consumption"
  )
]

model_split_data$Set <- ifelse(
  model_split_data$date <= train_end,
  "Train",
  "Test"
)

write.csv(
  model_split_data,
  split_path,
  row.names = FALSE
)


# ============================================================
# 18. FORMAL STATIONARITY ASSESSMENT: TRAINING SERIES
#
# IMPORTANT:
#   Stationarity assessment is performed AFTER the split
#   and uses TRAINING DATA ONLY.
#
#   The testing period must not influence preprocessing or
#   model-parameter decisions.
#
# ADF
#   H0 = unit root / non-stationary
#   p < 0.05 -> evidence against unit-root null
#
# KPSS-Level
#   H0 = level-stationary
#   p < 0.05 -> evidence against level stationarity
#
# KPSS-Trend
#   H0 = trend-stationary
#   p < 0.05 -> evidence against trend stationarity
# ============================================================

adf_train <- tseries::adf.test(
  train_ts,
  alternative = "stationary"
)

kpss_level_train <- tseries::kpss.test(
  train_ts,
  null = "Level"
)

kpss_trend_train <- tseries::kpss.test(
  train_ts,
  null = "Trend"
)

stationarity_original <- data.frame(
  Series = c(
    "Training series",
    "Training series",
    "Training series"
  ),

  Test = c(
    "ADF",
    "KPSS-Level",
    "KPSS-Trend"
  ),

  Null_Hypothesis = c(
    "Unit root / non-stationary",
    "Level-stationary",
    "Trend-stationary"
  ),

  Statistic = c(
    unname(adf_train$statistic),
    unname(kpss_level_train$statistic),
    unname(kpss_trend_train$statistic)
  ),

  P_Value = c(
    adf_train$p.value,
    kpss_level_train$p.value,
    kpss_trend_train$p.value
  ),

  stringsAsFactors = FALSE
)

cat("\nTRAINING-SERIES STATIONARITY TESTS\n")
print(stationarity_original)


# ============================================================
# 19. EVIDENCE-DRIVEN FIRST-DIFFERENCE ASSESSMENT
#
# Examine an ordinary first difference when either:
#
#   - ADF fails to reject unit root (p >= 0.05), OR
#   - KPSS-Level rejects level stationarity (p < 0.05).
#
# The purpose is to determine whether one ordinary
# difference appears sufficient to stabilise the level.
# ============================================================

needs_first_difference <- (
  adf_train$p.value >= 0.05 ||
    kpss_level_train$p.value < 0.05
)

stationarity_results <-
  stationarity_original


if (needs_first_difference) {

  cat(
    paste0(
      "\nTraining-series evidence indicates that ",
      "first differencing should be examined.\n"
    )
  )


  # ----------------------------------------------------------
  # 19.1 CREATE FIRST-DIFFERENCED TRAINING SERIES
  # ----------------------------------------------------------

  train_diff1 <- diff(
    train_ts,
    differences = 1
  )


  # ----------------------------------------------------------
  # 19.2 EXPORT FIRST-DIFFERENCED TRAINING VALUES
  # ----------------------------------------------------------

  first_diff_data <- data.frame(
    date =
      train_data$date[-1],

    first_difference =
      as.numeric(train_diff1)
  )

  write.csv(
    first_diff_data,
    paste0(
      "outputs/tables/",
      "first_differenced_training_series.csv"
    ),
    row.names = FALSE
  )


  # ----------------------------------------------------------
  # 19.3 FIRST-DIFFERENCED TRAINING PLOT
  # ----------------------------------------------------------

  png(
    "outputs/figures/05_first_differenced_training.png",
    width = 1200,
    height = 800,
    res = 150
  )

  plot(
    train_diff1,
    type = "l",
    ylab = "First Difference",
    xlab = "Year",
    main =
      "First-Differenced Training Series"
  )

  abline(
    h = 0,
    lty = 2
  )

  dev.off()


  # ----------------------------------------------------------
  # 19.4 ACF OF FIRST-DIFFERENCED TRAINING SERIES
  # ----------------------------------------------------------

  acf_first <- acf(
    train_diff1,
    lag.max = 36,
    plot = FALSE
  )

  png(
    paste0(
      "outputs/figures/",
      "06_acf_first_differenced_training.png"
    ),
    width = 1200,
    height = 800,
    res = 150
  )

  plot(
    acf_first,
    main =
      "ACF of First-Differenced Training Series"
  )

  dev.off()


  first_acf_selected <- data.frame(
    Lag = c(
      1,
      12,
      24,
      36
    ),

    ACF = c(
      get_acf_value(acf_first, 1),
      get_acf_value(acf_first, 12),
      get_acf_value(acf_first, 24),
      get_acf_value(acf_first, 36)
    )
  )

  write.csv(
    first_acf_selected,
    paste0(
      "outputs/tables/",
      "acf_first_difference_selected_lags.csv"
    ),
    row.names = FALSE
  )

  cat("\nSELECTED FIRST-DIFFERENCE ACF VALUES\n")
  print(first_acf_selected)


  # ----------------------------------------------------------
  # 19.5 RECHECK STATIONARITY AFTER FIRST DIFFERENCE
  # ----------------------------------------------------------

  adf_diff1 <- tseries::adf.test(
    train_diff1,
    alternative = "stationary"
  )

  kpss_diff1_level <- tseries::kpss.test(
    train_diff1,
    null = "Level"
  )

  stationarity_first <- data.frame(
    Series = c(
      "First-differenced training series",
      "First-differenced training series"
    ),

    Test = c(
      "ADF",
      "KPSS-Level"
    ),

    Null_Hypothesis = c(
      "Unit root / non-stationary",
      "Level-stationary"
    ),

    Statistic = c(
      unname(adf_diff1$statistic),
      unname(kpss_diff1_level$statistic)
    ),

    P_Value = c(
      adf_diff1$p.value,
      kpss_diff1_level$p.value
    ),

    stringsAsFactors = FALSE
  )

  stationarity_results <- rbind(
    stationarity_results,
    stationarity_first
  )

} else {

  cat(
    paste0(
      "\nTraining-series tests do not indicate ",
      "a need for first differencing.\n"
    )
  )
}


# ============================================================
# 20. EXPORT STATIONARITY RESULTS
# ============================================================

write.csv(
  stationarity_results,
  "outputs/tables/stationarity_test_results.csv",
  row.names = FALSE
)

cat("\nSTATIONARITY RESULTS\n")
print(stationarity_results)


# ============================================================
# 21. COMPACT EDA AND PREPROCESSING SUMMARY
# ============================================================

eda_summary <- data.frame(
  Diagnostic = c(
    "Full-series observations",
    "Full-series period",
    "Series frequency",
    "Balanced seasonal EDA period",
    "IQR outliers flagged",
    "Lag 1 ACF (full series)",
    "Lag 12 ACF (full series)",
    "Seasonal period selected",
    "Train-test strategy",
    "Training period",
    "Training observations",
    "Testing period",
    "Testing observations",
    "Testing seasonal cycles",
    "ADF p-value (training)",
    "KPSS-Level p-value (training)",
    "KPSS-Trend p-value (training)",
    "First-difference required"
  ),

  Value = c(
    length(electricity_ts),
    "Jan 2018-Jun 2024",
    frequency(electricity_ts),
    "Jan 2018-Dec 2023",
    nrow(outlier_candidates),

    round(
      get_acf_value(acf_original, 1),
      4
    ),

    round(
      get_acf_value(acf_original, 12),
      4
    ),

    seasonal_period,

    "One complete seasonal cycle reserved for testing",

    "Jan 2018-Jun 2023",
    length(train_ts),

    "Jul 2023-Jun 2024",
    length(test_ts),

    length(test_ts) /
      seasonal_period,

    round(
      adf_train$p.value,
      4
    ),

    round(
      kpss_level_train$p.value,
      4
    ),

    round(
      kpss_trend_train$p.value,
      4
    ),

    needs_first_difference
  ),

  stringsAsFactors = FALSE
)

write.csv(
  eda_summary,
  "outputs/tables/time_series_eda_summary.csv",
  row.names = FALSE
)

cat("\n============================================\n")
cat("EDA AND PREPROCESSING SUMMARY\n")
cat("============================================\n")

print(eda_summary)


# ============================================================
# 22. FINAL VERIFICATION
# ============================================================

cat("\n============================================\n")
cat("02 TIME-SERIES EDA + SPLIT COMPLETE\n")
cat("============================================\n")

cat(
  "Full series: Jan 2018-Jun 2024 (78 months)\n"
)

cat(
  "Seasonal frequency: 12 months\n"
)

cat(
  "Balanced seasonal EDA: Jan 2018-Dec 2023\n"
)

cat(
  "Training: Jan 2018-Jun 2023 (66 months)\n"
)

cat(
  "Testing : Jul 2023-Jun 2024 (12 months)\n"
)

cat(
  "Testing period covers one complete seasonal cycle.\n"
)

cat(
  "Training file:",
  train_path,
  "\n"
)

cat(
  "Testing file:",
  test_path,
  "\n"
)

cat(
  "Combined split file:",
  split_path,
  "\n"
)

cat(
  "First-difference required:",
  needs_first_difference,
  "\n"
)

cat("============================================\n")