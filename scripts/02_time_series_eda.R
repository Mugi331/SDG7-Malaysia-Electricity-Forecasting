# ============================================================
# 02_time_series_eda.R
# Exploratory Time-Series Analysis + Common Train-Test Split
#
# Full observed period:
#   January 2018 - June 2024
#
# PURPOSE
#   1. Load and verify the clean monthly series created by
#      01_data_setup.R.
#   2. Screen unusual observations without automatically
#      removing them.
#   3. Examine the original series in terms of:
#        - overall level
#        - variability
#        - annual seasonality
#        - trend
#        - irregular behaviour
#        - autocorrelation
#   4. Determine the seasonal period supported by EDA.
#   5. Create one common chronological train-test split for
#      all forecasting methods.
#   6. Export shared training and testing datasets.
#
# ============================================================


# ============================================================
# 0. DEFINE FILE PATHS
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
# 1. CREATE OUTPUT DIRECTORIES
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
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


electricity_total$date <- as.Date(
  electricity_total$date
)


electricity_total$consumption <-
  suppressWarnings(
    as.numeric(
      electricity_total$consumption
    )
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
# 3. VERIFY EXPECTED MONTHLY SERIES
# ============================================================

expected_start <- as.Date("2018-01-01")
expected_end   <- as.Date("2024-06-01")


expected_months <- seq(
  from = expected_start,
  to = expected_end,
  by = "month"
)


if (
  nrow(electricity_total) !=
    length(expected_months)
) {

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


if (
  !all(
    electricity_total$date ==
      expected_months
  )
) {

  stop(
    paste0(
      "Processed monthly dates are incomplete ",
      "or misaligned."
    )
  )
}


if (
  !all(
    electricity_total$sector == "total"
  )
) {

  stop(
    paste0(
      "Processed dataset contains observations ",
      "outside sector = 'total'."
    )
  )
}


# ============================================================
# 4. CREATE FULL MONTHLY TIME-SERIES OBJECT
# ============================================================

seasonal_period <- 12


electricity_ts <- ts(
  electricity_total$consumption,
  start = c(2018, 1),
  frequency = seasonal_period
)


stopifnot(
  length(electricity_ts) == 78
)


cat("\n============================================\n")
cat("02 TIME-SERIES EDA\n")
cat("============================================\n")


cat(
  "Observed period:",
  format(
    min(electricity_total$date),
    "%b %Y"
  ),
  "to",
  format(
    max(electricity_total$date),
    "%b %Y"
  ),
  "\n"
)


cat(
  "Observations:",
  length(electricity_ts),
  "\n"
)


cat(
  "Monthly frequency:",
  frequency(electricity_ts),
  "\n"
)


# ============================================================
# STAGE 1: DESCRIPTIVE AND QUALITY-RELATED EDA
# ============================================================


# ============================================================
# 5. OVERALL DESCRIPTIVE SUMMARY
# ============================================================

overall_summary <- data.frame(

  Statistic = c(
    "Observations",
    "Mean",
    "Median",
    "Minimum",
    "Maximum",
    "Standard deviation"
  ),

  Value = c(
    length(electricity_ts),

    mean(
      electricity_total$consumption
    ),

    median(
      electricity_total$consumption
    ),

    min(
      electricity_total$consumption
    ),

    max(
      electricity_total$consumption
    ),

    sd(
      electricity_total$consumption
    )
  ),

  stringsAsFactors = FALSE
)


write.csv(
  overall_summary,
  "outputs/tables/overall_descriptive_summary.csv",
  row.names = FALSE
)


cat("\nOVERALL DESCRIPTIVE SUMMARY\n")
print(overall_summary)


# ============================================================
# 6. OUTLIER SCREENING USING IQR RULE
#
# Lower bound = Q1 - 1.5 * IQR
# Upper bound = Q3 + 1.5 * IQR
#
# Flagged observations are inspected but are NOT
# automatically removed.
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


lower_bound <-
  q1 - 1.5 * iqr_value


upper_bound <-
  q3 + 1.5 * iqr_value


outlier_flag <- (
  electricity_total$consumption <
    lower_bound |
    electricity_total$consumption >
      upper_bound
)


outlier_candidates <-
  electricity_total[
    outlier_flag,
    c(
      "date",
      "consumption"
    )
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


if (
  nrow(outlier_candidates) > 0
) {

  cat("\nFLAGGED OBSERVATIONS\n")
  print(outlier_candidates)
}


# ============================================================
# STAGE 2: LEVEL AND VARIABILITY
# ============================================================


# ============================================================
# 7. ORIGINAL TIME-SERIES PLOT
#
# Purpose:
#   Examine:
#     - overall level
#     - long-term movement
#     - variability
#     - possible seasonal behaviour
#     - unusual observations
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

  ylab =
    "Electricity Consumption",

  xlab =
    "Year",

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
#   Examine whether the mean level changes over time and
#   whether variability increases substantially together
#   with the level.
#
# Relatively stable variability supports retaining the
# original scale rather than applying a common log/Box-Cox
# transformation during EDA.
# ============================================================

electricity_total$Year <-
  as.integer(
    format(
      electricity_total$date,
      "%Y"
    )
  )


summarise_by_year <- function(fun) {

  aggregate(
    consumption ~ Year,
    data = electricity_total,
    FUN = fun
  )

}


yearly_mean <-
  summarise_by_year(mean)

yearly_sd <-
  summarise_by_year(sd)

yearly_min <-
  summarise_by_year(min)

yearly_max <-
  summarise_by_year(max)

yearly_n <-
  summarise_by_year(length)


yearly_summary <- data.frame(

  Year =
    yearly_mean$Year,

  Observations =
    yearly_n$consumption,

  Mean =
    yearly_mean$consumption,

  SD =
    yearly_sd$consumption,

  Min =
    yearly_min$consumption,

  Max =
    yearly_max$consumption
)


write.csv(
  yearly_summary,
  "outputs/tables/yearly_level_variability_summary.csv",
  row.names = FALSE
)


cat("\nYEARLY LEVEL AND VARIABILITY SUMMARY\n")
print(yearly_summary)


# ============================================================
# STAGE 3: SEASONALITY ASSESSMENT
# ============================================================


# ============================================================
# 9. DEFINE COMPLETE CALENDAR YEARS
#
# The complete observed dataset ends in June 2024.
#
# Including 2024 in calendar-month comparisons would give
# January-June one extra observation relative to
# July-December.
#
# Therefore, balanced seasonal EDA uses:
#   January 2018 - December 2023
#   = six complete years
# ============================================================

complete_year_data <- subset(
  electricity_total,

  date >= as.Date("2018-01-01") &
    date <= as.Date("2023-12-01")
)


if (
  nrow(complete_year_data) != 72
) {

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
  frequency = seasonal_period
)


# ============================================================
# 10. MONTHLY SEASONAL PLOT
#
# Purpose:
#   Examine whether similar calendar months display
#   recurring behaviour across years.
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

  ylab =
    "Electricity Consumption",

  xlab =
    "Month",

  main = paste0(
    "Monthly Seasonal Pattern of Electricity Consumption, ",
    "2018-2023"
  )
)


dev.off()


# ============================================================
# 11. BALANCED MONTHLY DESCRIPTIVE SUMMARY
# ============================================================

complete_year_data$Month_Number <-
  as.integer(
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


monthly_mean <-
  summarise_by_month(mean)

monthly_sd <-
  summarise_by_month(sd)

monthly_min <-
  summarise_by_month(min)

monthly_max <-
  summarise_by_month(max)

monthly_n <-
  summarise_by_month(length)


monthly_summary <- data.frame(

  Month =
    month.abb[
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
#     - irregular/remainder
#
# Additive decomposition is used because seasonal
# fluctuations do not appear to increase strongly in
# proportion to the overall series level.
# ============================================================

electricity_decomp <- decompose(
  complete_year_ts,
  type = "additive"
)


png(
  paste0(
    "outputs/figures/",
    "03_additive_decomposition_2018_2023.png"
  ),
  width = 1200,
  height = 1000,
  res = 150
)


plot(
  electricity_decomp
)


dev.off()


# ============================================================
# 13. ACF OF ORIGINAL SERIES
#
# Purpose:
#   Examine serial dependence and support the assessment
#   of annual seasonality.
#
# Monthly data:
#   lag 12 = 1 year
#   lag 24 = 2 years
#   lag 36 = 3 years
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
    get_acf_value(
      acf_original,
      1
    ),

    get_acf_value(
      acf_original,
      12
    ),

    get_acf_value(
      acf_original,
      24
    ),

    get_acf_value(
      acf_original,
      36
    )
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
# 14. SUMMARISE EVIDENCE OF SEASONALITY
#
# Annual seasonality is assessed using several pieces of
# descriptive evidence rather than frequency alone:
#
#   1. Original time-series plot
#   2. Monthly seasonal plot
#   3. Balanced monthly descriptive statistics
#   4. Additive decomposition
#   5. ACF at seasonal lag 12
#
# Together, these support recurring annual behaviour in
# monthly electricity consumption.
#
# Therefore:
#   seasonal period m = 12
#
# IMPORTANT:
#   This establishes the existence of annual seasonality.
#   It does NOT determine the SARIMA seasonal differencing
#   order D. That decision is made later in 04_sarima.R.
# ============================================================

seasonality_summary <- data.frame(

  Diagnostic = c(
    "Series frequency",
    "Seasonal period",
    "Seasonal interpretation",
    "ACF lag 12",
    "Balanced seasonal EDA period",
    "Seasonality conclusion"
  ),

  Value = c(
    frequency(electricity_ts),

    seasonal_period,

    "Annual pattern in monthly data",

    round(
      get_acf_value(
        acf_original,
        12
      ),
      4
    ),

    "Jan 2018-Dec 2023",

    "Annual seasonality supported"
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
# STAGE 4: COMMON TRAIN-TEST SPLIT
# ============================================================


# ============================================================
# 15. DEFINE COMMON TRAIN-TEST STRATEGY
#
# Annual seasonality has been identified with:
#   m = 12
#
# Therefore, one complete annual cycle is reserved for
# out-of-sample evaluation.
#
# Training:
#   January 2018 - June 2023
#   66 observations
#
# Testing:
#   July 2023 - June 2024
#   12 observations
#
# The same split is reused by every forecasting model to
# ensure fair out-of-sample comparison.
# ============================================================

test_horizon <- seasonal_period


if (
  test_horizon != 12
) {

  stop(
    paste0(
      "Expected monthly seasonal period of 12, but found ",
      test_horizon,
      "."
    )
  )
}


train_end <-
  as.Date("2023-06-01")


test_start <-
  as.Date("2023-07-01")


train_data <- subset(
  electricity_total,
  date <= train_end
)


test_data <- subset(
  electricity_total,
  date >= test_start
)


if (
  nrow(train_data) != 66
) {

  stop(
    paste0(
      "Training set should contain 66 observations, but found ",
      nrow(train_data),
      "."
    )
  )
}


if (
  nrow(test_data) != 12
) {

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


if (
  !all(
    test_data$date ==
      expected_test_dates
  )
) {

  stop(
    paste0(
      "Testing period is incomplete or does not contain ",
      "12 consecutive months."
    )
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
  format(
    min(train_data$date),
    "%b %Y"
  ),
  "to",
  format(
    max(train_data$date),
    "%b %Y"
  ),
  "-",
  length(train_ts),
  "observations\n"
)


cat(
  "Testing :",
  format(
    min(test_data$date),
    "%b %Y"
  ),
  "to",
  format(
    max(test_data$date),
    "%b %Y"
  ),
  "-",
  length(test_ts),
  "observations\n"
)


cat(
  "Test horizon:",
  length(test_ts),
  "months = one complete annual seasonal cycle\n"
)


# ============================================================
# 17. EXPORT COMMON TRAIN-TEST DATASETS
# ============================================================

train_export <- train_data[
  ,
  c(
    "date",
    "sector",
    "consumption"
  )
]


test_export <- test_data[
  ,
  c(
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


# Combined source with explicit Train/Test label.

model_split_data <- electricity_total[
  ,
  c(
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
# 18. EXPORT TRAIN-TEST SUMMARY
# ============================================================

split_summary <- data.frame(

  Dataset = c(
    "Training",
    "Testing",
    "Total"
  ),

  Period = c(
    "Jan 2018-Jun 2023",
    "Jul 2023-Jun 2024",
    "Jan 2018-Jun 2024"
  ),

  Observations = c(
    length(train_ts),
    length(test_ts),
    length(electricity_ts)
  ),

  stringsAsFactors = FALSE
)


write.csv(
  split_summary,
  "outputs/tables/train_test_split_summary.csv",
  row.names = FALSE
)


# ============================================================
# 19. COMPACT EDA SUMMARY
#
# This summary contains only COMMON descriptive evidence.
#
# Model-specific preprocessing decisions such as:
#   - D
#   - d
#   - p
#   - q
#   - P
#   - Q
#
# are intentionally excluded.
# ============================================================

eda_summary <- data.frame(

  Diagnostic = c(
    "Full-series observations",
    "Full-series period",
    "Monthly frequency",
    "Balanced seasonal EDA period",
    "IQR outliers flagged",
    "ACF lag 1",
    "ACF lag 12",
    "Seasonal period",
    "Seasonality conclusion",
    "Decomposition form",
    "Common transformation",
    "Train-test strategy",
    "Training period",
    "Training observations",
    "Testing period",
    "Testing observations",
    "Testing seasonal cycles"
  ),

  Value = c(
    length(electricity_ts),

    "Jan 2018-Jun 2024",

    frequency(electricity_ts),

    "Jan 2018-Dec 2023",

    nrow(outlier_candidates),

    round(
      get_acf_value(
        acf_original,
        1
      ),
      4
    ),

    round(
      get_acf_value(
        acf_original,
        12
      ),
      4
    ),

    seasonal_period,

    "Annual seasonality supported",

    "Additive",

    "None during common EDA",

    "One complete seasonal cycle reserved for testing",

    "Jan 2018-Jun 2023",

    length(train_ts),

    "Jul 2023-Jun 2024",

    length(test_ts),

    length(test_ts) /
      seasonal_period
  ),

  stringsAsFactors = FALSE
)


write.csv(
  eda_summary,
  "outputs/tables/time_series_eda_summary.csv",
  row.names = FALSE
)


cat("\n============================================\n")
cat("EDA SUMMARY\n")
cat("============================================\n")

print(eda_summary)


# ============================================================
# 20. FINAL VERIFICATION
# ============================================================

cat("\n============================================\n")
cat("02 TIME-SERIES EDA + SPLIT COMPLETE\n")
cat("============================================\n")


cat(
  "Full series : Jan 2018-Jun 2024 (78 months)\n"
)


cat(
  "Seasonality : annual pattern, m = 12\n"
)


cat(
  "Decomposition: additive\n"
)


cat(
  "Balanced seasonal EDA: Jan 2018-Dec 2023\n"
)


cat(
  "Training    : Jan 2018-Jun 2023 (66 months)\n"
)


cat(
  "Testing     : Jul 2023-Jun 2024 (12 months)\n"
)


cat(
  "Test set covers one complete seasonal cycle.\n"
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
  paste0(
    "No stationarity or differencing decision is made ",
    "in Script 02.\n"
  )
)


cat(
  paste0(
    "SARIMA-specific preprocessing continues in ",
    "04_sarima.R.\n"
  )
)


cat("============================================\n")