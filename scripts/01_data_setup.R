# ============================================================
# 01_data_setup.R
# Data Import, Cleaning, Validation, and Processed Data Export
#
# Study:
#   Monthly Electricity Consumption in Malaysia
#
# Study period:
#   January 2018 - June 2024
#
# Target series:
#   Sector = "total"
#
# PURPOSE
#   1. Import the original electricity-consumption dataset.
#   2. Validate required variables and convert data types.
#   3. Extract Malaysia's total monthly electricity consumption.
#   4. Restrict observations to January 2018-June 2024.
#   5. Check missing values, duplicates, and monthly completeness.
#   6. Produce basic descriptive statistics.
#   7. Export one clean canonical dataset for all later scripts.
#
# IMPORTANT
#   - No outliers are removed here.
#   - No transformation or differencing is performed here.
#   - No train-test split is created here.
#   - Later scripts must read the processed dataset produced here.
# ============================================================


# ============================================================
# 1. CREATE REQUIRED DIRECTORIES
# ============================================================

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. DEFINE FILE PATHS
# ============================================================

raw_path <- "data/raw/electricity_consumption.csv"

processed_path <-
  "data/processed/electricity_total_2018_2024.csv"


# ============================================================
# 3. IMPORT RAW DATA
# ============================================================

if (!file.exists(raw_path)) {
  stop(
    paste0(
      "Raw data file not found: ", raw_path,
      "\nCheck that the project working directory is correct ",
      "and that the file exists inside data/raw/."
    )
  )
}

electricity_raw <- read.csv(
  raw_path,
  stringsAsFactors = FALSE
)

cat("\n============================================\n")
cat("RAW DATA IMPORTED\n")
cat("============================================\n")

cat(
  "Raw dimensions:",
  nrow(electricity_raw), "rows x",
  ncol(electricity_raw), "columns\n"
)


# ============================================================
# 4. VALIDATE REQUIRED VARIABLES
# ============================================================

required_columns <- c(
  "date",
  "sector",
  "consumption"
)

missing_columns <- setdiff(
  required_columns,
  names(electricity_raw)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required column(s):",
      paste(missing_columns, collapse = ", ")
    )
  )
}


# ============================================================
# 5. CONVERT DATA TYPES
# ============================================================

electricity_raw$date <- as.Date(
  electricity_raw$date
)

electricity_raw$consumption <- suppressWarnings(
  as.numeric(electricity_raw$consumption)
)

# Stop if date conversion failed.
if (any(is.na(electricity_raw$date))) {
  stop(
    "At least one date could not be converted to Date format."
  )
}

# Stop if consumption contains invalid numeric values.
if (any(is.na(electricity_raw$consumption))) {
  stop(
    paste0(
      "At least one consumption value is missing or ",
      "could not be converted to numeric format."
    )
  )
}


# ============================================================
# 6. DEFINE STUDY PERIOD
# ============================================================

study_start <- as.Date("2018-01-01")
study_end   <- as.Date("2024-06-01")

expected_months <- seq(
  from = study_start,
  to = study_end,
  by = "month"
)

expected_observations <- length(expected_months)


# ============================================================
# 7. EXTRACT TOTAL-SECTOR MONTHLY SERIES
# ============================================================

electricity_total <- subset(
  electricity_raw,
  date >= study_start &
    date <= study_end &
    sector == "total"
)

# Keep only variables required for the univariate
# electricity-consumption time series.
electricity_total <- electricity_total[
  , c(
    "date",
    "sector",
    "consumption"
  )
]

if (nrow(electricity_total) == 0) {
  stop(
    paste0(
      "No observations were found for sector = 'total' ",
      "within January 2018-June 2024."
    )
  )
}


# ============================================================
# 8. SORT CHRONOLOGICALLY
# ============================================================

electricity_total <- electricity_total[
  order(electricity_total$date),
]

row.names(electricity_total) <- NULL


# ============================================================
# 9. DATA-QUALITY AND MONTHLY-COMPLETENESS CHECKS
# ============================================================

missing_values <- sum(
  is.na(
    electricity_total[
      , c("date", "consumption")
    ]
  )
)

exact_duplicate_rows <- sum(
  duplicated(electricity_total)
)

duplicate_months <- sum(
  duplicated(electricity_total$date)
)

missing_months <- expected_months[
  !(expected_months %in% electricity_total$date)
]

unexpected_months <- electricity_total$date[
  !(electricity_total$date %in% expected_months)
]

# Verify that all observations occur at the beginning
# of their corresponding month.
month_start_check <- all(
  format(electricity_total$date, "%d") == "01"
)

quality_summary <- data.frame(
  Check = c(
    "Missing values in date/consumption",
    "Exact duplicate rows",
    "Duplicate monthly observations",
    "Missing expected months",
    "Unexpected months",
    "Dates aligned to first day of month",
    "Expected observations",
    "Final observations retained"
  ),

  Result = c(
    missing_values,
    exact_duplicate_rows,
    duplicate_months,
    length(missing_months),
    length(unexpected_months),
    month_start_check,
    expected_observations,
    nrow(electricity_total)
  ),

  stringsAsFactors = FALSE
)

cat("\n============================================\n")
cat("DATA-QUALITY CHECKS\n")
cat("============================================\n")

print(quality_summary)


# ============================================================
# 10. STOP PIPELINE IF VALIDATION FAILS
# ============================================================

if (
  missing_values > 0 ||
    exact_duplicate_rows > 0 ||
    duplicate_months > 0 ||
    length(missing_months) > 0 ||
    length(unexpected_months) > 0 ||
    !month_start_check ||
    nrow(electricity_total) != expected_observations
) {

  if (length(missing_months) > 0) {
    cat("\nMissing month(s):\n")
    print(missing_months)
  }

  if (length(unexpected_months) > 0) {
    cat("\nUnexpected month(s):\n")
    print(unexpected_months)
  }

  stop(
    paste0(
      "Data-quality validation failed. ",
      "Review the checks above before continuing."
    )
  )
}


# ============================================================
# 11. OVERALL DESCRIPTIVE STATISTICS
# ============================================================

descriptive_summary <- data.frame(
  Statistic = c(
    "Number of observations",
    "Mean",
    "Median",
    "Minimum",
    "Maximum",
    "Standard deviation",
    "Study start",
    "Study end",
    "Data frequency"
  ),

  Value = c(
    nrow(electricity_total),
    mean(electricity_total$consumption),
    median(electricity_total$consumption),
    min(electricity_total$consumption),
    max(electricity_total$consumption),
    sd(electricity_total$consumption),
    format(
      min(electricity_total$date),
      "%b %Y"
    ),
    format(
      max(electricity_total$date),
      "%b %Y"
    ),
    "Monthly"
  ),

  stringsAsFactors = FALSE
)

cat("\n============================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("============================================\n")

print(descriptive_summary)


# ============================================================
# 12. EXPORT CLEAN DATA AND SUMMARY TABLES
# ============================================================

write.csv(
  electricity_total,
  processed_path,
  row.names = FALSE
)

write.csv(
  quality_summary,
  "outputs/tables/data_quality_summary.csv",
  row.names = FALSE
)

write.csv(
  descriptive_summary,
  "outputs/tables/descriptive_statistics.csv",
  row.names = FALSE
)


# ============================================================
# 13. VERIFY EXPORTED PROCESSED DATA
# ============================================================

if (!file.exists(processed_path)) {
  stop(
    paste0(
      "Processed dataset was not successfully created: ",
      processed_path
    )
  )
}

processed_check <- read.csv(
  processed_path,
  stringsAsFactors = FALSE
)

if (nrow(processed_check) != expected_observations) {
  stop(
    paste0(
      "Processed dataset verification failed. Expected ",
      expected_observations,
      " rows but found ",
      nrow(processed_check),
      "."
    )
  )
}


# ============================================================
# 14. FINAL VERIFICATION
# ============================================================

cat("\n============================================\n")
cat("01 DATA SETUP COMPLETE\n")
cat("============================================\n")

cat(
  "Study period:",
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
  "Sector:",
  unique(electricity_total$sector),
  "\n"
)

cat(
  "Frequency: Monthly (12 observations per year)\n"
)

cat(
  "Expected observations:",
  expected_observations,
  "\n"
)

cat(
  "Final observations:",
  nrow(electricity_total),
  "\n"
)

cat(
  "Processed dataset saved to:",
  processed_path,
  "\n"
)

cat("============================================\n")