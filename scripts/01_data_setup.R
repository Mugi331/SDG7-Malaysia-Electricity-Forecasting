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
# 2. IMPORT RAW DATA
# ============================================================

raw_path <- "data/raw/electricity_consumption.csv"

if (!file.exists(raw_path)) {
  stop(
    paste0(
      "Raw data file not found: ", raw_path,
      "\nCheck the project working directory and data/raw folder."
    )
  )
}

electricity_raw <- read.csv(
  raw_path,
  stringsAsFactors = FALSE
)

cat("\nRaw data dimensions:",
    nrow(electricity_raw), "rows x",
    ncol(electricity_raw), "columns\n")


# ============================================================
# 3. VALIDATE REQUIRED VARIABLES
# ============================================================

required_columns <- c("date", "sector", "consumption")
missing_columns <- setdiff(required_columns, names(electricity_raw))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required column(s):",
      paste(missing_columns, collapse = ", ")
    )
  )
}


# ============================================================
# 4. CONVERT DATA TYPES
# ============================================================

electricity_raw$date <- as.Date(electricity_raw$date)
electricity_raw$consumption <- as.numeric(electricity_raw$consumption)

if (any(is.na(electricity_raw$date))) {
  stop("At least one date could not be converted to Date format.")
}


# ============================================================
# 5. DEFINE STUDY PERIOD AND EXTRACT TOTAL SECTOR
# ============================================================

study_start <- as.Date("2018-01-01")
study_end <- as.Date("2024-06-01")

electricity_total <- subset(
  electricity_raw,
  date >= study_start &
    date <= study_end &
    sector == "total"
)

# Keep only variables required for the univariate time series.
electricity_total <- electricity_total[
  , c("date", "sector", "consumption")
]


# ============================================================
# 6. SORT CHRONOLOGICALLY
# ============================================================

electricity_total <- electricity_total[
  order(electricity_total$date),
]

row.names(electricity_total) <- NULL


# ============================================================
# 7. DATA-QUALITY AND MONTHLY-COMPLETENESS CHECKS
# ============================================================

missing_values <- sum(
  is.na(electricity_total[, c("date", "consumption")])
)

exact_duplicate_rows <- sum(duplicated(electricity_total))
duplicate_months <- sum(duplicated(electricity_total$date))

expected_months <- seq(
  from = study_start,
  to = study_end,
  by = "month"
)

missing_months <- expected_months[
  !(expected_months %in% electricity_total$date)
]

unexpected_months <- electricity_total$date[
  !(electricity_total$date %in% expected_months)
]

quality_summary <- data.frame(
  Check = c(
    "Missing values in date/consumption",
    "Exact duplicate rows",
    "Duplicate monthly observations",
    "Missing expected months",
    "Unexpected months",
    "Final observations retained"
  ),
  Result = c(
    missing_values,
    exact_duplicate_rows,
    duplicate_months,
    length(missing_months),
    length(unexpected_months),
    nrow(electricity_total)
  )
)

print(quality_summary)

# Stop the pipeline if the final monthly series is not valid.
if (
  missing_values > 0 ||
    exact_duplicate_rows > 0 ||
    duplicate_months > 0 ||
    length(missing_months) > 0 ||
    length(unexpected_months) > 0
) {
  stop(
    "Data-quality validation failed. Review the checks above before continuing."
  )
}


# ============================================================
# 8. OVERALL DESCRIPTIVE STATISTICS
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
    format(min(electricity_total$date), "%b %Y"),
    format(max(electricity_total$date), "%b %Y"),
    "Monthly"
  ),
  stringsAsFactors = FALSE
)

print(descriptive_summary)


# ============================================================
# 9. EXPORT CLEAN DATA AND SUMMARY TABLES
# ============================================================

processed_path <-
  "data/processed/electricity_total_2018_2024.csv"

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
# 10. FINAL VERIFICATION
# ============================================================

cat("\n============================================\n")
cat("01 DATA SETUP COMPLETE\n")
cat("============================================\n")
cat("Study period:",
    format(min(electricity_total$date), "%b %Y"), "to",
    format(max(electricity_total$date), "%b %Y"), "\n")
cat("Observations:", nrow(electricity_total), "\n")
cat("Clean dataset saved to:", processed_path, "\n")
cat("============================================\n")