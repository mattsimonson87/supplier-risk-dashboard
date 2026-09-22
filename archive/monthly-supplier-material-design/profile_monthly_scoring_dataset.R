# Profile Monthly Supplier-Material Scoring Dataset
#
# Reviews data quality, missingness, target distribution, and scoring
# coverage before model development.

library(dplyr)
library(readr)

scoring_file <- file.path(
  "data",
  "processed",
  "monthly_scoring_dataset.csv"
)

if (!file.exists(scoring_file)) {
  stop(
    "monthly_scoring_dataset.csv was not found. ",
    "Run R/build_monthly_scoring_dataset.R first."
  )
}

scoring_data <- read_csv(
  scoring_file,
  show_col_types = FALSE
)

scoring_data <- scoring_data %>%
  mutate(
    scoring_date = as.Date(scoring_date),
    prediction_end_date = as.Date(prediction_end_date)
  )

# Overall dataset summary
overall_summary <- scoring_data %>%
  summarize(
    scoring_records = n(),
    scoring_dates = n_distinct(scoring_date),
    suppliers = n_distinct(supplier_id),
    materials = n_distinct(material_id),
    supplier_material_relationships =
      n_distinct(supplier_material_id),
    positive_targets = sum(late_delivery_target),
    target_rate = mean(late_delivery_target),
    first_scoring_date = min(scoring_date),
    last_scoring_date = max(scoring_date)
  )

# Missingness profile
missingness_summary <- scoring_data %>%
  summarize(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "field",
    values_to = "missing_count"
  ) %>%
  mutate(
    missing_percent =
      missing_count / nrow(scoring_data)
  ) %>%
  arrange(
    desc(missing_percent),
    field
  )

# Target rate by scoring month
monthly_target_summary <- scoring_data %>%
  group_by(scoring_date) %>%
  summarize(
    scoring_records = n(),
    positive_targets = sum(late_delivery_target),
    target_rate = mean(late_delivery_target),
    .groups = "drop"
  )

# Coverage by supplier-material relationship
relationship_coverage <- scoring_data %>%
  group_by(
    supplier_material_id,
    supplier_id,
    supplier_name,
    material_id,
    material_name
  ) %>%
  summarize(
    scoring_records = n(),
    positive_targets = sum(late_delivery_target),
    target_rate = mean(late_delivery_target),
    first_scoring_date = min(scoring_date),
    last_scoring_date = max(scoring_date),
    .groups = "drop"
  ) %>%
  arrange(
    scoring_records,
    supplier_material_id
  )

# Distribution of completed historical orders
history_coverage <- scoring_data %>%
  summarize(
    records_with_no_90d_history =
      sum(completed_order_count_90d == 0),
    records_with_no_180d_history =
      sum(completed_order_count_180d == 0),
    records_with_no_365d_history =
      sum(completed_order_count_365d == 0),
    median_orders_90d =
      median(completed_order_count_90d),
    median_orders_180d =
      median(completed_order_count_180d),
    median_orders_365d =
      median(completed_order_count_365d),
    maximum_orders_180d =
      max(completed_order_count_180d)
  )

# Confirm target-definition consistency
target_validation <- scoring_data %>%
  summarize(
    invalid_target_rows = sum(
      late_delivery_target !=
        as.integer(late_quantity_rate_target > 0.20)
    ),
    invalid_late_quantity_rows = sum(
      late_quantity_next_30d >
        quantity_due_next_30d
    ),
    invalid_prediction_windows = sum(
      prediction_end_date !=
        scoring_date + 30
    )
  )

# Target-only fields that must be excluded from model predictors.
target_only_fields <- c(
  "late_quantity_next_30d",
  "late_quantity_rate_target",
  "late_delivery_target"
)

# Fields that identify records but should not be direct model predictors.
identifier_fields <- c(
  "scoring_record_id",
  "supplier_material_id",
  "supplier_id",
  "supplier_name",
  "material_id",
  "material_name"
)

# Print results
cat("\nOVERALL DATASET SUMMARY\n")
print(overall_summary)

cat("\nTARGET VALIDATION\n")
print(target_validation)

cat("\nHISTORICAL COVERAGE\n")
print(history_coverage)

cat("\nFIELDS WITH MISSING VALUES\n")
print(
  missingness_summary %>%
    filter(missing_count > 0)
)

cat("\nMONTHLY TARGET SUMMARY\n")
print(
  monthly_target_summary,
  n = Inf
)

cat("\nLOWEST RELATIONSHIP COVERAGE\n")
print(
  head(
    relationship_coverage,
    15
  )
)

cat("\nTARGET-ONLY FIELDS TO EXCLUDE FROM PREDICTORS\n")
print(target_only_fields)

cat("\nIDENTIFIER FIELDS TO EXCLUDE OR HANDLE CAREFULLY\n")
print(identifier_fields)

# Required validation checks
stopifnot(
  target_validation$invalid_target_rows == 0,
  target_validation$invalid_late_quantity_rows == 0,
  target_validation$invalid_prediction_windows == 0,
  overall_summary$scoring_records == nrow(scoring_data),
  overall_summary$target_rate > 0.05,
  overall_summary$target_rate < 0.40
)

message(
  "Monthly scoring dataset profile completed successfully."
)